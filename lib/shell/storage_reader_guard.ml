(** Reader-guarded Storage adapter

    This adapter delays unlink operations while reads are in progress.
    Useful for filesystems (e.g., CIFS/Azure Files mounts) where deleting
    a file while it is being read causes errors (e.g., SharingViolation).

    Internally maintains a reference-counted in-flight map to track
    files currently being read via get. *)

(** Tracks the number of active readers for a file *)
type reader_state = {
  count: int;
}

(** Reader-guarded Storage functor - wraps an existing Storage with read tracking. *)
module Make (Base : Storage_intf.S) : Storage_intf.S with type t = Base.t = struct
  type t = Base.t

  (** Hash table tracking files with active readers *)
  let inflight : (string, reader_state) Hashtbl.t = Hashtbl.create 64

  (** Mutex for synchronization *)
  let mutex = Eio.Mutex.create ()

  (** Condition variable for signaling when readers finish *)
  let condition = Eio.Condition.create ()

  (** Increment reader count for a file *)
  let increment_readers sha =
    Eio.Mutex.use_rw ~protect:true mutex (fun () ->
      let current = match Hashtbl.find_opt inflight sha with
        | Some state -> state.count
        | None -> 0
      in
      Hashtbl.replace inflight sha { count = current + 1 }
    )

  (** Decrement reader count; broadcast when count reaches zero *)
  let decrement_readers sha =
    Eio.Mutex.use_rw ~protect:true mutex (fun () ->
      match Hashtbl.find_opt inflight sha with
      | Some state ->
          let new_count = state.count - 1 in
          if new_count <= 0 then begin
            Hashtbl.remove inflight sha;
            Eio.Condition.broadcast condition
          end else
            Hashtbl.replace inflight sha { count = new_count }
      | None ->
          (* Already removed, nothing to do *)
          ()
    )

  (** Check if file has active readers (must be called with mutex held) *)
  let has_readers_for sha =
    match Hashtbl.find_opt inflight sha with
    | Some state -> state.count > 0
    | None -> false

  (** Wait until no readers remain for file.
      Blocks indefinitely until all readers finish. *)
  let wait_until_no_readers sha =
    Eio.Mutex.use_ro mutex (fun () ->
      while has_readers_for sha do
        Eio.Condition.await condition mutex
      done
    )

  let save storage ~body ~max_size =
    Base.save storage ~body ~max_size

  let get ~sw storage ~path ~size =
    (* Increment reader count *)
    increment_readers path;

    match Base.get ~sw storage ~path ~size with
    | Error e ->
        (* Decrement on error *)
        decrement_readers path;
        Error e
    | Ok body ->
        (* Wrap stream to decrement when complete *)
        let length = Piaf.Body.length body in
        let base_stream = Piaf.Body.to_string_stream body in
        let finished = ref false in

        let wrapped_stream, push = Piaf.Stream.create 2 in
        Eio.Fiber.fork ~sw (fun () ->
          Fun.protect
            ~finally:(fun () ->
              if not !finished then begin
                finished := true;
                decrement_readers path
              end
            )
            (fun () ->
              let rec forward () =
                match Piaf.Stream.take base_stream with
                | None ->
                    push None
                | Some chunk ->
                    push (Some chunk);
                    forward ()
              in
              forward ()
            )
        );
        Ok (Piaf.Body.of_string_stream ~length wrapped_stream)

  let exists storage ~path =
    Base.exists storage ~path

  let stat storage ~path =
    Base.stat storage ~path

  let unlink storage ~path =
    (* Wait indefinitely for active readers before unlinking *)
    wait_until_no_readers path;
    Base.unlink storage ~path

  let rename storage ~src ~dst =
    Base.rename storage ~src ~dst
end
