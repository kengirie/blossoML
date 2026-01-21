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
      let new_count = current + 1 in
      Eio.traceln "[ReaderGuard] increment_readers: %s -> count=%d" sha new_count;
      Hashtbl.replace inflight sha { count = new_count }
    )

  (** Decrement reader count; broadcast when count reaches zero *)
  let decrement_readers sha =
    Eio.traceln "[ReaderGuard] decrement_readers: %s (entering)" sha;
    Eio.Mutex.use_rw ~protect:true mutex (fun () ->
      match Hashtbl.find_opt inflight sha with
      | Some state ->
          let new_count = state.count - 1 in
          Eio.traceln "[ReaderGuard] decrement_readers: %s -> count=%d" sha new_count;
          if new_count <= 0 then begin
            Eio.traceln "[ReaderGuard] decrement_readers: %s -> removing and broadcasting" sha;
            Hashtbl.remove inflight sha;
            Eio.Condition.broadcast condition
          end else
            Hashtbl.replace inflight sha { count = new_count }
      | None ->
          Eio.traceln "[ReaderGuard] decrement_readers: %s -> not found (already removed)" sha;
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
    Eio.traceln "[ReaderGuard] wait_until_no_readers: %s (entering)" sha;
    Eio.Mutex.use_ro mutex (fun () ->
      let iter = ref 0 in
      while has_readers_for sha do
        incr iter;
        Eio.traceln "[ReaderGuard] wait_until_no_readers: %s -> waiting (iter=%d)" sha !iter;
        Eio.Condition.await condition mutex
      done;
      Eio.traceln "[ReaderGuard] wait_until_no_readers: %s -> done (total iters=%d)" sha !iter
    )

  let save storage ~body ~max_size =
    Base.save storage ~body ~max_size

  let get ~sw storage ~path ~size =
    Eio.traceln "[ReaderGuard] get: %s (start, size=%d)" path size;
    (* Increment reader count *)
    increment_readers path;

    Eio.traceln "[ReaderGuard] get: %s -> calling Base.get" path;
    match Base.get ~sw storage ~path ~size with
    | Error e ->
        Eio.traceln "[ReaderGuard] get: %s -> Base.get error, decrementing" path;
        (* Decrement on error *)
        decrement_readers path;
        Error e
    | Ok body ->
        Eio.traceln "[ReaderGuard] get: %s -> Base.get success, wrapping stream" path;
        (* Wrap stream to decrement when complete *)
        let length = Piaf.Body.length body in
        let base_stream = Piaf.Body.to_string_stream body in
        let finished = ref false in

        let wrapped_stream, push = Piaf.Stream.create 2 in
        Eio.Fiber.fork ~sw (fun () ->
          Eio.traceln "[ReaderGuard] get: %s -> forked fiber starting forward loop" path;
          Fun.protect
            ~finally:(fun () ->
              Eio.traceln "[ReaderGuard] get: %s -> fiber finally block (finished=%b)" path !finished;
              if not !finished then begin
                finished := true;
                decrement_readers path
              end
            )
            (fun () ->
              let chunk_count = ref 0 in
              let rec forward () =
                match Piaf.Stream.take base_stream with
                | None ->
                    Eio.traceln "[ReaderGuard] get: %s -> stream ended after %d chunks" path !chunk_count;
                    push None
                | Some chunk ->
                    incr chunk_count;
                    push (Some chunk);
                    forward ()
              in
              forward ()
            )
        );
        Eio.traceln "[ReaderGuard] get: %s -> returning wrapped body" path;
        Ok (Piaf.Body.of_string_stream ~length wrapped_stream)

  let exists storage ~path =
    Base.exists storage ~path

  let stat storage ~path =
    Base.stat storage ~path

  let unlink storage ~path =
    Eio.traceln "[ReaderGuard] unlink: %s (start)" path;
    (* Wait indefinitely for active readers before unlinking *)
    wait_until_no_readers path;
    Eio.traceln "[ReaderGuard] unlink: %s -> wait complete, calling Base.unlink" path;
    let result = Base.unlink storage ~path in
    Eio.traceln "[ReaderGuard] unlink: %s -> done (result=%s)" path
      (match result with Ok () -> "Ok" | Error _ -> "Error");
    result

  let rename storage ~src ~dst =
    Base.rename storage ~src ~dst
end
