open Blossom_core

(* Phase 1: Simple save (already exists) *)
let save ~dir ~data ~sha256 =
  let path = Eio.Path.(dir / sha256) in
  try
    Eio.Path.save ~create:(`Or_truncate 0o644) path data;
    Ok ()
  with exn ->
    Error (Domain.Storage_error (Printexc.to_string exn))

(* Phase 2: Streaming save with SHA256 calculation *)
let save_stream ~dir ~body =
  (* Digestif を使ってストリーミングでハッシュ計算 *)
  let ctx = ref (Digestif.SHA256.init ()) in
  let buffer = Buffer.create 4096 in

  (* ストリームからデータを読み込みながらハッシュ計算 *)
  let result = Piaf.Body.iter_string
    ~f:(fun chunk ->
      ctx := Digestif.SHA256.feed_string !ctx chunk;
      Buffer.add_string buffer chunk
    )
    body
  in

  match result with
  | Error e -> Error (Domain.Storage_error (Piaf.Error.to_string e))
  | Ok () ->
      let hash = Digestif.SHA256.(to_hex (get !ctx)) in
      let data = Buffer.contents buffer in
      let path = Eio.Path.(dir / hash) in

      (try
        Eio.Path.save ~create:(`Or_truncate 0o644) path data;
        Ok (hash, String.length data)
      with exn ->
        Error (Domain.Storage_error (Printexc.to_string exn)))

let get ~dir ~sha256 =
  let path = Eio.Path.(dir / sha256) in
  try
    let data = Eio.Path.load path in
    Ok data
  with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
      Error (Domain.Blob_not_found sha256)
  | exn ->
      Error (Domain.Storage_error (Printexc.to_string exn))

let exists ~dir ~sha256 =
  let path = Eio.Path.(dir / sha256) in
  try
    let _ = Eio.Path.load path in
    true
  with _ -> false
