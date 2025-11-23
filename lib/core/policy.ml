open Domain

(* デフォルトのアップロードポリシー *)
type upload_policy = {
  max_size : int; (* bytes *)
  allowed_mime_types : string list; (* 空リストの場合は全て許可 *)
}

let default_policy = {
  max_size = 100 * 1024 * 1024; (* 100MB *)
  allowed_mime_types = []; (* 全て許可 *)
}

let check_size ~policy size =
  if size < 0 then Error (Invalid_size size)
  else if size > policy.max_size then
    Error (Storage_error (Printf.sprintf "File too large: %d bytes (max: %d)" size policy.max_size))
  else Ok ()

let check_mime_type ~policy mime =
  if String.length mime = 0 then
    Error (Storage_error "Empty MIME type")
  else if policy.allowed_mime_types = [] then
    Ok () (* 空リストの場合は全て許可 *)
  else if List.mem mime policy.allowed_mime_types then
    Ok ()
  else
    Error (Storage_error (Printf.sprintf "MIME type not allowed: %s" mime))

let check_upload_policy ~policy ~size ~mime =
  match check_size ~policy size with
  | Error e -> Error e
  | Ok () -> check_mime_type ~policy mime
