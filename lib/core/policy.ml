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
    Error (Payload_too_large (size, policy.max_size))
  else Ok ()

let check_mime_type ~policy mime =
  match Content_type.normalize_media_type mime with
  | Error Content_type.Empty_input ->
      Error (Invalid_content_type "Empty MIME type")
  | Error (Content_type.Invalid_format msg) ->
      Error (Invalid_content_type (Printf.sprintf "Invalid Content-Type: %s" msg))
  | Ok normalized ->
      if policy.allowed_mime_types = [] then
        Ok () (* 空リストの場合は全て許可 *)
      else if List.mem normalized policy.allowed_mime_types then
        Ok ()
      else
        Error (Unsupported_media_type (Printf.sprintf "MIME type not allowed: %s" normalized))

let check_upload_policy ~policy ~size ~mime =
  match check_size ~policy size with
  | Error e -> Error e
  | Ok () -> check_mime_type ~policy mime
