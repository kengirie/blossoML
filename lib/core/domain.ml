type blob_descriptor = {
  url : string;
  sha256 : string;
  size : int;
  mime_type : string;
  uploaded : int64;
}

type error =
  | Invalid_hash of string
  | Invalid_size of int
  | Blob_not_found of string
  | Storage_error of string
  | Payload_too_large of int * int  (* actual_size, max_size *)
  | Auth_error of string            (* authentication failure -> 401 *)
  | Forbidden of string             (* authorization failure -> 403 *)
  | Unsupported_media_type of string (* MIME type not allowed -> 415 *)
  | Invalid_content_type of string  (* malformed Content-Type -> 400 *)
