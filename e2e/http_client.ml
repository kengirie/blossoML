(** HTTP client for E2E tests using Piaf. *)

module E2e_config = Config

open Piaf

type response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

let status_to_int status =
  match status with
  | `OK -> 200
  | `Created -> 201
  | `No_content -> 204
  | `Bad_request -> 400
  | `Unauthorized -> 401
  | `Forbidden -> 403
  | `Not_found -> 404
  | `Method_not_allowed -> 405
  | `Payload_too_large -> 413
  | `Unsupported_media_type -> 415
  | `Internal_server_error -> 500
  | `Code code -> code
  | _ -> Status.to_code status

let make_request_with_body ~sw ~env ~meth ~url ?(headers=[]) ?body () =
  let uri = Uri.of_string url in
  let config = { Config.default with connect_timeout = E2e_config.timeout_seconds } in
  match Client.Oneshot.request ~sw ~config ~meth ~headers ?body env uri with
  | Error e -> Error (Error.to_string e)
  | Ok response ->
    match Body.to_string response.body with
    | Error e -> Error (Error.to_string e)
    | Ok body_str ->
      Ok {
        status = status_to_int response.status;
        headers = Headers.to_list response.headers;
        body = body_str;
      }

let make_request ~sw ~env ~meth ~url ?(headers=[]) ?body () =
  match body with
  | None -> make_request_with_body ~sw ~env ~meth ~url ~headers ()
  | Some body_str ->
      let body = Body.of_string body_str in
      make_request_with_body ~sw ~env ~meth ~url ~headers ~body ()

let get ~sw ~env ~url ?(headers=[]) () =
  make_request ~sw ~env ~meth:`GET ~url ~headers ()

let head ~sw ~env ~url ?(headers=[]) () =
  make_request ~sw ~env ~meth:`HEAD ~url ~headers ()

let put ~sw ~env ~url ?(headers=[]) ~body () =
  make_request ~sw ~env ~meth:`PUT ~url ~headers ~body ()

let delete ~sw ~env ~url ?(headers=[]) () =
  make_request ~sw ~env ~meth:`DELETE ~url ~headers ()

let options ~sw ~env ~url ?(headers=[]) () =
  make_request ~sw ~env ~meth:`OPTIONS ~url ~headers ()

let put_streaming ~sw ~env ~url ?(headers=[]) ~chunk_size ~chunks () =
  let stream, push = Piaf.Stream.create 1 in
  let chunk = String.make chunk_size 'X' in
  Eio.Fiber.fork ~sw (fun () ->
    for _ = 1 to chunks do
      push (Some chunk)
    done;
    push None);
  let body = Body.of_string_stream stream in
  make_request_with_body ~sw ~env ~meth:`PUT ~url ~headers ~body ()
