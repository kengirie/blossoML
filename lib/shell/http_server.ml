open Piaf
open Blossom_core

let add_cors_headers response =
  let headers = Response.headers response in
  let headers =
    headers
    |> fun h -> Headers.remove h "access-control-allow-origin"
    |> fun h -> Headers.add h "access-control-allow-origin" "*"
  in
  Response.create
    ~version:response.version
    ~headers
    ~body:response.body
    response.status

let log_response ~request response =
  let status = Response.status response |> Piaf.Status.to_string in
  let headers =
    response
    |> Response.headers
    |> Headers.to_list
    |> List.map (fun (k, v) -> Printf.sprintf "%s: %s" k v)
    |> String.concat "; "
  in
  Printf.printf
    "Response: %s %s -> %s [%s]\n%!"
    (request |> Request.meth |> Method.to_string)
    (Request.target request)
    status
    headers

let handle_cors_preflight () =
  let headers = Headers.of_list [
    ("Access-Control-Allow-Origin", "*");
    ("Access-Control-Allow-Methods", "GET, HEAD, PUT, DELETE, OPTIONS");
    ("Access-Control-Allow-Headers", "Authorization, Content-Type, Content-Length, *");
    ("Access-Control-Max-Age", "86400");
  ] in
  Response.create ~headers `No_content

let error_response status message =
  let headers = Headers.of_list [("X-Reason", message)] in
  Response.of_string ~headers ~body:message status

let request_handler ~dir ~db { Server.Handler.request; _ } =
  Printf.printf "Request: %s %s\n%!" (Method.to_string request.meth) request.target;
  let response = match request.meth, request.target with
  | `OPTIONS, _ -> handle_cors_preflight ()
  | `GET, path ->
      let path_parts = String.split_on_char '/' path |> List.filter (fun s -> s <> "") in
      Printf.printf "Path parts: [%s]\n%!" (String.concat "; " path_parts);
      (match path_parts with
       | [hash_with_ext] ->
           (* 拡張子を取り除く *)
           let hash = try Filename.remove_extension hash_with_ext with _ -> hash_with_ext in
           if not (Integrity.validate_hash hash) then
             error_response `Not_found "Invalid path or hash"
           else
             (match Local_storage.get ~dir ~db ~sha256:hash with
              | Ok (data, metadata) ->
                  let headers = Headers.of_list [
                    ("Content-Type", metadata.mime_type);
                    ("Content-Length", string_of_int metadata.size);
                  ] in
                  Response.create ~headers ~body:(Body.of_string data) `OK
              | Error (Domain.Blob_not_found _) -> error_response `Not_found "Blob not found"
              | Error (Domain.Storage_error msg) -> error_response `Internal_server_error msg
              | Error _ -> error_response `Internal_server_error "Internal error")
       | _ -> error_response `Not_found "Invalid path")
  | `HEAD, path ->
      let path_parts = String.split_on_char '/' path |> List.filter (fun s -> s <> "") in
      (match path_parts with
       | [hash_with_ext] ->
           let hash = try Filename.remove_extension hash_with_ext with _ -> hash_with_ext in
           if not (Integrity.validate_hash hash) then
             error_response `Not_found "Invalid path or hash"
           else
             (match Local_storage.get ~dir ~db ~sha256:hash with
              | Ok (_, metadata) ->
                  let headers = Headers.of_list [
                    ("Content-Type", metadata.mime_type);
                    ("Content-Length", string_of_int metadata.size);
                  ] in
                  Response.create ~headers `OK
              | Error (Domain.Blob_not_found _) -> error_response `Not_found "Blob not found"
              | Error (Domain.Storage_error msg) -> error_response `Internal_server_error msg
              | Error _ -> error_response `Internal_server_error "Internal error")
       | _ -> error_response `Not_found "Invalid path")
  | `PUT, "/upload" ->
      (match Headers.get request.headers "authorization" with
       | None -> error_response `Unauthorized "Missing Authorization header"
       | Some auth_header ->
           let current_time = Int64.of_float (Unix.time ()) in
           match Auth.validate_auth ~header:auth_header ~action:Auth.Upload ~current_time with
           | Error (Domain.Storage_error msg) -> error_response `Unauthorized msg
           | Error _ -> error_response `Unauthorized "Authentication failed"
           | Ok pubkey ->
               (* Content-Type を取得 *)
               let mime_type = Headers.get request.headers "content-type" |> Option.value ~default:"application/octet-stream" in
               let content_length =
                 Headers.get request.headers "content-length"
                 |> Option.map int_of_string
                 |> Option.value ~default:0
               in

               (* ポリシーチェック *)
               let policy = Policy.default_policy in
               (match Policy.check_upload_policy ~policy ~size:content_length ~mime:mime_type with
                | Error e ->
                    let msg = match e with
                      | Domain.Storage_error m -> m
                      | Domain.Invalid_size s -> Printf.sprintf "Invalid size: %d" s
                      | _ -> "Unknown error"
                    in
                    error_response `Bad_request msg
                | Ok () ->
                    (match Local_storage.save_stream ~dir ~db ~body:request.body ~mime_type ~uploader:pubkey with
                     | Error e ->
                         let msg = match e with
                           | Domain.Storage_error m -> m
                           | _ -> "Unknown error"
                         in
                         Printf.printf "Save failed: %s\n%!" msg;
                         error_response `Internal_server_error msg
                     | Ok (hash, size) ->
                         Printf.printf "Upload successful: %s (%d bytes)\n%!" hash size;
                         let descriptor = {
                           Domain.url = Printf.sprintf "http://localhost:8082/%s" hash;
                           sha256 = hash;
                           size = size;
                           mime_type = mime_type;
                           uploaded = Int64.of_float (Unix.time ());
                         } in
                         let json = Printf.sprintf
                           {|{"url":"%s","sha256":"%s","size":%d,"type":"%s","uploaded":%Ld}|}
                           descriptor.url descriptor.sha256 descriptor.size descriptor.mime_type descriptor.uploaded
                         in
                         Printf.printf "Upload response: %s\n%!" json;
                         Response.of_string ~body:json `OK)))
  | _ -> error_response `Not_found "Not found"
  in
  let response = add_cors_headers response in
  log_response ~request response;
  response

let start ~sw ~env ~port ~dir ~db =
  let address = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  let config = Server.Config.create address in
  let server = Server.create ~config (request_handler ~dir ~db) in
  let _ = Server.Command.start ~sw env server in
  ()
