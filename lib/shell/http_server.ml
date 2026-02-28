open Piaf
open Blossom_core

(** Blob_serviceのインスタンス化 *)
module BlobService = Blob_service.Make(Storage_eio.Impl)(Blossom_db.Impl)

(** ログ出力（副作用） *)
let log_response ~request response =
  let status = Response.status response |> Piaf.Status.to_string in
  let headers =
    response
    |> Response.headers
    |> Headers.to_list
    |> List.map (fun (k, v) -> Printf.sprintf "%s: %s" k v)
    |> String.concat "; "
  in
  Eio.traceln
    "Response: %s %s -> %s [%s]"
    (request |> Request.meth |> Method.to_string)
    (Request.target request)
    status
    headers

(** Domain.errorをHttp_response.response_kindに変換するヘルパー *)
let error_to_response_kind = function
  | Domain.Blob_not_found _ -> Http_response.Error_not_found "Blob not found"
  | Domain.Storage_error msg -> Http_response.Error_internal msg
  | Domain.Invalid_size s -> Http_response.Error_bad_request (Printf.sprintf "Invalid size: %d" s)
  | Domain.Invalid_hash h -> Http_response.Error_bad_request (Printf.sprintf "Invalid hash: %s" h)
  | Domain.Payload_too_large (actual, max) ->
      Http_response.Error_payload_too_large (Printf.sprintf "File too large: %d bytes (max: %d)" actual max)
  | Domain.Auth_error msg -> Http_response.Error_unauthorized msg
  | Domain.Forbidden msg -> Http_response.Error_forbidden msg
  | Domain.Unsupported_media_type msg -> Http_response.Error_unsupported_media_type msg
  | Domain.Invalid_content_type msg -> Http_response.Error_bad_request msg
  | Domain.Mirror_invalid_url msg -> Http_response.Error_bad_request msg
  | Domain.Mirror_fetch_error msg -> Http_response.Error_bad_gateway msg
  | Domain.Mirror_ssrf_blocked _msg -> Http_response.Error_bad_request "URL not allowed"

let request_handler ~sw ~env ~clock ~data_dir ~db ~base_url { Server.Handler.request; _ } =
  Eio.traceln "Request: %s %s" (Method.to_string request.meth) request.target;

  (* レスポンス種別を決定 *)
  let response_kind = match request.meth, request.target with
  | `OPTIONS, _ ->
      Http_response.Cors_preflight

  | `GET, path ->
      let path_parts = String.split_on_char '/' path |> List.filter (fun s -> s <> "") in
      Eio.traceln "Path parts: [%s]" (String.concat "; " path_parts);
      (match path_parts with
       | [hash_with_ext] ->
           let hash = try Filename.remove_extension hash_with_ext with _ -> hash_with_ext in
           if not (Integrity.validate_hash hash) then
             Http_response.Error_not_found "Invalid path or hash"
           else
             (match BlobService.get ~sw ~storage:data_dir ~db ~sha256:hash with
              | Ok (body, metadata) ->
                  Http_response.Success_blob_stream {
                    body;
                    mime_type = metadata.mime_type;
                    size = metadata.size;
                  }
              | Error e -> error_to_response_kind e)
       | _ -> Http_response.Error_not_found "Invalid path")

  | `HEAD, "/upload" ->
      (* BUD-06: Upload requirements check *)
      (* 1. X-Content-Length ヘッダーを取得（必須） *)
      (match Headers.get request.headers "x-content-length" with
       | None -> Http_response.Error_length_required "Missing X-Content-Length header"
       | Some len_str ->
           match int_of_string_opt len_str with
           | None -> Http_response.Error_bad_request "Invalid X-Content-Length header format"
           | Some size when size < 0 -> Http_response.Error_bad_request "X-Content-Length must be non-negative"
           | Some size ->
               (* 2. X-SHA-256 ヘッダーを取得（任意だが形式検証） *)
               let sha256_opt = match Headers.get request.headers "x-sha-256" with
                 | None -> Ok None
                 | Some hash ->
                     if Integrity.validate_hash hash then Ok (Some hash)
                     else Error "Invalid X-SHA-256 header format"
               in
               match sha256_opt with
               | Error msg -> Http_response.Error_bad_request msg
               | Ok sha256_opt ->
                   (* 3. MIME type を取得（PUT /upload と同じ優先順位: Content-Type -> X-Content-Type -> default） *)
                   let mime_type =
                     match Headers.get request.headers "content-type" with
                     | Some ct when String.length ct > 0 -> ct
                     | _ ->
                         match Headers.get request.headers "x-content-type" with
                         | Some xct when String.length xct > 0 -> xct
                         | _ -> "application/octet-stream"
                   in
                   (* 4. Authorization ヘッダーの検証（PUT /upload と同じ挙動） *)
                   (* Authorization は必須（PUT /upload と同じ） *)
                   match Headers.get request.headers "authorization" with
                   | None -> Http_response.Error_unauthorized "Missing Authorization header"
                   | Some auth_header ->
                       let current_time = Int64.of_float (Eio.Time.now clock) in
                       (* X-SHA-256 がある場合は x タグ検証も行う *)
                       let auth_result = match sha256_opt with
                         | Some sha256 ->
                             Auth.validate_upload_auth ~header:auth_header ~sha256 ~current_time
                         | None ->
                             (* X-SHA-256 がない場合は基本検証のみ *)
                             Auth.validate_auth ~header:auth_header ~action:Auth.Upload ~current_time
                       in
                       (match auth_result with
                        | Error e -> error_to_response_kind e
                        | Ok _ ->
                            (* 5. Policy チェック（サイズ、MIMEタイプ） *)
                            let policy = Policy.default_policy in
                            match Policy.check_upload_policy ~policy ~size ~mime:mime_type with
                            | Error e -> error_to_response_kind e
                            | Ok () -> Http_response.Success_upload_check))

  | `HEAD, path ->
      let path_parts = String.split_on_char '/' path |> List.filter (fun s -> s <> "") in
      (match path_parts with
       | [hash_with_ext] ->
           let hash = try Filename.remove_extension hash_with_ext with _ -> hash_with_ext in
           if not (Integrity.validate_hash hash) then
             Http_response.Error_not_found "Invalid path or hash"
           else
             (match BlobService.get_metadata ~storage:data_dir ~db ~sha256:hash with
              | Ok metadata ->
                  Http_response.Success_metadata {
                    mime_type = metadata.mime_type;
                    size = metadata.size;
                  }
              | Error e -> error_to_response_kind e)
       | _ -> Http_response.Error_not_found "Invalid path")

  | `PUT, "/upload" ->
      (match Headers.get request.headers "authorization" with
       | None -> Http_response.Error_unauthorized "Missing Authorization header"
       | Some auth_header ->
           let current_time = Int64.of_float (Eio.Time.now clock) in
           (* まず認証イベントの基本検証（署名、期限、アクションタイプ） *)
           match Auth.validate_auth ~header:auth_header ~action:Auth.Upload ~current_time with
           | Error e -> error_to_response_kind e
           | Ok pubkey ->
               (* Content-Type -> X-Content-Type -> default の優先順位で fallback MIME type を取得 *)
               (* バイト検査で検出できない場合のフォールバックとして使用 *)
               let fallback_mime_type =
                 match Headers.get request.headers "content-type" with
                 | Some ct when String.length ct > 0 -> ct
                 | _ ->
                     match Headers.get request.headers "x-content-type" with
                     | Some xct when String.length xct > 0 -> xct
                     | _ -> "application/octet-stream"
               in
               let content_length_result =
                 match Headers.get request.headers "content-length" with
                 | None -> Ok 0
                 | Some s ->
                     match int_of_string_opt s with
                     | Some n when n >= 0 -> Ok n
                     | Some _ -> Error "Content-Length must be non-negative"
                     | None -> Error "Invalid Content-Length header"
               in

               (match content_length_result with
                | Error msg -> Http_response.Error_bad_request msg
                | Ok content_length ->
                    let policy = Policy.default_policy in
                    (* Content-Length が指定されている場合はサイズのみ事前チェック *)
                    (match if content_length > 0 then Policy.check_size ~policy content_length else Ok () with
                     | Error e -> error_to_response_kind e
                     | Ok () ->
                    (* ストリーミング中にサイズ制限を適用、バイト検査でMIME type検出
                       検出できない場合はクライアント提供のContent-Typeをフォールバックとして使用 *)
                    (match BlobService.save ~storage:data_dir ~db ~body:request.body ~mime_type:fallback_mime_type ~uploader:pubkey ~max_size:policy.max_size with
                     | Error e ->
                         Eio.traceln "Save failed: %s" (match e with Domain.Storage_error m -> m | _ -> "Unknown error");
                         error_to_response_kind e
                     | Ok (hash, size, detected_mime_type) ->
                         (* 検出されたMIME typeでPolicyチェック *)
                         (match Policy.check_mime_type ~policy detected_mime_type with
                          | Error e ->
                              (* MIME type Policy違反時はアップロードしたファイルを削除 *)
                              Eio.traceln "MIME type policy violation, deleting uploaded blob: %s (type: %s)" hash detected_mime_type;
                              let _ = BlobService.delete ~storage:data_dir ~db ~sha256:hash ~pubkey in
                              error_to_response_kind e
                          | Ok () ->
                              (* SHA256とxタグの照合を行う *)
                              (match Auth.validate_upload_auth ~header:auth_header ~sha256:hash ~current_time with
                               | Error e ->
                                   (* 照合失敗時はアップロードしたファイルを削除 *)
                                   Eio.traceln "SHA256 mismatch, deleting uploaded blob: %s" hash;
                                   let _ = BlobService.delete ~storage:data_dir ~db ~sha256:hash ~pubkey in
                                   error_to_response_kind e
                               | Ok _ ->
                                   Eio.traceln "Upload successful: %s (%d bytes, %s)" hash size detected_mime_type;
                                   let descriptor = {
                                     Domain.url = Printf.sprintf "%s/%s" base_url hash;
                                     sha256 = hash;
                                     size = size;
                                     mime_type = detected_mime_type;
                                     uploaded = Int64.of_float (Eio.Time.now clock);
                                   } in
                                   Eio.traceln "Upload response: %s" (Http_response.descriptor_to_json descriptor);
                                   Http_response.Success_upload descriptor))))))

  | `DELETE, path ->
      let path_parts = String.split_on_char '/' path |> List.filter (fun s -> s <> "") in
      (match path_parts with
       | [hash_with_ext] ->
           let hash = try Filename.remove_extension hash_with_ext with _ -> hash_with_ext in
           if not (Integrity.validate_hash hash) then
             Http_response.Error_not_found "Invalid path or hash"
           else
             (match Headers.get request.headers "authorization" with
              | None -> Http_response.Error_unauthorized "Missing Authorization header"
              | Some auth_header ->
                  let current_time = Int64.of_float (Eio.Time.now clock) in
                  match Auth.validate_delete_auth ~header:auth_header ~sha256:hash ~current_time with
                  | Error e -> error_to_response_kind e
                  | Ok pubkey ->
                      Eio.traceln "Delete request for %s by %s" hash pubkey;
                      (match BlobService.delete ~storage:data_dir ~db ~sha256:hash ~pubkey with
                       | Ok () ->
                           Eio.traceln "Delete successful: %s" hash;
                           Http_response.Success_delete
                       | Error e ->
                           Eio.traceln "Delete failed for %s: %s" hash
                             (match e with
                              | Domain.Storage_error msg -> msg
                              | Domain.Forbidden msg -> msg
                              | Domain.Blob_not_found _ -> "Blob not found"
                              | _ -> "Unknown error");
                           error_to_response_kind e))
       | _ -> Http_response.Error_not_found "Invalid path")

  | `PUT, "/mirror" ->
      (* BUD-04: Mirror blob from remote URL *)
      (match Headers.get request.headers "authorization" with
       | None -> Http_response.Error_unauthorized "Missing Authorization header"
       | Some auth_header ->
           let current_time = Int64.of_float (Eio.Time.now clock) in
           (* まず認証イベントの基本検証（署名、期限、アクションタイプ） *)
           match Auth.validate_auth ~header:auth_header ~action:Auth.Upload ~current_time with
           | Error e -> error_to_response_kind e
           | Ok pubkey ->
               (* リクエストボディをJSONとしてパース *)
               let body_str =
                 match Piaf.Body.to_string request.body with
                 | Ok s -> s
                 | Error _ -> ""
               in
               match Mirror.parse_request body_str with
               | Error e -> error_to_response_kind e
               | Ok mirror_req ->
                   match Mirror.validate_url mirror_req.url with
                   | Error e -> error_to_response_kind e
                   | Ok () ->
                       let policy = Policy.default_policy in
                       (* リモートURLからblobをダウンロードして保存（バイト検査でMIME type検出） *)
                       match BlobService.mirror ~sw ~env ~storage:data_dir ~db ~url:mirror_req.url ~uploader:pubkey ~max_size:policy.max_size with
                       | Error e ->
                           Eio.traceln "Mirror failed: %s" (match e with Domain.Mirror_fetch_error m -> m | Domain.Mirror_ssrf_blocked m -> m | Domain.Mirror_invalid_url m -> m | Domain.Storage_error m -> m | _ -> "Unknown error");
                           error_to_response_kind e
                       | Ok (hash, size, detected_mime_type) ->
                           (* 検出されたMIME typeでPolicyチェック *)
                           (match Policy.check_mime_type ~policy detected_mime_type with
                            | Error e ->
                                (* MIME type Policy違反時はミラーしたファイルを削除 *)
                                Eio.traceln "MIME type policy violation for mirrored blob, deleting: %s (type: %s)" hash detected_mime_type;
                                let _ = BlobService.delete ~storage:data_dir ~db ~sha256:hash ~pubkey in
                                error_to_response_kind e
                            | Ok () ->
                                (* SHA256とxタグの照合を行う *)
                                (match Auth.validate_upload_auth ~header:auth_header ~sha256:hash ~current_time with
                                 | Error e ->
                                     (* 照合失敗時はミラーしたファイルを削除 *)
                                     Eio.traceln "SHA256 mismatch for mirrored blob, deleting: %s" hash;
                                     let _ = BlobService.delete ~storage:data_dir ~db ~sha256:hash ~pubkey in
                                     error_to_response_kind e
                                 | Ok _ ->
                                     Eio.traceln "Mirror successful: %s (%d bytes, %s)" hash size detected_mime_type;
                                     let descriptor = {
                                       Domain.url = Printf.sprintf "%s/%s" base_url hash;
                                       sha256 = hash;
                                       size = size;
                                       mime_type = detected_mime_type;
                                       uploaded = Int64.of_float (Eio.Time.now clock);
                                     } in
                                     Http_response.Success_upload descriptor)))

  | _ -> Http_response.Error_not_found "Not found"
  in

  (* レスポンスを生成（CORSヘッダーは自動的に付与される） *)
  let response = Http_response.create response_kind in
  log_response ~request response;
  response

let start ~sw ~env ~port ~host ~clock ~data_dir ~db ~base_url ?cert ?key () =
  let ip_addr = match host with
    | "localhost" | "127.0.0.1" -> Eio.Net.Ipaddr.V4.loopback
    | "0.0.0.0" -> Eio.Net.Ipaddr.V4.any
    | _ ->
        (* Try to parse as IP address, fallback to any if invalid *)
        try Eio.Net.Ipaddr.of_raw (Unix.inet_addr_of_string host |> Obj.magic)
        with _ -> Eio.Net.Ipaddr.V4.any
  in
  let address = `Tcp (ip_addr, port) in

  let https =
    match cert, key with
    | Some cert_path, Some key_path ->
        Some (Server.Config.HTTPS.create
          ~address
          (Cert.Filepath cert_path, Cert.Filepath key_path))
    | _ -> None
  in

  let config =
    Server.Config.create
      ?https
      ~max_http_version:(if Option.is_some https then Versions.HTTP.HTTP_2 else Versions.HTTP.HTTP_1_1)
      address
  in

  let server = Server.create ~config (request_handler ~sw ~env ~clock ~data_dir ~db ~base_url) in
  let _ = Server.Command.start ~sw env server in
  ()
