open Piaf
open Blossom_core

(** Storage_eioを直接使用するBlob_serviceインスタンス（ローカル用） *)
module BlobServiceLocal = Blob_service.Make(Storage_eio.Impl)(Blossom_db.Impl)

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

(** BlobServiceの操作をまとめたレコード型 *)
type blob_ops = {
  get : sw:Eio.Switch.t -> storage:Storage_eio.Impl.t -> db:Blossom_db.Impl.t -> sha256:string ->
        (Piaf.Body.t * Domain.blob_descriptor, Domain.error) result;
  get_metadata : storage:Storage_eio.Impl.t -> db:Blossom_db.Impl.t -> sha256:string ->
                 (Domain.blob_descriptor, Domain.error) result;
  save : storage:Storage_eio.Impl.t -> db:Blossom_db.Impl.t -> body:Piaf.Body.t ->
         mime_type:string -> uploader:string -> max_size:int ->
         (string * int * string, Domain.error) result;
  delete : storage:Storage_eio.Impl.t -> db:Blossom_db.Impl.t -> sha256:string -> pubkey:string ->
           (unit, Domain.error) result;
}

let request_handler ~blob_ops ~sw ~clock ~data_dir ~db ~base_url { Server.Handler.request; _ } =
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
             (match blob_ops.get ~sw ~storage:data_dir ~db ~sha256:hash with
              | Ok (body, metadata) ->
                  Http_response.Success_blob_stream {
                    body;
                    mime_type = metadata.mime_type;
                    size = metadata.size;
                  }
              | Error e -> error_to_response_kind e)
       | _ -> Http_response.Error_not_found "Invalid path")

  | `HEAD, path ->
      let path_parts = String.split_on_char '/' path |> List.filter (fun s -> s <> "") in
      (match path_parts with
       | [hash_with_ext] ->
           let hash = try Filename.remove_extension hash_with_ext with _ -> hash_with_ext in
           if not (Integrity.validate_hash hash) then
             Http_response.Error_not_found "Invalid path or hash"
           else
             (match blob_ops.get_metadata ~storage:data_dir ~db ~sha256:hash with
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
               (* Content-Type -> X-Content-Type -> default の優先順位で MIME type を取得 *)
               (* 空文字列の場合もデフォルト値にフォールバック *)
               let mime_type =
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
                    (* Content-Length が指定されている場合は事前チェック *)
                    (match if content_length > 0 then Policy.check_upload_policy ~policy ~size:content_length ~mime:mime_type else Ok () with
                     | Error e -> error_to_response_kind e
                     | Ok () ->
                    (* Use original mime_type to preserve parameters like charset, boundary *)
                    (* ストリーミング中にサイズ制限を適用 *)
                    (match blob_ops.save ~storage:data_dir ~db ~body:request.body ~mime_type ~uploader:pubkey ~max_size:policy.max_size with
                     | Error e ->
                         Eio.traceln "Save failed: %s" (match e with Domain.Storage_error m -> m | _ -> "Unknown error");
                         error_to_response_kind e
                     | Ok (hash, size, detected_mime_type) ->
                         (* 保存後にSHA256とxタグの照合を行う *)
                         (match Auth.validate_upload_auth ~header:auth_header ~sha256:hash ~current_time with
                          | Error e ->
                              (* 照合失敗時はアップロードしたファイルを削除 *)
                              Eio.traceln "SHA256 mismatch, deleting uploaded blob: %s" hash;
                              let _ = blob_ops.delete ~storage:data_dir ~db ~sha256:hash ~pubkey in
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
                              Http_response.Success_upload descriptor)))))

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
                      (match blob_ops.delete ~storage:data_dir ~db ~sha256:hash ~pubkey with
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

  | _ -> Http_response.Error_not_found "Not found"
  in

  (* レスポンスを生成（CORSヘッダーは自動的に付与される） *)
  let response = Http_response.create response_kind in
  log_response ~request response;
  response

let start ~sw ~env ~port ~host ~clock ~data_dir ~db ~base_url ?(use_reader_guard=false) ?cert ?key () =
  (* Build BlobService operations
     use_reader_guard=true: Reader-guarded storage (delays unlink while reads in progress)
     use_reader_guard=false: Direct filesystem access *)
  let blob_ops =
    if use_reader_guard then
      (* Reader-guarded: wrap storage to track active readers *)
      let module GuardedStorage = Storage_reader_guard.Make(Storage_eio.Impl) in
      let module BlobServiceGuarded = Blob_service.Make(GuardedStorage)(Blossom_db.Impl) in
      {
        get = BlobServiceGuarded.get;
        get_metadata = BlobServiceGuarded.get_metadata;
        save = BlobServiceGuarded.save;
        delete = BlobServiceGuarded.delete;
      }
    else
      (* Direct: use Storage_eio directly *)
      {
        get = BlobServiceLocal.get;
        get_metadata = BlobServiceLocal.get_metadata;
        save = BlobServiceLocal.save;
        delete = BlobServiceLocal.delete;
      }
  in

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

  let server = Server.create ~config (request_handler ~blob_ops ~sw ~clock ~data_dir ~db ~base_url) in
  let _ = Server.Command.start ~sw env server in
  ()
