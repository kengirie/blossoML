open Piaf
open Blossom_core
open Syntax

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
  | Domain.Report_error msg -> Http_response.Error_bad_request msg

(** ハンドラー内部ではErrorも[Http_response.response_kind]で持ち回り、
    最後に[to_response]で畳み込む。Domain.errorは[of_domain]で変換する *)
let of_domain r = Result.map_error error_to_response_kind r

let to_response = function Ok kind -> kind | Error kind -> kind

(** パスを空でないセグメントのリストに分割する *)
let split_path path =
  String.split_on_char '/' path |> List.filter (fun s -> s <> "")

(** パスセグメントから拡張子を除いたハッシュを取り出し、形式を検証する *)
let hash_of_path_part part =
  let hash = try Filename.remove_extension part with _ -> part in
  if Integrity.validate_hash hash then Ok hash
  else Error (Http_response.Error_not_found "Invalid path or hash")

(** Authorizationヘッダーの取得（必須） *)
let require_auth_header headers =
  Headers.get headers "authorization"
  |> Option.to_result ~none:(Http_response.Error_unauthorized "Missing Authorization header")

(** Content-Type -> X-Content-Type -> default の優先順位で fallback MIME type を取得 *)
let fallback_mime_type headers =
  match Headers.get headers "content-type" with
  | Some ct when String.length ct > 0 -> ct
  | _ ->
      match Headers.get headers "x-content-type" with
      | Some xct when String.length xct > 0 -> xct
      | _ -> "application/octet-stream"

(** BUD-09: GET / で返すサーバー規約（terms of service）テキスト *)
let terms_of_service =
  let policy = Policy.default_policy in
  Printf.sprintf
{|blossoML - Terms of Service
===========================

This is a Blossom (BUD-01..) server. By uploading, mirroring, or
otherwise interacting with this server you agree to the following:

1. Acceptable use
   - No illegal content (including but not limited to CSAM).
   - No malware or other harmful software.
   - No content that violates the rights of others.

2. Limits
   - Maximum blob size: %d bytes.
   - The operator may reject uploads based on MIME type or other
     policy at any time.

3. Reporting (BUD-09)
   - Send a signed NIP-56 (kind:1984) event to PUT /report.
   - Each `x` tag MUST contain the sha256 of the reported blob and
     one of the following report types:
       nudity / malware / profanity / illegal / spam /
       impersonation / other.

4. Moderation
   - Reports are stored for operator review.
   - The operator may remove or refuse content at their discretion.

5. No warranty
   - This service is provided as-is, without warranty of any kind.
|}
    policy.max_size

(** GET /<sha256>: Blob取得（BUD-01、Rangeリクエスト対応） *)
let handle_get_blob ~sw ~data_dir ~db ~headers hash_with_ext =
  to_response @@
  let* hash = hash_of_path_part hash_with_ext in
  (* Blob全体をストリーミングで返すヘルパー *)
  let serve_full () =
    let* (body, metadata) = of_domain (BlobService.get ~sw ~storage:data_dir ~db ~sha256:hash) in
    Ok (Http_response.Success_blob_stream {
      body;
      mime_type = metadata.mime_type;
      size = metadata.size;
    })
  in
  match Headers.get headers "range" with
  | None -> serve_full ()
  | Some range_header ->
      (* BUD-01: Rangeリクエスト対応 *)
      let* metadata = of_domain (BlobService.get_metadata ~storage:data_dir ~db ~sha256:hash) in
      match Range.parse range_header ~total_size:metadata.size with
      | Range.Not_applicable ->
          (* bytes以外の単位や複数rangeはヘッダーを無視して全体を返す *)
          serve_full ()
      | Range.Unsatisfiable ->
          Ok (Http_response.Error_range_not_satisfiable { total = metadata.size })
      | Range.Satisfiable range ->
          let* body =
            of_domain (BlobService.get_range ~sw ~storage:data_dir ~db ~sha256:hash
                         ~offset:range.start ~length:(Range.length range))
          in
          Ok (Http_response.Success_blob_range {
            body;
            mime_type = metadata.mime_type;
            start = range.start;
            end_ = range.end_;
            total = metadata.size;
          })

(** GET /list/<pubkey>: Blob一覧（BUD-12） *)
let handle_list ~clock ~data_dir ~db ~base_url ~headers ~uri pubkey =
  to_response @@
  let* () =
    if Integrity.validate_hash pubkey then Ok ()
    else Error (Http_response.Error_bad_request "Invalid pubkey format")
  in
  let parse_int64_param name =
    match Uri.get_query_param uri name with
    | None -> Ok None
    | Some s ->
        (match Int64.of_string_opt s with
         | Some n when n >= 0L -> Ok (Some n)
         | _ -> Error (Http_response.Error_bad_request (Printf.sprintf "Invalid %s parameter" name)))
  in
  let parse_int_param name =
    match Uri.get_query_param uri name with
    | None -> Ok None
    | Some s ->
        (match int_of_string_opt s with
         | Some n -> Ok (Some n)
         | None -> Error (Http_response.Error_bad_request (Printf.sprintf "Invalid %s parameter" name)))
  in
  let* since_opt = parse_int64_param "since" in
  let* until_opt = parse_int64_param "until" in
  let* limit_opt = parse_int_param "limit" in
  let since = Option.value since_opt ~default:0L in
  let until = Option.value until_opt ~default:Int64.max_int in
  let limit = match limit_opt with
    | Some n -> max 1 (min 1000 n)
    | None -> 50
  in
  let* cursor =
    match Uri.get_query_param uri "cursor" with
    | None -> Ok None
    | Some sha ->
        if not (Integrity.validate_hash sha) then
          Error (Http_response.Error_bad_request "Invalid cursor")
        else
          (match BlobService.get_metadata ~storage:data_dir ~db ~sha256:sha with
           | Ok meta -> Ok (Some (meta.uploaded, sha))
           | Error _ -> Error (Http_response.Error_bad_request "Invalid cursor"))
  in
  (* Authorization is optional for /list (BUD-11/BUD-12) *)
  let* () =
    match Headers.get headers "authorization" with
    | None -> Ok ()
    | Some auth_header ->
        let current_time = Int64.of_float (Eio.Time.now clock) in
        (match Auth.validate_auth ~header:auth_header ~action:Auth.List ~current_time with
         | Ok _ -> Ok ()
         | Error e -> Error (error_to_response_kind e))
  in
  let* descriptors = of_domain (Blossom_db.list_by_pubkey db ~pubkey ~since ~until ~cursor ~limit) in
  let descriptors = List.map (fun (d : Domain.blob_descriptor) ->
    { d with Domain.url = Printf.sprintf "%s/%s" base_url d.sha256 }
  ) descriptors in
  Ok (Http_response.Success_list descriptors)

(** HEAD /upload: アップロード要件チェック（BUD-06） *)
let handle_head_upload ~clock headers =
  to_response @@
  (* 1. X-Content-Length ヘッダーを取得（必須） *)
  let* len_str =
    Headers.get headers "x-content-length"
    |> Option.to_result ~none:(Http_response.Error_length_required "Missing X-Content-Length header")
  in
  let* size =
    match int_of_string_opt len_str with
    | None -> Error (Http_response.Error_bad_request "Invalid X-Content-Length header format")
    | Some size when size < 0 -> Error (Http_response.Error_bad_request "X-Content-Length must be non-negative")
    | Some size -> Ok size
  in
  (* 2. X-SHA-256 ヘッダーを取得（任意だが形式検証） *)
  let* sha256_opt =
    match Headers.get headers "x-sha-256" with
    | None -> Ok None
    | Some hash ->
        if Integrity.validate_hash hash then Ok (Some hash)
        else Error (Http_response.Error_bad_request "Invalid X-SHA-256 header format")
  in
  (* 3. MIME type を取得（PUT /upload と同じ優先順位: Content-Type -> X-Content-Type -> default） *)
  let mime_type = fallback_mime_type headers in
  (* 4. Authorization ヘッダーの検証（PUT /upload と同じく必須） *)
  let* auth_header = require_auth_header headers in
  let current_time = Int64.of_float (Eio.Time.now clock) in
  (* X-SHA-256 がある場合は x タグ検証も行う *)
  let* _pubkey =
    of_domain (match sha256_opt with
      | Some sha256 -> Auth.validate_upload_auth ~header:auth_header ~sha256 ~current_time
      | None -> Auth.validate_auth ~header:auth_header ~action:Auth.Upload ~current_time)
  in
  (* 5. Policy チェック（サイズ、MIMEタイプ） *)
  let policy = Policy.default_policy in
  let* () = of_domain (Policy.check_upload_policy ~policy ~size ~mime:mime_type) in
  Ok Http_response.Success_upload_check

(** HEAD /<sha256>: メタデータ取得 *)
let handle_head_blob ~data_dir ~db hash_with_ext =
  to_response @@
  let* hash = hash_of_path_part hash_with_ext in
  let* metadata = of_domain (BlobService.get_metadata ~storage:data_dir ~db ~sha256:hash) in
  Ok (Http_response.Success_metadata {
    mime_type = metadata.mime_type;
    size = metadata.size;
  })

(** PUT /report: signed NIP-56 (kind 1984) report event in body（BUD-09） *)
let handle_report ~clock ~db body =
  to_response @@
  let body_str =
    match Piaf.Body.to_string body with
    | Ok s -> s
    | Error _ -> ""
  in
  let current_time = Int64.of_float (Eio.Time.now clock) in
  let* report = of_domain (Report.validate ~current_time body_str) in
  let received_at = current_time in
  let rec persist = function
    | [] -> Ok ()
    | (entry : Report.entry) :: rest ->
        let report_type_str = Report.report_type_to_string entry.report_type in
        let* () =
          Blossom_db.save_report db
            ~event_id:report.event_id
            ~sha256:entry.sha256
            ~reporter_pubkey:report.reporter_pubkey
            ~report_type:report_type_str
            ~content:report.content
            ~e_tag:report.e_tag
            ~p_tag:report.p_tag
            ~raw_event_json:report.raw_event_json
            ~event_created_at:report.created_at
            ~received_at
        in
        persist rest
  in
  match persist report.entries with
  | Error e ->
      Eio.traceln "Failed to persist report %s: %s"
        report.event_id
        (match e with Domain.Storage_error m -> m | _ -> "unknown");
      Error (error_to_response_kind e)
  | Ok () ->
      Eio.traceln "Report received: event=%s reporter=%s entries=%d"
        report.event_id report.reporter_pubkey
        (List.length report.entries);
      Ok Http_response.Success_report

(** upload/mirror 共通の後処理:
    検出されたMIME typeのPolicyチェック → SHA256とxタグの照合 →
    いずれか失敗時は保存済みblobを削除。成功時はdescriptorを構築する *)
let finalize_stored_blob ~clock ~data_dir ~db ~base_url ~auth_header ~current_time ~pubkey
    ~context (hash, size, detected_mime_type) =
  let policy = Policy.default_policy in
  let delete_blob () =
    ignore (BlobService.delete ~storage:data_dir ~db ~sha256:hash ~pubkey)
  in
  (* 検出されたMIME typeでPolicyチェック（違反時は保存したファイルを削除） *)
  let* () =
    match Policy.check_mime_type ~policy detected_mime_type with
    | Ok () -> Ok ()
    | Error e ->
        Eio.traceln "MIME type policy violation, deleting %s blob: %s (type: %s)"
          context hash detected_mime_type;
        delete_blob ();
        Error (error_to_response_kind e)
  in
  (* SHA256とxタグの照合を行う（照合失敗時は保存したファイルを削除） *)
  let* _pubkey =
    match Auth.validate_upload_auth ~header:auth_header ~sha256:hash ~current_time with
    | Ok p -> Ok p
    | Error e ->
        Eio.traceln "SHA256 mismatch, deleting %s blob: %s" context hash;
        delete_blob ();
        Error (error_to_response_kind e)
  in
  Eio.traceln "%s successful: %s (%d bytes, %s)" context hash size detected_mime_type;
  Ok {
    Domain.url = Printf.sprintf "%s/%s" base_url hash;
    sha256 = hash;
    size;
    mime_type = detected_mime_type;
    uploaded = Int64.of_float (Eio.Time.now clock);
  }

(** PUT /upload: Blobアップロード（BUD-02） *)
let handle_upload ~clock ~data_dir ~db ~base_url ~headers ~body =
  to_response @@
  let* auth_header = require_auth_header headers in
  let current_time = Int64.of_float (Eio.Time.now clock) in
  (* まず認証イベントの基本検証（署名、期限、アクションタイプ） *)
  let* pubkey = of_domain (Auth.validate_auth ~header:auth_header ~action:Auth.Upload ~current_time) in
  (* バイト検査で検出できない場合のフォールバックMIME type *)
  let fallback = fallback_mime_type headers in
  let* content_length =
    match Headers.get headers "content-length" with
    | None -> Ok 0
    | Some s ->
        match int_of_string_opt s with
        | Some n when n >= 0 -> Ok n
        | Some _ -> Error (Http_response.Error_bad_request "Content-Length must be non-negative")
        | None -> Error (Http_response.Error_bad_request "Invalid Content-Length header")
  in
  let policy = Policy.default_policy in
  (* Content-Length が指定されている場合はサイズのみ事前チェック *)
  let* () =
    of_domain (if content_length > 0 then Policy.check_size ~policy content_length else Ok ())
  in
  (* ストリーミング中にサイズ制限を適用、バイト検査でMIME type検出
     検出できない場合はクライアント提供のContent-Typeをフォールバックとして使用 *)
  let* stored =
    match BlobService.save ~storage:data_dir ~db ~body ~mime_type:fallback ~uploader:pubkey ~max_size:policy.max_size with
    | Ok s -> Ok s
    | Error e ->
        Eio.traceln "Save failed: %s" (match e with Domain.Storage_error m -> m | _ -> "Unknown error");
        Error (error_to_response_kind e)
  in
  let* descriptor =
    finalize_stored_blob ~clock ~data_dir ~db ~base_url ~auth_header ~current_time ~pubkey
      ~context:"Upload" stored
  in
  Eio.traceln "Upload response: %s" (Http_response.descriptor_to_json descriptor);
  Ok (Http_response.Success_upload descriptor)

(** DELETE /<sha256>: Blob削除（BUD-02） *)
let handle_delete ~clock ~data_dir ~db ~headers hash_with_ext =
  to_response @@
  let* hash = hash_of_path_part hash_with_ext in
  let* auth_header = require_auth_header headers in
  let current_time = Int64.of_float (Eio.Time.now clock) in
  let* pubkey = of_domain (Auth.validate_delete_auth ~header:auth_header ~sha256:hash ~current_time) in
  Eio.traceln "Delete request for %s by %s" hash pubkey;
  match BlobService.delete ~storage:data_dir ~db ~sha256:hash ~pubkey with
  | Ok () ->
      Eio.traceln "Delete successful: %s" hash;
      Ok Http_response.Success_delete
  | Error e ->
      Eio.traceln "Delete failed for %s: %s" hash
        (match e with
         | Domain.Storage_error msg -> msg
         | Domain.Forbidden msg -> msg
         | Domain.Blob_not_found _ -> "Blob not found"
         | _ -> "Unknown error");
      Error (error_to_response_kind e)

(** PUT /mirror: リモートURLからのミラーリング（BUD-04） *)
let handle_mirror ~sw ~env ~clock ~data_dir ~db ~base_url ~headers ~body =
  to_response @@
  let* auth_header = require_auth_header headers in
  let current_time = Int64.of_float (Eio.Time.now clock) in
  (* まず認証イベントの基本検証（署名、期限、アクションタイプ） *)
  let* pubkey = of_domain (Auth.validate_auth ~header:auth_header ~action:Auth.Upload ~current_time) in
  (* リクエストボディをJSONとしてパース *)
  let body_str =
    match Piaf.Body.to_string body with
    | Ok s -> s
    | Error _ -> ""
  in
  let* mirror_req = of_domain (Mirror.parse_request body_str) in
  let* () = of_domain (Mirror.validate_url mirror_req.url) in
  let policy = Policy.default_policy in
  (* リモートURLからblobをダウンロードして保存（バイト検査でMIME type検出） *)
  let* stored =
    match BlobService.mirror ~sw ~env ~storage:data_dir ~db ~url:mirror_req.url ~uploader:pubkey ~max_size:policy.max_size with
    | Ok s -> Ok s
    | Error e ->
        Eio.traceln "Mirror failed: %s"
          (match e with
           | Domain.Mirror_fetch_error m -> m
           | Domain.Mirror_ssrf_blocked m -> m
           | Domain.Mirror_invalid_url m -> m
           | Domain.Storage_error m -> m
           | _ -> "Unknown error");
        Error (error_to_response_kind e)
  in
  let* descriptor =
    finalize_stored_blob ~clock ~data_dir ~db ~base_url ~auth_header ~current_time ~pubkey
      ~context:"Mirror" stored
  in
  Ok (Http_response.Success_upload descriptor)

(** ルーティング: メソッドとパスからハンドラーを選択する *)
let request_handler ~sw ~env ~clock ~data_dir ~db ~base_url { Server.Handler.request; _ } =
  Eio.traceln "Request: %s %s" (Method.to_string request.meth) request.target;
  let headers = request.headers in

  let response_kind = match request.meth, request.target with
  | `OPTIONS, _ ->
      Http_response.Cors_preflight

  | `GET, target ->
      let uri = Uri.of_string target in
      let path_parts = split_path (Uri.path uri) in
      Eio.traceln "Path parts: [%s]" (String.concat "; " path_parts);
      (match path_parts with
       | [] ->
           (* BUD-09: GET / returns server's terms of service *)
           Http_response.Success_terms_of_service terms_of_service
       | [hash_with_ext] -> handle_get_blob ~sw ~data_dir ~db ~headers hash_with_ext
       | ["list"; pubkey] -> handle_list ~clock ~data_dir ~db ~base_url ~headers ~uri pubkey
       | _ -> Http_response.Error_not_found "Invalid path")

  | `HEAD, "/upload" -> handle_head_upload ~clock headers

  | `HEAD, path ->
      (match split_path path with
       | [hash_with_ext] -> handle_head_blob ~data_dir ~db hash_with_ext
       | _ -> Http_response.Error_not_found "Invalid path")

  | `PUT, "/report" -> handle_report ~clock ~db request.body

  | `PUT, "/upload" -> handle_upload ~clock ~data_dir ~db ~base_url ~headers ~body:request.body

  | `PUT, "/mirror" -> handle_mirror ~sw ~env ~clock ~data_dir ~db ~base_url ~headers ~body:request.body

  | `DELETE, path ->
      (match split_path path with
       | [hash_with_ext] -> handle_delete ~clock ~data_dir ~db ~headers hash_with_ext
       | _ -> Http_response.Error_not_found "Invalid path")

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
