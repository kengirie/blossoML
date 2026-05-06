open Alcotest
open Blossom_shell
open Blossom_core
open Piaf

(* ヘルパー関数 *)
let get_header response name =
  response |> Response.headers |> fun h -> Headers.get h name

let get_status response =
  Response.status response |> Status.to_code

(* CORSヘッダーの検証ヘルパー *)
let check_cors_headers response =
  check (option string) "CORS origin" (Some "*") (get_header response "access-control-allow-origin");
  check (option string) "CORS methods" (Some "*") (get_header response "access-control-allow-methods");
  check (option string) "CORS headers" (Some "Authorization, Content-Type, Content-Length, *") (get_header response "access-control-allow-headers");
  check (option string) "CORS expose-headers" (Some "*") (get_header response "access-control-expose-headers");
  check (option string) "CORS max-age" (Some "86400") (get_header response "access-control-max-age")

(* Success_blob レスポンステスト *)
let test_success_blob () =
  let response = Http_response.create (Success_blob {
    data = "test data";
    mime_type = "text/plain";
    size = 9;
  }) in
  check (option string) "Content-Type header" (Some "text/plain") (get_header response "content-type");
  check (option string) "Content-Length header" (Some "9") (get_header response "content-length");
  check int "Status code" 200 (get_status response);
  check_cors_headers response

(* Success_metadata レスポンステスト *)
let test_success_metadata () =
  let response = Http_response.create (Success_metadata {
    mime_type = "image/png";
    size = 1024;
  }) in
  check (option string) "Content-Type header" (Some "image/png") (get_header response "content-type");
  check (option string) "Content-Length header" (Some "1024") (get_header response "content-length");
  check int "Status code" 200 (get_status response);
  check_cors_headers response

(* Success_upload レスポンステスト *)
let test_success_upload () =
  let descriptor = {
    Domain.url = "http://localhost:8082/abc123";
    sha256 = "abc123";
    size = 500;
    mime_type = "application/octet-stream";
    uploaded = 1234567890L;
  } in
  let response = Http_response.create (Success_upload descriptor) in
  check int "Status code" 200 (get_status response);
  check_cors_headers response

(* BUD-08: Success_upload should include nip94 field and preserve base fields *)
let test_success_upload_includes_nip94 () =
  let descriptor = {
    Domain.url = "http://localhost:8082/abc123";
    sha256 = "abc123";
    size = 500;
    mime_type = "application/pdf";
    uploaded = 1234567890L;
  } in
  let response = Http_response.create (Success_upload descriptor) in
  let body = match Response.body response |> Body.to_string with
    | Ok s -> s
    | Error _ -> failwith "Failed to read body"
  in
  let json = Yojson.Basic.from_string body in
  let open Yojson.Basic.Util in
  (* Base descriptor fields must still be present alongside nip94 *)
  check string "base url" "http://localhost:8082/abc123" (json |> member "url" |> to_string);
  check string "base sha256" "abc123" (json |> member "sha256" |> to_string);
  check int "base size" 500 (json |> member "size" |> to_int);
  check string "base type" "application/pdf" (json |> member "type" |> to_string);
  check int "base uploaded" 1234567890 (json |> member "uploaded" |> to_int);
  let nip94 = json |> member "nip94" in
  (match nip94 with
   | `Null -> failwith "Missing nip94 field"
   | _ -> ());
  let pairs = nip94 |> to_list |> List.map (fun item ->
    match item |> to_list with
    | [`String k; `String v] -> (k, v)
    | _ -> failwith "Invalid nip94 entry"
  ) in
  let lookup k =
    match List.assoc_opt k pairs with
    | Some v -> v
    | None -> Alcotest.failf "nip94 missing %s tag" k
  in
  check string "url tag" "http://localhost:8082/abc123" (lookup "url");
  check string "m tag" "application/pdf" (lookup "m");
  check string "x tag" "abc123" (lookup "x");
  check string "ox tag" "abc123" (lookup "ox");
  check string "size tag" "500" (lookup "size")

(* BUD-08: Success_list should NOT include nip94 *)
let test_success_list_no_nip94 () =
  let descriptors = [{
    Domain.url = "http://localhost:8082/aaa";
    sha256 = "aaa";
    size = 100;
    mime_type = "text/plain";
    uploaded = 1000L;
  }] in
  let response = Http_response.create (Success_list descriptors) in
  let body = match Response.body response |> Body.to_string with
    | Ok s -> s
    | Error _ -> failwith "Failed to read body"
  in
  let json = Yojson.Basic.from_string body in
  let open Yojson.Basic.Util in
  let first = json |> to_list |> List.hd in
  check bool "list entries have no nip94" true (first |> member "nip94" = `Null)

(* Success_delete レスポンステスト *)
let test_success_delete () =
  let response = Http_response.create Success_delete in
  check int "Status code" 200 (get_status response);
  check_cors_headers response

(* Success_upload_check レスポンステスト *)
let test_success_upload_check () =
  let response = Http_response.create Success_upload_check in
  check int "Status code" 200 (get_status response);
  check_cors_headers response

(* Cors_preflight レスポンステスト *)
let test_cors_preflight () =
  let response = Http_response.create Cors_preflight in
  check int "Status code" 204 (get_status response);
  check_cors_headers response

(* Error_not_found レスポンステスト *)
let test_error_not_found () =
  let response = Http_response.create (Error_not_found "Resource not found") in
  check int "Status code" 404 (get_status response);
  check (option string) "X-Reason header" (Some "Resource not found") (get_header response "x-reason");
  check_cors_headers response

(* Error_unauthorized レスポンステスト *)
let test_error_unauthorized () =
  let response = Http_response.create (Error_unauthorized "Missing credentials") in
  check int "Status code" 401 (get_status response);
  check (option string) "X-Reason header" (Some "Missing credentials") (get_header response "x-reason");
  check_cors_headers response

(* Error_bad_request レスポンステスト *)
let test_error_bad_request () =
  let response = Http_response.create (Error_bad_request "Invalid input") in
  check int "Status code" 400 (get_status response);
  check (option string) "X-Reason header" (Some "Invalid input") (get_header response "x-reason");
  check_cors_headers response

(* Error_length_required レスポンステスト *)
let test_error_length_required () =
  let response = Http_response.create (Error_length_required "Missing X-Content-Length header") in
  check int "Status code" 411 (get_status response);
  check (option string) "X-Reason header" (Some "Missing X-Content-Length header") (get_header response "x-reason");
  check_cors_headers response

(* Error_internal レスポンステスト *)
let test_error_internal () =
  let response = Http_response.create (Error_internal "Database error") in
  check int "Status code" 500 (get_status response);
  check (option string) "X-Reason header" (Some "Database error") (get_header response "x-reason");
  check_cors_headers response

(* Success_list レスポンステスト（空配列） *)
let test_success_list_empty () =
  let response = Http_response.create (Success_list []) in
  check int "Status code" 200 (get_status response);
  check (option string) "Content-Type header" (Some "application/json") (get_header response "content-type");
  check_cors_headers response;
  let body = match Response.body response |> Body.to_string with
    | Ok s -> s
    | Error _ -> failwith "Failed to read body"
  in
  check string "Empty array body" "[]" body

(* Success_list レスポンステスト（複数） *)
let test_success_list_populated () =
  let descriptors = [
    {
      Domain.url = "http://localhost:8082/aaa";
      sha256 = "aaa";
      size = 100;
      mime_type = "text/plain";
      uploaded = 1000L;
    };
    {
      Domain.url = "http://localhost:8082/bbb";
      sha256 = "bbb";
      size = 200;
      mime_type = "image/png";
      uploaded = 2000L;
    };
  ] in
  let response = Http_response.create (Success_list descriptors) in
  check int "Status code" 200 (get_status response);
  check (option string) "Content-Type header" (Some "application/json") (get_header response "content-type");
  check_cors_headers response;
  let body = match Response.body response |> Body.to_string with
    | Ok s -> s
    | Error _ -> failwith "Failed to read body"
  in
  let json = Yojson.Basic.from_string body in
  let open Yojson.Basic.Util in
  let items = json |> to_list in
  check int "Array length" 2 (List.length items);
  let first = List.nth items 0 in
  check string "First sha256" "aaa" (first |> member "sha256" |> to_string);
  check string "First url" "http://localhost:8082/aaa" (first |> member "url" |> to_string);
  check int "First size" 100 (first |> member "size" |> to_int);
  check string "First type" "text/plain" (first |> member "type" |> to_string);
  check int "First uploaded" 1000 (first |> member "uploaded" |> to_int);
  let second = List.nth items 1 in
  check string "Second sha256" "bbb" (second |> member "sha256" |> to_string);
  check string "Second type" "image/png" (second |> member "type" |> to_string)

(* descriptor_to_json テスト *)
let test_descriptor_to_json () =
  let descriptor = {
    Domain.url = "http://example.com/hash";
    sha256 = "abc123def456";
    size = 2048;
    mime_type = "image/jpeg";
    uploaded = 9876543210L;
  } in
  let json_str = Http_response.descriptor_to_json descriptor in
  let json = Yojson.Basic.from_string json_str in
  let open Yojson.Basic.Util in
  check string "URL" "http://example.com/hash" (json |> member "url" |> to_string);
  check string "SHA256" "abc123def456" (json |> member "sha256" |> to_string);
  check int "Size" 2048 (json |> member "size" |> to_int);
  check string "Type" "image/jpeg" (json |> member "type" |> to_string);
  check int "Uploaded" 9876543210 (json |> member "uploaded" |> to_int)

(* Error_forbidden レスポンステスト *)
let test_error_forbidden () =
  let response = Http_response.create (Error_forbidden "Not authorized") in
  check int "Status code" 403 (get_status response);
  check (option string) "X-Reason header" (Some "Not authorized") (get_header response "x-reason");
  check_cors_headers response

(* error_to_response_kind マッピングテスト *)
let test_error_to_response_kind_auth_error () =
  let response_kind = Http_server.error_to_response_kind (Domain.Auth_error "Invalid signature") in
  let response = Http_response.create response_kind in
  check int "Auth_error maps to 401" 401 (get_status response)

let test_error_to_response_kind_forbidden () =
  let response_kind = Http_server.error_to_response_kind (Domain.Forbidden "Not authorized to delete") in
  let response = Http_response.create response_kind in
  check int "Forbidden maps to 403" 403 (get_status response)

let test_error_to_response_kind_unsupported_media_type () =
  let response_kind = Http_server.error_to_response_kind (Domain.Unsupported_media_type "MIME type not allowed: video/mp4") in
  let response = Http_response.create response_kind in
  check int "Unsupported_media_type maps to 415" 415 (get_status response)

let test_error_to_response_kind_invalid_content_type () =
  let response_kind = Http_server.error_to_response_kind (Domain.Invalid_content_type "Empty MIME type") in
  let response = Http_response.create response_kind in
  check int "Invalid_content_type maps to 400" 400 (get_status response)

let test_error_to_response_kind_storage_error () =
  let response_kind = Http_server.error_to_response_kind (Domain.Storage_error "Database connection failed") in
  let response = Http_response.create response_kind in
  check int "Storage_error maps to 500" 500 (get_status response)

let test_error_to_response_kind_blob_not_found () =
  let response_kind = Http_server.error_to_response_kind (Domain.Blob_not_found "abc123") in
  let response = Http_response.create response_kind in
  check int "Blob_not_found maps to 404" 404 (get_status response)

let test_error_to_response_kind_payload_too_large () =
  let response_kind = Http_server.error_to_response_kind (Domain.Payload_too_large (200, 100)) in
  let response = Http_response.create response_kind in
  check int "Payload_too_large maps to 413" 413 (get_status response)

let tests = [
  test_case "Success_blob response" `Quick test_success_blob;
  test_case "Success_metadata response" `Quick test_success_metadata;
  test_case "Success_upload response" `Quick test_success_upload;
  test_case "Success_upload includes nip94 (BUD-08)" `Quick test_success_upload_includes_nip94;
  test_case "Success_list excludes nip94 (BUD-08)" `Quick test_success_list_no_nip94;
  test_case "Success_list empty response" `Quick test_success_list_empty;
  test_case "Success_list populated response" `Quick test_success_list_populated;
  test_case "Success_delete response" `Quick test_success_delete;
  test_case "Success_upload_check response" `Quick test_success_upload_check;
  test_case "Cors_preflight response" `Quick test_cors_preflight;
  test_case "Error_not_found response" `Quick test_error_not_found;
  test_case "Error_unauthorized response" `Quick test_error_unauthorized;
  test_case "Error_forbidden response" `Quick test_error_forbidden;
  test_case "Error_bad_request response" `Quick test_error_bad_request;
  test_case "Error_length_required response" `Quick test_error_length_required;
  test_case "Error_internal response" `Quick test_error_internal;
  test_case "descriptor_to_json function" `Quick test_descriptor_to_json;
  (* error_to_response_kind mapping tests *)
  test_case "Auth_error -> 401" `Quick test_error_to_response_kind_auth_error;
  test_case "Forbidden -> 403" `Quick test_error_to_response_kind_forbidden;
  test_case "Unsupported_media_type -> 415" `Quick test_error_to_response_kind_unsupported_media_type;
  test_case "Invalid_content_type -> 400" `Quick test_error_to_response_kind_invalid_content_type;
  test_case "Storage_error -> 500" `Quick test_error_to_response_kind_storage_error;
  test_case "Blob_not_found -> 404" `Quick test_error_to_response_kind_blob_not_found;
  test_case "Payload_too_large -> 413" `Quick test_error_to_response_kind_payload_too_large;
]
