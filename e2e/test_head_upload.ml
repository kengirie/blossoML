(** E2E tests for HEAD /upload (BUD-06: Upload requirements check). *)

(** Test: HEAD /upload with valid headers and auth returns 200 *)
let test_head_upload_success ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let upload_url = base_url ^ "/upload" in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let sha256 = "88a74d0b866c8ba79251a11fe5ac807839226870e77355f02eaf68b156522576" in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in

  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "1024");
      ("X-Content-Type", "image/png");
      ("X-SHA-256", sha256);
      ("Authorization", auth_header);
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Expected 200, got %d" response.status)

(** Test: HEAD /upload without Authorization returns 401 *)
let test_head_upload_missing_auth ~sw ~env =
  let base_url = Config.base_url in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "1024");
      ("X-Content-Type", "image/png");
      ("X-SHA-256", "88a74d0b866c8ba79251a11fe5ac807839226870e77355f02eaf68b156522576");
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for missing Authorization, got %d" response.status)

(** Test: HEAD /upload without X-Content-Length returns 411 *)
let test_head_upload_missing_content_length ~sw ~env =
  let base_url = Config.base_url in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Type", "image/png");
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    if response.status <> 411 then
      failwith (Printf.sprintf "Expected 411 for missing X-Content-Length, got %d" response.status);
    (* Check X-Reason header *)
    let x_reason = List.find_opt (fun (k, _) -> String.lowercase_ascii k = "x-reason") response.headers in
    match x_reason with
    | None -> failwith "Expected X-Reason header in 411 response"
    | Some (_, _) -> ()

(** Test: HEAD /upload with invalid X-Content-Length format returns 400 *)
let test_head_upload_invalid_content_length ~sw ~env =
  let base_url = Config.base_url in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "not-a-number");
      ("X-Content-Type", "image/png");
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    if response.status <> 400 then
      failwith (Printf.sprintf "Expected 400 for invalid X-Content-Length, got %d" response.status)

(** Test: HEAD /upload with negative X-Content-Length returns 400 *)
let test_head_upload_negative_content_length ~sw ~env =
  let base_url = Config.base_url in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "-100");
      ("X-Content-Type", "image/png");
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    if response.status <> 400 then
      failwith (Printf.sprintf "Expected 400 for negative X-Content-Length, got %d" response.status)

(** Test: HEAD /upload with invalid X-SHA-256 format returns 400 *)
let test_head_upload_invalid_sha256 ~sw ~env =
  let base_url = Config.base_url in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "1024");
      ("X-Content-Type", "image/png");
      ("X-SHA-256", "invalid-hash");
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    if response.status <> 400 then
      failwith (Printf.sprintf "Expected 400 for invalid X-SHA-256, got %d" response.status)

(** Test: HEAD /upload with size exceeding max returns 413 *)
let test_head_upload_size_exceeds_max ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let upload_url = base_url ^ "/upload" in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let sha256 = "88a74d0b866c8ba79251a11fe5ac807839226870e77355f02eaf68b156522576" in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in

  (* 200MB exceeds the 100MB limit *)
  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "209715200");
      ("X-Content-Type", "image/png");
      ("X-SHA-256", sha256);
      ("Authorization", auth_header);
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    if response.status <> 413 then
      failwith (Printf.sprintf "Expected 413 for size exceeding max, got %d" response.status)

(** Test: HEAD /upload without X-Content-Type uses default *)
let test_head_upload_default_content_type ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let upload_url = base_url ^ "/upload" in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let sha256 = "88a74d0b866c8ba79251a11fe5ac807839226870e77355f02eaf68b156522576" in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in

  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "1024");
      ("X-SHA-256", sha256);
      ("Authorization", auth_header);
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    (* Should succeed with default application/octet-stream *)
    if response.status <> 200 then
      failwith (Printf.sprintf "Expected 200 for default Content-Type, got %d" response.status)

(** Test: HEAD /upload with valid authorization returns 200 *)
let test_head_upload_with_valid_auth ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let upload_url = base_url ^ "/upload" in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let sha256 = "88a74d0b866c8ba79251a11fe5ac807839226870e77355f02eaf68b156522576" in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in

  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "1024");
      ("X-Content-Type", "image/png");
      ("X-SHA-256", sha256);
      ("Authorization", auth_header);
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Expected 200 with valid auth, got %d" response.status)

(** Test: HEAD /upload with invalid authorization returns 401 (same as PUT) *)
let test_head_upload_with_invalid_auth ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let upload_url = base_url ^ "/upload" in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let sha256 = "88a74d0b866c8ba79251a11fe5ac807839226870e77355f02eaf68b156522576" in
  (* Create auth with invalid signature *)
  let auth_event = Nostr_signer.create_upload_auth_invalid_sig ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in

  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "1024");
      ("X-Content-Type", "image/png");
      ("X-SHA-256", sha256);
      ("Authorization", auth_header);
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for invalid auth, got %d" response.status)

(** Test: HEAD /upload with expired authorization returns 401 (same as PUT) *)
let test_head_upload_with_expired_auth ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let upload_url = base_url ^ "/upload" in

  let keypair = Nostr_signer.generate_keypair () in
  (* Expiration in the past *)
  let expiration = Int64.of_float (now -. 3600.) in
  let sha256 = "88a74d0b866c8ba79251a11fe5ac807839226870e77355f02eaf68b156522576" in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in

  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "1024");
      ("X-Content-Type", "image/png");
      ("X-SHA-256", sha256);
      ("Authorization", auth_header);
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for expired auth, got %d" response.status)

(** Test: HEAD /upload with wrong t tag returns 401 (same as PUT) *)
let test_head_upload_with_wrong_t_tag ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let upload_url = base_url ^ "/upload" in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let sha256 = "88a74d0b866c8ba79251a11fe5ac807839226870e77355f02eaf68b156522576" in
  (* Create auth with wrong t tag (delete instead of upload) *)
  let auth_event = Nostr_signer.create_upload_auth_wrong_t_tag ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in

  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "1024");
      ("X-Content-Type", "image/png");
      ("X-SHA-256", sha256);
      ("Authorization", auth_header);
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for wrong t tag, got %d" response.status)

(** Test: HEAD /upload with malformed auth header returns 401 (same as PUT) *)
let test_head_upload_with_malformed_auth ~sw ~env =
  let base_url = Config.base_url in
  let upload_url = base_url ^ "/upload" in

  (* Invalid base64 in auth header *)
  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "1024");
      ("X-Content-Type", "image/png");
      ("Authorization", "Nostr !!!invalid-base64!!!");
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for malformed auth, got %d" response.status)

(** Test: HEAD /upload returns CORS headers *)
let test_head_upload_cors_headers ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let upload_url = base_url ^ "/upload" in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let sha256 = "88a74d0b866c8ba79251a11fe5ac807839226870e77355f02eaf68b156522576" in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in

  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "1024");
      ("X-Content-Type", "image/png");
      ("X-SHA-256", sha256);
      ("Authorization", auth_header);
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Expected 200, got %d" response.status);
    (* Check for CORS headers *)
    let has_cors_origin = List.exists
      (fun (k, v) -> String.lowercase_ascii k = "access-control-allow-origin" && v = "*")
      response.headers
    in
    if not has_cors_origin then
      failwith "Missing Access-Control-Allow-Origin header"

(** Test: HEAD /upload with zero size is valid *)
let test_head_upload_zero_size ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let upload_url = base_url ^ "/upload" in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let sha256 = "88a74d0b866c8ba79251a11fe5ac807839226870e77355f02eaf68b156522576" in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in

  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "0");
      ("X-Content-Type", "application/octet-stream");
      ("X-SHA-256", sha256);
      ("Authorization", auth_header);
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Expected 200 for zero size, got %d" response.status)

(** Test: HEAD /upload at exactly max size is valid *)
let test_head_upload_max_size ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let upload_url = base_url ^ "/upload" in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let sha256 = "88a74d0b866c8ba79251a11fe5ac807839226870e77355f02eaf68b156522576" in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in

  (* Exactly 100MB = 104857600 bytes *)
  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "104857600");
      ("X-Content-Type", "application/octet-stream");
      ("X-SHA-256", sha256);
      ("Authorization", auth_header);
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Expected 200 for max size, got %d" response.status)

(** Test: HEAD /upload at max size + 1 byte returns 413 *)
let test_head_upload_over_max_size ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let upload_url = base_url ^ "/upload" in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let sha256 = "88a74d0b866c8ba79251a11fe5ac807839226870e77355f02eaf68b156522576" in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in

  (* 100MB + 1 = 104857601 bytes *)
  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "104857601");
      ("X-Content-Type", "application/octet-stream");
      ("X-SHA-256", sha256);
      ("Authorization", auth_header);
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    if response.status <> 413 then
      failwith (Printf.sprintf "Expected 413 for over max size, got %d" response.status)

(** Test: HEAD /upload with X-SHA-256 mismatch in auth x tag returns 401 (same as PUT) *)
let test_head_upload_x_tag_mismatch ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let upload_url = base_url ^ "/upload" in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  (* Auth event has x tag for different hash *)
  let auth_sha256 = "1111111111111111111111111111111111111111111111111111111111111111" in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256:auth_sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in

  (* X-SHA-256 header has different hash than auth x tag *)
  let header_sha256 = "2222222222222222222222222222222222222222222222222222222222222222" in

  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "1024");
      ("X-Content-Type", "image/png");
      ("X-SHA-256", header_sha256);
      ("Authorization", auth_header);
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    (* Should return 401 because x tag doesn't match X-SHA-256 *)
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for x tag mismatch, got %d" response.status)

(** Test: HEAD /upload with matching X-SHA-256 and auth x tag returns 200 *)
let test_head_upload_x_tag_match ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let upload_url = base_url ^ "/upload" in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let sha256 = "88a74d0b866c8ba79251a11fe5ac807839226870e77355f02eaf68b156522576" in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in

  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "1024");
      ("X-Content-Type", "image/png");
      ("X-SHA-256", sha256);
      ("Authorization", auth_header);
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Expected 200 for matching x tag, got %d" response.status)

(** Test: HEAD /upload without X-SHA-256 but with auth still validates basic auth *)
let test_head_upload_auth_without_x_sha256 ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let upload_url = base_url ^ "/upload" in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let sha256 = "88a74d0b866c8ba79251a11fe5ac807839226870e77355f02eaf68b156522576" in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in

  (* No X-SHA-256 header, but Authorization is provided *)
  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "1024");
      ("X-Content-Type", "image/png");
      ("Authorization", auth_header);
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    (* Should succeed - basic auth is valid, x tag not checked without X-SHA-256 *)
    if response.status <> 200 then
      failwith (Printf.sprintf "Expected 200 for auth without X-SHA-256, got %d" response.status)

(** Test: HEAD /upload Content-Type takes priority over X-Content-Type (same as PUT) *)
let test_head_upload_content_type_priority ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let upload_url = base_url ^ "/upload" in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let sha256 = "88a74d0b866c8ba79251a11fe5ac807839226870e77355f02eaf68b156522576" in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in

  (* Both Content-Type and X-Content-Type provided, Content-Type should win *)
  let result = Http_client.head
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("X-Content-Length", "1024");
      ("Content-Type", "image/png");
      ("X-Content-Type", "application/pdf");
      ("X-SHA-256", sha256);
      ("Authorization", auth_header);
    ]
    ()
  in

  match result with
  | Error e -> failwith ("HEAD /upload failed: " ^ e)
  | Ok response ->
    (* Should succeed - image/png is used (Content-Type priority) *)
    if response.status <> 200 then
      failwith (Printf.sprintf "Expected 200, got %d" response.status)

(** All tests *)
let tests = [
  ("HEAD /upload success", test_head_upload_success);
  ("HEAD /upload missing Authorization", test_head_upload_missing_auth);
  ("HEAD /upload missing X-Content-Length", test_head_upload_missing_content_length);
  ("HEAD /upload invalid X-Content-Length", test_head_upload_invalid_content_length);
  ("HEAD /upload negative X-Content-Length", test_head_upload_negative_content_length);
  ("HEAD /upload invalid X-SHA-256", test_head_upload_invalid_sha256);
  ("HEAD /upload size exceeds max", test_head_upload_size_exceeds_max);
  ("HEAD /upload default Content-Type", test_head_upload_default_content_type);
  ("HEAD /upload with valid auth", test_head_upload_with_valid_auth);
  ("HEAD /upload with invalid auth", test_head_upload_with_invalid_auth);
  ("HEAD /upload with expired auth", test_head_upload_with_expired_auth);
  ("HEAD /upload with wrong t tag", test_head_upload_with_wrong_t_tag);
  ("HEAD /upload with malformed auth", test_head_upload_with_malformed_auth);
  ("HEAD /upload CORS headers", test_head_upload_cors_headers);
  ("HEAD /upload zero size", test_head_upload_zero_size);
  ("HEAD /upload max size", test_head_upload_max_size);
  ("HEAD /upload over max size", test_head_upload_over_max_size);
  ("HEAD /upload x tag mismatch", test_head_upload_x_tag_mismatch);
  ("HEAD /upload x tag match", test_head_upload_x_tag_match);
  ("HEAD /upload auth without X-SHA-256", test_head_upload_auth_without_x_sha256);
  ("HEAD /upload Content-Type priority", test_head_upload_content_type_priority);
]
