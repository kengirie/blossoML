(** E2E tests for upload functionality. *)

(** Compute SHA-256 hash of content as hex string *)
let sha256_hex content =
  let hash = Digestif.SHA256.digest_string content in
  Digestif.SHA256.to_hex hash

(** Test: Upload a file and download it back *)
let test_upload_and_download ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  (* Upload content *)
  let content = "Hello, Blossom! This is a test file." in
  let sha256 = sha256_hex content in

  (* Generate keypair and auth event *)
  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  let upload_result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  match upload_result with
  | Error e -> failwith ("Upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Upload returned status %d: %s" response.status response.body);

    (* Parse response to get sha256 *)
    let json = Yojson.Safe.from_string response.body in
    let sha256 = match json with
      | `Assoc fields ->
        (match List.assoc_opt "sha256" fields with
         | Some (`String s) -> s
         | _ -> failwith "No sha256 in response")
      | _ -> failwith "Invalid upload response format"
    in

    (* Download the file *)
    let download_url = base_url ^ "/" ^ sha256 in
    let download_result = Http_client.get ~sw ~env ~url:download_url () in

    match download_result with
    | Error e -> failwith ("Download failed: " ^ e)
    | Ok dl_response ->
      if dl_response.status <> 200 then
        failwith (Printf.sprintf "Download returned status %d" dl_response.status);
      if dl_response.body <> content then
        failwith (Printf.sprintf "Content mismatch: expected '%s', got '%s'" content dl_response.body)

(** Test: Upload without authorization should fail with 401 *)
let test_upload_without_auth ~sw ~env =
  let base_url = Config.base_url in
  let content = "This should fail" in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[("Content-Type", "text/plain")]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401, got %d" response.status)

(** Test: Upload with expired auth should fail *)
let test_upload_with_expired_auth ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let content = "This should fail due to expired auth" in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  (* Expiration in the past *)
  let expiration = Int64.of_float (now -. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for expired auth, got %d" response.status)

(** Test: HEAD request for uploaded file *)
let test_head_request ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  (* First upload a file *)
  let content = "Test content for HEAD request" in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  let upload_result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  match upload_result with
  | Error e -> failwith ("Upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Upload returned status %d" response.status);

    let json = Yojson.Safe.from_string response.body in
    let response_sha256 = match json with
      | `Assoc fields ->
        (match List.assoc_opt "sha256" fields with
         | Some (`String s) -> s
         | _ -> failwith "No sha256 in response")
      | _ -> failwith "Invalid upload response format"
    in

    (* Verify sha256 matches *)
    if response_sha256 <> sha256 then
      failwith (Printf.sprintf "sha256 mismatch: expected %s, got %s" sha256 response_sha256);

    (* HEAD request *)
    let head_url = base_url ^ "/" ^ response_sha256 in
    let head_result = Http_client.head ~sw ~env ~url:head_url () in

    match head_result with
    | Error e -> failwith ("HEAD request failed: " ^ e)
    | Ok head_response ->
      if head_response.status <> 200 then
        failwith (Printf.sprintf "HEAD returned status %d" head_response.status);
      (* Check Content-Length header *)
      let content_length =
        List.find_opt (fun (k, _) -> String.lowercase_ascii k = "content-length") head_response.headers
      in
      match content_length with
      | None -> failwith "No Content-Length header in HEAD response"
      | Some (_, len_str) ->
        let expected_len = String.length content in
        if int_of_string len_str <> expected_len then
          failwith (Printf.sprintf "Content-Length mismatch: expected %d, got %s" expected_len len_str)

(** Test: Download non-existent file should return 404 *)
let test_download_not_found ~sw ~env =
  let base_url = Config.base_url in
  let fake_hash = "0000000000000000000000000000000000000000000000000000000000000000" in
  let url = base_url ^ "/" ^ fake_hash in

  let result = Http_client.get ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 404 then
      failwith (Printf.sprintf "Expected 404, got %d" response.status)

(** Test: Upload with invalid signature should fail with 401 *)
let test_upload_with_invalid_signature ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let content = "Test content for invalid signature" in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth_invalid_sig ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for invalid signature, got %d" response.status)

(** Test: Upload with invalid pubkey should fail with 401 *)
let test_upload_with_invalid_pubkey ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let content = "Test content for invalid pubkey" in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth_invalid_pubkey ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for invalid pubkey, got %d" response.status)

(** Test: Upload with invalid JSON in auth header should fail with 401 *)
let test_upload_with_invalid_json ~sw ~env =
  let base_url = Config.base_url in
  let content = "Test content for invalid JSON" in
  let upload_url = base_url ^ "/upload" in

  (* Create invalid JSON and base64 encode it *)
  let invalid_json = "{invalid json" in
  let auth_header = "Nostr " ^ Base64.encode_string invalid_json in

  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for invalid JSON, got %d" response.status)

(** Test: Upload with non-base64 auth header should fail with 401 *)
let test_upload_with_invalid_base64 ~sw ~env =
  let base_url = Config.base_url in
  let content = "Test content for invalid base64" in
  let upload_url = base_url ^ "/upload" in

  (* Non-base64 characters *)
  let auth_header = "Nostr !!!not-valid-base64!!!" in

  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for invalid base64, got %d" response.status)

(** Test: Upload with wrong t tag should fail with 401 *)
let test_upload_with_wrong_t_tag ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let content = "Test content for wrong t tag" in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth_wrong_t_tag ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for wrong t tag, got %d" response.status)

(** Test: Upload with sha256 mismatch should fail with 401 and blob should be deleted *)
let test_upload_with_sha256_mismatch ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let content = "Test content for sha256 mismatch" in
  (* Calculate the actual sha256 of the content we're uploading *)
  let actual_sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  (* Auth event has wrong sha256 *)
  let auth_event = Nostr_signer.create_upload_auth_wrong_sha256 ~keypair ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for sha256 mismatch, got %d" response.status);

    (* Verify the blob was deleted after sha256 mismatch *)
    let get_url = base_url ^ "/" ^ actual_sha256 in
    let get_result = Http_client.get ~sw ~env ~url:get_url () in
    match get_result with
    | Error e -> failwith ("GET request failed unexpectedly: " ^ e)
    | Ok get_response ->
      if get_response.status <> 404 then
        failwith (Printf.sprintf "Expected 404 for deleted blob after sha256 mismatch, got %d" get_response.status)

(** Test: Upload without expiration tag should fail with 401 *)
let test_upload_without_expiration ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let content = "Test content without expiration" in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let auth_event = Nostr_signer.create_upload_auth_no_expiration ~keypair ~sha256 ~created_at:now in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for missing expiration, got %d" response.status)

(** Test: Upload with wrong kind should fail with 401 *)
let test_upload_with_wrong_kind ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let content = "Test content for wrong kind" in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth_wrong_kind ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for wrong kind, got %d" response.status)

(** Test: Upload with future created_at should fail with 401 *)
let test_upload_with_future_created_at ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let content = "Test content for future created_at" in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 7200.) in
  let auth_event = Nostr_signer.create_upload_auth_future_created_at ~keypair ~sha256 ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for future created_at, got %d" response.status)

(** Test: Upload empty file *)
let test_upload_empty_file ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let content = "" in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "application/octet-stream");
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Upload returned status %d: %s" response.status response.body);

    (* Verify response contains correct sha256 *)
    let json = Yojson.Safe.from_string response.body in
    let response_sha256 = match json with
      | `Assoc fields ->
        (match List.assoc_opt "sha256" fields with
         | Some (`String s) -> s
         | _ -> failwith "No sha256 in response")
      | _ -> failwith "Invalid upload response format"
    in
    if response_sha256 <> sha256 then
      failwith (Printf.sprintf "sha256 mismatch: expected %s, got %s" sha256 response_sha256);

    (* Verify size is 0 *)
    let size = match json with
      | `Assoc fields ->
        (match List.assoc_opt "size" fields with
         | Some (`Int n) -> n
         | _ -> failwith "No size in response")
      | _ -> failwith "Invalid upload response format"
    in
    if size <> 0 then
      failwith (Printf.sprintf "Expected size 0, got %d" size)

(** Test: Upload large file *)
let test_upload_large_file ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  (* Create a 1MB file *)
  let content = String.make (1024 * 1024) 'X' in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "application/octet-stream");
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Upload returned status %d: %s" response.status response.body);

    (* Verify size *)
    let json = Yojson.Safe.from_string response.body in
    let size = match json with
      | `Assoc fields ->
        (match List.assoc_opt "size" fields with
         | Some (`Int n) -> n
         | _ -> failwith "No size in response")
      | _ -> failwith "Invalid upload response format"
    in
    if size <> 1024 * 1024 then
      failwith (Printf.sprintf "Expected size %d, got %d" (1024 * 1024) size);

    (* Download and verify content *)
    let response_sha256 = match json with
      | `Assoc fields ->
        (match List.assoc_opt "sha256" fields with
         | Some (`String s) -> s
         | _ -> failwith "No sha256 in response")
      | _ -> failwith "Invalid upload response format"
    in
    let download_url = base_url ^ "/" ^ response_sha256 in
    let download_result = Http_client.get ~sw ~env ~url:download_url () in
    match download_result with
    | Error e -> failwith ("Download failed: " ^ e)
    | Ok dl_response ->
      if dl_response.status <> 200 then
        failwith (Printf.sprintf "Download returned status %d" dl_response.status);
      if dl_response.body <> content then
        failwith "Downloaded content does not match uploaded content"

(** Test: Upload binary data *)
let test_upload_binary_data ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  (* Create binary content with all byte values 0-255 *)
  let content = String.init 256 (fun i -> Char.chr i) in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "application/octet-stream");
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Upload returned status %d: %s" response.status response.body);

    (* Download and verify content *)
    let json = Yojson.Safe.from_string response.body in
    let response_sha256 = match json with
      | `Assoc fields ->
        (match List.assoc_opt "sha256" fields with
         | Some (`String s) -> s
         | _ -> failwith "No sha256 in response")
      | _ -> failwith "Invalid upload response format"
    in
    let download_url = base_url ^ "/" ^ response_sha256 in
    let download_result = Http_client.get ~sw ~env ~url:download_url () in
    match download_result with
    | Error e -> failwith ("Download failed: " ^ e)
    | Ok dl_response ->
      if dl_response.status <> 200 then
        failwith (Printf.sprintf "Download returned status %d" dl_response.status);
      if dl_response.body <> content then
        failwith "Downloaded binary content does not match uploaded content"

(** Test: Upload same file twice (idempotency) *)
let test_upload_idempotent ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let content = "Idempotent upload test content" in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let upload_url = base_url ^ "/upload" in

  (* First upload *)
  let auth_event1 = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header1 = Nostr_signer.to_auth_header auth_event1 in

  let result1 = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header1);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  (match result1 with
  | Error e -> failwith ("First upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "First upload returned status %d" response.status));

  (* Second upload with same content *)
  let auth_event2 = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header2 = Nostr_signer.to_auth_header auth_event2 in

  let result2 = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header2);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  match result2 with
  | Error e -> failwith ("Second upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Second upload returned status %d" response.status);

    (* Verify sha256 is the same *)
    let json = Yojson.Safe.from_string response.body in
    let response_sha256 = match json with
      | `Assoc fields ->
        (match List.assoc_opt "sha256" fields with
         | Some (`String s) -> s
         | _ -> failwith "No sha256 in response")
      | _ -> failwith "Invalid upload response format"
    in
    if response_sha256 <> sha256 then
      failwith (Printf.sprintf "sha256 mismatch on second upload: expected %s, got %s" sha256 response_sha256)

(** Test: Upload same file from different pubkeys *)
let test_upload_same_file_different_pubkeys ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let content = "Same content different pubkeys" in
  let sha256 = sha256_hex content in
  let expiration = Int64.of_float (now +. 3600.) in
  let upload_url = base_url ^ "/upload" in

  (* First upload with keypair1 *)
  let keypair1 = Nostr_signer.generate_keypair () in
  let auth_event1 = Nostr_signer.create_upload_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let auth_header1 = Nostr_signer.to_auth_header auth_event1 in

  let result1 = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header1);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  (match result1 with
  | Error e -> failwith ("First upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "First upload returned status %d" response.status));

  (* Second upload with keypair2 *)
  let keypair2 = Nostr_signer.generate_keypair () in
  let auth_event2 = Nostr_signer.create_upload_auth ~keypair:keypair2 ~sha256 ~created_at:now ~expiration in
  let auth_header2 = Nostr_signer.to_auth_header auth_event2 in

  let result2 = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header2);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  match result2 with
  | Error e -> failwith ("Second upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Second upload returned status %d" response.status);

    (* Both uploads should succeed and return same sha256 *)
    let json = Yojson.Safe.from_string response.body in
    let response_sha256 = match json with
      | `Assoc fields ->
        (match List.assoc_opt "sha256" fields with
         | Some (`String s) -> s
         | _ -> failwith "No sha256 in response")
      | _ -> failwith "Invalid upload response format"
    in
    if response_sha256 <> sha256 then
      failwith (Printf.sprintf "sha256 mismatch: expected %s, got %s" sha256 response_sha256)

(** Test: Upload without Content-Type header *)
let test_upload_without_content_type ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let content = "Content without Content-Type header" in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[("Authorization", auth_header)]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Upload failed: " ^ e)
  | Ok response ->
    (* Should succeed - Content-Type is optional *)
    if response.status <> 200 then
      failwith (Printf.sprintf "Upload without Content-Type returned status %d: %s" response.status response.body)

(** Test: Upload with various Content-Type headers
    Note: MIME type is now determined by byte inspection first, then falls back to
    client-provided Content-Type if detection fails. Since text content cannot be
    detected by magic bytes, the fallback Content-Type is used. *)
let test_upload_various_content_types ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let upload_url = base_url ^ "/upload" in

  (* For text content that can't be detected, the fallback Content-Type is used *)
  let content_types = [
    "image/png";
    "image/jpeg";
    "application/octet-stream";
    "application/pdf";
    "video/mp4";
    "audio/mpeg";
  ] in

  List.iter (fun content_type ->
    let content = Printf.sprintf "Content with type: %s" content_type in
    let sha256 = sha256_hex content in

    let keypair = Nostr_signer.generate_keypair () in
    let expiration = Int64.of_float (now +. 3600.) in
    let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
    let auth_header = Nostr_signer.to_auth_header auth_event in

    let result = Http_client.put
      ~sw ~env
      ~url:upload_url
      ~headers:[
        ("Authorization", auth_header);
        ("Content-Type", content_type);
      ]
      ~body:content
      ()
    in

    match result with
    | Error e -> failwith (Printf.sprintf "Upload with Content-Type %s failed: %s" content_type e)
    | Ok response ->
      if response.status <> 200 then
        failwith (Printf.sprintf "Upload with Content-Type %s returned status %d" content_type response.status);

      (* Verify the type is returned in response
         Since content is plain text, byte detection fails and fallback is used *)
      let json = Yojson.Safe.from_string response.body in
      let response_type = match json with
        | `Assoc fields ->
          (match List.assoc_opt "type" fields with
           | Some (`String s) -> s
           | _ -> failwith "No type in response")
        | _ -> failwith "Invalid upload response format"
      in
      if response_type <> content_type then
        failwith (Printf.sprintf "Content-Type mismatch: expected %s, got %s" content_type response_type)
  ) content_types

(** Test: Verify upload response format contains all required fields *)
let test_upload_response_format ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let content = "Content for response format test" in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Upload returned status %d" response.status);

    let json = Yojson.Safe.from_string response.body in
    match json with
    | `Assoc fields ->
      (* Check url field *)
      (match List.assoc_opt "url" fields with
       | Some (`String url) ->
         if String.length url = 0 then
           failwith "url field is empty";
         if String.length url < 4 || String.sub url 0 4 <> "http" then
           failwith (Printf.sprintf "url should start with http, got: %s" url)
       | _ -> failwith "Missing or invalid 'url' field");

      (* Check sha256 field *)
      (match List.assoc_opt "sha256" fields with
       | Some (`String s) ->
         if String.length s <> 64 then
           failwith (Printf.sprintf "sha256 should be 64 chars, got %d" (String.length s));
         if s <> sha256 then
           failwith (Printf.sprintf "sha256 mismatch: expected %s, got %s" sha256 s)
       | _ -> failwith "Missing or invalid 'sha256' field");

      (* Check size field *)
      (match List.assoc_opt "size" fields with
       | Some (`Int size) ->
         if size <> String.length content then
           failwith (Printf.sprintf "size mismatch: expected %d, got %d" (String.length content) size)
       | _ -> failwith "Missing or invalid 'size' field");

      (* Check type field *)
      (match List.assoc_opt "type" fields with
       | Some (`String t) ->
         if t <> "text/plain" then
           failwith (Printf.sprintf "type mismatch: expected text/plain, got %s" t)
       | _ -> failwith "Missing or invalid 'type' field");

      (* Check uploaded field (timestamp) *)
      (match List.assoc_opt "uploaded" fields with
       | Some (`Int _) -> ()
       | _ -> failwith "Missing or invalid 'uploaded' field")
    | _ -> failwith "Response is not a JSON object"

(** Test: Upload with very long Content-Type *)
let test_upload_long_content_type ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let content = "Content with long Content-Type" in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  (* Create a long but valid Content-Type with parameters *)
  let long_content_type = "application/octet-stream; charset=utf-8; boundary=" ^ String.make 200 'x' in

  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", long_content_type);
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Upload with long Content-Type failed: " ^ e)
  | Ok response ->
    (* Should succeed - server should handle long Content-Type *)
    if response.status <> 200 then
      failwith (Printf.sprintf "Upload with long Content-Type returned status %d: %s" response.status response.body)

(** Test: Upload with Content-Type containing special characters *)
let test_upload_special_content_type ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let content = "Content with special Content-Type" in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  let special_content_types = [
    "text/plain; charset=utf-8";
    "multipart/form-data; boundary=----WebKitFormBoundary";
    "application/x-www-form-urlencoded";
  ] in

  List.iter (fun content_type ->
    let result = Http_client.put
      ~sw ~env
      ~url:upload_url
      ~headers:[
        ("Authorization", auth_header);
        ("Content-Type", content_type);
      ]
      ~body:content
      ()
    in

    match result with
    | Error e -> failwith (Printf.sprintf "Upload with Content-Type '%s' failed: %s" content_type e)
    | Ok response ->
      if response.status <> 200 then
        failwith (Printf.sprintf "Upload with Content-Type '%s' returned status %d" content_type response.status)
  ) special_content_types

(** Test: Upload with X-Content-Type header (fallback when Content-Type is empty) *)
let test_upload_with_x_content_type ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let content = "Content with X-Content-Type header" in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  (* Empty Content-Type, but X-Content-Type is set *)
  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "");
      ("X-Content-Type", "image/png");
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Upload with X-Content-Type failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Upload with X-Content-Type returned status %d: %s" response.status response.body);

    (* Verify X-Content-Type was used *)
    let json = Yojson.Safe.from_string response.body in
    let response_type = match json with
      | `Assoc fields ->
        (match List.assoc_opt "type" fields with
         | Some (`String s) -> s
         | _ -> failwith "No type in response")
      | _ -> failwith "Invalid upload response format"
    in
    if response_type <> "image/png" then
      failwith (Printf.sprintf "Expected type 'image/png' from X-Content-Type, got '%s'" response_type)

(** Test: Upload relies on MIME sniffing when no Content-Type headers *)
let test_upload_mime_sniffing ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  (* PNG magic bytes + IHDR chunk (required for conan detection) *)
  let png_header = "\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR" in
  let content = png_header ^ String.make 100 '\000' in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  (* No Content-Type or X-Content-Type - should fall back to MIME sniffing *)
  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[("Authorization", auth_header)]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Upload for MIME sniffing failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Upload for MIME sniffing returned status %d: %s" response.status response.body);

    (* Verify MIME sniffing detected PNG *)
    let json = Yojson.Safe.from_string response.body in
    let response_type = match json with
      | `Assoc fields ->
        (match List.assoc_opt "type" fields with
         | Some (`String s) -> s
         | _ -> failwith "No type in response")
      | _ -> failwith "Invalid upload response format"
    in
    if response_type <> "image/png" then
      failwith (Printf.sprintf "Expected MIME sniffing to detect 'image/png', got '%s'" response_type)

(** Test: Byte inspection overrides client-provided Content-Type
    When client says "text/plain" but content is actually PNG, server should detect PNG *)
let test_upload_byte_detection_overrides_content_type ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  (* PNG magic bytes + IHDR chunk - this IS a PNG file *)
  let png_header = "\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR" in
  let content = png_header ^ String.make 100 '\000' in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  (* Client claims it's text/plain, but it's actually PNG *)
  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Upload returned status %d: %s" response.status response.body);

    (* Verify byte detection detected PNG, not the client's text/plain *)
    let json = Yojson.Safe.from_string response.body in
    let response_type = match json with
      | `Assoc fields ->
        (match List.assoc_opt "type" fields with
         | Some (`String s) -> s
         | _ -> failwith "No type in response")
      | _ -> failwith "Invalid upload response format"
    in
    if response_type <> "image/png" then
      failwith (Printf.sprintf "Expected byte detection to override Content-Type with 'image/png', got '%s'" response_type)

(* Note: Tests for invalid/negative Content-Length are not possible in E2E tests
   because the HTTP client (Piaf/httpun) validates Content-Length before sending.
   These cases should be tested via unit tests if needed. *)

(** Test: Upload with Content-Length exceeding max size should return 413 before reading body *)
let test_upload_content_length_exceeds_max ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let content = "Small content" in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let upload_url = base_url ^ "/upload" in

  (* Claim Content-Length is 200MB (exceeds 100MB limit) *)
  let result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "text/plain");
      ("Content-Length", "209715200");
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 413 then
      failwith (Printf.sprintf "Expected 413 for Content-Length exceeding max, got %d" response.status)

(** Test: Upload exceeding max size during streaming should return 413 *)
let test_upload_streaming_exceeds_max ~sw ~env =
  let base_url = Config.base_url in
  let upload_url = base_url ^ "/upload" in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let fake_sha = String.make 64 '0' in
  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256:fake_sha ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in

  (* Stream 101 chunks of 1MB each (>100MB) using chunked encoding so the server
     must enforce the limit during BlobService.save. *)
  let chunk_size = 1024 * 1024 in
  let chunks = 101 in

  let result = Http_client.put_streaming
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "application/octet-stream");
    ]
    ~chunk_size
    ~chunks
    ()
  in

  match result with
  | Error e -> failwith ("Streaming upload request failed: " ^ e)
  | Ok response ->
    if response.status <> 413 then
      failwith (Printf.sprintf "Expected 413 for streaming size exceed, got %d" response.status)

(** All tests *)
let tests = [
  ("upload and download", test_upload_and_download);
  ("upload without auth", test_upload_without_auth);
  ("upload with expired auth", test_upload_with_expired_auth);
  ("upload with invalid signature", test_upload_with_invalid_signature);
  ("upload with invalid pubkey", test_upload_with_invalid_pubkey);
  ("upload with invalid JSON", test_upload_with_invalid_json);
  ("upload with invalid base64", test_upload_with_invalid_base64);
  ("upload with wrong t tag", test_upload_with_wrong_t_tag);
  ("upload with sha256 mismatch", test_upload_with_sha256_mismatch);
  ("upload without expiration", test_upload_without_expiration);
  ("upload with wrong kind", test_upload_with_wrong_kind);
  ("upload with future created_at", test_upload_with_future_created_at);
  ("upload empty file", test_upload_empty_file);
  ("upload large file", test_upload_large_file);
  ("upload binary data", test_upload_binary_data);
  ("upload idempotent", test_upload_idempotent);
  ("upload same file different pubkeys", test_upload_same_file_different_pubkeys);
  ("upload without Content-Type", test_upload_without_content_type);
  ("upload various Content-Types", test_upload_various_content_types);
  ("upload response format", test_upload_response_format);
  ("upload long Content-Type", test_upload_long_content_type);
  ("upload special Content-Type", test_upload_special_content_type);
  ("upload with X-Content-Type", test_upload_with_x_content_type);
  ("upload MIME sniffing", test_upload_mime_sniffing);
  ("upload byte detection overrides Content-Type", test_upload_byte_detection_overrides_content_type);
  ("upload Content-Length exceeds max", test_upload_content_length_exceeds_max);
  ("upload streaming exceeds max", test_upload_streaming_exceeds_max);
  ("HEAD request", test_head_request);
  ("download not found", test_download_not_found);
]
