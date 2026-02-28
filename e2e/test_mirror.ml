(** E2E tests for BUD-04 mirror functionality. *)

(** Extract SHA256 from a Blossom blob URL.
    e.g. "https://host/5c5285...d1" → "5c5285...d1"
    Also handles URLs with extensions like "https://host/abc123.mp4" *)
let sha256_from_url url =
  let path =
    (* Remove query/fragment *)
    let s = match String.index_opt url '?' with
      | Some i -> String.sub url 0 i | None -> url in
    match String.index_opt s '#' with
    | Some i -> String.sub s 0 i | None -> s
  in
  let basename = match String.rindex_opt path '/' with
    | Some i -> String.sub path (i + 1) (String.length path - i - 1)
    | None -> path
  in
  (* Strip extension if present *)
  match String.index_opt basename '.' with
  | Some i -> String.sub basename 0 i
  | None -> basename

(** Mirror happy-path tests require E2E_MIRROR_URL (an external Blossom blob URL).
    When unset, these tests are skipped. *)
let skip_unless_mirror_url () =
  match Config.mirror_url with
  | Some _ -> true
  | None ->
    Eio.traceln "(skipped: E2E_MIRROR_URL not set)";
    false

(** Get the external mirror URL, or failwith *)
let get_mirror_url () =
  match Config.mirror_url with
  | Some url -> url
  | None -> failwith "E2E_MIRROR_URL not set"

(** Test: Mirror a blob from an external Blossom server *)
let test_mirror_basic ~sw ~env =
  if not (skip_unless_mirror_url ()) then () else
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let source_url = get_mirror_url () in
  let sha256 = sha256_from_url source_url in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let mirror_url = base_url ^ "/mirror" in
  let mirror_body = Printf.sprintf {|{"url": "%s"}|} source_url in

  let mirror_result = Http_client.put
    ~sw ~env
    ~url:mirror_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "application/json");
    ]
    ~body:mirror_body
    ()
  in

  match mirror_result with
  | Error e -> failwith ("Mirror failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Mirror returned status %d: %s" response.status response.body);

    (* Verify response contains correct sha256 *)
    let json = Yojson.Safe.from_string response.body in
    let response_sha256 = match json with
      | `Assoc fields ->
        (match List.assoc_opt "sha256" fields with
         | Some (`String s) -> s
         | _ -> failwith "No sha256 in mirror response")
      | _ -> failwith "Invalid mirror response format"
    in
    if response_sha256 <> sha256 then
      failwith (Printf.sprintf "sha256 mismatch: expected %s, got %s" sha256 response_sha256)

(** Test: Mirror without authorization should fail with 401 *)
let test_mirror_without_auth ~sw ~env =
  let base_url = Config.base_url in
  let mirror_url = base_url ^ "/mirror" in
  let mirror_body = {|{"url": "https://example.com/blob"}|} in

  let result = Http_client.put
    ~sw ~env
    ~url:mirror_url
    ~headers:[("Content-Type", "application/json")]
    ~body:mirror_body
    ()
  in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401, got %d" response.status)

(** Test: Mirror with invalid JSON body should fail with 400 *)
let test_mirror_invalid_json ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let keypair = Nostr_signer.generate_keypair () in
  let sha256 = String.make 64 '0' in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let mirror_url = base_url ^ "/mirror" in

  let result = Http_client.put
    ~sw ~env
    ~url:mirror_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "application/json");
    ]
    ~body:"{invalid json"
    ()
  in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 400 then
      failwith (Printf.sprintf "Expected 400 for invalid JSON, got %d" response.status)

(** Test: Mirror with missing url field should fail with 400 *)
let test_mirror_missing_url ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let keypair = Nostr_signer.generate_keypair () in
  let sha256 = String.make 64 '0' in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let mirror_url = base_url ^ "/mirror" in

  let result = Http_client.put
    ~sw ~env
    ~url:mirror_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "application/json");
    ]
    ~body:{|{"other": "value"}|}
    ()
  in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 400 then
      failwith (Printf.sprintf "Expected 400 for missing url, got %d" response.status)

(** Test: Mirror with invalid URL scheme should fail with 400 *)
let test_mirror_invalid_url_scheme ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let keypair = Nostr_signer.generate_keypair () in
  let sha256 = String.make 64 '0' in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let mirror_url = base_url ^ "/mirror" in

  let result = Http_client.put
    ~sw ~env
    ~url:mirror_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "application/json");
    ]
    ~body:{|{"url": "ftp://example.com/blob"}|}
    ()
  in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 400 then
      failwith (Printf.sprintf "Expected 400 for invalid URL scheme, got %d" response.status)

(** Test: Mirror with sha256 mismatch should fail with 401 *)
let test_mirror_sha256_mismatch ~sw ~env =
  if not (skip_unless_mirror_url ()) then () else
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let source_url = get_mirror_url () in
  (* Use a wrong sha256 that doesn't match the external blob *)
  let wrong_sha256 = String.make 64 'f' in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256:wrong_sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let mirror_url = base_url ^ "/mirror" in
  let mirror_body = Printf.sprintf {|{"url": "%s"}|} source_url in

  let mirror_result = Http_client.put
    ~sw ~env
    ~url:mirror_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "application/json");
    ]
    ~body:mirror_body
    ()
  in

  match mirror_result with
  | Error e -> failwith ("Mirror request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for sha256 mismatch, got %d" response.status)

(** Test: Mirror with expired auth should fail with 401 *)
let test_mirror_expired_auth ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let keypair = Nostr_signer.generate_keypair () in
  let sha256 = String.make 64 '0' in
  (* Expiration in the past *)
  let expiration = Int64.of_float (now -. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let mirror_url = base_url ^ "/mirror" in

  let result = Http_client.put
    ~sw ~env
    ~url:mirror_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "application/json");
    ]
    ~body:{|{"url": "https://example.com/blob"}|}
    ()
  in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for expired auth, got %d" response.status)

(** Test: Mirror response format contains all required fields *)
let test_mirror_response_format ~sw ~env =
  if not (skip_unless_mirror_url ()) then () else
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let source_url = get_mirror_url () in
  let sha256 = sha256_from_url source_url in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let mirror_url = base_url ^ "/mirror" in
  let mirror_body = Printf.sprintf {|{"url": "%s"}|} source_url in

  let mirror_result = Http_client.put
    ~sw ~env
    ~url:mirror_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "application/json");
    ]
    ~body:mirror_body
    ()
  in

  match mirror_result with
  | Error e -> failwith ("Mirror failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Mirror returned status %d: %s" response.status response.body);

    let json = Yojson.Safe.from_string response.body in
    match json with
    | `Assoc fields ->
      (* Check url field *)
      (match List.assoc_opt "url" fields with
       | Some (`String url) ->
         if String.length url = 0 then
           failwith "url field is empty"
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
       | Some (`Int _) -> ()
       | _ -> failwith "Missing or invalid 'size' field");

      (* Check type field *)
      (match List.assoc_opt "type" fields with
       | Some (`String _) -> ()
       | _ -> failwith "Missing or invalid 'type' field");

      (* Check uploaded field *)
      (match List.assoc_opt "uploaded" fields with
       | Some (`Int _) -> ()
       | _ -> failwith "Missing or invalid 'uploaded' field")
    | _ -> failwith "Response is not a JSON object"

(** Helper: make an authenticated mirror request and return the response *)
let mirror_ssrf_request ~sw ~env url_value =
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let keypair = Nostr_signer.generate_keypair () in
  let sha256 = String.make 64 '0' in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in
  let mirror_url = Config.base_url ^ "/mirror" in
  let body = Printf.sprintf {|{"url": "%s"}|} url_value in
  Http_client.put ~sw ~env ~url:mirror_url
    ~headers:[
      ("Authorization", auth_header);
      ("Content-Type", "application/json");
    ]
    ~body ()

(** Assert: response is 400 with body containing "URL not allowed" *)
let assert_ssrf_blocked response label =
  if response.Http_client.status <> 400 then
    failwith (Printf.sprintf "%s: expected 400, got %d (body: %s)"
      label response.status response.body);
  if not (String.length response.body >= 15 &&
          String.sub response.body 0 15 = "URL not allowed") then
    failwith (Printf.sprintf "%s: expected body 'URL not allowed', got '%s'"
      label response.body)

(** Test: Mirror localhost should be blocked with 400 *)
let test_mirror_ssrf_localhost ~sw ~env =
  match mirror_ssrf_request ~sw ~env "http://localhost/" with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response -> assert_ssrf_blocked response "localhost"

(** Test: Mirror loopback IP should be blocked with 400 *)
let test_mirror_ssrf_loopback_ip ~sw ~env =
  match mirror_ssrf_request ~sw ~env "http://127.0.0.1/" with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response -> assert_ssrf_blocked response "loopback IP"

(** Test: Mirror cloud metadata IP should be blocked with 400 *)
let test_mirror_ssrf_cloud_metadata ~sw ~env =
  match mirror_ssrf_request ~sw ~env "http://169.254.169.254/latest/meta-data/" with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response -> assert_ssrf_blocked response "cloud metadata"

(** Test: Mirror bracketed scoped IPv6 should be blocked with 400 *)
let test_mirror_ssrf_bracketed_scoped_ipv6 ~sw ~env =
  match mirror_ssrf_request ~sw ~env "http://[fe80::1%25en0]/" with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response -> assert_ssrf_blocked response "bracketed scoped IPv6"

(** All tests *)
let tests = [
  ("mirror basic", test_mirror_basic);
  ("mirror without auth", test_mirror_without_auth);
  ("mirror invalid JSON", test_mirror_invalid_json);
  ("mirror missing url", test_mirror_missing_url);
  ("mirror invalid URL scheme", test_mirror_invalid_url_scheme);
  ("mirror sha256 mismatch", test_mirror_sha256_mismatch);
  ("mirror expired auth", test_mirror_expired_auth);
  ("mirror response format", test_mirror_response_format);
  ("mirror SSRF: localhost blocked", test_mirror_ssrf_localhost);
  ("mirror SSRF: loopback IP blocked", test_mirror_ssrf_loopback_ip);
  ("mirror SSRF: cloud metadata blocked", test_mirror_ssrf_cloud_metadata);
  ("mirror SSRF: bracketed scoped IPv6 blocked", test_mirror_ssrf_bracketed_scoped_ipv6);
]
