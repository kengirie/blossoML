(** E2E tests for BUD-09 PUT /report and GET / (Terms of Service). *)

let sha256_hex content =
  let hash = Digestif.SHA256.digest_string content in
  Digestif.SHA256.to_hex hash

(** Test: GET / returns 200 and text/plain Terms of Service *)
let test_get_root_terms_of_service ~sw ~env =
  let base_url = Config.base_url in
  let result = Http_client.get ~sw ~env ~url:base_url () in
  match result with
  | Error e -> failwith ("GET / failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Expected 200, got %d" response.status);
    let ct = List.find_opt
      (fun (k, _) -> String.lowercase_ascii k = "content-type") response.headers
    in
    (match ct with
     | None -> failwith "Missing Content-Type"
     | Some (_, v) ->
       if not (String.starts_with ~prefix:"text/plain" v) then
         failwith (Printf.sprintf "Expected text/plain, got %s" v));
    if String.length response.body = 0 then
      failwith "Empty ToS body";
    (* must mention the report endpoint *)
    let contains s sub =
      try
        let len_sub = String.length sub in
        let len_s = String.length s in
        let rec loop i =
          if i + len_sub > len_s then false
          else if String.sub s i len_sub = sub then true
          else loop (i + 1)
        in loop 0
      with _ -> false
    in
    if not (contains response.body "report") then
      failwith "ToS should mention reporting"

(** Test: PUT /report with a valid signed report returns 200 *)
let test_put_report_accepted ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let keypair = Nostr_signer.generate_keypair () in
  let some_sha = sha256_hex "irrelevant content for report-only test" in
  let event = Nostr_signer.create_report
    ~keypair ~created_at:now
    ~entries:[(some_sha, "spam")]
    ~content:"this is spam"
    ()
  in
  let body = Nostr_signer.event_to_json event in
  let result = Http_client.put ~sw ~env ~url:(base_url ^ "/report")
    ~headers:[("Content-Type", "application/json")] ~body ()
  in
  match result with
  | Error e -> failwith ("PUT /report failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Expected 200, got %d (body: %s)"
                  response.status response.body)

(** Test: PUT /report with multiple x tags is accepted *)
let test_put_report_multiple_x_tags ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let keypair = Nostr_signer.generate_keypair () in
  let sha1 = sha256_hex "blob 1" in
  let sha2 = sha256_hex "blob 2" in
  let event = Nostr_signer.create_report
    ~keypair ~created_at:now
    ~entries:[(sha1, "malware"); (sha2, "illegal")]
    ~content:""
    ()
  in
  let body = Nostr_signer.event_to_json event in
  let result = Http_client.put ~sw ~env ~url:(base_url ^ "/report")
    ~headers:[("Content-Type", "application/json")] ~body ()
  in
  match result with
  | Error e -> failwith ("PUT /report failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Expected 200, got %d (body: %s)"
                  response.status response.body)

(** Test: PUT /report rejects an invalid (non-NIP-56) body *)
let test_put_report_rejects_invalid_body ~sw ~env =
  let base_url = Config.base_url in
  let result = Http_client.put ~sw ~env ~url:(base_url ^ "/report")
    ~headers:[("Content-Type", "application/json")]
    ~body:"{\"not\":\"a report\"}" ()
  in
  match result with
  | Error e -> failwith ("Request failed: " ^ e)
  | Ok response ->
    if response.status <> 400 then
      failwith (Printf.sprintf "Expected 400 for invalid body, got %d" response.status)

(** Test: PUT /report rejects an unknown report type *)
let test_put_report_rejects_unknown_type ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
  let keypair = Nostr_signer.generate_keypair () in
  let some_sha = sha256_hex "x" in
  let event = Nostr_signer.create_report
    ~keypair ~created_at:now
    ~entries:[(some_sha, "vandalism")]
    ~content:""
    ()
  in
  let body = Nostr_signer.event_to_json event in
  let result = Http_client.put ~sw ~env ~url:(base_url ^ "/report")
    ~headers:[("Content-Type", "application/json")] ~body ()
  in
  match result with
  | Error e -> failwith ("Request failed: " ^ e)
  | Ok response ->
    if response.status <> 400 then
      failwith (Printf.sprintf "Expected 400 for invalid report type, got %d" response.status)

let tests = [
  ("GET / returns ToS", test_get_root_terms_of_service);
  ("PUT /report accepted", test_put_report_accepted);
  ("PUT /report multiple x tags", test_put_report_multiple_x_tags);
  ("PUT /report rejects invalid body", test_put_report_rejects_invalid_body);
  ("PUT /report rejects unknown type", test_put_report_rejects_unknown_type);
]
