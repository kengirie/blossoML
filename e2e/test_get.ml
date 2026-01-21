(** E2E tests for GET and HEAD functionality. *)

(** Compute SHA-256 hash of content as hex string *)
let sha256_hex content =
  let hash = Digestif.SHA256.digest_string content in
  Digestif.SHA256.to_hex hash

(** Helper to upload a file and return its sha256 *)
let upload_file ~sw ~env ?(content_type="text/plain") content =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in
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
      ("Content-Type", content_type);
    ]
    ~body:content
    ()
  in

  match result with
  | Error e -> failwith ("Upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Upload returned status %d: %s" response.status response.body);
    let json = Yojson.Safe.from_string response.body in
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "sha256" fields with
       | Some (`String s) -> s
       | _ -> failwith "No sha256 in response")
    | _ -> failwith "Invalid upload response format"

(* ===========================================
   GET Tests
   =========================================== *)

(** Test: Basic GET request for existing blob *)
let test_get_existing_blob ~sw ~env =
  let base_url = Config.base_url in
  let content = "Test content for basic GET request" in
  let sha256 = upload_file ~sw ~env content in

  let url = base_url ^ "/" ^ sha256 in
  let result = Http_client.get ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("GET failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "GET returned status %d" response.status);
    if response.body <> content then
      failwith (Printf.sprintf "Content mismatch: expected '%s', got '%s'" content response.body)

(** Test: GET non-existent blob returns 404 *)
let test_get_not_found ~sw ~env =
  let base_url = Config.base_url in
  let fake_hash = "0000000000000000000000000000000000000000000000000000000000000000" in
  let url = base_url ^ "/" ^ fake_hash in

  let result = Http_client.get ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 404 then
      failwith (Printf.sprintf "Expected 404, got %d" response.status)

(** Test: GET with invalid hash format returns 404 *)
let test_get_invalid_hash_format ~sw ~env =
  let base_url = Config.base_url in

  (* Too short *)
  let url1 = base_url ^ "/abc123" in
  let result1 = Http_client.get ~sw ~env ~url:url1 () in
  (match result1 with
   | Error e -> failwith ("Request failed unexpectedly: " ^ e)
   | Ok response ->
     if response.status <> 404 then
       failwith (Printf.sprintf "Expected 404 for short hash, got %d" response.status));

  (* Non-hex characters *)
  let url2 = base_url ^ "/zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" in
  let result2 = Http_client.get ~sw ~env ~url:url2 () in
  (match result2 with
   | Error e -> failwith ("Request failed unexpectedly: " ^ e)
   | Ok response ->
     if response.status <> 404 then
       failwith (Printf.sprintf "Expected 404 for non-hex hash, got %d" response.status));

  (* Too long *)
  let url3 = base_url ^ "/" ^ (String.make 128 'a') in
  let result3 = Http_client.get ~sw ~env ~url:url3 () in
  match result3 with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 404 then
      failwith (Printf.sprintf "Expected 404 for long hash, got %d" response.status)

(** Test: GET with file extension works *)
let test_get_with_extension ~sw ~env =
  let base_url = Config.base_url in
  let content = "Test content for extension test" in
  let sha256 = upload_file ~sw ~env content in

  (* Try various extensions *)
  let extensions = [".txt"; ".png"; ".jpg"; ".pdf"; ".bin"; ""] in
  List.iter (fun ext ->
    let url = base_url ^ "/" ^ sha256 ^ ext in
    let result = Http_client.get ~sw ~env ~url () in
    match result with
    | Error e -> failwith (Printf.sprintf "GET with extension '%s' failed: %s" ext e)
    | Ok response ->
      if response.status <> 200 then
        failwith (Printf.sprintf "GET with extension '%s' returned status %d" ext response.status);
      if response.body <> content then
        failwith (Printf.sprintf "Content mismatch with extension '%s'" ext)
  ) extensions

(** Test: GET returns correct Content-Type header matching uploaded MIME type *)
let test_get_content_type_header ~sw ~env =
  let base_url = Config.base_url in
  let expected_content_type = "text/plain" in

  (* Upload with specific content type *)
  let content = "Test content for Content-Type test" in
  let sha256 = upload_file ~sw ~env ~content_type:expected_content_type content in

  let url = base_url ^ "/" ^ sha256 in
  let result = Http_client.get ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("GET failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "GET returned status %d" response.status);
    let content_type =
      List.find_opt (fun (k, _) -> String.lowercase_ascii k = "content-type") response.headers
    in
    match content_type with
    | None -> failwith "No Content-Type header in response"
    | Some (_, ct) ->
      if ct <> expected_content_type then
        failwith (Printf.sprintf "Content-Type mismatch: expected '%s', got '%s'" expected_content_type ct)

(** Test: GET returns correct Content-Length header *)
let test_get_content_length_header ~sw ~env =
  let base_url = Config.base_url in
  let content = "Test content for Content-Length test" in
  let expected_len = String.length content in
  let sha256 = upload_file ~sw ~env content in

  let url = base_url ^ "/" ^ sha256 in
  let result = Http_client.get ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("GET failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "GET returned status %d" response.status);
    let content_length =
      List.find_opt (fun (k, _) -> String.lowercase_ascii k = "content-length") response.headers
    in
    match content_length with
    | None -> failwith "No Content-Length header in response"
    | Some (_, len_str) ->
      let actual_len = int_of_string len_str in
      if actual_len <> expected_len then
        failwith (Printf.sprintf "Content-Length mismatch: expected %d, got %d" expected_len actual_len)

(** Test: GET empty blob *)
let test_get_empty_blob ~sw ~env =
  let base_url = Config.base_url in
  let content = "" in
  let sha256 = upload_file ~sw ~env ~content_type:"application/octet-stream" content in

  let url = base_url ^ "/" ^ sha256 in
  let result = Http_client.get ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("GET failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "GET returned status %d" response.status);
    if response.body <> "" then
      failwith (Printf.sprintf "Expected empty body, got '%s'" response.body);

    (* Check Content-Length is 0 *)
    let content_length =
      List.find_opt (fun (k, _) -> String.lowercase_ascii k = "content-length") response.headers
    in
    match content_length with
    | None -> failwith "No Content-Length header"
    | Some (_, len_str) ->
      if int_of_string len_str <> 0 then
        failwith (Printf.sprintf "Expected Content-Length 0, got %s" len_str)

(** Test: GET large blob *)
let test_get_large_blob ~sw ~env =
  let base_url = Config.base_url in
  (* 1MB file *)
  let content = String.make (1024 * 1024) 'X' in
  let sha256 = upload_file ~sw ~env ~content_type:"application/octet-stream" content in

  let url = base_url ^ "/" ^ sha256 in
  let result = Http_client.get ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("GET failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "GET returned status %d" response.status);
    if response.body <> content then
      failwith "Content mismatch for large blob";

    let content_length =
      List.find_opt (fun (k, _) -> String.lowercase_ascii k = "content-length") response.headers
    in
    match content_length with
    | None -> failwith "No Content-Length header"
    | Some (_, len_str) ->
      if int_of_string len_str <> 1024 * 1024 then
        failwith (Printf.sprintf "Expected Content-Length %d, got %s" (1024 * 1024) len_str)

(** Test: GET binary blob preserves all bytes *)
let test_get_binary_blob ~sw ~env =
  let base_url = Config.base_url in
  (* All byte values 0-255 *)
  let content = String.init 256 (fun i -> Char.chr i) in
  let sha256 = upload_file ~sw ~env ~content_type:"application/octet-stream" content in

  let url = base_url ^ "/" ^ sha256 in
  let result = Http_client.get ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("GET failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "GET returned status %d" response.status);
    if response.body <> content then
      failwith "Binary content mismatch"

(** Test: GET with invalid path patterns *)
let test_get_invalid_paths ~sw ~env =
  let base_url = Config.base_url in

  let invalid_paths = [
    "/";                     (* Root path *)
    "/upload";               (* Upload endpoint *)
    "/../etc/passwd";        (* Path traversal attempt *)
    "/a/b/c";                (* Multiple segments *)
    "";                      (* Empty path *)
  ] in

  List.iter (fun path ->
    let url = base_url ^ path in
    let result = Http_client.get ~sw ~env ~url () in
    match result with
    | Error _ -> () (* Connection error is acceptable for some paths *)
    | Ok response ->
      if response.status <> 404 && response.status <> 405 then
        failwith (Printf.sprintf "Expected 404 or 405 for path '%s', got %d" path response.status)
  ) invalid_paths

(** Test: GET includes CORS headers *)
let test_get_cors_headers ~sw ~env =
  let base_url = Config.base_url in
  let content = "Test content for CORS test" in
  let sha256 = upload_file ~sw ~env content in

  let url = base_url ^ "/" ^ sha256 in
  let result = Http_client.get ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("GET failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "GET returned status %d" response.status);

    (* Check CORS headers *)
    let acao = List.find_opt
      (fun (k, _) -> String.lowercase_ascii k = "access-control-allow-origin")
      response.headers
    in
    match acao with
    | None -> failwith "Missing Access-Control-Allow-Origin header"
    | Some (_, v) ->
      if v <> "*" then
        failwith (Printf.sprintf "Expected ACAO '*', got '%s'" v)

(** Test: GET same blob multiple times returns same content *)
let test_get_multiple_times ~sw ~env =
  let base_url = Config.base_url in
  let content = "Test content for multiple GET test" in
  let sha256 = upload_file ~sw ~env content in
  let url = base_url ^ "/" ^ sha256 in

  for i = 1 to 5 do
    let result = Http_client.get ~sw ~env ~url () in
    match result with
    | Error e -> failwith (Printf.sprintf "GET #%d failed: %s" i e)
    | Ok response ->
      if response.status <> 200 then
        failwith (Printf.sprintf "GET #%d returned status %d" i response.status);
      if response.body <> content then
        failwith (Printf.sprintf "GET #%d content mismatch" i)
  done

(** Test: GET hash is case-sensitive (uppercase hash returns 404) *)
let test_get_hash_case_sensitive ~sw ~env =
  let base_url = Config.base_url in
  let content = "Test content for case sensitivity test" in
  let sha256 = upload_file ~sw ~env content in

  (* Uppercase hash should return 404 since hashes are case-sensitive *)
  let url_upper = base_url ^ "/" ^ (String.uppercase_ascii sha256) in
  let result = Http_client.get ~sw ~env ~url:url_upper () in
  match result with
  | Error e -> failwith ("GET with uppercase hash failed: " ^ e)
  | Ok response ->
    if response.status <> 404 then
      failwith (Printf.sprintf "Expected 404 for uppercase hash (case-sensitive), got %d" response.status)

(** Test: GET specific content types are preserved in response header *)
let test_get_various_content_types ~sw ~env =
  let base_url = Config.base_url in

  let test_cases = [
    ("image/png", "\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR" ^ String.make 100 '\000');
    ("image/jpeg", "\xff\xd8\xff\xe0\x00\x10JFIF" ^ String.make 100 '\000');
    ("application/pdf", "%PDF-1.4" ^ String.make 100 '\000');
    ("text/html", "<html><body>Test</body></html>");
    ("application/json", "{\"test\": true}");
  ] in

  List.iter (fun (expected_content_type, content) ->
    let sha256 = upload_file ~sw ~env ~content_type:expected_content_type content in
    let url = base_url ^ "/" ^ sha256 in
    let result = Http_client.get ~sw ~env ~url () in

    match result with
    | Error e -> failwith (Printf.sprintf "GET for %s failed: %s" expected_content_type e)
    | Ok response ->
      if response.status <> 200 then
        failwith (Printf.sprintf "GET for %s returned status %d" expected_content_type response.status);
      if response.body <> content then
        failwith (Printf.sprintf "Content mismatch for %s" expected_content_type);

      (* Verify Content-Type header matches what was uploaded *)
      let ct_header = List.find_opt
        (fun (k, _) -> String.lowercase_ascii k = "content-type")
        response.headers
      in
      match ct_header with
      | None -> failwith (Printf.sprintf "No Content-Type header for %s" expected_content_type)
      | Some (_, actual_ct) ->
        if actual_ct <> expected_content_type then
          failwith (Printf.sprintf "Content-Type mismatch for %s: expected '%s', got '%s'"
            expected_content_type expected_content_type actual_ct)
  ) test_cases

(* ===========================================
   HEAD Tests
   =========================================== *)

(** Test: Basic HEAD request for existing blob *)
let test_head_existing_blob ~sw ~env =
  let base_url = Config.base_url in
  let content = "Test content for basic HEAD request" in
  let sha256 = upload_file ~sw ~env content in

  let url = base_url ^ "/" ^ sha256 in
  let result = Http_client.head ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("HEAD failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "HEAD returned status %d" response.status);
    (* HEAD should not have body *)
    if String.length response.body > 0 then
      failwith (Printf.sprintf "HEAD should not return body, got '%s'" response.body)

(** Test: HEAD non-existent blob returns 404 *)
let test_head_not_found ~sw ~env =
  let base_url = Config.base_url in
  let fake_hash = "0000000000000000000000000000000000000000000000000000000000000000" in
  let url = base_url ^ "/" ^ fake_hash in

  let result = Http_client.head ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 404 then
      failwith (Printf.sprintf "Expected 404, got %d" response.status)

(** Test: HEAD with invalid hash format returns 404 *)
let test_head_invalid_hash_format ~sw ~env =
  let base_url = Config.base_url in

  let invalid_hashes = [
    "abc123";  (* Too short *)
    "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz";  (* Non-hex *)
    String.make 128 'a';  (* Too long *)
    "";  (* Empty *)
  ] in

  List.iter (fun hash ->
    let url = base_url ^ "/" ^ hash in
    let result = Http_client.head ~sw ~env ~url () in
    match result with
    | Error _ -> ()  (* Connection error is acceptable for empty path *)
    | Ok response ->
      if response.status <> 404 then
        failwith (Printf.sprintf "Expected 404 for hash '%s', got %d" hash response.status)
  ) invalid_hashes

(** Test: HEAD with file extension works *)
let test_head_with_extension ~sw ~env =
  let base_url = Config.base_url in
  let content = "Test content for HEAD extension test" in
  let sha256 = upload_file ~sw ~env content in

  let extensions = [".txt"; ".png"; ".jpg"; ".pdf"; ".bin"; ""] in
  List.iter (fun ext ->
    let url = base_url ^ "/" ^ sha256 ^ ext in
    let result = Http_client.head ~sw ~env ~url () in
    match result with
    | Error e -> failwith (Printf.sprintf "HEAD with extension '%s' failed: %s" ext e)
    | Ok response ->
      if response.status <> 200 then
        failwith (Printf.sprintf "HEAD with extension '%s' returned status %d" ext response.status)
  ) extensions

(** Test: HEAD returns correct Content-Length header *)
let test_head_content_length_header ~sw ~env =
  let base_url = Config.base_url in
  let content = "Test content for HEAD Content-Length test" in
  let expected_len = String.length content in
  let sha256 = upload_file ~sw ~env content in

  let url = base_url ^ "/" ^ sha256 in
  let result = Http_client.head ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("HEAD failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "HEAD returned status %d" response.status);
    let content_length =
      List.find_opt (fun (k, _) -> String.lowercase_ascii k = "content-length") response.headers
    in
    match content_length with
    | None -> failwith "No Content-Length header in HEAD response"
    | Some (_, len_str) ->
      let actual_len = int_of_string len_str in
      if actual_len <> expected_len then
        failwith (Printf.sprintf "Content-Length mismatch: expected %d, got %d" expected_len actual_len)

(** Test: HEAD returns correct Content-Type header matching uploaded MIME type *)
let test_head_content_type_header ~sw ~env =
  let base_url = Config.base_url in
  let expected_content_type = "text/plain" in
  let content = "Test content for HEAD Content-Type test" in
  let sha256 = upload_file ~sw ~env ~content_type:expected_content_type content in

  let url = base_url ^ "/" ^ sha256 in
  let result = Http_client.head ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("HEAD failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "HEAD returned status %d" response.status);
    let content_type =
      List.find_opt (fun (k, _) -> String.lowercase_ascii k = "content-type") response.headers
    in
    match content_type with
    | None -> failwith "No Content-Type header in HEAD response"
    | Some (_, ct) ->
      if ct <> expected_content_type then
        failwith (Printf.sprintf "Content-Type mismatch: expected '%s', got '%s'" expected_content_type ct)

(** Test: HEAD for empty blob *)
let test_head_empty_blob ~sw ~env =
  let base_url = Config.base_url in
  let content = "" in
  let sha256 = upload_file ~sw ~env ~content_type:"application/octet-stream" content in

  let url = base_url ^ "/" ^ sha256 in
  let result = Http_client.head ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("HEAD failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "HEAD returned status %d" response.status);
    let content_length =
      List.find_opt (fun (k, _) -> String.lowercase_ascii k = "content-length") response.headers
    in
    match content_length with
    | None -> failwith "No Content-Length header"
    | Some (_, len_str) ->
      if int_of_string len_str <> 0 then
        failwith (Printf.sprintf "Expected Content-Length 0, got %s" len_str)

(** Test: HEAD for large blob returns correct size *)
let test_head_large_blob ~sw ~env =
  let base_url = Config.base_url in
  let content = String.make (1024 * 1024) 'Y' in
  let sha256 = upload_file ~sw ~env ~content_type:"application/octet-stream" content in

  let url = base_url ^ "/" ^ sha256 in
  let result = Http_client.head ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("HEAD failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "HEAD returned status %d" response.status);
    let content_length =
      List.find_opt (fun (k, _) -> String.lowercase_ascii k = "content-length") response.headers
    in
    match content_length with
    | None -> failwith "No Content-Length header"
    | Some (_, len_str) ->
      if int_of_string len_str <> 1024 * 1024 then
        failwith (Printf.sprintf "Expected Content-Length %d, got %s" (1024 * 1024) len_str)

(** Test: HEAD includes CORS headers *)
let test_head_cors_headers ~sw ~env =
  let base_url = Config.base_url in
  let content = "Test content for HEAD CORS test" in
  let sha256 = upload_file ~sw ~env content in

  let url = base_url ^ "/" ^ sha256 in
  let result = Http_client.head ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("HEAD failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "HEAD returned status %d" response.status);
    let acao = List.find_opt
      (fun (k, _) -> String.lowercase_ascii k = "access-control-allow-origin")
      response.headers
    in
    match acao with
    | None -> failwith "Missing Access-Control-Allow-Origin header in HEAD response"
    | Some (_, v) ->
      if v <> "*" then
        failwith (Printf.sprintf "Expected ACAO '*', got '%s'" v)

(** Test: HEAD multiple times returns consistent headers *)
let test_head_multiple_times ~sw ~env =
  let base_url = Config.base_url in
  let content = "Test content for multiple HEAD test" in
  let expected_len = String.length content in
  let sha256 = upload_file ~sw ~env content in
  let url = base_url ^ "/" ^ sha256 in

  for i = 1 to 5 do
    let result = Http_client.head ~sw ~env ~url () in
    match result with
    | Error e -> failwith (Printf.sprintf "HEAD #%d failed: %s" i e)
    | Ok response ->
      if response.status <> 200 then
        failwith (Printf.sprintf "HEAD #%d returned status %d" i response.status);
      let content_length =
        List.find_opt (fun (k, _) -> String.lowercase_ascii k = "content-length") response.headers
      in
      match content_length with
      | None -> failwith (Printf.sprintf "HEAD #%d missing Content-Length" i)
      | Some (_, len_str) ->
        if int_of_string len_str <> expected_len then
          failwith (Printf.sprintf "HEAD #%d Content-Length mismatch" i)
  done

(** Test: HEAD and GET return consistent headers *)
let test_head_get_consistent_headers ~sw ~env =
  let base_url = Config.base_url in
  let content = "Test content for HEAD/GET consistency" in
  let sha256 = upload_file ~sw ~env content in
  let url = base_url ^ "/" ^ sha256 in

  (* Get headers from HEAD *)
  let head_result = Http_client.head ~sw ~env ~url () in
  let head_headers = match head_result with
    | Error e -> failwith ("HEAD failed: " ^ e)
    | Ok response ->
      if response.status <> 200 then
        failwith (Printf.sprintf "HEAD returned status %d" response.status);
      response.headers
  in

  (* Get headers from GET *)
  let get_result = Http_client.get ~sw ~env ~url () in
  let get_headers = match get_result with
    | Error e -> failwith ("GET failed: " ^ e)
    | Ok response ->
      if response.status <> 200 then
        failwith (Printf.sprintf "GET returned status %d" response.status);
      response.headers
  in

  (* Compare Content-Length *)
  let head_cl = List.find_opt (fun (k, _) -> String.lowercase_ascii k = "content-length") head_headers in
  let get_cl = List.find_opt (fun (k, _) -> String.lowercase_ascii k = "content-length") get_headers in
  (match head_cl, get_cl with
   | Some (_, h), Some (_, g) ->
     if h <> g then
       failwith (Printf.sprintf "Content-Length mismatch: HEAD=%s, GET=%s" h g)
   | None, _ -> failwith "HEAD missing Content-Length"
   | _, None -> failwith "GET missing Content-Length");

  (* Compare Content-Type *)
  let head_ct = List.find_opt (fun (k, _) -> String.lowercase_ascii k = "content-type") head_headers in
  let get_ct = List.find_opt (fun (k, _) -> String.lowercase_ascii k = "content-type") get_headers in
  match head_ct, get_ct with
  | Some (_, h), Some (_, g) ->
    if h <> g then
      failwith (Printf.sprintf "Content-Type mismatch: HEAD=%s, GET=%s" h g)
  | None, _ -> failwith "HEAD missing Content-Type"
  | _, None -> failwith "GET missing Content-Type"

(** Test: HEAD with invalid path patterns *)
let test_head_invalid_paths ~sw ~env =
  let base_url = Config.base_url in

  (* Note: /upload is now a valid endpoint (BUD-06), so it's not included here *)
  let invalid_paths = [
    "/";
    "/../etc/passwd";
    "/a/b/c";
  ] in

  List.iter (fun path ->
    let url = base_url ^ path in
    let result = Http_client.head ~sw ~env ~url () in
    match result with
    | Error _ -> ()
    | Ok response ->
      if response.status <> 404 && response.status <> 405 then
        failwith (Printf.sprintf "Expected 404 or 405 for path '%s', got %d" path response.status)
  ) invalid_paths

(** All tests *)
let tests = [
  (* GET tests *)
  ("GET existing blob", test_get_existing_blob);
  ("GET not found", test_get_not_found);
  ("GET invalid hash format", test_get_invalid_hash_format);
  ("GET with extension", test_get_with_extension);
  ("GET Content-Type header", test_get_content_type_header);
  ("GET Content-Length header", test_get_content_length_header);
  ("GET empty blob", test_get_empty_blob);
  ("GET large blob", test_get_large_blob);
  ("GET binary blob", test_get_binary_blob);
  ("GET invalid paths", test_get_invalid_paths);
  ("GET CORS headers", test_get_cors_headers);
  ("GET multiple times", test_get_multiple_times);
  ("GET hash case sensitive", test_get_hash_case_sensitive);
  ("GET various content types", test_get_various_content_types);
  (* HEAD tests *)
  ("HEAD existing blob", test_head_existing_blob);
  ("HEAD not found", test_head_not_found);
  ("HEAD invalid hash format", test_head_invalid_hash_format);
  ("HEAD with extension", test_head_with_extension);
  ("HEAD Content-Length header", test_head_content_length_header);
  ("HEAD Content-Type header", test_head_content_type_header);
  ("HEAD empty blob", test_head_empty_blob);
  ("HEAD large blob", test_head_large_blob);
  ("HEAD CORS headers", test_head_cors_headers);
  ("HEAD multiple times", test_head_multiple_times);
  ("HEAD/GET consistent headers", test_head_get_consistent_headers);
  ("HEAD invalid paths", test_head_invalid_paths);
]
