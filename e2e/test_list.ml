(** E2E tests for BUD-12 GET /list/<pubkey>. *)

let sha256_hex content =
  let hash = Digestif.SHA256.digest_string content in
  Digestif.SHA256.to_hex hash

(** Upload a single blob with the given keypair. Returns the response sha256. *)
let upload_blob ~sw ~env ~keypair ~content ~now =
  let base_url = Config.base_url in
  let sha256 = sha256_hex content in
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
      failwith (Printf.sprintf "Upload returned %d: %s" response.status response.body);
    let json = Yojson.Safe.from_string response.body in
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "sha256" fields with
       | Some (`String s) -> s
       | _ -> failwith "No sha256 in upload response")
    | _ -> failwith "Invalid upload response"

(** Parse list response body into list of (sha256, size, type, uploaded). *)
let parse_list_response body =
  let json = Yojson.Safe.from_string body in
  match json with
  | `List items ->
    List.map (fun item ->
      match item with
      | `Assoc fields ->
        let sha = match List.assoc_opt "sha256" fields with
          | Some (`String s) -> s | _ -> failwith "missing sha256"
        in
        let size = match List.assoc_opt "size" fields with
          | Some (`Int n) -> n | _ -> failwith "missing size"
        in
        let typ = match List.assoc_opt "type" fields with
          | Some (`String s) -> s | _ -> failwith "missing type"
        in
        let uploaded = match List.assoc_opt "uploaded" fields with
          | Some (`Int n) -> n | _ -> failwith "missing uploaded"
        in
        let url = match List.assoc_opt "url" fields with
          | Some (`String s) -> s | _ -> failwith "missing url"
        in
        (sha, size, typ, uploaded, url)
      | _ -> failwith "list item not an object"
    ) items
  | _ -> failwith "list response is not an array"

(** Test: upload 3 blobs for the same pubkey and list them. *)
let test_list_returns_owned_blobs ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let keypair = Nostr_signer.generate_keypair () in
  let _ = upload_blob ~sw ~env ~keypair ~content:("list-test-1-" ^ string_of_float now) ~now in
  let _ = upload_blob ~sw ~env ~keypair ~content:("list-test-2-" ^ string_of_float now) ~now in
  let _ = upload_blob ~sw ~env ~keypair ~content:("list-test-3-" ^ string_of_float now) ~now in

  let list_url = base_url ^ "/list/" ^ keypair.pubkey in
  let result = Http_client.get ~sw ~env ~url:list_url () in
  match result with
  | Error e -> failwith ("List failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "List returned %d: %s" response.status response.body);
    let items = parse_list_response response.body in
    if List.length items < 3 then
      failwith (Printf.sprintf "Expected >= 3 items, got %d" (List.length items));
    (* Verify URL is fully-qualified and includes base_url *)
    let (_, _, _, _, first_url) = List.hd items in
    if String.length first_url < String.length base_url
       || String.sub first_url 0 (String.length base_url) <> base_url then
      failwith (Printf.sprintf "URL should start with base_url, got: %s" first_url);
    (* Verify uploaded is descending *)
    let uploaded_values = List.map (fun (_, _, _, u, _) -> u) items in
    let rec is_desc = function
      | [] | [_] -> true
      | a :: (b :: _ as rest) -> a >= b && is_desc rest
    in
    if not (is_desc uploaded_values) then
      failwith "List is not sorted by uploaded DESC"

(** Test: list for an unknown pubkey returns empty array. *)
let test_list_unknown_pubkey ~sw ~env =
  let base_url = Config.base_url in
  let unknown_pubkey = String.make 64 'a' in
  let list_url = base_url ^ "/list/" ^ unknown_pubkey in
  let result = Http_client.get ~sw ~env ~url:list_url () in
  match result with
  | Error e -> failwith ("List failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Expected 200, got %d" response.status);
    let items = parse_list_response response.body in
    if List.length items <> 0 then
      failwith (Printf.sprintf "Expected empty list, got %d items" (List.length items))

(** Test: list with malformed pubkey returns 400. *)
let test_list_invalid_pubkey ~sw ~env =
  let base_url = Config.base_url in
  let list_url = base_url ^ "/list/not-a-hex-pubkey" in
  let result = Http_client.get ~sw ~env ~url:list_url () in
  match result with
  | Error e -> failwith ("Request failed: " ^ e)
  | Ok response ->
    if response.status <> 400 then
      failwith (Printf.sprintf "Expected 400, got %d" response.status)

(** Test: list with limit and cursor pagination returns expected slices. *)
let test_list_cursor_pagination ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let keypair = Nostr_signer.generate_keypair () in
  let _ = upload_blob ~sw ~env ~keypair ~content:("list-cursor-1-" ^ string_of_float now) ~now in
  let _ = upload_blob ~sw ~env ~keypair ~content:("list-cursor-2-" ^ string_of_float now) ~now in
  let _ = upload_blob ~sw ~env ~keypair ~content:("list-cursor-3-" ^ string_of_float now) ~now in

  (* First page: limit=2 *)
  let url1 = Printf.sprintf "%s/list/%s?limit=2" base_url keypair.pubkey in
  let r1 = Http_client.get ~sw ~env ~url:url1 () in
  let items1 = match r1 with
    | Error e -> failwith ("First page failed: " ^ e)
    | Ok response ->
      if response.status <> 200 then
        failwith (Printf.sprintf "First page returned %d" response.status);
      parse_list_response response.body
  in
  if List.length items1 <> 2 then
    failwith (Printf.sprintf "First page expected 2 items, got %d" (List.length items1));

  (* Second page: cursor = last sha256 of first page *)
  let (last_sha, _, _, _, _) = List.nth items1 1 in
  let url2 = Printf.sprintf "%s/list/%s?limit=2&cursor=%s" base_url keypair.pubkey last_sha in
  let r2 = Http_client.get ~sw ~env ~url:url2 () in
  let items2 = match r2 with
    | Error e -> failwith ("Second page failed: " ^ e)
    | Ok response ->
      if response.status <> 200 then
        failwith (Printf.sprintf "Second page returned %d: %s" response.status response.body);
      parse_list_response response.body
  in
  if List.length items2 < 1 then
    failwith (Printf.sprintf "Second page expected >= 1 item, got %d" (List.length items2));
  (* Cursor blob itself MUST NOT appear in next page *)
  List.iter (fun (sha, _, _, _, _) ->
    if sha = last_sha then
      failwith "Cursor blob leaked into next page"
  ) items2

(** Test: list with invalid cursor (not hex 64) returns 400. *)
let test_list_invalid_cursor ~sw ~env =
  let base_url = Config.base_url in
  let pubkey = String.make 64 'b' in
  let url = Printf.sprintf "%s/list/%s?cursor=not-a-cursor" base_url pubkey in
  let result = Http_client.get ~sw ~env ~url () in
  match result with
  | Error e -> failwith ("Request failed: " ^ e)
  | Ok response ->
    if response.status <> 400 then
      failwith (Printf.sprintf "Expected 400 for invalid cursor, got %d" response.status)

(** Test: list with since/until range filters by uploaded timestamp. *)
let test_list_since_until ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let keypair = Nostr_signer.generate_keypair () in
  let _ = upload_blob ~sw ~env ~keypair ~content:("list-since-" ^ string_of_float now) ~now in

  (* until=0 should yield empty (no blobs uploaded before unix=0) *)
  let url = Printf.sprintf "%s/list/%s?until=0" base_url keypair.pubkey in
  let result = Http_client.get ~sw ~env ~url () in
  match result with
  | Error e -> failwith ("Request failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Expected 200, got %d" response.status);
    let items = parse_list_response response.body in
    if List.length items <> 0 then
      failwith (Printf.sprintf "Expected 0 with until=0, got %d" (List.length items))

(** Test: list with malformed since query param returns 400. *)
let test_list_invalid_since ~sw ~env =
  let base_url = Config.base_url in
  let pubkey = String.make 64 'c' in
  let url = Printf.sprintf "%s/list/%s?since=abc" base_url pubkey in
  let result = Http_client.get ~sw ~env ~url () in
  match result with
  | Error e -> failwith ("Request failed: " ^ e)
  | Ok response ->
    if response.status <> 400 then
      failwith (Printf.sprintf "Expected 400, got %d" response.status)

(** Test: list with valid Authorization header (t=list) returns 200. *)
let test_list_with_valid_auth ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let auth_event = Nostr_signer.create_list_auth ~keypair ~created_at:now ~expiration in
  let auth_header = Nostr_signer.to_auth_header auth_event in

  let url = base_url ^ "/list/" ^ keypair.pubkey in
  let result = Http_client.get ~sw ~env ~url ~headers:[("Authorization", auth_header)] () in
  match result with
  | Error e -> failwith ("Request failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Expected 200 with valid auth, got %d" response.status)

(** Test: list with invalid Authorization header returns 401. *)
let test_list_with_invalid_auth ~sw ~env =
  let base_url = Config.base_url in
  let pubkey = String.make 64 'd' in
  let url = base_url ^ "/list/" ^ pubkey in
  (* Garbage base64 → 401 *)
  let result = Http_client.get ~sw ~env ~url
    ~headers:[("Authorization", "Nostr !!!invalid!!!")] ()
  in
  match result with
  | Error e -> failwith ("Request failed: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for invalid auth, got %d" response.status)

(** Test: list with auth event whose t tag is not "list" returns 401. *)
let test_list_with_wrong_t_tag ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  (* Send an upload-style auth event when listing → should be rejected *)
  let auth_event = Nostr_signer.create_upload_auth
    ~keypair ~sha256:(String.make 64 '0') ~created_at:now ~expiration
  in
  let auth_header = Nostr_signer.to_auth_header auth_event in

  let url = base_url ^ "/list/" ^ keypair.pubkey in
  let result = Http_client.get ~sw ~env ~url ~headers:[("Authorization", auth_header)] () in
  match result with
  | Error e -> failwith ("Request failed: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for wrong t tag, got %d" response.status)

let tests = [
  ("list returns owned blobs sorted DESC", test_list_returns_owned_blobs);
  ("list unknown pubkey returns empty", test_list_unknown_pubkey);
  ("list with invalid pubkey returns 400", test_list_invalid_pubkey);
  ("list cursor pagination", test_list_cursor_pagination);
  ("list with invalid cursor returns 400", test_list_invalid_cursor);
  ("list with since/until range filter", test_list_since_until);
  ("list with invalid since returns 400", test_list_invalid_since);
  ("list with valid t=list auth header", test_list_with_valid_auth);
  ("list with invalid auth header returns 401", test_list_with_invalid_auth);
  ("list with wrong t tag returns 401", test_list_with_wrong_t_tag);
]
