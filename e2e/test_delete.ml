(** E2E tests for delete functionality. *)

(** Compute SHA-256 hash of content as hex string *)
let sha256_hex content =
  let hash = Digestif.SHA256.digest_string content in
  Digestif.SHA256.to_hex hash

(** Test: Delete uploaded file *)
let test_delete ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  (* First upload a file *)
  let content = "File to be deleted" in
  let sha256 = sha256_hex content in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let upload_auth = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
  let upload_auth_header = Nostr_signer.to_auth_header upload_auth in
  let upload_url = base_url ^ "/upload" in

  let upload_result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", upload_auth_header);
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
    let sha256 = match json with
      | `Assoc fields ->
        (match List.assoc_opt "sha256" fields with
         | Some (`String s) -> s
         | _ -> failwith "No sha256 in response")
      | _ -> failwith "Invalid upload response format"
    in

    (* Create delete auth *)
    let delete_auth = Nostr_signer.create_delete_auth ~keypair ~sha256 ~created_at:now ~expiration in
    let delete_auth_header = Nostr_signer.to_auth_header delete_auth in

    (* Delete the file *)
    let delete_url = base_url ^ "/" ^ sha256 in
    let delete_result = Http_client.delete
      ~sw ~env
      ~url:delete_url
      ~headers:[("Authorization", delete_auth_header)]
      ()
    in

    match delete_result with
    | Error e -> failwith ("Delete failed: " ^ e)
    | Ok del_response ->
      if del_response.status <> 200 then
        failwith (Printf.sprintf "Delete returned status %d: %s" del_response.status del_response.body);

      (* Verify file is gone *)
      let get_result = Http_client.get ~sw ~env ~url:delete_url () in
      match get_result with
      | Error e -> failwith ("GET after delete failed unexpectedly: " ^ e)
      | Ok get_response ->
        if get_response.status <> 404 then
          failwith (Printf.sprintf "Expected 404 after delete, got %d" get_response.status)

(** Test: Delete without Authorization header should return 401 *)
let test_delete_without_auth ~sw ~env =
  let base_url = Config.base_url in
  let fake_hash = "0000000000000000000000000000000000000000000000000000000000000000" in
  let delete_url = base_url ^ "/" ^ fake_hash in

  let result = Http_client.delete ~sw ~env ~url:delete_url () in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 401 then
      failwith (Printf.sprintf "Expected 401 for missing auth, got %d" response.status)

(** Test: Delete with non-owner pubkey should return 403 *)
let test_delete_not_owner ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  (* First upload a file with keypair1 *)
  let content = "File owned by keypair1" in
  let sha256 = sha256_hex content in

  let keypair1 = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let upload_auth = Nostr_signer.create_upload_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let upload_auth_header = Nostr_signer.to_auth_header upload_auth in
  let upload_url = base_url ^ "/upload" in

  let upload_result = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", upload_auth_header);
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
    let sha256 = match json with
      | `Assoc fields ->
        (match List.assoc_opt "sha256" fields with
         | Some (`String s) -> s
         | _ -> failwith "No sha256 in response")
      | _ -> failwith "Invalid upload response format"
    in

    (* Try to delete with keypair2 (not the owner) *)
    let keypair2 = Nostr_signer.generate_keypair () in
    let delete_auth = Nostr_signer.create_delete_auth ~keypair:keypair2 ~sha256 ~created_at:now ~expiration in
    let delete_auth_header = Nostr_signer.to_auth_header delete_auth in

    let delete_url = base_url ^ "/" ^ sha256 in
    let delete_result = Http_client.delete
      ~sw ~env
      ~url:delete_url
      ~headers:[("Authorization", delete_auth_header)]
      ()
    in

    match delete_result with
    | Error e -> failwith ("Delete request failed unexpectedly: " ^ e)
    | Ok del_response ->
      if del_response.status <> 403 then
        failwith (Printf.sprintf "Expected 403 for non-owner delete, got %d" del_response.status);

      (* Verify file still exists *)
      let get_result = Http_client.get ~sw ~env ~url:delete_url () in
      match get_result with
      | Error e -> failwith ("GET after failed delete failed unexpectedly: " ^ e)
      | Ok get_response ->
        if get_response.status <> 200 then
          failwith (Printf.sprintf "Expected 200 (file should still exist), got %d" get_response.status)

(** Test: Delete non-existent file should return 404 *)
let test_delete_not_found ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  let fake_hash = "0000000000000000000000000000000000000000000000000000000000000000" in

  let keypair = Nostr_signer.generate_keypair () in
  let expiration = Int64.of_float (now +. 3600.) in
  let delete_auth = Nostr_signer.create_delete_auth ~keypair ~sha256:fake_hash ~created_at:now ~expiration in
  let delete_auth_header = Nostr_signer.to_auth_header delete_auth in

  let delete_url = base_url ^ "/" ^ fake_hash in
  let result = Http_client.delete
    ~sw ~env
    ~url:delete_url
    ~headers:[("Authorization", delete_auth_header)]
    ()
  in

  match result with
  | Error e -> failwith ("Request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 404 then
      failwith (Printf.sprintf "Expected 404 for non-existent file, got %d" response.status)

(** All tests *)
let tests = [
  ("delete", test_delete);
  ("delete without auth", test_delete_without_auth);
  ("delete not owner", test_delete_not_owner);
  ("delete not found", test_delete_not_found);
]
