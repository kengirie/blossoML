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

(** Test: Different pubkey can upload same file after first owner deletes *)
let test_reupload_after_delete_by_different_pubkey ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  (* Use unique content with timestamp to avoid collision with previous test runs *)
  let content = Printf.sprintf "Re-upload test %f %d" now (Random.int 1000000) in
  let sha256 = sha256_hex content in
  let expiration = Int64.of_float (now +. 3600.) in
  let upload_url = base_url ^ "/upload" in

  (* First, upload with keypair1 *)
  let keypair1 = Nostr_signer.generate_keypair () in
  let upload_auth1 = Nostr_signer.create_upload_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let upload_auth_header1 = Nostr_signer.to_auth_header upload_auth1 in

  let upload_result1 = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", upload_auth_header1);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  (match upload_result1 with
  | Error e -> failwith ("First upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "First upload returned status %d" response.status));

  (* Delete with keypair1 *)
  let delete_auth1 = Nostr_signer.create_delete_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let delete_auth_header1 = Nostr_signer.to_auth_header delete_auth1 in
  let delete_url = base_url ^ "/" ^ sha256 in

  let delete_result = Http_client.delete
    ~sw ~env
    ~url:delete_url
    ~headers:[("Authorization", delete_auth_header1)]
    ()
  in

  (match delete_result with
  | Error e -> failwith ("Delete failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Delete returned status %d" response.status));

  (* Verify blob is gone *)
  let get_result1 = Http_client.get ~sw ~env ~url:delete_url () in
  (match get_result1 with
  | Error e -> failwith ("GET after delete failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 404 then
      failwith (Printf.sprintf "Expected 404 after delete, got %d" response.status));

  (* Now, upload same content with keypair2 *)
  let keypair2 = Nostr_signer.generate_keypair () in
  let upload_auth2 = Nostr_signer.create_upload_auth ~keypair:keypair2 ~sha256 ~created_at:now ~expiration in
  let upload_auth_header2 = Nostr_signer.to_auth_header upload_auth2 in

  let upload_result2 = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", upload_auth_header2);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  match upload_result2 with
  | Error e -> failwith ("Re-upload by different pubkey failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Re-upload returned status %d: %s" response.status response.body);

    (* Verify blob is accessible again *)
    let get_result2 = Http_client.get ~sw ~env ~url:delete_url () in
    match get_result2 with
    | Error e -> failwith ("GET after re-upload failed: " ^ e)
    | Ok get_response ->
      if get_response.status <> 200 then
        failwith (Printf.sprintf "Expected 200 after re-upload, got %d" get_response.status);
      if get_response.body <> content then
        failwith "Downloaded content does not match uploaded content"

(** Test: When multiple pubkeys upload same file, first owner's delete doesn't remove blob *)
let test_multi_owner_first_delete_keeps_blob ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  (* Use unique content with timestamp to avoid collision with previous test runs *)
  let content = Printf.sprintf "Multi-owner test %f %d" now (Random.int 1000000) in
  let sha256 = sha256_hex content in
  let expiration = Int64.of_float (now +. 3600.) in
  let upload_url = base_url ^ "/upload" in

  (* Upload with keypair1 *)
  let keypair1 = Nostr_signer.generate_keypair () in
  let upload_auth1 = Nostr_signer.create_upload_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let upload_auth_header1 = Nostr_signer.to_auth_header upload_auth1 in

  let upload_result1 = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", upload_auth_header1);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  (match upload_result1 with
  | Error e -> failwith ("First upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "First upload returned status %d" response.status));

  (* Upload same file with keypair2 (adds second owner) *)
  let keypair2 = Nostr_signer.generate_keypair () in
  let upload_auth2 = Nostr_signer.create_upload_auth ~keypair:keypair2 ~sha256 ~created_at:now ~expiration in
  let upload_auth_header2 = Nostr_signer.to_auth_header upload_auth2 in

  let upload_result2 = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", upload_auth_header2);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  (match upload_result2 with
  | Error e -> failwith ("Second upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Second upload returned status %d" response.status));

  (* First owner deletes - should succeed but blob should remain *)
  let delete_auth1 = Nostr_signer.create_delete_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let delete_auth_header1 = Nostr_signer.to_auth_header delete_auth1 in
  let blob_url = base_url ^ "/" ^ sha256 in

  let delete_result = Http_client.delete
    ~sw ~env
    ~url:blob_url
    ~headers:[("Authorization", delete_auth_header1)]
    ()
  in

  (match delete_result with
  | Error e -> failwith ("First owner delete failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "First owner delete returned status %d" response.status));

  (* Blob should still be accessible (second owner still owns it) *)
  let get_result = Http_client.get ~sw ~env ~url:blob_url () in
  match get_result with
  | Error e -> failwith ("GET after first owner delete failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Expected 200 (blob should still exist), got %d" response.status);
    if response.body <> content then
      failwith "Content mismatch after first owner delete"

(** Test: After first owner deletes, they lose access to delete again *)
let test_first_owner_cannot_delete_twice ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  (* Use unique content with timestamp to avoid collision with previous test runs *)
  let content = Printf.sprintf "Double delete test %f %d" now (Random.int 1000000) in
  let sha256 = sha256_hex content in
  let expiration = Int64.of_float (now +. 3600.) in
  let upload_url = base_url ^ "/upload" in

  (* Upload with keypair1 *)
  let keypair1 = Nostr_signer.generate_keypair () in
  let upload_auth1 = Nostr_signer.create_upload_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let upload_auth_header1 = Nostr_signer.to_auth_header upload_auth1 in

  let upload_result1 = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", upload_auth_header1);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  (match upload_result1 with
  | Error e -> failwith ("First upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "First upload returned status %d" response.status));

  (* Upload same file with keypair2 *)
  let keypair2 = Nostr_signer.generate_keypair () in
  let upload_auth2 = Nostr_signer.create_upload_auth ~keypair:keypair2 ~sha256 ~created_at:now ~expiration in
  let upload_auth_header2 = Nostr_signer.to_auth_header upload_auth2 in

  let upload_result2 = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", upload_auth_header2);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  (match upload_result2 with
  | Error e -> failwith ("Second upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Second upload returned status %d" response.status));

  let blob_url = base_url ^ "/" ^ sha256 in

  (* First owner deletes *)
  let delete_auth1 = Nostr_signer.create_delete_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let delete_auth_header1 = Nostr_signer.to_auth_header delete_auth1 in

  let delete_result1 = Http_client.delete
    ~sw ~env
    ~url:blob_url
    ~headers:[("Authorization", delete_auth_header1)]
    ()
  in

  (match delete_result1 with
  | Error e -> failwith ("First delete failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "First delete returned status %d" response.status));

  (* First owner tries to delete again - should fail with 403 *)
  let delete_auth1_again = Nostr_signer.create_delete_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let delete_auth_header1_again = Nostr_signer.to_auth_header delete_auth1_again in

  let delete_result2 = Http_client.delete
    ~sw ~env
    ~url:blob_url
    ~headers:[("Authorization", delete_auth_header1_again)]
    ()
  in

  match delete_result2 with
  | Error e -> failwith ("Second delete request failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 403 then
      failwith (Printf.sprintf "Expected 403 for non-owner delete attempt, got %d" response.status)

(** Test: Both owners delete - blob is fully removed *)
let test_all_owners_delete_removes_blob ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  (* Use unique content with timestamp to avoid collision with previous test runs *)
  let content = Printf.sprintf "All owners delete test %f %d" now (Random.int 1000000) in
  let sha256 = sha256_hex content in
  let expiration = Int64.of_float (now +. 3600.) in
  let upload_url = base_url ^ "/upload" in

  (* Upload with keypair1 *)
  let keypair1 = Nostr_signer.generate_keypair () in
  let upload_auth1 = Nostr_signer.create_upload_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let upload_auth_header1 = Nostr_signer.to_auth_header upload_auth1 in

  let upload_result1 = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", upload_auth_header1);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  (match upload_result1 with
  | Error e -> failwith ("First upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "First upload returned status %d" response.status));

  (* Upload same file with keypair2 *)
  let keypair2 = Nostr_signer.generate_keypair () in
  let upload_auth2 = Nostr_signer.create_upload_auth ~keypair:keypair2 ~sha256 ~created_at:now ~expiration in
  let upload_auth_header2 = Nostr_signer.to_auth_header upload_auth2 in

  let upload_result2 = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", upload_auth_header2);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  (match upload_result2 with
  | Error e -> failwith ("Second upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Second upload returned status %d" response.status));

  let blob_url = base_url ^ "/" ^ sha256 in

  (* First owner deletes *)
  let delete_auth1 = Nostr_signer.create_delete_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let delete_auth_header1 = Nostr_signer.to_auth_header delete_auth1 in

  let delete_result1 = Http_client.delete
    ~sw ~env
    ~url:blob_url
    ~headers:[("Authorization", delete_auth_header1)]
    ()
  in

  (match delete_result1 with
  | Error e -> failwith ("First owner delete failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "First owner delete returned status %d" response.status));

  (* Blob should still exist *)
  let get_result1 = Http_client.get ~sw ~env ~url:blob_url () in
  (match get_result1 with
  | Error e -> failwith ("GET after first delete failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Expected 200 after first delete, got %d" response.status));

  (* Second owner deletes *)
  let delete_auth2 = Nostr_signer.create_delete_auth ~keypair:keypair2 ~sha256 ~created_at:now ~expiration in
  let delete_auth_header2 = Nostr_signer.to_auth_header delete_auth2 in

  let delete_result2 = Http_client.delete
    ~sw ~env
    ~url:blob_url
    ~headers:[("Authorization", delete_auth_header2)]
    ()
  in

  (match delete_result2 with
  | Error e -> failwith ("Second owner delete failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Second owner delete returned status %d" response.status));

  (* Blob should now be gone *)
  let get_result2 = Http_client.get ~sw ~env ~url:blob_url () in
  match get_result2 with
  | Error e -> failwith ("GET after full delete failed unexpectedly: " ^ e)
  | Ok response ->
    if response.status <> 404 then
      failwith (Printf.sprintf "Expected 404 after all owners delete, got %d" response.status)

(** Test: Second owner can still delete after first owner deletes *)
let test_second_owner_can_delete_after_first ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  (* Use unique content with timestamp to avoid collision with previous test runs *)
  let content = Printf.sprintf "Second owner delete test %f %d" now (Random.int 1000000) in
  let sha256 = sha256_hex content in
  let expiration = Int64.of_float (now +. 3600.) in
  let upload_url = base_url ^ "/upload" in

  (* Upload with keypair1 *)
  let keypair1 = Nostr_signer.generate_keypair () in
  let upload_auth1 = Nostr_signer.create_upload_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let upload_auth_header1 = Nostr_signer.to_auth_header upload_auth1 in

  let upload_result1 = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", upload_auth_header1);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  (match upload_result1 with
  | Error e -> failwith ("First upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "First upload returned status %d" response.status));

  (* Upload same file with keypair2 *)
  let keypair2 = Nostr_signer.generate_keypair () in
  let upload_auth2 = Nostr_signer.create_upload_auth ~keypair:keypair2 ~sha256 ~created_at:now ~expiration in
  let upload_auth_header2 = Nostr_signer.to_auth_header upload_auth2 in

  let upload_result2 = Http_client.put
    ~sw ~env
    ~url:upload_url
    ~headers:[
      ("Authorization", upload_auth_header2);
      ("Content-Type", "text/plain");
    ]
    ~body:content
    ()
  in

  (match upload_result2 with
  | Error e -> failwith ("Second upload failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Second upload returned status %d" response.status));

  let blob_url = base_url ^ "/" ^ sha256 in

  (* First owner deletes *)
  let delete_auth1 = Nostr_signer.create_delete_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let delete_auth_header1 = Nostr_signer.to_auth_header delete_auth1 in

  let delete_result1 = Http_client.delete
    ~sw ~env
    ~url:blob_url
    ~headers:[("Authorization", delete_auth_header1)]
    ()
  in

  (match delete_result1 with
  | Error e -> failwith ("First owner delete failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "First owner delete returned status %d" response.status));

  (* Second owner should still be able to delete successfully *)
  let delete_auth2 = Nostr_signer.create_delete_auth ~keypair:keypair2 ~sha256 ~created_at:now ~expiration in
  let delete_auth_header2 = Nostr_signer.to_auth_header delete_auth2 in

  let delete_result2 = Http_client.delete
    ~sw ~env
    ~url:blob_url
    ~headers:[("Authorization", delete_auth_header2)]
    ()
  in

  match delete_result2 with
  | Error e -> failwith ("Second owner delete failed: " ^ e)
  | Ok response ->
    if response.status <> 200 then
      failwith (Printf.sprintf "Second owner delete returned status %d: %s" response.status response.body)

(** Test: Ownership toggle - delete, re-upload by same pubkey, delete again *)
let test_ownership_toggle ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  (* Use unique content *)
  let content = Printf.sprintf "Ownership toggle test %f %d" now (Random.int 1000000) in
  let sha256 = sha256_hex content in
  let expiration = Int64.of_float (now +. 3600.) in
  let upload_url = base_url ^ "/upload" in
  let blob_url = base_url ^ "/" ^ sha256 in

  let keypair1 = Nostr_signer.generate_keypair () in
  let keypair2 = Nostr_signer.generate_keypair () in

  (* Step 1: keypair1 uploads *)
  let upload_auth1 = Nostr_signer.create_upload_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let upload_result1 = Http_client.put ~sw ~env ~url:upload_url
    ~headers:[("Authorization", Nostr_signer.to_auth_header upload_auth1); ("Content-Type", "text/plain")]
    ~body:content () in
  (match upload_result1 with
  | Error e -> failwith ("Step 1 upload failed: " ^ e)
  | Ok r -> if r.status <> 200 then failwith (Printf.sprintf "Step 1 upload status %d" r.status));

  (* Step 2: keypair1 deletes *)
  let delete_auth1 = Nostr_signer.create_delete_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let delete_result1 = Http_client.delete ~sw ~env ~url:blob_url
    ~headers:[("Authorization", Nostr_signer.to_auth_header delete_auth1)] () in
  (match delete_result1 with
  | Error e -> failwith ("Step 2 delete failed: " ^ e)
  | Ok r -> if r.status <> 200 then failwith (Printf.sprintf "Step 2 delete status %d" r.status));

  (* Verify blob is gone *)
  let get_result1 = Http_client.get ~sw ~env ~url:blob_url () in
  (match get_result1 with
  | Error e -> failwith ("GET after step 2 failed: " ^ e)
  | Ok r -> if r.status <> 404 then failwith (Printf.sprintf "Expected 404 after step 2, got %d" r.status));

  (* Step 3: keypair2 uploads same content *)
  let upload_auth2 = Nostr_signer.create_upload_auth ~keypair:keypair2 ~sha256 ~created_at:now ~expiration in
  let upload_result2 = Http_client.put ~sw ~env ~url:upload_url
    ~headers:[("Authorization", Nostr_signer.to_auth_header upload_auth2); ("Content-Type", "text/plain")]
    ~body:content () in
  (match upload_result2 with
  | Error e -> failwith ("Step 3 upload failed: " ^ e)
  | Ok r -> if r.status <> 200 then failwith (Printf.sprintf "Step 3 upload status %d" r.status));

  (* Step 4: keypair1 tries to delete - should fail (not owner anymore) *)
  let delete_auth1_again = Nostr_signer.create_delete_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let delete_result2 = Http_client.delete ~sw ~env ~url:blob_url
    ~headers:[("Authorization", Nostr_signer.to_auth_header delete_auth1_again)] () in
  (match delete_result2 with
  | Error e -> failwith ("Step 4 delete request failed: " ^ e)
  | Ok r -> if r.status <> 403 then failwith (Printf.sprintf "Expected 403 at step 4, got %d" r.status));

  (* Step 5: keypair1 re-uploads to regain ownership *)
  let upload_auth1_again = Nostr_signer.create_upload_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let upload_result3 = Http_client.put ~sw ~env ~url:upload_url
    ~headers:[("Authorization", Nostr_signer.to_auth_header upload_auth1_again); ("Content-Type", "text/plain")]
    ~body:content () in
  (match upload_result3 with
  | Error e -> failwith ("Step 5 upload failed: " ^ e)
  | Ok r -> if r.status <> 200 then failwith (Printf.sprintf "Step 5 upload status %d" r.status));

  (* Step 6: keypair1 can now delete again *)
  let delete_auth1_final = Nostr_signer.create_delete_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let delete_result3 = Http_client.delete ~sw ~env ~url:blob_url
    ~headers:[("Authorization", Nostr_signer.to_auth_header delete_auth1_final)] () in
  (match delete_result3 with
  | Error e -> failwith ("Step 6 delete failed: " ^ e)
  | Ok r -> if r.status <> 200 then failwith (Printf.sprintf "Step 6 delete status %d" r.status));

  (* Blob should still exist (keypair2 still owns it) *)
  let get_result2 = Http_client.get ~sw ~env ~url:blob_url () in
  (match get_result2 with
  | Error e -> failwith ("GET after step 6 failed: " ^ e)
  | Ok r ->
    if r.status <> 200 then failwith (Printf.sprintf "Expected 200 after step 6, got %d" r.status);
    if r.body <> content then failwith "Content mismatch after step 6");

  (* Step 7: keypair2 deletes - blob should be fully removed *)
  let delete_auth2 = Nostr_signer.create_delete_auth ~keypair:keypair2 ~sha256 ~created_at:now ~expiration in
  let delete_result4 = Http_client.delete ~sw ~env ~url:blob_url
    ~headers:[("Authorization", Nostr_signer.to_auth_header delete_auth2)] () in
  (match delete_result4 with
  | Error e -> failwith ("Step 7 delete failed: " ^ e)
  | Ok r -> if r.status <> 200 then failwith (Printf.sprintf "Step 7 delete status %d" r.status));

  (* Blob should now be gone *)
  let get_result3 = Http_client.get ~sw ~env ~url:blob_url () in
  match get_result3 with
  | Error e -> failwith ("GET after step 7 failed: " ^ e)
  | Ok r ->
    if r.status <> 404 then failwith (Printf.sprintf "Expected 404 after step 7, got %d" r.status)

(** Test: Three owners with sequential deletes *)
let test_triple_owner_sequential_delete ~sw ~env =
  let base_url = Config.base_url in
  let clock = Eio.Stdenv.clock env in
  let now = Eio.Time.now clock in

  (* Use unique content *)
  let content = Printf.sprintf "Triple owner test %f %d" now (Random.int 1000000) in
  let sha256 = sha256_hex content in
  let expiration = Int64.of_float (now +. 3600.) in
  let upload_url = base_url ^ "/upload" in
  let blob_url = base_url ^ "/" ^ sha256 in

  let keypair1 = Nostr_signer.generate_keypair () in
  let keypair2 = Nostr_signer.generate_keypair () in
  let keypair3 = Nostr_signer.generate_keypair () in

  (* All three upload the same content *)
  let upload ~keypair =
    let auth = Nostr_signer.create_upload_auth ~keypair ~sha256 ~created_at:now ~expiration in
    let result = Http_client.put ~sw ~env ~url:upload_url
      ~headers:[("Authorization", Nostr_signer.to_auth_header auth); ("Content-Type", "text/plain")]
      ~body:content () in
    match result with
    | Error e -> failwith ("Upload failed: " ^ e)
    | Ok r -> if r.status <> 200 then failwith (Printf.sprintf "Upload status %d" r.status)
  in

  upload ~keypair:keypair1;
  upload ~keypair:keypair2;
  upload ~keypair:keypair3;

  (* Verify blob exists *)
  let get_result0 = Http_client.get ~sw ~env ~url:blob_url () in
  (match get_result0 with
  | Error e -> failwith ("Initial GET failed: " ^ e)
  | Ok r -> if r.status <> 200 then failwith (Printf.sprintf "Initial GET status %d" r.status));

  (* keypair2 deletes first (out of order) *)
  let delete_auth2 = Nostr_signer.create_delete_auth ~keypair:keypair2 ~sha256 ~created_at:now ~expiration in
  let delete_result1 = Http_client.delete ~sw ~env ~url:blob_url
    ~headers:[("Authorization", Nostr_signer.to_auth_header delete_auth2)] () in
  (match delete_result1 with
  | Error e -> failwith ("keypair2 delete failed: " ^ e)
  | Ok r -> if r.status <> 200 then failwith (Printf.sprintf "keypair2 delete status %d" r.status));

  (* Blob should still exist *)
  let get_result1 = Http_client.get ~sw ~env ~url:blob_url () in
  (match get_result1 with
  | Error e -> failwith ("GET after keypair2 delete failed: " ^ e)
  | Ok r -> if r.status <> 200 then failwith (Printf.sprintf "Expected 200 after keypair2 delete, got %d" r.status));

  (* keypair2 cannot delete again *)
  let delete_auth2_again = Nostr_signer.create_delete_auth ~keypair:keypair2 ~sha256 ~created_at:now ~expiration in
  let delete_result2 = Http_client.delete ~sw ~env ~url:blob_url
    ~headers:[("Authorization", Nostr_signer.to_auth_header delete_auth2_again)] () in
  (match delete_result2 with
  | Error e -> failwith ("keypair2 second delete request failed: " ^ e)
  | Ok r -> if r.status <> 403 then failwith (Printf.sprintf "Expected 403 for keypair2 second delete, got %d" r.status));

  (* keypair1 deletes *)
  let delete_auth1 = Nostr_signer.create_delete_auth ~keypair:keypair1 ~sha256 ~created_at:now ~expiration in
  let delete_result3 = Http_client.delete ~sw ~env ~url:blob_url
    ~headers:[("Authorization", Nostr_signer.to_auth_header delete_auth1)] () in
  (match delete_result3 with
  | Error e -> failwith ("keypair1 delete failed: " ^ e)
  | Ok r -> if r.status <> 200 then failwith (Printf.sprintf "keypair1 delete status %d" r.status));

  (* Blob should still exist (keypair3 still owns it) *)
  let get_result2 = Http_client.get ~sw ~env ~url:blob_url () in
  (match get_result2 with
  | Error e -> failwith ("GET after keypair1 delete failed: " ^ e)
  | Ok r ->
    if r.status <> 200 then failwith (Printf.sprintf "Expected 200 after keypair1 delete, got %d" r.status);
    if r.body <> content then failwith "Content mismatch after keypair1 delete");

  (* keypair3 deletes - blob should be fully removed *)
  let delete_auth3 = Nostr_signer.create_delete_auth ~keypair:keypair3 ~sha256 ~created_at:now ~expiration in
  let delete_result4 = Http_client.delete ~sw ~env ~url:blob_url
    ~headers:[("Authorization", Nostr_signer.to_auth_header delete_auth3)] () in
  (match delete_result4 with
  | Error e -> failwith ("keypair3 delete failed: " ^ e)
  | Ok r -> if r.status <> 200 then failwith (Printf.sprintf "keypair3 delete status %d" r.status));

  (* Blob should now be gone *)
  let get_result3 = Http_client.get ~sw ~env ~url:blob_url () in
  match get_result3 with
  | Error e -> failwith ("GET after all deletes failed: " ^ e)
  | Ok r ->
    if r.status <> 404 then failwith (Printf.sprintf "Expected 404 after all owners delete, got %d" r.status)

(** All tests *)
let tests = [
  ("delete", test_delete);
  ("delete without auth", test_delete_without_auth);
  ("delete not owner", test_delete_not_owner);
  ("delete not found", test_delete_not_found);
  ("re-upload after delete by different pubkey", test_reupload_after_delete_by_different_pubkey);
  ("multi-owner first delete keeps blob", test_multi_owner_first_delete_keeps_blob);
  ("first owner cannot delete twice", test_first_owner_cannot_delete_twice);
  ("all owners delete removes blob", test_all_owners_delete_removes_blob);
  ("second owner can delete after first", test_second_owner_can_delete_after_first);
  ("ownership toggle", test_ownership_toggle);
  ("triple owner sequential delete", test_triple_owner_sequential_delete);
]
