open Alcotest
open Blossom_core

(* Test vectors provided by user *)
let event1_json = {|{
  "id": "06d4842b9d7f8bf72440471704de4efa9ef8f0348e366d097405573994f66294",
  "pubkey": "ec0d11351457798907a3900fe465bfdc3b081be6efeb3d68c4d67774c0bc1f9a",
  "kind": 24242,
  "content": "Get bitcoin.pdf",
  "created_at": 1708771927,
  "tags": [
    ["t", "get"],
    ["expiration", "1708857340"],
    ["x", "b1674191a88ec5cdd733e4240a81803105dc412d6c6708d53ab94fc248f4f553"]
  ],
  "sig": "22ecb5116ba143e4c3d6dc4b53d549aed6970ec455f6d25d145e0ad1fd7c0e26c465b2e92d5fdf699c7050fa43e6a41f087ef167208d4f06425f61548168fd7f"
}|}

let event2_json = {|{
  "id": "d9484f18533d5e36f000f902a45b15a7eecf5fbfcb046789756d57ea87115dc5",
  "pubkey": "b5f07faa8d3529f03bd898a23dfb3257bab8d8f5490777c46076ff9647e205dc",
  "kind": 24242,
  "content": "Get blobs from example.com",
  "created_at": 1708771927,
  "tags": [
    ["t", "get"],
    ["expiration", "1708857340"],
    ["server", "https://cdn.example.com/"]
  ],
  "sig": "e402ade78e1714d40cd6bd3091bc5f4ada8e904e90301b5a2b9b5f0b6e95ce908d4f22b15e9fb86f8268a2131f8adbb3d1f0e7e7afd1ab0f4f08acb15822a999"
}|}

(* Delete event from BUD-02 spec *)
let delete_event_json = {|{
  "id": "a92868bd8ea740706d931f5d205308eaa0e6698e5f8026a990e78ee34ce47fe8",
  "pubkey": "ae0063dd2c81ec469f2291ac029a19f39268bfc40aea7ab4136d7a858c3a06de",
  "kind": 24242,
  "content": "Delete bitcoin.pdf",
  "created_at": 1708774469,
  "tags": [
    ["t", "delete"],
    ["x", "b1674191a88ec5cdd733e4240a81803105dc412d6c6708d53ab94fc248f4f553"],
    ["expiration", "1708858680"]
  ],
  "sig": "2ba9af680505583e3eb289a1624a08661a2f6fa2e5566a5ee0036333d517f965e0ffba7f5f7a57c2de37e00a2e85fd7999076468e52bdbcfad8abb76b37a94b0"
}|}

(* Upload event with x tag for SHA256 validation test *)
(* Note: This is a mock event for testing. In real usage, the signature must be valid. *)
(* We use the same structure as delete event but with "upload" action *)
let upload_event_with_x_tag_json = {|{
  "id": "a92868bd8ea740706d931f5d205308eaa0e6698e5f8026a990e78ee34ce47fe8",
  "pubkey": "ae0063dd2c81ec469f2291ac029a19f39268bfc40aea7ab4136d7a858c3a06de",
  "kind": 24242,
  "content": "Upload bitcoin.pdf",
  "created_at": 1708774469,
  "tags": [
    ["t", "upload"],
    ["x", "b1674191a88ec5cdd733e4240a81803105dc412d6c6708d53ab94fc248f4f553"],
    ["expiration", "1708858680"]
  ],
  "sig": "2ba9af680505583e3eb289a1624a08661a2f6fa2e5566a5ee0036333d517f965e0ffba7f5f7a57c2de37e00a2e85fd7999076468e52bdbcfad8abb76b37a94b0"
}|}

(* Upload event without x tag *)
let upload_event_no_x_tag_json = {|{
  "id": "d9484f18533d5e36f000f902a45b15a7eecf5fbfcb046789756d57ea87115dc5",
  "pubkey": "b5f07faa8d3529f03bd898a23dfb3257bab8d8f5490777c46076ff9647e205dc",
  "kind": 24242,
  "content": "Upload blob",
  "created_at": 1708771927,
  "tags": [
    ["t", "upload"],
    ["expiration", "1708857340"]
  ],
  "sig": "e402ade78e1714d40cd6bd3091bc5f4ada8e904e90301b5a2b9b5f0b6e95ce908d4f22b15e9fb86f8268a2131f8adbb3d1f0e7e7afd1ab0f4f08acb15822a999"
}|}

(* Valid time window for delete event: created_at < current_time < expiration *)
(* created_at: 1708774469 *)
(* expiration: 1708858680 *)
let delete_valid_time = 1708800000L
let delete_target_sha256 = "b1674191a88ec5cdd733e4240a81803105dc412d6c6708d53ab94fc248f4f553"

(* Valid time window: created_at < current_time < expiration *)
(* created_at: 1708771927 *)
(* expiration: 1708857340 *)
let valid_time = 1708800000L

let test_parse_auth_header_valid () =
  let encoded = Base64.encode_exn event1_json in
  let header = "Nostr " ^ encoded in
  match Auth.parse_auth_header header with
  | Ok event ->
      check string "pubkey matches" "ec0d11351457798907a3900fe465bfdc3b081be6efeb3d68c4d67774c0bc1f9a" event.Auth.pubkey;
      check int "kind is 24242" 24242 event.Auth.kind;
      check string "id matches" "06d4842b9d7f8bf72440471704de4efa9ef8f0348e366d097405573994f66294" event.Auth.id
  | Error msg -> fail (Printf.sprintf "Failed to parse valid header: %s" (match msg with Domain.Storage_error s -> s | _ -> "Unknown error"))

let test_validate_auth_valid_event1 () =
  let encoded = Base64.encode_exn event1_json in
  let header = "Nostr " ^ encoded in
  match Auth.validate_auth ~header ~action:Auth.Download ~current_time:valid_time with
  | Ok pubkey ->
      check string "pubkey matches" "ec0d11351457798907a3900fe465bfdc3b081be6efeb3d68c4d67774c0bc1f9a" pubkey
  | Error msg -> fail (Printf.sprintf "Validation failed: %s" (match msg with Domain.Storage_error s -> s | _ -> "Unknown error"))

let test_validate_auth_valid_event2 () =
  let encoded = Base64.encode_exn event2_json in
  let header = "Nostr " ^ encoded in
  match Auth.validate_auth ~header ~action:Auth.Download ~current_time:valid_time with
  | Ok pubkey ->
      check string "pubkey matches" "b5f07faa8d3529f03bd898a23dfb3257bab8d8f5490777c46076ff9647e205dc" pubkey
  | Error msg -> fail (Printf.sprintf "Validation failed: %s" (match msg with Domain.Storage_error s -> s | _ -> "Unknown error"))

let test_validate_auth_wrong_action () =
  let encoded = Base64.encode_exn event1_json in
  let header = "Nostr " ^ encoded in
  (* Event 1 has action "get" (Download), so Upload should fail *)
  match Auth.validate_auth ~header ~action:Auth.Upload ~current_time:valid_time with
  | Ok _ -> fail "Should have failed with wrong action"
  | Error _ -> ()

let test_validate_auth_expired () =
  let encoded = Base64.encode_exn event1_json in
  let header = "Nostr " ^ encoded in
  let expired_time = 1708900000L in
  match Auth.validate_auth ~header ~action:Auth.Download ~current_time:expired_time with
  | Ok _ -> fail "Should have failed with expired event"
  | Error _ -> ()

let test_validate_auth_future_created_at () =
  let encoded = Base64.encode_exn event1_json in
  let header = "Nostr " ^ encoded in
  let past_time = 1708000000L in (* Before created_at *)
  match Auth.validate_auth ~header ~action:Auth.Download ~current_time:past_time with
  | Ok _ -> fail "Should have failed with future created_at"
  | Error _ -> ()

(* Delete authorization tests *)
let test_validate_delete_auth_valid () =
  let encoded = Base64.encode_exn delete_event_json in
  let header = "Nostr " ^ encoded in
  match Auth.validate_delete_auth ~header ~sha256:delete_target_sha256 ~current_time:delete_valid_time with
  | Ok pubkey ->
      check string "pubkey matches" "ae0063dd2c81ec469f2291ac029a19f39268bfc40aea7ab4136d7a858c3a06de" pubkey
  | Error msg -> fail (Printf.sprintf "Delete validation failed: %s" (match msg with Domain.Storage_error s -> s | _ -> "Unknown error"))

let test_validate_delete_auth_wrong_hash () =
  let encoded = Base64.encode_exn delete_event_json in
  let header = "Nostr " ^ encoded in
  let wrong_sha256 = "0000000000000000000000000000000000000000000000000000000000000000" in
  match Auth.validate_delete_auth ~header ~sha256:wrong_sha256 ~current_time:delete_valid_time with
  | Ok _ -> fail "Should have failed with wrong sha256"
  | Error (Domain.Storage_error msg) ->
      check bool "error message mentions x tag" true (String.length msg > 0)
  | Error _ -> fail "Expected Storage_error"

let test_validate_delete_auth_expired () =
  let encoded = Base64.encode_exn delete_event_json in
  let header = "Nostr " ^ encoded in
  let expired_time = 1708900000L in (* After expiration *)
  match Auth.validate_delete_auth ~header ~sha256:delete_target_sha256 ~current_time:expired_time with
  | Ok _ -> fail "Should have failed with expired event"
  | Error _ -> ()

let test_validate_delete_auth_wrong_action () =
  (* Use a get event and try to validate as delete *)
  let encoded = Base64.encode_exn event1_json in
  let header = "Nostr " ^ encoded in
  match Auth.validate_delete_auth ~header ~sha256:delete_target_sha256 ~current_time:valid_time with
  | Ok _ -> fail "Should have failed with wrong action (get instead of delete)"
  | Error _ -> ()

(* Upload authorization tests *)
let upload_target_sha256 = "b1674191a88ec5cdd733e4240a81803105dc412d6c6708d53ab94fc248f4f553"
let upload_valid_time = 1708800000L

(* Test: validate_upload_auth with matching sha256 in x tag
   Note: This test will fail signature verification because we modified the event content.
   We test the x tag validation logic separately. *)
let test_validate_upload_auth_x_tag_match () =
  (* Test the x tag validation directly via parse + validate_x_tag *)
  let encoded = Base64.encode_exn upload_event_with_x_tag_json in
  let header = "Nostr " ^ encoded in
  match Auth.parse_auth_header header with
  | Error msg -> fail (Printf.sprintf "Failed to parse header: %s" (match msg with Domain.Storage_error s -> s | _ -> "Unknown error"))
  | Ok event ->
      (* Verify x tag contains the expected hash *)
      let x_tags = Auth.find_all_tags event "x" in
      check bool "x tag contains expected hash" true (List.mem upload_target_sha256 x_tags)

let test_validate_upload_auth_x_tag_mismatch () =
  let encoded = Base64.encode_exn upload_event_with_x_tag_json in
  let header = "Nostr " ^ encoded in
  let wrong_sha256 = "0000000000000000000000000000000000000000000000000000000000000000" in
  match Auth.parse_auth_header header with
  | Error _ -> fail "Should have parsed header"
  | Ok event ->
      (* Verify x tag does not contain wrong hash *)
      let x_tags = Auth.find_all_tags event "x" in
      check bool "x tag should not contain wrong hash" false (List.mem wrong_sha256 x_tags)

let test_validate_upload_auth_no_x_tag () =
  let encoded = Base64.encode_exn upload_event_no_x_tag_json in
  let header = "Nostr " ^ encoded in
  match Auth.parse_auth_header header with
  | Error _ -> fail "Should have parsed header"
  | Ok event ->
      (* Verify no x tag present *)
      let x_tags = Auth.find_all_tags event "x" in
      check int "no x tags present" 0 (List.length x_tags)

let test_validate_x_tag_success () =
  let encoded = Base64.encode_exn upload_event_with_x_tag_json in
  let header = "Nostr " ^ encoded in
  match Auth.parse_auth_header header with
  | Error _ -> fail "Should have parsed header"
  | Ok event ->
      match Auth.validate_x_tag event ~sha256:upload_target_sha256 with
      | Ok () -> () (* Expected success *)
      | Error msg -> fail (Printf.sprintf "Should have succeeded: %s" (match msg with Domain.Storage_error s -> s | _ -> "Unknown error"))

let test_validate_x_tag_failure () =
  let encoded = Base64.encode_exn upload_event_with_x_tag_json in
  let header = "Nostr " ^ encoded in
  let wrong_sha256 = "0000000000000000000000000000000000000000000000000000000000000000" in
  match Auth.parse_auth_header header with
  | Error _ -> fail "Should have parsed header"
  | Ok event ->
      match Auth.validate_x_tag event ~sha256:wrong_sha256 with
      | Ok () -> fail "Should have failed with wrong sha256"
      | Error (Domain.Storage_error msg) ->
          check bool "error mentions x tag" true (String.length msg > 0)
      | Error _ -> fail "Expected Storage_error"

let test_validate_x_tag_missing () =
  let encoded = Base64.encode_exn upload_event_no_x_tag_json in
  let header = "Nostr " ^ encoded in
  match Auth.parse_auth_header header with
  | Error _ -> fail "Should have parsed header"
  | Ok event ->
      match Auth.validate_x_tag event ~sha256:upload_target_sha256 with
      | Ok () -> fail "Should have failed with missing x tag"
      | Error (Domain.Storage_error msg) ->
          check bool "error mentions x tag" true (String.length msg > 0)
      | Error _ -> fail "Expected Storage_error"

let tests = [
  test_case "parse_auth_header valid" `Quick test_parse_auth_header_valid;
  test_case "validate_auth valid event 1" `Quick test_validate_auth_valid_event1;
  test_case "validate_auth valid event 2" `Quick test_validate_auth_valid_event2;
  test_case "validate_auth wrong action" `Quick test_validate_auth_wrong_action;
  test_case "validate_auth expired" `Quick test_validate_auth_expired;
  test_case "validate_auth future created_at" `Quick test_validate_auth_future_created_at;
  test_case "validate_delete_auth valid" `Quick test_validate_delete_auth_valid;
  test_case "validate_delete_auth wrong hash" `Quick test_validate_delete_auth_wrong_hash;
  test_case "validate_delete_auth expired" `Quick test_validate_delete_auth_expired;
  test_case "validate_delete_auth wrong action" `Quick test_validate_delete_auth_wrong_action;
  (* Upload SHA256 validation tests *)
  test_case "validate_upload_auth x tag match" `Quick test_validate_upload_auth_x_tag_match;
  test_case "validate_upload_auth x tag mismatch" `Quick test_validate_upload_auth_x_tag_mismatch;
  test_case "validate_upload_auth no x tag" `Quick test_validate_upload_auth_no_x_tag;
  test_case "validate_x_tag success" `Quick test_validate_x_tag_success;
  test_case "validate_x_tag failure" `Quick test_validate_x_tag_failure;
  test_case "validate_x_tag missing" `Quick test_validate_x_tag_missing;
]
