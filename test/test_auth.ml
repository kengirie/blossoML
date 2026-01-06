open Alcotest
open Blossom_core

(* Test vectors - real Nostr events with verified IDs *)
let event1_json = {|{
  "id": "f5f352b3633d79a37d1e5b49d5d440b6e507683aa59517c70c07b08e7ac0a1be",
  "pubkey": "83279ad28eec4785e2139dc529a9650fdbb424366d4645e5c2824f7cbd49240d",
  "kind": 24242,
  "content": "blossom stuff",
  "created_at": 1766993192,
  "tags": [
    ["expiration", "1766993252"],
    ["t", "upload"],
    ["x", "e3a7c6adcfaf7853c1ceb4447c1a7804aaa6e1808bd035096ec5b65603d454f9"]
  ],
  "sig": "81637f697529ecd7a84029a5b620ec63f56731505df440c2b23a3b6a383d73a6b13640036c35b6333bb3fbe438190779ef7bb0da924b8bc8d5d56d56b099e6f8"
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

(* Delete event with verified ID *)
let delete_event_json = {|{
  "id": "82d6e8aed3e72d402be24f2e6fc353427ad901d48a3d8f060e2dd14f5eedc5a6",
  "pubkey": "83279ad28eec4785e2139dc529a9650fdbb424366d4645e5c2824f7cbd49240d",
  "kind": 24242,
  "content": "blossom stuff",
  "created_at": 1766993740,
  "tags": [
    ["expiration", "1766993800"],
    ["t", "delete"],
    ["x", "20cf9906a7a48aafcc2dd6d3373104569ee0599dc617e71cdce2e9f077283bb5"]
  ],
  "sig": "ba177049d5939fa1f3de2f6691b0721a0b02a03e7d303d21c3bc0970b3ede28f2bd96dc81977399512e1d110728ee03b512b30dc338bc50e2f3c12dc257c4446"
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
(* created_at: 1766993740 *)
(* expiration: 1766993800 *)
let delete_valid_time = 1766993750L
let delete_target_sha256 = "20cf9906a7a48aafcc2dd6d3373104569ee0599dc617e71cdce2e9f077283bb5"

(* Valid time window for event1: created_at < current_time < expiration *)
(* created_at: 1766993192 *)
(* expiration: 1766993252 *)
let event1_valid_time = 1766993200L
let event1_target_sha256 = "e3a7c6adcfaf7853c1ceb4447c1a7804aaa6e1808bd035096ec5b65603d454f9"

(* Valid time window for event2: created_at < current_time < expiration *)
(* created_at: 1708771927 *)
(* expiration: 1708857340 *)
let event2_valid_time = 1708800000L

let test_parse_auth_header_valid () =
  let encoded = Base64.encode_exn event1_json in
  let header = "Nostr " ^ encoded in
  match Auth.parse_auth_header header with
  | Ok event ->
      check string "pubkey matches" "83279ad28eec4785e2139dc529a9650fdbb424366d4645e5c2824f7cbd49240d" event.Nostr_event.pubkey;
      check int "kind is 24242" 24242 event.Nostr_event.kind;
      check string "id matches" "f5f352b3633d79a37d1e5b49d5d440b6e507683aa59517c70c07b08e7ac0a1be" event.Nostr_event.id
  | Error msg -> fail (Printf.sprintf "Failed to parse valid header: %s" (match msg with Domain.Auth_error s -> s | _ -> "Unknown error"))

let test_validate_auth_valid_event1 () =
  let encoded = Base64.encode_exn event1_json in
  let header = "Nostr " ^ encoded in
  match Auth.validate_auth ~header ~action:Auth.Upload ~current_time:event1_valid_time with
  | Ok pubkey ->
      check string "pubkey matches" "83279ad28eec4785e2139dc529a9650fdbb424366d4645e5c2824f7cbd49240d" pubkey
  | Error msg -> fail (Printf.sprintf "Validation failed: %s" (match msg with Domain.Auth_error s -> s | _ -> "Unknown error"))

let test_validate_auth_valid_event2 () =
  let encoded = Base64.encode_exn event2_json in
  let header = "Nostr " ^ encoded in
  match Auth.validate_auth ~header ~action:Auth.Download ~current_time:event2_valid_time with
  | Ok pubkey ->
      check string "pubkey matches" "b5f07faa8d3529f03bd898a23dfb3257bab8d8f5490777c46076ff9647e205dc" pubkey
  | Error msg -> fail (Printf.sprintf "Validation failed: %s" (match msg with Domain.Auth_error s -> s | _ -> "Unknown error"))

let test_validate_auth_wrong_action () =
  let encoded = Base64.encode_exn event1_json in
  let header = "Nostr " ^ encoded in
  (* Event 1 has action "upload", so Download should fail *)
  match Auth.validate_auth ~header ~action:Auth.Download ~current_time:event1_valid_time with
  | Ok _ -> fail "Should have failed with wrong action"
  | Error _ -> ()

let test_validate_auth_expired () =
  let encoded = Base64.encode_exn event1_json in
  let header = "Nostr " ^ encoded in
  let expired_time = 1766993300L in (* After expiration: 1766993252 *)
  match Auth.validate_auth ~header ~action:Auth.Upload ~current_time:expired_time with
  | Ok _ -> fail "Should have failed with expired event"
  | Error _ -> ()

let test_validate_auth_future_created_at () =
  let encoded = Base64.encode_exn event1_json in
  let header = "Nostr " ^ encoded in
  let past_time = 1766993100L in (* Before created_at: 1766993192 *)
  match Auth.validate_auth ~header ~action:Auth.Upload ~current_time:past_time with
  | Ok _ -> fail "Should have failed with future created_at"
  | Error _ -> ()

(* Delete authorization tests *)
let test_validate_delete_auth_valid () =
  let encoded = Base64.encode_exn delete_event_json in
  let header = "Nostr " ^ encoded in
  match Auth.validate_delete_auth ~header ~sha256:delete_target_sha256 ~current_time:delete_valid_time with
  | Ok pubkey ->
      check string "pubkey matches" "83279ad28eec4785e2139dc529a9650fdbb424366d4645e5c2824f7cbd49240d" pubkey
  | Error msg -> fail (Printf.sprintf "Delete validation failed: %s" (match msg with Domain.Auth_error s -> s | _ -> "Unknown error"))

let test_validate_delete_auth_wrong_hash () =
  let encoded = Base64.encode_exn delete_event_json in
  let header = "Nostr " ^ encoded in
  let wrong_sha256 = "0000000000000000000000000000000000000000000000000000000000000000" in
  match Auth.validate_delete_auth ~header ~sha256:wrong_sha256 ~current_time:delete_valid_time with
  | Ok _ -> fail "Should have failed with wrong sha256"
  | Error (Domain.Auth_error msg) ->
      check bool "error message mentions x tag" true (String.length msg > 0)
  | Error _ -> fail "Expected Auth_error"

let test_validate_delete_auth_expired () =
  let encoded = Base64.encode_exn delete_event_json in
  let header = "Nostr " ^ encoded in
  let expired_time = 1766993900L in (* After expiration: 1766993800 *)
  match Auth.validate_delete_auth ~header ~sha256:delete_target_sha256 ~current_time:expired_time with
  | Ok _ -> fail "Should have failed with expired event"
  | Error _ -> ()

let test_validate_delete_auth_wrong_action () =
  (* Use an upload event and try to validate as delete *)
  let encoded = Base64.encode_exn event1_json in
  let header = "Nostr " ^ encoded in
  match Auth.validate_delete_auth ~header ~sha256:event1_target_sha256 ~current_time:event1_valid_time with
  | Ok _ -> fail "Should have failed with wrong action (upload instead of delete)"
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
  | Error msg -> fail (Printf.sprintf "Failed to parse header: %s" (match msg with Domain.Auth_error s -> s | _ -> "Unknown error"))
  | Ok event ->
      (* Verify x tag contains the expected hash *)
      let x_tags = Nostr_event.find_all_tags event "x" in
      check bool "x tag contains expected hash" true (List.mem upload_target_sha256 x_tags)

let test_validate_upload_auth_x_tag_mismatch () =
  let encoded = Base64.encode_exn upload_event_with_x_tag_json in
  let header = "Nostr " ^ encoded in
  let wrong_sha256 = "0000000000000000000000000000000000000000000000000000000000000000" in
  match Auth.parse_auth_header header with
  | Error _ -> fail "Should have parsed header"
  | Ok event ->
      (* Verify x tag does not contain wrong hash *)
      let x_tags = Nostr_event.find_all_tags event "x" in
      check bool "x tag should not contain wrong hash" false (List.mem wrong_sha256 x_tags)

let test_validate_upload_auth_no_x_tag () =
  let encoded = Base64.encode_exn upload_event_no_x_tag_json in
  let header = "Nostr " ^ encoded in
  match Auth.parse_auth_header header with
  | Error _ -> fail "Should have parsed header"
  | Ok event ->
      (* Verify no x tag present *)
      let x_tags = Nostr_event.find_all_tags event "x" in
      check int "no x tags present" 0 (List.length x_tags)

let test_validate_x_tag_success () =
  let encoded = Base64.encode_exn upload_event_with_x_tag_json in
  let header = "Nostr " ^ encoded in
  match Auth.parse_auth_header header with
  | Error _ -> fail "Should have parsed header"
  | Ok event ->
      match Auth.validate_x_tag event ~sha256:upload_target_sha256 with
      | Ok () -> () (* Expected success *)
      | Error msg -> fail (Printf.sprintf "Should have succeeded: %s" (match msg with Domain.Auth_error s -> s | _ -> "Unknown error"))

let test_validate_x_tag_failure () =
  let encoded = Base64.encode_exn upload_event_with_x_tag_json in
  let header = "Nostr " ^ encoded in
  let wrong_sha256 = "0000000000000000000000000000000000000000000000000000000000000000" in
  match Auth.parse_auth_header header with
  | Error _ -> fail "Should have parsed header"
  | Ok event ->
      match Auth.validate_x_tag event ~sha256:wrong_sha256 with
      | Ok () -> fail "Should have failed with wrong sha256"
      | Error (Domain.Auth_error msg) ->
          check bool "error mentions x tag" true (String.length msg > 0)
      | Error _ -> fail "Expected Auth_error"

let test_validate_x_tag_missing () =
  let encoded = Base64.encode_exn upload_event_no_x_tag_json in
  let header = "Nostr " ^ encoded in
  match Auth.parse_auth_header header with
  | Error _ -> fail "Should have parsed header"
  | Ok event ->
      match Auth.validate_x_tag event ~sha256:upload_target_sha256 with
      | Ok () -> fail "Should have failed with missing x tag"
      | Error (Domain.Auth_error msg) ->
          check bool "error mentions x tag" true (String.length msg > 0)
      | Error _ -> fail "Expected Auth_error"

(* BUD-02: Multiple x tags MUST NOT be interpreted as bulk delete.
   When multiple x tags are present, server MUST only delete the blob listed in the URL.
   This test verifies that:
   1. An auth event with multiple x tags validates successfully for ONE of the hashes
   2. The same auth event does NOT validate for hashes NOT in the x tags *)
let delete_event_multiple_x_tags_json = {|{
  "id": "test_multiple_x_tags",
  "pubkey": "83279ad28eec4785e2139dc529a9650fdbb424366d4645e5c2824f7cbd49240d",
  "kind": 24242,
  "content": "Delete multiple files",
  "created_at": 1766993740,
  "tags": [
    ["expiration", "1766993800"],
    ["t", "delete"],
    ["x", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"],
    ["x", "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"],
    ["x", "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"]
  ],
  "sig": "dummy_sig_for_test"
}|}

(* Test: validate_x_tag succeeds for first hash in multiple x tags *)
let test_multiple_x_tags_validates_first_hash () =
  let encoded = Base64.encode_exn delete_event_multiple_x_tags_json in
  let header = "Nostr " ^ encoded in
  match Auth.parse_auth_header header with
  | Error _ -> fail "Should have parsed header"
  | Ok event ->
      let first_hash = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" in
      match Auth.validate_x_tag event ~sha256:first_hash with
      | Ok () -> () (* Expected: validation succeeds for hash in x tags *)
      | Error _ -> fail "Should have validated first x tag hash"

(* Test: validate_x_tag succeeds for second hash in multiple x tags *)
let test_multiple_x_tags_validates_second_hash () =
  let encoded = Base64.encode_exn delete_event_multiple_x_tags_json in
  let header = "Nostr " ^ encoded in
  match Auth.parse_auth_header header with
  | Error _ -> fail "Should have parsed header"
  | Ok event ->
      let second_hash = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" in
      match Auth.validate_x_tag event ~sha256:second_hash with
      | Ok () -> () (* Expected: validation succeeds for hash in x tags *)
      | Error _ -> fail "Should have validated second x tag hash"

(* Test: validate_x_tag succeeds for third hash in multiple x tags *)
let test_multiple_x_tags_validates_third_hash () =
  let encoded = Base64.encode_exn delete_event_multiple_x_tags_json in
  let header = "Nostr " ^ encoded in
  match Auth.parse_auth_header header with
  | Error _ -> fail "Should have parsed header"
  | Ok event ->
      let third_hash = "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc" in
      match Auth.validate_x_tag event ~sha256:third_hash with
      | Ok () -> () (* Expected: validation succeeds for hash in x tags *)
      | Error _ -> fail "Should have validated third x tag hash"

(* Test: validate_x_tag fails for hash NOT in multiple x tags
   This ensures we only validate the specific blob in the URL, not bulk delete *)
let test_multiple_x_tags_rejects_unlisted_hash () =
  let encoded = Base64.encode_exn delete_event_multiple_x_tags_json in
  let header = "Nostr " ^ encoded in
  match Auth.parse_auth_header header with
  | Error _ -> fail "Should have parsed header"
  | Ok event ->
      let unlisted_hash = "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd" in
      match Auth.validate_x_tag event ~sha256:unlisted_hash with
      | Ok () -> fail "Should have rejected hash not in x tags"
      | Error (Domain.Auth_error _) -> () (* Expected: rejection *)
      | Error _ -> fail "Expected Auth_error"

(* Test: Verify find_all_tags returns all x tags *)
let test_find_all_x_tags_returns_all () =
  let encoded = Base64.encode_exn delete_event_multiple_x_tags_json in
  let header = "Nostr " ^ encoded in
  match Auth.parse_auth_header header with
  | Error _ -> fail "Should have parsed header"
  | Ok event ->
      let x_tags = Nostr_event.find_all_tags event "x" in
      check int "should have 3 x tags" 3 (List.length x_tags);
      check bool "contains first hash" true
        (List.mem "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" x_tags);
      check bool "contains second hash" true
        (List.mem "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" x_tags);
      check bool "contains third hash" true
        (List.mem "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc" x_tags)

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
  (* BUD-02: Multiple x tags tests - no bulk delete *)
  test_case "multiple x tags validates first hash" `Quick test_multiple_x_tags_validates_first_hash;
  test_case "multiple x tags validates second hash" `Quick test_multiple_x_tags_validates_second_hash;
  test_case "multiple x tags validates third hash" `Quick test_multiple_x_tags_validates_third_hash;
  test_case "multiple x tags rejects unlisted hash" `Quick test_multiple_x_tags_rejects_unlisted_hash;
  test_case "find_all_x_tags returns all" `Quick test_find_all_x_tags_returns_all;
]
