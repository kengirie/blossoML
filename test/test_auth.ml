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

let tests = [
  test_case "parse_auth_header valid" `Quick test_parse_auth_header_valid;
  test_case "validate_auth valid event 1" `Quick test_validate_auth_valid_event1;
  test_case "validate_auth valid event 2" `Quick test_validate_auth_valid_event2;
  test_case "validate_auth wrong action" `Quick test_validate_auth_wrong_action;
  test_case "validate_auth expired" `Quick test_validate_auth_expired;
  test_case "validate_auth future created_at" `Quick test_validate_auth_future_created_at;
]
