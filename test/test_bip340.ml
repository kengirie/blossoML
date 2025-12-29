open Alcotest
open Blossom_core

(* Test vectors - real Nostr events with verified IDs *)
let event1_id = "f5f352b3633d79a37d1e5b49d5d440b6e507683aa59517c70c07b08e7ac0a1be"
let event1_pubkey = "83279ad28eec4785e2139dc529a9650fdbb424366d4645e5c2824f7cbd49240d"
let event1_signature = "81637f697529ecd7a84029a5b620ec63f56731505df440c2b23a3b6a383d73a6b13640036c35b6333bb3fbe438190779ef7bb0da924b8bc8d5d56d56b099e6f8"

let event2_id = "d9484f18533d5e36f000f902a45b15a7eecf5fbfcb046789756d57ea87115dc5"
let event2_pubkey = "b5f07faa8d3529f03bd898a23dfb3257bab8d8f5490777c46076ff9647e205dc"
let event2_signature = "e402ade78e1714d40cd6bd3091bc5f4ada8e904e90301b5a2b9b5f0b6e95ce908d4f22b15e9fb86f8268a2131f8adbb3d1f0e7e7afd1ab0f4f08acb15822a999"

let test_verify_valid_event1 () =
  let result = Bip340.verify
    ~pubkey:event1_pubkey
    ~msg:event1_id
    ~signature:event1_signature
  in
  check bool "verification should succeed" true result

let test_verify_valid_event2 () =
  let result = Bip340.verify
    ~pubkey:event2_pubkey
    ~msg:event2_id
    ~signature:event2_signature
  in
  check bool "verification should succeed" true result

let test_verify_invalid_signature () =
  (* Use event1 but tamper with signature *)
  let invalid_sig = String.map (fun c -> if c = 'a' then 'b' else c) event1_signature in
  let result = Bip340.verify
    ~pubkey:event1_pubkey
    ~msg:event1_id
    ~signature:invalid_sig
  in
  check bool "verification should fail" false result

let test_verify_invalid_msg () =
  (* Use event1 but tamper with msg (id) *)
  let invalid_msg = String.map (fun c -> if c = '0' then '1' else c) event1_id in
  let result = Bip340.verify
    ~pubkey:event1_pubkey
    ~msg:invalid_msg
    ~signature:event1_signature
  in
  check bool "verification should fail" false result

let test_verify_invalid_pubkey () =
  (* Use event1 but tamper with pubkey *)
  let invalid_pubkey = String.map (fun c -> if c = 'e' then 'f' else c) event1_pubkey in
  let result = Bip340.verify
    ~pubkey:invalid_pubkey
    ~msg:event1_id
    ~signature:event1_signature
  in
  check bool "verification should fail" false result

let tests = [
  test_case "verify valid event 1" `Quick test_verify_valid_event1;
  test_case "verify valid event 2" `Quick test_verify_valid_event2;
  test_case "verify invalid signature" `Quick test_verify_invalid_signature;
  test_case "verify invalid msg" `Quick test_verify_invalid_msg;
  test_case "verify invalid pubkey" `Quick test_verify_invalid_pubkey;
]
