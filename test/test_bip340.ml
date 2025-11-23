open Alcotest
open Blossom_core

(* Test vectors provided by user *)
let event1_id = "06d4842b9d7f8bf72440471704de4efa9ef8f0348e366d097405573994f66294"
let event1_pubkey = "ec0d11351457798907a3900fe465bfdc3b081be6efeb3d68c4d67774c0bc1f9a"
let event1_signature = "22ecb5116ba143e4c3d6dc4b53d549aed6970ec455f6d25d145e0ad1fd7c0e26c465b2e92d5fdf699c7050fa43e6a41f087ef167208d4f06425f61548168fd7f"

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
