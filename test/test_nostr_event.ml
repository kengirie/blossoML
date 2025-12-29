open Alcotest
open Blossom_core

(* Test vectors - real Nostr events with valid signatures and verified IDs *)

(* Event 1: Upload event with verified ID *)
let event1 = Nostr_event.{
  id = "f5f352b3633d79a37d1e5b49d5d440b6e507683aa59517c70c07b08e7ac0a1be";
  pubkey = "83279ad28eec4785e2139dc529a9650fdbb424366d4645e5c2824f7cbd49240d";
  created_at = 1766993192L;
  kind = 24242;
  tags = [["expiration"; "1766993252"]; ["t"; "upload"]; ["x"; "e3a7c6adcfaf7853c1ceb4447c1a7804aaa6e1808bd035096ec5b65603d454f9"]];
  content = "blossom stuff";
  sig_ = "81637f697529ecd7a84029a5b620ec63f56731505df440c2b23a3b6a383d73a6b13640036c35b6333bb3fbe438190779ef7bb0da924b8bc8d5d56d56b099e6f8";
}

(* Event 2: Download event with server tag *)
let event2 = Nostr_event.{
  id = "d9484f18533d5e36f000f902a45b15a7eecf5fbfcb046789756d57ea87115dc5";
  pubkey = "b5f07faa8d3529f03bd898a23dfb3257bab8d8f5490777c46076ff9647e205dc";
  created_at = 1708771927L;
  kind = 24242;
  tags = [["t"; "get"]; ["expiration"; "1708857340"]; ["server"; "https://cdn.example.com/"]];
  content = "Get blobs from example.com";
  sig_ = "e402ade78e1714d40cd6bd3091bc5f4ada8e904e90301b5a2b9b5f0b6e95ce908d4f22b15e9fb86f8268a2131f8adbb3d1f0e7e7afd1ab0f4f08acb15822a999";
}

(* Delete event from BUD-02 spec *)
let delete_event = Nostr_event.{
  id = "a92868bd8ea740706d931f5d205308eaa0e6698e5f8026a990e78ee34ce47fe8";
  pubkey = "ae0063dd2c81ec469f2291ac029a19f39268bfc40aea7ab4136d7a858c3a06de";
  created_at = 1708774469L;
  kind = 24242;
  tags = [["t"; "delete"]; ["x"; "b1674191a88ec5cdd733e4240a81803105dc412d6c6708d53ab94fc248f4f553"]; ["expiration"; "1708858680"]];
  content = "Delete bitcoin.pdf";
  sig_ = "2ba9af680505583e3eb289a1624a08661a2f6fa2e5566a5ee0036333d517f965e0ffba7f5f7a57c2de37e00a2e85fd7999076468e52bdbcfad8abb76b37a94b0";
}

(* Test escape_json_string *)
let test_escape_basic () =
  check string "no escape needed" "hello" (Nostr_event.escape_json_string "hello");
  check string "empty string" "" (Nostr_event.escape_json_string "")

let test_escape_special_chars () =
  check string "newline" "hello\\nworld" (Nostr_event.escape_json_string "hello\nworld");
  check string "tab" "hello\\tworld" (Nostr_event.escape_json_string "hello\tworld");
  check string "carriage return" "hello\\rworld" (Nostr_event.escape_json_string "hello\rworld");
  check string "backslash" "hello\\\\world" (Nostr_event.escape_json_string "hello\\world");
  check string "quote" "hello\\\"world" (Nostr_event.escape_json_string "hello\"world");
  check string "backspace" "hello\\bworld" (Nostr_event.escape_json_string "hello\bworld");
  check string "form feed" "hello\\fworld" (Nostr_event.escape_json_string "hello\012world")

let test_escape_multiple () =
  check string "multiple escapes" "a\\nb\\tc\\\\" (Nostr_event.escape_json_string "a\nb\tc\\")

(* Test serialize_tags *)
let test_serialize_tags_empty () =
  check string "empty tags" "[]" (Nostr_event.serialize_tags [])

let test_serialize_tags_single () =
  check string "single tag" "[[\"t\",\"get\"]]" (Nostr_event.serialize_tags [["t"; "get"]])

let test_serialize_tags_multiple () =
  let tags = [["t"; "get"]; ["expiration"; "1708857340"]] in
  check string "multiple tags" "[[\"t\",\"get\"],[\"expiration\",\"1708857340\"]]" (Nostr_event.serialize_tags tags)

let test_serialize_tags_with_special_chars () =
  let tags = [["content"; "hello\nworld"]] in
  check string "tag with newline" "[[\"content\",\"hello\\nworld\"]]" (Nostr_event.serialize_tags tags)

(* Test serialize_for_id *)
let test_serialize_for_id_simple () =
  let result = Nostr_event.serialize_for_id
    ~pubkey:"abc123"
    ~created_at:1234567890L
    ~kind:1
    ~tags:[]
    ~content:"hello"
  in
  check string "simple serialization" "[0,\"abc123\",1234567890,1,[],\"hello\"]" result

let test_serialize_for_id_with_tags () =
  let result = Nostr_event.serialize_for_id
    ~pubkey:"abc123"
    ~created_at:1234567890L
    ~kind:1
    ~tags:[["t"; "test"]]
    ~content:"hello"
  in
  check string "with tags" "[0,\"abc123\",1234567890,1,[[\"t\",\"test\"]],\"hello\"]" result

(* Test verify_id with real events *)
let test_verify_id_event1 () =
  check bool "event1 id verification" true (Nostr_event.verify_id event1)

let test_verify_id_event2 () =
  check bool "event2 id verification" true (Nostr_event.verify_id event2)

let test_verify_id_tampered () =
  (* Tamper with the content - use event2 which has verified correct ID *)
  let tampered = { event2 with content = "Modified content" } in
  check bool "tampered event should fail" false (Nostr_event.verify_id tampered)

let test_verify_id_wrong_id () =
  (* Use wrong id *)
  let wrong_id = { event2 with id = "0000000000000000000000000000000000000000000000000000000000000000" } in
  check bool "wrong id should fail" false (Nostr_event.verify_id wrong_id)

let test_verify_id_tampered_tags () =
  (* Tamper with the tags *)
  let tampered = { event2 with tags = [["t"; "upload"]] } in
  check bool "tampered tags should fail" false (Nostr_event.verify_id tampered)

let test_verify_id_tampered_pubkey () =
  (* Tamper with the pubkey *)
  let tampered = { event2 with pubkey = "0000000000000000000000000000000000000000000000000000000000000000" } in
  check bool "tampered pubkey should fail" false (Nostr_event.verify_id tampered)

(* Test compute_id *)
let test_compute_id_event1 () =
  let computed = Nostr_event.compute_id
    ~pubkey:event1.pubkey
    ~created_at:event1.created_at
    ~kind:event1.kind
    ~tags:event1.tags
    ~content:event1.content
  in
  check string "computed id matches" event1.id computed

let test_compute_id_event2 () =
  let computed = Nostr_event.compute_id
    ~pubkey:event2.pubkey
    ~created_at:event2.created_at
    ~kind:event2.kind
    ~tags:event2.tags
    ~content:event2.content
  in
  check string "computed id matches" event2.id computed

(* Test make function *)
let test_make () =
  let event = Nostr_event.make
    ~pubkey:event2.pubkey
    ~created_at:event2.created_at
    ~kind:event2.kind
    ~tags:event2.tags
    ~content:event2.content
    ~sig_:event2.sig_
  in
  check string "make computes correct id" event2.id event.id;
  check bool "make creates verifiable event" true (Nostr_event.verify_id event)

(* Test find_tag *)
let test_find_tag_exists () =
  check (option string) "find t tag" (Some "upload") (Nostr_event.find_tag event1 "t");
  check (option string) "find expiration tag" (Some "1766993252") (Nostr_event.find_tag event1 "expiration")

let test_find_tag_not_exists () =
  check (option string) "find nonexistent tag" None (Nostr_event.find_tag event1 "nonexistent")

let test_find_tag_multiple_values () =
  (* find_tag returns first match *)
  check (option string) "find first t tag" (Some "get") (Nostr_event.find_tag event2 "t")

(* Test find_all_tags *)
let test_find_all_tags_single () =
  check (list string) "find all t tags" ["upload"] (Nostr_event.find_all_tags event1 "t")

let test_find_all_tags_none () =
  check (list string) "find all nonexistent" [] (Nostr_event.find_all_tags event1 "nonexistent")

let test_find_all_tags_x () =
  check (list string) "find all x tags" ["e3a7c6adcfaf7853c1ceb4447c1a7804aaa6e1808bd035096ec5b65603d454f9"] (Nostr_event.find_all_tags event1 "x")

(* Test verify_signature *)
let test_verify_signature_valid_event1 () =
  check bool "event1 signature valid" true (Nostr_event.verify_signature event1)

let test_verify_signature_valid_event2 () =
  check bool "event2 signature valid" true (Nostr_event.verify_signature event2)

let test_verify_signature_tampered_sig () =
  let tampered = { event1 with sig_ = "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" } in
  check bool "tampered sig should fail" false (Nostr_event.verify_signature tampered)

let test_verify_signature_wrong_length () =
  let short_sig = { event1 with sig_ = "abc" } in
  check bool "short sig should fail" false (Nostr_event.verify_signature short_sig)

(* Test verify (full verification: id + signature) *)
let test_verify_valid_event1 () =
  check bool "event1 full verify" true (Nostr_event.verify event1)

let test_verify_valid_event2 () =
  check bool "event2 full verify" true (Nostr_event.verify event2)

let test_verify_tampered_content () =
  let tampered = { event1 with content = "tampered" } in
  check bool "tampered content fails full verify" false (Nostr_event.verify tampered)

let test_verify_tampered_sig () =
  let tampered = { event1 with sig_ = "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" } in
  check bool "tampered sig fails full verify" false (Nostr_event.verify tampered)

let tests = [
  (* escape_json_string tests *)
  test_case "escape basic" `Quick test_escape_basic;
  test_case "escape special chars" `Quick test_escape_special_chars;
  test_case "escape multiple" `Quick test_escape_multiple;
  (* serialize_tags tests *)
  test_case "serialize empty tags" `Quick test_serialize_tags_empty;
  test_case "serialize single tag" `Quick test_serialize_tags_single;
  test_case "serialize multiple tags" `Quick test_serialize_tags_multiple;
  test_case "serialize tags with special chars" `Quick test_serialize_tags_with_special_chars;
  (* serialize_for_id tests *)
  test_case "serialize for id simple" `Quick test_serialize_for_id_simple;
  test_case "serialize for id with tags" `Quick test_serialize_for_id_with_tags;
  (* verify_id tests with real events *)
  test_case "verify id event1" `Quick test_verify_id_event1;
  test_case "verify id event2" `Quick test_verify_id_event2;
  test_case "verify id tampered content" `Quick test_verify_id_tampered;
  test_case "verify id wrong id" `Quick test_verify_id_wrong_id;
  test_case "verify id tampered tags" `Quick test_verify_id_tampered_tags;
  test_case "verify id tampered pubkey" `Quick test_verify_id_tampered_pubkey;
  (* compute_id tests *)
  test_case "compute id event1" `Quick test_compute_id_event1;
  test_case "compute id event2" `Quick test_compute_id_event2;
  (* make tests *)
  test_case "make function" `Quick test_make;
  (* find_tag tests *)
  test_case "find_tag exists" `Quick test_find_tag_exists;
  test_case "find_tag not exists" `Quick test_find_tag_not_exists;
  test_case "find_tag multiple values" `Quick test_find_tag_multiple_values;
  (* find_all_tags tests *)
  test_case "find_all_tags single" `Quick test_find_all_tags_single;
  test_case "find_all_tags none" `Quick test_find_all_tags_none;
  test_case "find_all_tags x" `Quick test_find_all_tags_x;
  (* verify_signature tests *)
  test_case "verify_signature valid event1" `Quick test_verify_signature_valid_event1;
  test_case "verify_signature valid event2" `Quick test_verify_signature_valid_event2;
  test_case "verify_signature tampered sig" `Quick test_verify_signature_tampered_sig;
  test_case "verify_signature wrong length" `Quick test_verify_signature_wrong_length;
  (* verify (full) tests *)
  test_case "verify valid event1" `Quick test_verify_valid_event1;
  test_case "verify valid event2" `Quick test_verify_valid_event2;
  test_case "verify tampered content" `Quick test_verify_tampered_content;
  test_case "verify tampered sig" `Quick test_verify_tampered_sig;
]
