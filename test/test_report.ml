open Alcotest
open Blossom_core

(** Helper to build a signed NIP-56 (kind 1984) report event JSON. *)
let make_report_json ~secret_key ~pubkey ~created_at ~tags ~content =
  let kind = 1984 in
  let id = Nostr_event.compute_id ~pubkey ~created_at ~kind ~tags ~content in
  let sig_ =
    match Bip340.sign ~secret_key ~msg:id with
    | Ok (s, _) -> s
    | Error _ -> failwith "signing failed"
  in
  let tags_json =
    tags
    |> List.map (fun tag -> `List (List.map (fun s -> `String s) tag))
  in
  let json = `Assoc [
    ("id", `String id);
    ("pubkey", `String pubkey);
    ("created_at", `Int (Int64.to_int created_at));
    ("kind", `Int kind);
    ("tags", `List tags_json);
    ("content", `String content);
    ("sig", `String sig_);
  ] in
  Yojson.Safe.to_string json

let fresh_keypair () =
  let random_bytes = Bytes.create 32 in
  for i = 0 to 31 do Bytes.set random_bytes i (Char.chr (Random.int 256)) done;
  let buf = Buffer.create 64 in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) random_bytes;
  let secret_key = Buffer.contents buf in
  let dummy = "0000000000000000000000000000000000000000000000000000000000000000" in
  match Bip340.sign ~secret_key ~msg:dummy with
  | Ok (_, pubkey) -> (secret_key, pubkey)
  | Error _ -> failwith "keypair gen failed"

let sha_a = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
let sha_b = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

let current () = Int64.of_float (Unix.time ())

(* ---- report_type_of_string / report_type_to_string ---- *)
let test_report_type_roundtrip () =
  let all = [
    "nudity"; "malware"; "profanity"; "illegal"; "spam";
    "impersonation"; "other";
  ] in
  List.iter (fun s ->
    match Report.report_type_of_string s with
    | None -> failf "expected Some for %s" s
    | Some rt ->
        check string ("roundtrip " ^ s) s (Report.report_type_to_string rt)
  ) all

let test_report_type_unknown () =
  check bool "unknown type rejected" true
    (Report.report_type_of_string "unknown" = None);
  check bool "empty string rejected" true
    (Report.report_type_of_string "" = None)

(* ---- validate happy path ---- *)
let test_validate_single_x_tag () =
  let secret_key, pubkey = fresh_keypair () in
  let created_at = Int64.sub (current ()) 10L in
  let body = make_report_json
    ~secret_key ~pubkey ~created_at
    ~tags:[["x"; sha_a; "malware"]]
    ~content:"contains a virus"
  in
  match Report.validate ~current_time:(current ()) body with
  | Error _ -> fail "expected validation success"
  | Ok r ->
      check string "reporter pubkey" pubkey r.reporter_pubkey;
      check int "one entry" 1 (List.length r.entries);
      let entry = List.hd r.entries in
      check string "sha256" sha_a entry.sha256;
      check string "report type" "malware" (Report.report_type_to_string entry.report_type);
      check string "content" "contains a virus" r.content;
      check (option string) "no e_tag" None r.e_tag;
      check (option string) "no p_tag" None r.p_tag

let test_validate_multiple_x_tags () =
  let secret_key, pubkey = fresh_keypair () in
  let created_at = current () in
  let body = make_report_json
    ~secret_key ~pubkey ~created_at
    ~tags:[
      ["x"; sha_a; "illegal"];
      ["x"; sha_b; "spam"];
      ["e"; "deadbeef"];
      ["p"; "feedface"];
    ]
    ~content:""
  in
  match Report.validate ~current_time:(current ()) body with
  | Error _ -> fail "expected success"
  | Ok r ->
      check int "two entries" 2 (List.length r.entries);
      check (option string) "e_tag preserved" (Some "deadbeef") r.e_tag;
      check (option string) "p_tag preserved" (Some "feedface") r.p_tag

(* ---- validate rejects bad input ---- *)
let test_validate_wrong_kind () =
  let secret_key, pubkey = fresh_keypair () in
  let kind = 1 in
  let created_at = current () in
  let tags = [["x"; sha_a; "malware"]] in
  let id = Nostr_event.compute_id ~pubkey ~created_at ~kind ~tags ~content:"" in
  let sig_ =
    match Bip340.sign ~secret_key ~msg:id with
    | Ok (s, _) -> s | Error _ -> failwith "sign"
  in
  let body =
    `Assoc [
      ("id", `String id);
      ("pubkey", `String pubkey);
      ("created_at", `Int (Int64.to_int created_at));
      ("kind", `Int kind);
      ("tags", `List [`List [`String "x"; `String sha_a; `String "malware"]]);
      ("content", `String "");
      ("sig", `String sig_);
    ] |> Yojson.Safe.to_string
  in
  match Report.validate ~current_time:(current ()) body with
  | Ok _ -> fail "expected failure"
  | Error (Domain.Report_error msg) ->
      check bool "kind error message" true (String.length msg > 0)
  | Error _ -> fail "expected Report_error"

let test_validate_no_x_tag () =
  let secret_key, pubkey = fresh_keypair () in
  let body = make_report_json
    ~secret_key ~pubkey ~created_at:(current ())
    ~tags:[["e"; "deadbeef"]]
    ~content:""
  in
  match Report.validate ~current_time:(current ()) body with
  | Ok _ -> fail "expected failure"
  | Error (Domain.Report_error _) -> ()
  | Error _ -> fail "expected Report_error"

let test_validate_invalid_sha256 () =
  let secret_key, pubkey = fresh_keypair () in
  let body = make_report_json
    ~secret_key ~pubkey ~created_at:(current ())
    ~tags:[["x"; "not-a-sha256"; "malware"]]
    ~content:""
  in
  match Report.validate ~current_time:(current ()) body with
  | Ok _ -> fail "expected failure"
  | Error (Domain.Report_error _) -> ()
  | Error _ -> fail "expected Report_error"

let test_validate_invalid_report_type () =
  let secret_key, pubkey = fresh_keypair () in
  let body = make_report_json
    ~secret_key ~pubkey ~created_at:(current ())
    ~tags:[["x"; sha_a; "vandalism"]]
    ~content:""
  in
  match Report.validate ~current_time:(current ()) body with
  | Ok _ -> fail "expected failure"
  | Error (Domain.Report_error _) -> ()
  | Error _ -> fail "expected Report_error"

let test_validate_x_tag_missing_type () =
  let secret_key, pubkey = fresh_keypair () in
  let body = make_report_json
    ~secret_key ~pubkey ~created_at:(current ())
    ~tags:[["x"; sha_a]]
    ~content:""
  in
  match Report.validate ~current_time:(current ()) body with
  | Ok _ -> fail "expected failure"
  | Error (Domain.Report_error _) -> ()
  | Error _ -> fail "expected Report_error"

let test_validate_tampered_signature () =
  let secret_key, pubkey = fresh_keypair () in
  let body = make_report_json
    ~secret_key ~pubkey ~created_at:(current ())
    ~tags:[["x"; sha_a; "spam"]]
    ~content:"hi"
  in
  (* Replace the content after signing so id will mismatch *)
  let json = Yojson.Safe.from_string body in
  let tampered =
    match json with
    | `Assoc fields ->
        let fields = List.map (fun (k, v) ->
          if k = "content" then (k, `String "tampered") else (k, v)
        ) fields in
        Yojson.Safe.to_string (`Assoc fields)
    | _ -> failwith "expected object"
  in
  match Report.validate ~current_time:(current ()) tampered with
  | Ok _ -> fail "expected failure"
  | Error (Domain.Report_error _) -> ()
  | Error _ -> fail "expected Report_error"

let test_validate_malformed_json () =
  match Report.validate ~current_time:(current ()) "not-json" with
  | Ok _ -> fail "expected failure"
  | Error (Domain.Report_error _) -> ()
  | Error _ -> fail "expected Report_error"

let tests = [
  test_case "report_type roundtrip" `Quick test_report_type_roundtrip;
  test_case "report_type unknown" `Quick test_report_type_unknown;
  test_case "validate single x tag" `Quick test_validate_single_x_tag;
  test_case "validate multiple x tags + e/p" `Quick test_validate_multiple_x_tags;
  test_case "validate rejects wrong kind" `Quick test_validate_wrong_kind;
  test_case "validate rejects no x tag" `Quick test_validate_no_x_tag;
  test_case "validate rejects invalid sha256" `Quick test_validate_invalid_sha256;
  test_case "validate rejects invalid report type" `Quick test_validate_invalid_report_type;
  test_case "validate rejects x tag without type" `Quick test_validate_x_tag_missing_type;
  test_case "validate rejects tampered signature" `Quick test_validate_tampered_signature;
  test_case "validate rejects malformed JSON" `Quick test_validate_malformed_json;
]
