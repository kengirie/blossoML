open Alcotest
open E2e
open Blossom_core

(* ===== generate_keypair tests ===== *)

let test_generate_keypair_length () =
  let kp = Nostr_signer.generate_keypair () in
  check int "secret_key length" 64 (String.length kp.secret_key);
  check int "pubkey length" 64 (String.length kp.pubkey)

let test_generate_keypair_hex () =
  let kp = Nostr_signer.generate_keypair () in
  let is_hex s = String.for_all (fun c ->
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
  ) s in
  check bool "secret_key is hex" true (is_hex kp.secret_key);
  check bool "pubkey is hex" true (is_hex kp.pubkey)

let test_generate_keypair_unique () =
  let kp1 = Nostr_signer.generate_keypair () in
  let kp2 = Nostr_signer.generate_keypair () in
  check bool "secret_keys are different" true (kp1.secret_key <> kp2.secret_key);
  check bool "pubkeys are different" true (kp1.pubkey <> kp2.pubkey)

let test_generate_keypair_consistent_pubkey () =
  (* Same secret key should produce same pubkey *)
  let kp = Nostr_signer.generate_keypair () in
  let dummy_msg = "0000000000000000000000000000000000000000000000000000000000000000" in
  match Bip340.sign ~secret_key:kp.secret_key ~msg:dummy_msg with
  | Error _ -> fail "signing should succeed"
  | Ok (_, derived_pubkey) ->
    check string "pubkey matches" kp.pubkey derived_pubkey

(* ===== create_upload_auth tests ===== *)

let test_create_upload_auth_valid_event () =
  let kp = Nostr_signer.generate_keypair () in
  let sha256 = "b1674191a88ec5cdd733e4240a81803105dc412d6c6708d53ab94fc248f4f553" in
  let now = Unix.time () in
  let expiration = Int64.of_float (now +. 3600.) in
  let event = Nostr_signer.create_upload_auth ~keypair:kp ~sha256 ~created_at:now ~expiration in
  (* Verify the event *)
  check bool "event is valid" true (Nostr_event.verify event)

let test_create_upload_auth_correct_kind () =
  let kp = Nostr_signer.generate_keypair () in
  let sha256 = "b1674191a88ec5cdd733e4240a81803105dc412d6c6708d53ab94fc248f4f553" in
  let now = Unix.time () in
  let expiration = Int64.of_float (now +. 3600.) in
  let event = Nostr_signer.create_upload_auth ~keypair:kp ~sha256 ~created_at:now ~expiration in
  check int "kind is 24242" 24242 event.kind

let test_create_upload_auth_correct_pubkey () =
  let kp = Nostr_signer.generate_keypair () in
  let sha256 = "b1674191a88ec5cdd733e4240a81803105dc412d6c6708d53ab94fc248f4f553" in
  let now = Unix.time () in
  let expiration = Int64.of_float (now +. 3600.) in
  let event = Nostr_signer.create_upload_auth ~keypair:kp ~sha256 ~created_at:now ~expiration in
  check string "pubkey matches" kp.pubkey event.pubkey

let test_create_upload_auth_has_upload_tag () =
  let kp = Nostr_signer.generate_keypair () in
  let sha256 = "b1674191a88ec5cdd733e4240a81803105dc412d6c6708d53ab94fc248f4f553" in
  let now = Unix.time () in
  let expiration = Int64.of_float (now +. 3600.) in
  let event = Nostr_signer.create_upload_auth ~keypair:kp ~sha256 ~created_at:now ~expiration in
  let t_tag = Nostr_event.find_tag event "t" in
  check (option string) "has t=upload tag" (Some "upload") t_tag

let test_create_upload_auth_has_expiration_tag () =
  let kp = Nostr_signer.generate_keypair () in
  let sha256 = "b1674191a88ec5cdd733e4240a81803105dc412d6c6708d53ab94fc248f4f553" in
  let now = Unix.time () in
  let expiration = Int64.of_float (now +. 3600.) in
  let event = Nostr_signer.create_upload_auth ~keypair:kp ~sha256 ~created_at:now ~expiration in
  let exp_tag = Nostr_event.find_tag event "expiration" in
  check (option string) "has expiration tag" (Some (Int64.to_string expiration)) exp_tag

(* ===== create_delete_auth tests ===== *)

let test_create_delete_auth_valid_event () =
  let kp = Nostr_signer.generate_keypair () in
  let sha256 = "b1674191a88ec5cdd733e4240a81803105dc412d6c6708d53ab94fc248f4f553" in
  let now = Unix.time () in
  let expiration = Int64.of_float (now +. 3600.) in
  let event = Nostr_signer.create_delete_auth ~keypair:kp ~sha256 ~created_at:now ~expiration in
  check bool "event is valid" true (Nostr_event.verify event)

let test_create_delete_auth_has_delete_tag () =
  let kp = Nostr_signer.generate_keypair () in
  let sha256 = "b1674191a88ec5cdd733e4240a81803105dc412d6c6708d53ab94fc248f4f553" in
  let now = Unix.time () in
  let expiration = Int64.of_float (now +. 3600.) in
  let event = Nostr_signer.create_delete_auth ~keypair:kp ~sha256 ~created_at:now ~expiration in
  let t_tag = Nostr_event.find_tag event "t" in
  check (option string) "has t=delete tag" (Some "delete") t_tag

let test_create_delete_auth_has_x_tag () =
  let kp = Nostr_signer.generate_keypair () in
  let sha256 = "b1674191a88ec5cdd733e4240a81803105dc412d6c6708d53ab94fc248f4f553" in
  let now = Unix.time () in
  let expiration = Int64.of_float (now +. 3600.) in
  let event = Nostr_signer.create_delete_auth ~keypair:kp ~sha256 ~created_at:now ~expiration in
  let x_tag = Nostr_event.find_tag event "x" in
  check (option string) "has x tag with sha256" (Some sha256) x_tag

(* ===== event_to_json tests ===== *)

let test_event_to_json_valid_json () =
  let kp = Nostr_signer.generate_keypair () in
  let sha256 = "b1674191a88ec5cdd733e4240a81803105dc412d6c6708d53ab94fc248f4f553" in
  let now = Unix.time () in
  let expiration = Int64.of_float (now +. 3600.) in
  let event = Nostr_signer.create_upload_auth ~keypair:kp ~sha256 ~created_at:now ~expiration in
  let json_str = Nostr_signer.event_to_json event in
  (* Should be parseable *)
  let parsed = Yojson.Safe.from_string json_str in
  match parsed with
  | `Assoc fields ->
    check bool "has id field" true (List.mem_assoc "id" fields);
    check bool "has pubkey field" true (List.mem_assoc "pubkey" fields);
    check bool "has sig field" true (List.mem_assoc "sig" fields)
  | _ -> fail "should be JSON object"

(* ===== to_auth_header tests ===== *)

let test_to_auth_header_format () =
  let kp = Nostr_signer.generate_keypair () in
  let sha256 = "b1674191a88ec5cdd733e4240a81803105dc412d6c6708d53ab94fc248f4f553" in
  let now = Unix.time () in
  let expiration = Int64.of_float (now +. 3600.) in
  let event = Nostr_signer.create_upload_auth ~keypair:kp ~sha256 ~created_at:now ~expiration in
  let header = Nostr_signer.to_auth_header event in
  check bool "starts with 'Nostr '" true (String.starts_with ~prefix:"Nostr " header)

let test_to_auth_header_decodable () =
  let kp = Nostr_signer.generate_keypair () in
  let sha256 = "b1674191a88ec5cdd733e4240a81803105dc412d6c6708d53ab94fc248f4f553" in
  let now = Unix.time () in
  let expiration = Int64.of_float (now +. 3600.) in
  let event = Nostr_signer.create_upload_auth ~keypair:kp ~sha256 ~created_at:now ~expiration in
  let header = Nostr_signer.to_auth_header event in
  (* Extract base64 part *)
  let base64_part = String.sub header 6 (String.length header - 6) in
  match Base64.decode base64_part with
  | Error _ -> fail "should be valid base64"
  | Ok decoded ->
    (* Should be valid JSON *)
    let parsed = Yojson.Safe.from_string decoded in
    match parsed with
    | `Assoc fields ->
      let id = List.assoc "id" fields in
      (match id with
       | `String s -> check string "id matches" event.id s
       | _ -> fail "id should be string")
    | _ -> fail "should be JSON object"

let test_to_auth_header_roundtrip () =
  let kp = Nostr_signer.generate_keypair () in
  let sha256 = "b1674191a88ec5cdd733e4240a81803105dc412d6c6708d53ab94fc248f4f553" in
  let now = Unix.time () in
  let expiration = Int64.of_float (now +. 3600.) in
  let event = Nostr_signer.create_upload_auth ~keypair:kp ~sha256 ~created_at:now ~expiration in
  let header = Nostr_signer.to_auth_header event in
  (* Should be usable with Auth.parse_auth_header *)
  match Auth.parse_auth_header header with
  | Error _ -> fail "should be parseable by Auth.parse_auth_header"
  | Ok parsed_event ->
    check string "id matches" event.id parsed_event.id;
    check string "pubkey matches" event.pubkey parsed_event.pubkey;
    check string "sig matches" event.sig_ parsed_event.sig_

let tests = [
  (* generate_keypair *)
  test_case "generate_keypair length" `Quick test_generate_keypair_length;
  test_case "generate_keypair hex" `Quick test_generate_keypair_hex;
  test_case "generate_keypair unique" `Quick test_generate_keypair_unique;
  test_case "generate_keypair consistent pubkey" `Quick test_generate_keypair_consistent_pubkey;
  (* create_upload_auth *)
  test_case "create_upload_auth valid event" `Quick test_create_upload_auth_valid_event;
  test_case "create_upload_auth correct kind" `Quick test_create_upload_auth_correct_kind;
  test_case "create_upload_auth correct pubkey" `Quick test_create_upload_auth_correct_pubkey;
  test_case "create_upload_auth has upload tag" `Quick test_create_upload_auth_has_upload_tag;
  test_case "create_upload_auth has expiration tag" `Quick test_create_upload_auth_has_expiration_tag;
  (* create_delete_auth *)
  test_case "create_delete_auth valid event" `Quick test_create_delete_auth_valid_event;
  test_case "create_delete_auth has delete tag" `Quick test_create_delete_auth_has_delete_tag;
  test_case "create_delete_auth has x tag" `Quick test_create_delete_auth_has_x_tag;
  (* event_to_json *)
  test_case "event_to_json valid json" `Quick test_event_to_json_valid_json;
  (* to_auth_header *)
  test_case "to_auth_header format" `Quick test_to_auth_header_format;
  test_case "to_auth_header decodable" `Quick test_to_auth_header_decodable;
  test_case "to_auth_header roundtrip" `Quick test_to_auth_header_roundtrip;
]
