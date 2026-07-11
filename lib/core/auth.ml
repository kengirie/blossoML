(** Blossom authentication for kind 24242 events (BUD-01/BUD-02). *)

open Syntax

type action = Upload | Download | Delete | List

let action_to_string = function
  | Upload -> "upload"
  | Download -> "get"
  | Delete -> "delete"
  | List -> "list"

(* Validate that x tag contains the specified hash *)
let validate_x_tag event ~sha256 =
  let x_tags = Nostr_event.find_all_tags event "x" in
  if List.mem sha256 x_tags then
    Ok ()
  else
    Error (Domain.Auth_error "Authorization event does not contain matching x tag for this blob")

let parse_auth_header header : (Nostr_event.t, Domain.error) result =
  (* Format: "Nostr <base64-encoded-json>" *)
  match String.split_on_char ' ' header with
  | ["Nostr"; encoded] | ["nostr"; encoded] ->
      (match Base64.decode encoded with
       | Error (`Msg msg) -> Error (Domain.Auth_error ("Base64 decode error: " ^ msg))
       | Ok decoded ->
           (try
             let json = Yojson.Safe.from_string decoded in
             let open Yojson.Safe.Util in
             let event : Nostr_event.t = {
               id = json |> member "id" |> to_string;
               pubkey = json |> member "pubkey" |> to_string;
               created_at = json |> member "created_at" |> to_int |> Int64.of_int;
               kind = json |> member "kind" |> to_int;
               tags = json |> member "tags" |> to_list |> List.map (fun tag -> tag |> to_list |> List.map to_string);
               content = json |> member "content" |> to_string;
               sig_ = json |> member "sig" |> to_string;
             } in
             Ok event
           with
           | Yojson.Json_error msg -> Error (Domain.Auth_error ("JSON parse error: " ^ msg))
           | Yojson.Safe.Util.Type_error (msg, _) -> Error (Domain.Auth_error ("JSON type error: " ^ msg))))
  | _ -> Error (Domain.Auth_error "Invalid Authorization header format")

(* Validate Blossom-specific event structure *)
let validate_blossom_event (event : Nostr_event.t) ~action ~current_time =
  let check cond msg = if cond then Ok () else Error (Domain.Auth_error msg) in
  let* () = check (event.kind = 24242) "Invalid event kind, must be 24242" in
  let* () = check (event.created_at <= current_time) "Event created_at is in the future" in
  let* exp_str =
    Nostr_event.find_tag event "expiration"
    |> Option.to_result ~none:(Domain.Auth_error "Missing expiration tag")
  in
  let* expiration =
    Int64.of_string_opt exp_str
    |> Option.to_result ~none:(Domain.Auth_error "Invalid expiration timestamp")
  in
  let* () = check (expiration > current_time) "Event has expired" in
  let* t_value =
    Nostr_event.find_tag event "t"
    |> Option.to_result ~none:(Domain.Auth_error "Missing t tag")
  in
  check (t_value = action_to_string action)
    (Printf.sprintf "Invalid action, expected %s" (action_to_string action))

(* Verify event using Nostr_event, returning Domain.error *)
let verify_event (event : Nostr_event.t) =
  if not (Nostr_event.verify_id event) then
    Error (Domain.Auth_error "Event ID does not match computed hash")
  else
    match Nostr_event.verify_signature event with
    | Ok () -> Ok ()
    | Error Nostr_event.Invalid_id_format ->
        Error (Domain.Auth_error "Invalid event ID format")
    | Error Nostr_event.Invalid_pubkey_format ->
        Error (Domain.Auth_error "Invalid pubkey format")
    | Error Nostr_event.Invalid_signature_format ->
        Error (Domain.Auth_error "Invalid signature format")
    | Error Nostr_event.Signature_mismatch ->
        Error (Domain.Auth_error "Invalid signature")

let validate_auth ~header ~action ~current_time =
  let* event = parse_auth_header header in
  let* () = validate_blossom_event event ~action ~current_time in
  let* () = verify_event event in
  Ok event.pubkey

let validate_auth_with_x_tag ~header ~sha256 ~action ~current_time =
  let* event = parse_auth_header header in
  let* () = validate_blossom_event event ~action ~current_time in
  let* () = validate_x_tag event ~sha256 in
  let* () = verify_event event in
  Ok event.pubkey

let validate_delete_auth ~header ~sha256 ~current_time =
  validate_auth_with_x_tag ~header ~sha256 ~action:Delete ~current_time

let validate_upload_auth ~header ~sha256 ~current_time =
  validate_auth_with_x_tag ~header ~sha256 ~action:Upload ~current_time
