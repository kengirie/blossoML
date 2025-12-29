(** Blossom authentication for kind 24242 events (BUD-01/BUD-02). *)

type action = Upload | Download | Delete

let action_to_string = function
  | Upload -> "upload"
  | Download -> "get"
  | Delete -> "delete"

(* Validate that x tag contains the specified hash *)
let validate_x_tag event ~sha256 =
  let x_tags = Nostr_event.find_all_tags event "x" in
  if List.mem sha256 x_tags then
    Ok ()
  else
    Error (Domain.Storage_error "Authorization event does not contain matching x tag for this blob")

let parse_auth_header header : (Nostr_event.t, Domain.error) result =
  try
    (* Format: "Nostr <base64-encoded-json>" *)
    match String.split_on_char ' ' header with
    | ["Nostr"; encoded] | ["nostr"; encoded] ->
        let decoded = Base64.decode_exn encoded in
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
    | _ -> Error (Domain.Storage_error "Invalid Authorization header format")
  with
  | Yojson.Json_error msg -> Error (Domain.Storage_error ("JSON parse error: " ^ msg))
  | _ -> Error (Domain.Storage_error "Failed to parse Authorization header")

(* Validate Blossom-specific event structure *)
let validate_blossom_event (event : Nostr_event.t) ~action ~current_time =
  if event.kind <> 24242 then
    Error (Domain.Storage_error "Invalid event kind, must be 24242")
  else if event.created_at > current_time then
    Error (Domain.Storage_error "Event created_at is in the future")
  else
    match Nostr_event.find_tag event "expiration" with
    | None -> Error (Domain.Storage_error "Missing expiration tag")
    | Some exp_str ->
        (try
          let expiration = Int64.of_string exp_str in
          if expiration <= current_time then
            Error (Domain.Storage_error "Event has expired")
          else
            match Nostr_event.find_tag event "t" with
            | None -> Error (Domain.Storage_error "Missing t tag")
            | Some t_value ->
                if t_value <> action_to_string action then
                  Error (Domain.Storage_error (Printf.sprintf "Invalid action, expected %s" (action_to_string action)))
                else
                  Ok ()
        with _ -> Error (Domain.Storage_error "Invalid expiration timestamp"))

(* Verify event using Nostr_event, returning Domain.error *)
let verify_event (event : Nostr_event.t) =
  if not (Nostr_event.verify_id event) then
    Error (Domain.Storage_error "Event ID does not match computed hash")
  else if not (Nostr_event.verify_signature event) then
    Error (Domain.Storage_error "Invalid signature")
  else
    Ok ()

let validate_auth ~header ~action ~current_time =
  match parse_auth_header header with
  | Error e -> Error e
  | Ok event ->
      match validate_blossom_event event ~action ~current_time with
      | Error e -> Error e
      | Ok () ->
          match verify_event event with
          | Error e -> Error e
          | Ok () -> Ok event.pubkey

let validate_auth_with_x_tag ~header ~sha256 ~action ~current_time =
  match parse_auth_header header with
  | Error e -> Error e
  | Ok event ->
      match validate_blossom_event event ~action ~current_time with
      | Error e -> Error e
      | Ok () ->
          match validate_x_tag event ~sha256 with
          | Error e -> Error e
          | Ok () ->
              match verify_event event with
              | Error e -> Error e
              | Ok () -> Ok event.pubkey

let validate_delete_auth ~header ~sha256 ~current_time =
  validate_auth_with_x_tag ~header ~sha256 ~action:Delete ~current_time

let validate_upload_auth ~header ~sha256 ~current_time =
  validate_auth_with_x_tag ~header ~sha256 ~action:Upload ~current_time
