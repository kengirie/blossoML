(** NIP-56 blob report event validation (BUD-09).

    A report is a kind:1984 Nostr event with one or more `x` tags, each
    containing the sha256 of a reported blob and a report type. *)

open Syntax

type report_type =
  | Nudity
  | Malware
  | Profanity
  | Illegal
  | Spam
  | Impersonation
  | Other

let report_type_of_string = function
  | "nudity" -> Some Nudity
  | "malware" -> Some Malware
  | "profanity" -> Some Profanity
  | "illegal" -> Some Illegal
  | "spam" -> Some Spam
  | "impersonation" -> Some Impersonation
  | "other" -> Some Other
  | _ -> None

let report_type_to_string = function
  | Nudity -> "nudity"
  | Malware -> "malware"
  | Profanity -> "profanity"
  | Illegal -> "illegal"
  | Spam -> "spam"
  | Impersonation -> "impersonation"
  | Other -> "other"

type entry = {
  sha256 : string;
  report_type : report_type;
}

type t = {
  event_id : string;
  reporter_pubkey : string;
  created_at : int64;
  entries : entry list;
  content : string;
  e_tag : string option;
  p_tag : string option;
  raw_event_json : string;
}

(** Parse a JSON string as a Nostr event. *)
let parse_event_json (body : string) : (Nostr_event.t, Domain.error) result =
  try
    let json = Yojson.Safe.from_string body in
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
  | Yojson.Json_error msg ->
      Error (Domain.Report_error ("JSON parse error: " ^ msg))
  | Yojson.Safe.Util.Type_error (msg, _) ->
      Error (Domain.Report_error ("JSON type error: " ^ msg))

(** Extract all `x` tag entries (sha256, type). Each x tag MUST be
    ["x", "<sha256>", "<report_type>"]. *)
let extract_entries (event : Nostr_event.t) : (entry list, Domain.error) result =
  let rec collect acc = function
    | [] -> Ok (List.rev acc)
    | ("x" :: sha :: type_str :: _) :: rest ->
        if not (Integrity.validate_hash sha) then
          Error (Domain.Report_error ("Invalid sha256 in x tag: " ^ sha))
        else
          let* rt =
            report_type_of_string type_str
            |> Option.to_result ~none:(Domain.Report_error
                 (Printf.sprintf "Invalid report type '%s' (expected nudity/malware/profanity/illegal/spam/impersonation/other)" type_str))
          in
          collect ({ sha256 = sha; report_type = rt } :: acc) rest
    | ("x" :: _) :: _ ->
        Error (Domain.Report_error "x tag must contain sha256 and report type")
    | _ :: rest -> collect acc rest
  in
  let* entries = collect [] event.tags in
  if entries = [] then
    Error (Domain.Report_error "Report event must contain at least one x tag")
  else
    Ok entries

(** Validate the Nostr event's id and signature. *)
let verify_event (event : Nostr_event.t) : (unit, Domain.error) result =
  if not (Nostr_event.verify_id event) then
    Error (Domain.Report_error "Event ID does not match computed hash")
  else
    match Nostr_event.verify_signature event with
    | Ok () -> Ok ()
    | Error Nostr_event.Invalid_id_format ->
        Error (Domain.Report_error "Invalid event ID format")
    | Error Nostr_event.Invalid_pubkey_format ->
        Error (Domain.Report_error "Invalid pubkey format")
    | Error Nostr_event.Invalid_signature_format ->
        Error (Domain.Report_error "Invalid signature format")
    | Error Nostr_event.Signature_mismatch ->
        Error (Domain.Report_error "Invalid signature")

(** Validate a NIP-56 report body for BUD-09 PUT /report.
    [current_time] is accepted for future use (e.g., rejecting events
    created far in the future). Currently we do not require an
    `expiration` tag, as NIP-56 does not mandate one. *)
let validate ~current_time (body : string) : (t, Domain.error) result =
  let _ = current_time in
  let* event = parse_event_json body in
  let* () =
    if event.kind = 1984 then Ok ()
    else Error (Domain.Report_error "Invalid event kind, must be 1984")
  in
  let* entries = extract_entries event in
  let* () = verify_event event in
  let e_tag = Nostr_event.find_tag event "e" in
  let p_tag = Nostr_event.find_tag event "p" in
  Ok {
    event_id = event.id;
    reporter_pubkey = event.pubkey;
    created_at = event.created_at;
    entries;
    content = event.content;
    e_tag;
    p_tag;
    raw_event_json = body;
  }
