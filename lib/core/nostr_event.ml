(** Nostr event ID computation and verification according to NIP-01.

    The event ID is computed as the SHA-256 hash of the canonical JSON
    serialization of the event data:
    [0, pubkey, created_at, kind, tags, content]
*)

type t = {
  id : string;
  pubkey : string;
  created_at : int64;
  kind : int;
  tags : string list list;
  content : string;
  sig_ : string;
}

(** Escape a string according to JSON rules.
    This handles: newline, carriage return, tab, backspace, formfeed,
    backslash, double quote, and control characters. *)
let escape_json_string s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | '\b' -> Buffer.add_string buf "\\b"
    | '\012' -> Buffer.add_string buf "\\f"  (* form feed *)
    | c when Char.code c < 32 ->
        (* Other control characters: use \uXXXX *)
        Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(** Serialize tags to JSON array format: [["t","upload"],["expiration","123"]] *)
let serialize_tags tags =
  let serialize_tag tag =
    let elements = List.map (fun s -> "\"" ^ escape_json_string s ^ "\"") tag in
    "[" ^ String.concat "," elements ^ "]"
  in
  "[" ^ String.concat "," (List.map serialize_tag tags) ^ "]"

(** Serialize an event to the canonical JSON format for ID computation.
    Format: [0, pubkey, created_at, kind, tags, content] *)
let serialize_for_id ~pubkey ~created_at ~kind ~tags ~content =
  Printf.sprintf "[0,\"%s\",%Ld,%d,%s,\"%s\"]"
    pubkey
    created_at
    kind
    (serialize_tags tags)
    (escape_json_string content)

(** Compute the event ID from serialized data.
    Returns lowercase hex-encoded SHA-256 hash. *)
let compute_id ~pubkey ~created_at ~kind ~tags ~content =
  let serialized = serialize_for_id ~pubkey ~created_at ~kind ~tags ~content in
  let hash = Digestif.SHA256.digest_string serialized in
  Digestif.SHA256.to_hex hash

(** Verify that the event ID matches the computed hash. *)
let verify_id event =
  let computed = compute_id
    ~pubkey:event.pubkey
    ~created_at:event.created_at
    ~kind:event.kind
    ~tags:event.tags
    ~content:event.content
  in
  String.equal computed event.id

(** Create an event with a computed ID. *)
let make ~pubkey ~created_at ~kind ~tags ~content ~sig_ =
  let id = compute_id ~pubkey ~created_at ~kind ~tags ~content in
  { id; pubkey; created_at; kind; tags; content; sig_ }

(** Find the first tag with the given name and return its value. *)
let find_tag event tag_name =
  try
    let tag = List.find (fun t ->
      match t with
      | name :: _ -> name = tag_name
      | _ -> false
    ) event.tags in
    match tag with
    | _ :: value :: _ -> Some value
    | _ -> None
  with Not_found -> None

(** Find all values for tags with the given name. *)
let find_all_tags event tag_name =
  event.tags
  |> List.filter_map (fun t ->
      match t with
      | name :: value :: _ when name = tag_name -> Some value
      | _ -> None)

(** Verify the BIP-340 signature of an event.
    Returns true if the signature is valid. *)
let verify_signature event =
  if String.length event.id <> 64 then
    false
  else if String.length event.pubkey <> 64 then
    false
  else if String.length event.sig_ <> 128 then
    false
  else
    try
      Bip340.verify ~pubkey:event.pubkey ~msg:event.id ~signature:event.sig_
    with _ -> false

(** Fully verify an event: ID matches content and signature is valid. *)
let verify event =
  verify_id event && verify_signature event
