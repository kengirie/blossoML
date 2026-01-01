(** Nostr event signing for E2E tests.
    Creates and signs BUD-01 authentication events. *)

open Blossom_core

type keypair = {
  secret_key : string;  (** 64 hex chars (32 bytes) *)
  pubkey : string;      (** 64 hex chars (32 bytes) *)
}

(** Generate a random keypair for testing.
    Uses a random 32-byte secret key and derives the public key. *)
let generate_keypair () =
  (* Generate 32 random bytes *)
  let random_bytes = Bytes.create 32 in
  for i = 0 to 31 do
    Bytes.set random_bytes i (Char.chr (Random.int 256))
  done;
  (* Convert to hex *)
  let secret_key =
    let buf = Buffer.create 64 in
    Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) random_bytes;
    Buffer.contents buf
  in
  (* Sign a dummy message to get the public key *)
  let dummy_msg = "0000000000000000000000000000000000000000000000000000000000000000" in
  match Bip340.sign ~secret_key ~msg:dummy_msg with
  | Error _ -> failwith "Failed to generate keypair"
  | Ok (_, pubkey) -> { secret_key; pubkey }

(** Create a BUD-01 upload authentication event (kind 24242).
    The event authorizes uploading to the Blossom server.
    sha256 is the hash of the content to be uploaded. *)
let create_upload_auth ~keypair ~sha256 ~created_at ~expiration =
  let created_at = Int64.of_float created_at in
  let tags = [
    ["t"; "upload"];
    ["x"; sha256];
    ["expiration"; Int64.to_string expiration];
  ] in
  let content = "Upload file" in
  let kind = 24242 in
  (* Compute event ID *)
  let id = Nostr_event.compute_id
    ~pubkey:keypair.pubkey
    ~created_at
    ~kind
    ~tags
    ~content
  in
  (* Sign the event ID *)
  match Bip340.sign ~secret_key:keypair.secret_key ~msg:id with
  | Error _ -> failwith "Failed to sign upload auth event"
  | Ok (sig_, _) ->
    { Nostr_event.id; pubkey = keypair.pubkey; created_at; kind; tags; content; sig_ }

(** Create a BUD-01 delete authentication event (kind 24242).
    The event authorizes deleting a specific blob. *)
let create_delete_auth ~keypair ~sha256 ~created_at ~expiration =
  let created_at = Int64.of_float created_at in
  let tags = [
    ["t"; "delete"];
    ["x"; sha256];
    ["expiration"; Int64.to_string expiration];
  ] in
  let content = "Delete file" in
  let kind = 24242 in
  let id = Nostr_event.compute_id
    ~pubkey:keypair.pubkey
    ~created_at
    ~kind
    ~tags
    ~content
  in
  match Bip340.sign ~secret_key:keypair.secret_key ~msg:id with
  | Error _ -> failwith "Failed to sign delete auth event"
  | Ok (sig_, _) ->
    { Nostr_event.id; pubkey = keypair.pubkey; created_at; kind; tags; content; sig_ }

(** Convert a Nostr event to JSON string. *)
let event_to_json event =
  let open Nostr_event in
  let tags_json =
    event.tags
    |> List.map (fun tag ->
        `List (List.map (fun s -> `String s) tag))
  in
  let json = `Assoc [
    ("id", `String event.id);
    ("pubkey", `String event.pubkey);
    ("created_at", `Int (Int64.to_int event.created_at));
    ("kind", `Int event.kind);
    ("tags", `List tags_json);
    ("content", `String event.content);
    ("sig", `String event.sig_);
  ] in
  Yojson.Safe.to_string json

(** Convert a Nostr event to a Nostr Authorization header value.
    Format: "Nostr <base64-encoded-event-json>" *)
let to_auth_header event =
  let json = event_to_json event in
  let encoded = Base64.encode_string json in
  "Nostr " ^ encoded
