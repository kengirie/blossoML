(* BIP-340 Schnorr signatures via the secp256k1 (dakk/secp256k1-ml) library.

   Thread safety note (see secp256k1.h):
   "A constructed context can safely be used from multiple threads
   simultaneously, but API calls that take a non-const pointer to a
   context need exclusive access to it."

   The operations we use (Schnorr.verify, Schnorr.sign32, XOPubkey.parse,
   Keypair.create, ...) only read from the context, so they are safe to
   call concurrently without locking. Only Context.randomize and
   destruction require exclusive access, and we do neither after creation. *)

(* Error types *)

type verify_error =
  | Invalid_hex of { field : string; value : string }
  | Invalid_length of { field : string; expected : int; actual : int }
  | Pubkey_parse_failed
  | Signature_verification_failed

(* Context initialization *)

let ctx = Secp256k1.Context.create [ Secp256k1.Context.Sign; Secp256k1.Context.Verify ]

(* Helper functions *)

let is_hex_char c =
  (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

(* Convert a hex string to a Bigarray buffer, validating length and charset. *)
let hex_to_buffer ~field hex : (Secp256k1.buffer, verify_error) result =
  let len = String.length hex in
  if len mod 2 <> 0 then
    Error (Invalid_length { field; expected = len + 1; actual = len })
  else if not (String.for_all is_hex_char hex) then
    Error (Invalid_hex { field; value = hex })
  else begin
    let buf = Bigarray.(Array1.create char c_layout (len / 2)) in
    for i = 0 to (len / 2) - 1 do
      let v = Scanf.sscanf (String.sub hex (i * 2) 2) "%x" (fun x -> x) in
      Bigarray.Array1.set buf i (char_of_int v)
    done;
    Ok buf
  end

let buffer_to_hex (buf : Secp256k1.buffer) =
  let len = Bigarray.Array1.dim buf in
  let b = Buffer.create (len * 2) in
  for i = 0 to len - 1 do
    Buffer.add_string b (Printf.sprintf "%02x" (int_of_char (Bigarray.Array1.get buf i)))
  done;
  Buffer.contents b

let verify ~pubkey ~msg ~signature : (unit, verify_error) result =
  (* Validate lengths first *)
  if String.length pubkey <> 64 then
    Error (Invalid_length { field = "pubkey"; expected = 64; actual = String.length pubkey })
  else if String.length msg <> 64 then
    Error (Invalid_length { field = "msg"; expected = 64; actual = String.length msg })
  else if String.length signature <> 128 then
    Error (Invalid_length { field = "signature"; expected = 128; actual = String.length signature })
  else
    match hex_to_buffer ~field:"msg" msg with
    | Error e -> Error e
    | Ok msg_buf ->
      match hex_to_buffer ~field:"signature" signature with
      | Error e -> Error e
      | Ok sig_buf ->
        match hex_to_buffer ~field:"pubkey" pubkey with
        | Error e -> Error e
        | Ok pubkey_buf ->
          (* Parse pubkey (raises Invalid_argument on malformed key) *)
          match Secp256k1.XOPubkey.parse_exn ctx pubkey_buf with
          | exception (Invalid_argument _) -> Error Pubkey_parse_failed
          | xonly_pubkey ->
            let signature = Secp256k1.Schnorr.of_bytes sig_buf in
            if Secp256k1.Schnorr.verify ctx signature msg_buf xonly_pubkey then Ok ()
            else Error Signature_verification_failed

type sign_error =
  | Sign_invalid_secret_key_length of { expected : int; actual : int }
  | Sign_invalid_secret_key_hex
  | Sign_invalid_msg_length of { expected : int; actual : int }
  | Sign_invalid_msg_hex
  | Sign_keypair_create_failed
  | Sign_signing_failed
  | Sign_pubkey_extract_failed

(** Sign a message with a secret key using BIP-340 Schnorr signature.
    @param secret_key 64-character hex string (32 bytes)
    @param msg 64-character hex string (32 bytes, typically an event ID)
    @return (signature_hex, pubkey_hex) where both are hex strings *)
let sign ~secret_key ~msg : ((string * string), sign_error) result =
  if String.length secret_key <> 64 then
    Error (Sign_invalid_secret_key_length { expected = 64; actual = String.length secret_key })
  else if String.length msg <> 64 then
    Error (Sign_invalid_msg_length { expected = 64; actual = String.length msg })
  else
    match hex_to_buffer ~field:"secret_key" secret_key with
    | Error _ -> Error Sign_invalid_secret_key_hex
    | Ok secret_buf ->
      match hex_to_buffer ~field:"msg" msg with
      | Error _ -> Error Sign_invalid_msg_hex
      | Ok msg_buf ->
        (* Read secret key and build a keypair *)
        match Secp256k1.Key.read_sk ctx secret_buf with
        | Error _ -> Error Sign_keypair_create_failed
        | Ok sk ->
          match Secp256k1.Keypair.create_exn ctx sk with
          | exception (Invalid_argument _) -> Error Sign_keypair_create_failed
          | keypair ->
            match Secp256k1.Schnorr.sign32 ctx msg_buf keypair None with
            | exception (Invalid_argument _) -> Error Sign_signing_failed
            | signature ->
              match Secp256k1.Keypair.xonly_pub_exn ctx keypair with
              | exception (Invalid_argument _) -> Error Sign_pubkey_extract_failed
              | xonly_pubkey ->
                let sig_hex = buffer_to_hex (Secp256k1.Schnorr.to_bytes signature) in
                let pubkey_hex = buffer_to_hex (Secp256k1.XOPubkey.serialize_exn ctx xonly_pubkey) in
                Ok (sig_hex, pubkey_hex)
