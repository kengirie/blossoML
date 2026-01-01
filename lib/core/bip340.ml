open Ctypes
open Foreign

(* Error types *)

type verify_error =
  | Invalid_hex of { field : string; value : string }
  | Invalid_length of { field : string; expected : int; actual : int }
  | Pubkey_parse_failed
  | Signature_verification_failed

(* FFI types *)

type secp256k1_context = unit ptr

let secp256k1_context : secp256k1_context typ = ptr void

type secp256k1_keypair = unit ptr

let secp256k1_keypair : unit abstract typ =
  abstract ~name:"secp256k1_keypair" ~size:96 ~alignment:0
;;

type secp256k1_xonly_pubkey = unit ptr

let secp256k1_xonly_pubkey : unit abstract typ =
  abstract ~name:"secp256k1_xonly_pubkey" ~size:64 ~alignment:0
;;

(* FFI bindings *)

let secp256k1_context_create =
  foreign "secp256k1_context_create" (uint @-> returning secp256k1_context)
;;

let secp256k1_schnorrsig_verify =
  foreign
    "secp256k1_schnorrsig_verify"
    (secp256k1_context
     @-> ptr char
     @-> ptr char
     @-> size_t
     @-> ptr secp256k1_xonly_pubkey
     @-> returning int)
;;

let secp256k1_xonly_pubkey_parse =
  foreign
    "secp256k1_xonly_pubkey_parse"
    (secp256k1_context @-> ptr secp256k1_xonly_pubkey @-> ptr char @-> returning int)
;;

let secp256k1_keypair_create =
  foreign
    "secp256k1_keypair_create"
    (secp256k1_context @-> ptr secp256k1_keypair @-> ptr char @-> returning int)
;;

let secp256k1_schnorrsig_sign32 =
  foreign
    "secp256k1_schnorrsig_sign32"
    (secp256k1_context
     @-> ptr char (* sig64 output *)
     @-> ptr char (* msg32 *)
     @-> ptr secp256k1_keypair
     @-> ptr char (* aux_rand32, can be NULL *)
     @-> returning int)
;;

let secp256k1_keypair_xonly_pub =
  foreign
    "secp256k1_keypair_xonly_pub"
    (secp256k1_context
     @-> ptr secp256k1_xonly_pubkey
     @-> ptr int (* pk_parity, can be NULL *)
     @-> ptr secp256k1_keypair
     @-> returning int)
;;

let secp256k1_xonly_pubkey_serialize =
  foreign
    "secp256k1_xonly_pubkey_serialize"
    (secp256k1_context @-> ptr char @-> ptr secp256k1_xonly_pubkey @-> returning int)
;;

(* Context initialization

   Thread safety note (see secp256k1.h):
   "A constructed context can safely be used from multiple threads
   simultaneously, but API calls that take a non-const pointer to a
   context need exclusive access to it."

   The functions we use (secp256k1_schnorrsig_verify, secp256k1_schnorrsig_sign32,
   secp256k1_xonly_pubkey_parse, etc.) all take `const secp256k1_context *`,
   so they are safe to call concurrently without locking.

   Only secp256k1_context_destroy, secp256k1_context_preallocated_destroy,
   and secp256k1_context_randomize require exclusive access. *)

let ctx =
  secp256k1_context_create (Unsigned.UInt.of_int ((1 lsl 0) lor (1 lsl 8) lor (1 lsl 9)))
;;

(* Helper functions *)

let is_hex_char c =
  (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
;;

let hex_to_bytes ~field hex : (Bytes.t, verify_error) result =
  let len = String.length hex in
  if len mod 2 <> 0 then
    Error (Invalid_length { field; expected = len + 1; actual = len })
  else if not (String.for_all is_hex_char hex) then
    Error (Invalid_hex { field; value = hex })
  else begin
    let bytes = Bytes.create (len / 2) in
    for i = 0 to (len / 2) - 1 do
      let v = Scanf.sscanf (String.sub hex (i * 2) 2) "%x" (fun x -> x) in
      Bytes.set bytes i (char_of_int v)
    done;
    Ok bytes
  end
;;

let verify ~pubkey ~msg ~signature : (unit, verify_error) result =
  (* Validate lengths first *)
  if String.length pubkey <> 64 then
    Error (Invalid_length { field = "pubkey"; expected = 64; actual = String.length pubkey })
  else if String.length msg <> 64 then
    Error (Invalid_length { field = "msg"; expected = 64; actual = String.length msg })
  else if String.length signature <> 128 then
    Error (Invalid_length { field = "signature"; expected = 128; actual = String.length signature })
  else
    match hex_to_bytes ~field:"msg" msg with
    | Error e -> Error e
    | Ok msg_bytes ->
      match hex_to_bytes ~field:"signature" signature with
      | Error e -> Error e
      | Ok sig_bytes ->
        match hex_to_bytes ~field:"pubkey" pubkey with
        | Error e -> Error e
        | Ok pubkey_bytes ->
          (* Prepare msg32 *)
          let msg32 = allocate_n char ~count:32 in
          Bytes.iteri (fun i b -> msg32 +@ i <-@ b) msg_bytes;

          (* Prepare sig64 *)
          let sig64 = allocate_n char ~count:64 in
          Bytes.iteri (fun i b -> sig64 +@ i <-@ b) sig_bytes;

          (* Prepare pubkey *)
          let xonly_pubkey_alloc = allocate_n secp256k1_xonly_pubkey ~count:1 in
          let xonly_pubkey = xonly_pubkey_alloc +@ 0 in
          let pubkey32 = allocate_n char ~count:32 in
          Bytes.iteri (fun i b -> pubkey32 +@ i <-@ b) pubkey_bytes;

          (* Parse pubkey *)
          let parse_ret = secp256k1_xonly_pubkey_parse ctx xonly_pubkey pubkey32 in
          if parse_ret <> 1 then
            Error Pubkey_parse_failed
          else
            let ok =
              secp256k1_schnorrsig_verify ctx sig64 msg32 (Unsigned.Size_t.of_int 32) xonly_pubkey
            in
            if ok = 1 then Ok ()
            else Error Signature_verification_failed
;;

let bytes_to_hex bytes =
  let buf = Buffer.create (Bytes.length bytes * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (int_of_char c))) bytes;
  Buffer.contents buf
;;

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
    match hex_to_bytes ~field:"secret_key" secret_key with
    | Error _ -> Error Sign_invalid_secret_key_hex
    | Ok secret_bytes ->
      match hex_to_bytes ~field:"msg" msg with
      | Error _ -> Error Sign_invalid_msg_hex
      | Ok msg_bytes ->
        (* Prepare secret key *)
        let seckey32 = allocate_n char ~count:32 in
        Bytes.iteri (fun i b -> seckey32 +@ i <-@ b) secret_bytes;

        (* Create keypair *)
        let keypair_alloc = allocate_n secp256k1_keypair ~count:1 in
        let keypair = keypair_alloc +@ 0 in
        let ret = secp256k1_keypair_create ctx keypair seckey32 in
        if ret <> 1 then
          Error Sign_keypair_create_failed
        else begin
          (* Prepare message *)
          let msg32 = allocate_n char ~count:32 in
          Bytes.iteri (fun i b -> msg32 +@ i <-@ b) msg_bytes;

          (* Sign *)
          let sig64 = allocate_n char ~count:64 in
          let sign_ret = secp256k1_schnorrsig_sign32 ctx sig64 msg32 keypair (from_voidp char null) in
          if sign_ret <> 1 then
            Error Sign_signing_failed
          else begin
            (* Extract public key *)
            let xonly_pubkey_alloc = allocate_n secp256k1_xonly_pubkey ~count:1 in
            let xonly_pubkey = xonly_pubkey_alloc +@ 0 in
            let pub_ret = secp256k1_keypair_xonly_pub ctx xonly_pubkey (from_voidp int null) keypair in
            if pub_ret <> 1 then
              Error Sign_pubkey_extract_failed
            else begin
              (* Serialize public key.
                 Note: secp256k1_xonly_pubkey_serialize always returns 1
                 per the API documentation ("Returns: 1 always.") *)
              let pubkey32 = allocate_n char ~count:32 in
              let _ = secp256k1_xonly_pubkey_serialize ctx pubkey32 xonly_pubkey in

              (* Convert to hex *)
              let sig_bytes = Bytes.create 64 in
              for i = 0 to 63 do
                Bytes.set sig_bytes i !@(sig64 +@ i)
              done;

              let pubkey_bytes = Bytes.create 32 in
              for i = 0 to 31 do
                Bytes.set pubkey_bytes i !@(pubkey32 +@ i)
              done;

              Ok (bytes_to_hex sig_bytes, bytes_to_hex pubkey_bytes)
            end
          end
        end
;;
