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

(* Context initialization *)

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
