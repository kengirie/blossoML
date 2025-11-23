open Ctypes
open Foreign

(* types *)

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

(* bindings *)

let secp256k1_context_create =
  foreign "secp256k1_context_create" (uint @-> returning secp256k1_context)
;;

(* We don't need signing for verification, but keeping bindings if needed later *)
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

(* wrappers *)

(* Create a context for verification (SECP256K1_CONTEXT_VERIFY | SECP256K1_CONTEXT_SIGN) *)
let ctx =
  secp256k1_context_create (Unsigned.UInt.of_int ((1 lsl 0) lor (1 lsl 8) lor (1 lsl 9)))
;;

let verify ~pubkey ~msg ~signature =
  (* Convert hex string to binary bytes *)
  let hex_to_bytes hex =
    let len = String.length hex in
    let bytes = Bytes.create (len / 2) in
    for i = 0 to (len / 2) - 1 do
      let v = Scanf.sscanf (String.sub hex (i * 2) 2) "%x" (fun x -> x) in
      Bytes.set bytes i (char_of_int v)
    done;
    bytes
  in

  (* Prepare msg32 (the hash/event ID) *)
  let msg32 = allocate_n char ~count:32 in
  let msg_bytes = hex_to_bytes msg in
  Bytes.iteri (fun i b -> msg32 +@ i <-@ b) msg_bytes;

  (* Prepare sig64 *)
  let sig64 = allocate_n char ~count:64 in
  let sig_bytes = hex_to_bytes signature in
  Bytes.iteri (fun i b -> sig64 +@ i <-@ b) sig_bytes;

  (* Prepare pubkey *)
  let xonly_pubkey_alloc = allocate_n secp256k1_xonly_pubkey ~count:1 in
  let xonly_pubkey = xonly_pubkey_alloc +@ 0 in
  let pubkey32 = allocate_n char ~count:32 in
  let pubkey_bytes = hex_to_bytes pubkey in
  Bytes.iteri (fun i b -> pubkey32 +@ i <-@ b) pubkey_bytes;

  (* Parse pubkey *)
  let parse_ret = secp256k1_xonly_pubkey_parse ctx xonly_pubkey pubkey32 in
  if parse_ret <> 1 then false
  else
    let ok =
      secp256k1_schnorrsig_verify ctx sig64 msg32 (Unsigned.Size_t.of_int 32) xonly_pubkey
    in
    ok == 1
;;
