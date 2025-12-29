(** Content-Type header parser and normalizer (RFC 2045)

    This module provides parsing and normalization of Content-Type headers.
    It follows RFC 2045 syntax: type "/" subtype *(";" parameter)
*)

(** Parsed Content-Type representation *)
type t = {
  media_type : string;  (* "type/subtype", lowercase normalized *)
  parameters : (string * string) list;  (* attribute-value pairs *)
}

(** Parse errors *)
type error =
  | Empty_input
  | Invalid_format of string

(** Get the media type (type/subtype) *)
let media_type t = t.media_type

(** Get parameters *)
let parameters t = t.parameters

(** Convert to string representation *)
let to_string t =
  if t.parameters = [] then
    t.media_type
  else
    let params =
      t.parameters
      |> List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v)
      |> String.concat "; "
    in
    Printf.sprintf "%s; %s" t.media_type params

(** Pretty printer *)
let pp fmt t =
  Format.fprintf fmt "%s" (to_string t)

(* --- Angstrom Parser --- *)

open Angstrom

(* RFC 2045: tspecials *)
let is_tspecials = function
  | '(' | ')' | '<' | '>'  | '@'
  | ',' | ';' | ':' | '\\' | '"'
  | '/' | '[' | ']' | '?'  | '=' -> true
  | _ -> false

(* RFC 2045: CTL (control characters) *)
let is_ctl = function
  | '\000' .. '\031' | '\127' -> true
  | _ -> false

let is_space = (=) ' '

(* RFC 2045: token - any char except tspecials, CTL, or space *)
let is_token_char c =
  not (is_tspecials c) && not (is_ctl c) && not (is_space c)

(* Whitespace: SP or HTAB *)
let is_wsp = function
  | ' ' | '\t' -> true
  | _ -> false

(* Skip optional whitespace *)
let ows = skip_while is_wsp

(* token = 1*<any CHAR except tspecials, CTL, SP> *)
let token = take_while1 is_token_char

(* quoted-string parsing (simplified RFC 822) *)
let is_qtext = function
  | '"' | '\\' -> false
  | c -> not (is_ctl c)

let quoted_pair =
  char '\\' *> any_char >>| String.make 1

let qcontent =
  (take_while1 is_qtext)
  <|> quoted_pair

let quoted_string =
  char '"'
  *> (many qcontent >>| String.concat "")
  <* char '"'

(* value = token / quoted-string *)
let value =
  quoted_string <|> token

(* attribute = token (case-insensitive, normalized to lowercase) *)
let attribute =
  token >>| String.lowercase_ascii

(* parameter = attribute "=" value *)
let parameter =
  ows *> char ';' *> ows *>
  lift2 (fun attr v -> (attr, v))
    (attribute <* ows <* char '=' <* ows)
    value

(* type = token (normalized to lowercase) *)
let media_type_part =
  token >>| String.lowercase_ascii

(* subtype = token (normalized to lowercase) *)
let subtype_part =
  token >>| String.lowercase_ascii

(* content-type = type "/" subtype *(";" parameter) *)
let content_type_parser =
  lift3 (fun ty subty params ->
    { media_type = ty ^ "/" ^ subty
    ; parameters = params
    })
    (ows *> media_type_part <* ows)
    (char '/' *> ows *> subtype_part <* ows)
    (many parameter <* ows)

(** Parse a Content-Type header string *)
let parse s =
  if String.length s = 0 then
    Error Empty_input
  else
    match parse_string ~consume:All content_type_parser s with
    | Ok v -> Ok v
    | Error msg -> Error (Invalid_format msg)

(** Normalize a Content-Type string to just the media type (type/subtype).
    Strips parameters and normalizes case. *)
let normalize_media_type s =
  match parse s with
  | Ok t -> Ok t.media_type
  | Error e -> Error e

(** Check if a Content-Type string is valid *)
let is_valid s =
  match parse s with
  | Ok _ -> true
  | Error _ -> false

(** Format error as string *)
let error_to_string = function
  | Empty_input -> "Empty Content-Type"
  | Invalid_format msg -> Printf.sprintf "Invalid Content-Type format: %s" msg
