(** Mirror request parsing and URL validation for BUD-04 *)

(** Parse mirror request JSON body: {"url": "https://..."} *)
let parse_request (json_str : string) : (Domain.mirror_request, Domain.error) result =
  try
    let json = Yojson.Safe.from_string json_str in
    match json with
    | `Assoc fields ->
        (match List.assoc_opt "url" fields with
         | Some (`String url) -> Ok { Domain.url }
         | Some _ -> Error (Domain.Mirror_invalid_url "url must be a string")
         | None -> Error (Domain.Mirror_invalid_url "missing url field"))
    | _ -> Error (Domain.Mirror_invalid_url "request body must be a JSON object")
  with
  | Yojson.Json_error msg ->
      Error (Domain.Mirror_invalid_url (Printf.sprintf "invalid JSON: %s" msg))

(** Validate mirror URL: must be http or https *)
let validate_url (url : string) : (unit, Domain.error) result =
  (* Basic URL validation - must start with http:// or https:// *)
  let url_lower = String.lowercase_ascii url in
  if String.length url < 8 then
    Error (Domain.Mirror_invalid_url "URL too short")
  else if not (String.sub url_lower 0 7 = "http://" || String.sub url_lower 0 8 = "https://") then
    Error (Domain.Mirror_invalid_url "URL must use http or https scheme")
  else
    Ok ()

(** Extract file extension from URL path for MIME type inference *)
let extension_from_url (url : string) : string option =
  (* Remove query string and fragment *)
  let path =
    let without_fragment =
      match String.index_opt url '#' with
      | Some i -> String.sub url 0 i
      | None -> url
    in
    match String.index_opt without_fragment '?' with
    | Some i -> String.sub without_fragment 0 i
    | None -> without_fragment
  in
  (* Find the last path component *)
  match String.rindex_opt path '/' with
  | None -> None
  | Some slash_pos ->
      let filename = String.sub path (slash_pos + 1) (String.length path - slash_pos - 1) in
      (* Find extension *)
      match String.rindex_opt filename '.' with
      | None -> None
      | Some dot_pos ->
          let ext = String.sub filename (dot_pos + 1) (String.length filename - dot_pos - 1) in
          if String.length ext > 0 then Some ext else None

(** {1 SSRF Protection - Pure IP/URL Validation} *)

(** Result of host extraction: either a bracket-enclosed host (IP literal)
    or an unbracketed host (hostname or bare IPv4). *)
type host_kind =
  | Bracketed of string   (** Content between [ ] — must be a valid IP *)
  | Unbracketed of string (** Hostname, bare IPv4, etc. *)

(** Extract host from URL (without port or brackets).
    Returns the bare host string and whether it was bracket-enclosed. *)
let extract_host_kind (url : string) : (host_kind, Domain.error) result =
  let url_lower = String.lowercase_ascii url in
  let after_scheme =
    if String.length url_lower >= 8 && String.sub url_lower 0 8 = "https://" then
      Some (String.sub url 8 (String.length url - 8))
    else if String.length url_lower >= 7 && String.sub url_lower 0 7 = "http://" then
      Some (String.sub url 7 (String.length url - 7))
    else
      None
  in
  match after_scheme with
  | None -> Error (Domain.Mirror_invalid_url "URL must use http or https scheme")
  | Some rest ->
    (* Extract authority (before first /) *)
    let authority =
      match String.index_opt rest '/' with
      | Some i -> String.sub rest 0 i
      | None -> rest
    in
    (* Remove query/fragment from authority if no path *)
    let authority =
      match String.index_opt authority '?' with
      | Some i -> String.sub authority 0 i
      | None -> authority
    in
    let authority =
      match String.index_opt authority '#' with
      | Some i -> String.sub authority 0 i
      | None -> authority
    in
    (* Remove userinfo if present *)
    let host_port =
      match String.rindex_opt authority '@' with
      | Some i -> String.sub authority (i + 1) (String.length authority - i - 1)
      | None -> authority
    in
    (* Handle IPv6 bracket notation: [::1]:8080 *)
    if String.length host_port > 0 && host_port.[0] = '[' then
      match String.index_opt host_port ']' with
      | Some i ->
        let host = String.sub host_port 1 (i - 1) in
        if String.length host = 0 then
          Error (Domain.Mirror_invalid_url "Empty IPv6 address in URL")
        else
          Ok (Bracketed host)
      | None -> Error (Domain.Mirror_invalid_url "Malformed IPv6 address in URL")
    else
      (* Remove port *)
      let host =
        match String.rindex_opt host_port ':' with
        | Some i -> String.sub host_port 0 i
        | None -> host_port
      in
      if String.length host = 0 then
        Error (Domain.Mirror_invalid_url "Empty host in URL")
      else
        Ok (Unbracketed host)

(** Extract host from URL (without port or brackets).
    Returns the bare host string for IP literal or hostname. *)
let extract_host (url : string) : (string, Domain.error) result =
  match extract_host_kind url with
  | Ok (Bracketed h) -> Ok h
  | Ok (Unbracketed h) -> Ok h
  | Error e -> Error e

(** Parse IPv4 address string "a.b.c.d" to 4-element int array of octets.
    Returns None if format is invalid or any octet is out of [0,255]. *)
let parse_ipv4 (s : string) : int array option =
  let parts = String.split_on_char '.' s in
  if List.length parts <> 4 then None
  else
    let octets = List.filter_map (fun p ->
      match int_of_string_opt p with
      | Some n when n >= 0 && n <= 255 -> Some n
      | _ -> None
    ) parts in
    if List.length octets = 4 then
      Some (Array.of_list octets)
    else
      None

(** Check if IPv4 octets represent a dangerous address.
    Returns Ok () for safe addresses, Error reason for dangerous ones. *)
let check_ipv4_safety (octets : int array) : (unit, string) result =
  if Array.length octets <> 4 then Error "invalid IPv4 address"
  else
    let a = octets.(0) and b = octets.(1)
    and c = octets.(2) and d = octets.(3) in
    if a = 0 && b = 0 && c = 0 && d = 0 then
      Error "unspecified address (0.0.0.0)"
    else if a = 0 then
      Error (Printf.sprintf "reserved address (%d.%d.%d.%d in 0.0.0.0/8)" a b c d)
    else if a = 127 then
      Error (Printf.sprintf "loopback address (%d.%d.%d.%d in 127.0.0.0/8)" a b c d)
    else if a = 10 then
      Error (Printf.sprintf "private address (%d.%d.%d.%d in 10.0.0.0/8)" a b c d)
    else if a = 172 && b >= 16 && b <= 31 then
      Error (Printf.sprintf "private address (%d.%d.%d.%d in 172.16.0.0/12)" a b c d)
    else if a = 192 && b = 168 then
      Error (Printf.sprintf "private address (%d.%d.%d.%d in 192.168.0.0/16)" a b c d)
    else if a = 169 && b = 254 then
      if c = 169 && d = 254 then
        Error "cloud metadata address (169.254.169.254)"
      else
        Error (Printf.sprintf "link-local address (%d.%d.%d.%d in 169.254.0.0/16)" a b c d)
    else
      Ok ()

(** Parse a single hex group (1-4 hex digits) to a 16-bit integer. *)
let parse_hex_group (s : string) : int option =
  if String.length s = 0 || String.length s > 4 then None
  else
    match int_of_string_opt ("0x" ^ s) with
    | Some v when v >= 0 && v <= 0xffff -> Some v
    | _ -> None

(** Parse IPv6 address string to 8-element array of 16-bit groups.
    Handles :: expansion and IPv4-mapped suffix (::ffff:a.b.c.d). *)
let parse_ipv6_groups (s : string) : int array option =
  let find_double_colon str =
    let len = String.length str in
    let rec scan i =
      if i > len - 2 then None
      else if str.[i] = ':' && str.[i + 1] = ':' then Some i
      else scan (i + 1)
    in
    if len < 2 then None else scan 0
  in
  (* Parse colon-separated groups, handling IPv4 suffix in last position *)
  let parse_group_list str =
    if String.length str = 0 then Some []
    else
      let parts = String.split_on_char ':' str in
      let rec parse_all = function
        | [] -> Some []
        | [last] when String.contains last '.' ->
          (* IPv4-mapped suffix: parse as IPv4 and convert to 2 groups *)
          (match parse_ipv4 last with
           | Some octets ->
             let g1 = octets.(0) * 256 + octets.(1) in
             let g2 = octets.(2) * 256 + octets.(3) in
             Some [g1; g2]
           | None -> None)
        | hd :: tl ->
          (match parse_hex_group hd with
           | Some v ->
             (match parse_all tl with
              | Some rest -> Some (v :: rest)
              | None -> None)
           | None -> None)
      in
      parse_all parts
  in
  match find_double_colon s with
  | None ->
    (* No :: — must have exactly 8 groups (or 6 hex + IPv4 suffix = 8) *)
    (match parse_group_list s with
     | Some groups when List.length groups = 8 -> Some (Array.of_list groups)
     | _ -> None)
  | Some pos ->
    let before = String.sub s 0 pos in
    let after = String.sub s (pos + 2) (String.length s - pos - 2) in
    let before_result =
      if String.length before = 0 then Some []
      else parse_group_list before
    in
    let after_result =
      if String.length after = 0 then Some []
      else parse_group_list after
    in
    (match before_result, after_result with
     | Some bg, Some ag ->
       let total = List.length bg + List.length ag in
       if total > 8 then None
       else
         let zeros = List.init (8 - total) (fun _ -> 0) in
         Some (Array.of_list (bg @ zeros @ ag))
     | _ -> None)

(** Check if IPv6 groups represent a dangerous address.
    Returns Ok () for safe addresses, Error reason for dangerous ones. *)
let check_ipv6_safety (groups : int array) : (unit, string) result =
  if Array.length groups <> 8 then Error "invalid IPv6 address"
  else
    let is_all_zero = Array.for_all (fun g -> g = 0) groups in
    if is_all_zero then
      Error "unspecified address (::)"
    else
      let is_loopback =
        groups.(0) = 0 && groups.(1) = 0 && groups.(2) = 0 && groups.(3) = 0 &&
        groups.(4) = 0 && groups.(5) = 0 && groups.(6) = 0 && groups.(7) = 1
      in
      if is_loopback then
        Error "loopback address (::1)"
      else
        (* fe80::/10 *)
        let is_link_local = (groups.(0) land 0xffc0) = 0xfe80 in
        if is_link_local then
          Error "link-local address (fe80::/10)"
        else
          (* fc00::/7 — unique-local *)
          let is_unique_local = (groups.(0) land 0xfe00) = 0xfc00 in
          if is_unique_local then
            Error "unique-local address (fc00::/7)"
          else
            (* ::ffff:a.b.c.d — IPv4-mapped *)
            let is_ipv4_mapped =
              groups.(0) = 0 && groups.(1) = 0 && groups.(2) = 0 && groups.(3) = 0 &&
              groups.(4) = 0 && groups.(5) = 0xffff
            in
            if is_ipv4_mapped then
              let octets = [|
                groups.(6) lsr 8; groups.(6) land 0xff;
                groups.(7) lsr 8; groups.(7) land 0xff;
              |] in
              (match check_ipv4_safety octets with
               | Error reason -> Error (Printf.sprintf "IPv4-mapped %s" reason)
               | Ok () -> Ok ())
            else
              Ok ()

(** Validate an IP address string for SSRF safety.
    Tries IPv4 first, then IPv6.
    Returns Ok () for safe IPs, Error reason for dangerous ones. *)
let validate_ip_string (ip : string) : (unit, string) result =
  match parse_ipv4 ip with
  | Some octets -> check_ipv4_safety octets
  | None ->
    match parse_ipv6_groups ip with
    | Some groups -> check_ipv6_safety groups
    | None -> Error (Printf.sprintf "unable to parse IP address: %s" ip)

(** Validate URL for SSRF protection.
    Checks scheme, extracts host, and validates IP literal hosts.
    Bracket hosts that fail IP parsing are rejected (e.g. zone identifiers).
    Hostname-based hosts pass here; they are validated after DNS resolution in the shell layer. *)
let validate_url_ssrf (url : string) : (unit, Domain.error) result =
  match validate_url url with
  | Error e -> Error e
  | Ok () ->
    match extract_host_kind url with
    | Error e -> Error e
    | Ok kind ->
      let host = match kind with Bracketed h -> h | Unbracketed h -> h in
      let host_lower = String.lowercase_ascii host in
      (* Block "localhost" hostname explicitly *)
      if host_lower = "localhost" then
        Error (Domain.Mirror_ssrf_blocked "localhost is not allowed")
      else
        match kind with
        | Bracketed _ ->
          (* Bracket host MUST parse as a valid IP.
             If it doesn't (e.g. zone identifier like fe80::1%25en0),
             reject it rather than treating as a hostname. *)
          (match validate_ip_string host with
           | Ok () -> Ok ()
           | Error reason ->
             Error (Domain.Mirror_ssrf_blocked
               (Printf.sprintf "IP literal [%s] blocked: %s" host reason)))
        | Unbracketed _ ->
          (* Check if host is a bare IPv4 literal *)
          (match parse_ipv4 host with
           | Some octets ->
             (match check_ipv4_safety octets with
              | Ok () -> Ok ()
              | Error reason ->
                Error (Domain.Mirror_ssrf_blocked
                  (Printf.sprintf "IP literal %s blocked: %s" host reason)))
           | None ->
             (* Hostname — DNS validation happens in shell layer *)
             Ok ())
