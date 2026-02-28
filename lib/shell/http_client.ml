(** HTTP client for fetching remote blobs (BUD-04 mirror) with SSRF protection.

    DNS rebinding defense: wraps Eio.Net with an SSRF-safe layer that validates
    all resolved IP addresses BEFORE Piaf connects. Since Piaf uses the same
    wrapped network for both DNS resolution and connection, the validated IPs
    are exactly what gets connected to — no TOCTOU gap.

    Eio-friendly: DNS resolution goes through Eio.Net (non-blocking),
    not raw Unix.getaddrinfo. *)

open Blossom_core

type fetch_result = {
  body: Piaf.Body.t;
  content_type: string option;
  content_length: int option;
}

(** {1 SSRF-safe Eio.Net wrapper} *)

(** Internal state for the SSRF-safe network resource *)
type ssrf_net_state = {
  inner : [`Unix | `Generic] Eio.Net.ty Eio.Resource.t;
  validate : string -> (unit, string) result;
}

(** Raised inside getaddrinfo when a resolved IP is blocked *)
exception Ssrf_blocked of string

(** Eio.Net.Pi.NETWORK implementation that intercepts DNS resolution.
    All resolved IPs are validated; if any is dangerous the entire
    resolution is rejected (requirement: 1つでも危険なら拒否). *)
module Ssrf_network
  : Eio.Net.Pi.NETWORK
    with type t = ssrf_net_state
     and type tag = [`Unix | `Generic]
= struct
  type t = ssrf_net_state
  type tag = [`Unix | `Generic]

  let listen t ~reuse_addr ~reuse_port ~backlog ~sw addr =
    (Eio.Net.listen ~reuse_addr ~reuse_port ~backlog ~sw t.inner addr
     :> tag Eio.Net.listening_socket_ty Eio.Resource.t)

  let connect t ~sw addr =
    (Eio.Net.connect ~sw t.inner addr
     :> tag Eio.Net.stream_socket_ty Eio.Resource.t)

  let datagram_socket t ~reuse_addr ~reuse_port ~sw proto =
    (Eio.Net.datagram_socket ~reuse_addr ~reuse_port ~sw t.inner proto
     :> tag Eio.Net.datagram_socket_ty Eio.Resource.t)

  let getaddrinfo t ~service hostname =
    let addrs = Eio.Net.getaddrinfo ~service t.inner hostname in
    (* Validate ALL resolved addresses — reject if any is dangerous *)
    List.iter (fun addr ->
      match addr with
      | `Tcp (ip, _) ->
        let ip_str = Format.asprintf "%a" Eio.Net.Ipaddr.pp ip in
        (match t.validate ip_str with
         | Error reason ->
           Eio.traceln "SSRF blocked: %s resolved to %s (%s)" hostname ip_str reason;
           raise (Ssrf_blocked
             (Printf.sprintf "DNS resolved %s to blocked address %s: %s"
                hostname ip_str reason))
         | Ok () -> ())
      | _ -> ()
    ) addrs;
    addrs

  let getnameinfo t addr =
    Eio.Net.getnameinfo t.inner addr
end

(** Create an SSRF-safe network resource wrapping the real one *)
let make_ssrf_net
    (inner : [`Unix | `Generic] Eio.Net.ty Eio.Resource.t)
    (validate : string -> (unit, string) result)
  : [`Unix | `Generic] Eio.Net.ty Eio.Resource.t =
  let state = { inner; validate } in
  let handler = Eio.Net.Pi.network (module Ssrf_network) in
  (Eio.Resource.T (state, handler)
   :> [`Unix | `Generic] Eio.Net.ty Eio.Resource.t)

(** Create a copy of the Eio environment with SSRF-safe DNS resolution *)
let make_ssrf_env
    (env : Eio_unix.Stdenv.base)
    (validate : string -> (unit, string) result)
  : Eio_unix.Stdenv.base =
  let safe_net = make_ssrf_net env#net validate in
  object
    method net = safe_net
    method clock = env#clock
    method mono_clock = env#mono_clock
    method domain_mgr = env#domain_mgr
    method fs = env#fs
    method cwd = env#cwd
    method process_mgr = env#process_mgr
    method secure_random = env#secure_random
    method stdin = env#stdin
    method stdout = env#stdout
    method stderr = env#stderr
    method debug = env#debug
    method backend_id = env#backend_id
  end

(** {1 Public API} *)

(** Fetch a blob from a remote URL with SSRF protection.

    Defense layers:
    1. [Mirror.validate_url_ssrf] — pure check: scheme, localhost, IP literals
    2. [Ssrf_network.getaddrinfo] — DNS resolution validates all IPs before
       Piaf connects (same resolution path = no DNS rebinding)
    3. Redirect rejection — 3xx responses are blocked *)
let fetch ~sw ~env (url : string) : (fetch_result, Domain.error) result =
  (* 1. Pure URL validation: scheme + IP literal + localhost *)
  match Mirror.validate_url_ssrf url with
  | Error e -> Error e
  | Ok () ->
    let uri = Uri.of_string url in
    (* Shared ref: records SSRF block even if Piaf swallows the exception.
       The getaddrinfo hook writes here BEFORE raising Ssrf_blocked, so we
       can detect the block regardless of whether Piaf converts the
       exception into Error err. *)
    let ssrf_reason = ref None in
    let validate ip =
      match Mirror.validate_ip_string ip with
      | Ok () -> Ok ()
      | Error reason ->
        ssrf_reason := Some reason;
        Error reason
    in
    (* 2. Build SSRF-safe env — Piaf's DNS goes through our validator *)
    let safe_env = make_ssrf_env env validate in
    let config = { Piaf.Config.default with follow_redirects = false } in
    (* 3. Make request through the safe env *)
    let result =
      try
        match Piaf.Client.Oneshot.get ~config ~headers:[] ~sw safe_env uri with
        | Error err ->
          let msg = Piaf.Error.to_string err in
          Error (Domain.Mirror_fetch_error
            (Printf.sprintf "Failed to fetch %s: %s" url msg))
        | Ok response ->
          let status = Piaf.Response.status response in
          let code = Piaf.Status.to_code status in
          (* 4. Reject redirects — in case follow_redirects leaked one *)
          if code >= 300 && code < 400 then
            Error (Domain.Mirror_ssrf_blocked
              (Printf.sprintf "Redirect response (%d) from %s is not allowed"
                 code url))
          else if Piaf.Status.is_successful status then
            let headers = Piaf.Response.headers response in
            let content_type = Piaf.Headers.get headers "content-type" in
            let content_length =
              match Piaf.Headers.get headers "content-length" with
              | Some s -> int_of_string_opt s
              | None -> None
            in
            Ok { body = response.body; content_type; content_length }
          else
            let msg = Printf.sprintf "Remote server returned %d for %s" code url in
            Error (Domain.Mirror_fetch_error msg)
      with
      | Ssrf_blocked msg ->
        Error (Domain.Mirror_ssrf_blocked msg)
    in
    (* 5. If the ref was set, the request hit an SSRF-blocked IP.
       Promote to Mirror_ssrf_blocked even if Piaf masked the exception. *)
    match result, !ssrf_reason with
    | Error (Domain.Mirror_fetch_error _), Some reason ->
      Error (Domain.Mirror_ssrf_blocked
        (Printf.sprintf "DNS resolved to blocked address: %s" reason))
    | _ -> result
