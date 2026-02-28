open Alcotest
open Blossom_core

(* Helper to check for specific error types *)
let is_mirror_invalid_url = function
  | Domain.Mirror_invalid_url _ -> true
  | _ -> false

(* parse_request tests *)
let test_parse_request_valid () =
  let json = {|{"url": "https://example.com/blob.pdf"}|} in
  match Mirror.parse_request json with
  | Ok req -> check string "url parsed" "https://example.com/blob.pdf" req.url
  | Error _ -> fail "expected Ok"

let test_parse_request_http_url () =
  let json = {|{"url": "http://example.com/blob.pdf"}|} in
  match Mirror.parse_request json with
  | Ok req -> check string "http url parsed" "http://example.com/blob.pdf" req.url
  | Error _ -> fail "expected Ok"

let test_parse_request_missing_url () =
  let json = {|{"other": "value"}|} in
  match Mirror.parse_request json with
  | Error e -> check bool "is invalid url error" true (is_mirror_invalid_url e)
  | Ok _ -> fail "expected Error"

let test_parse_request_url_not_string () =
  let json = {|{"url": 123}|} in
  match Mirror.parse_request json with
  | Error e -> check bool "is invalid url error" true (is_mirror_invalid_url e)
  | Ok _ -> fail "expected Error"

let test_parse_request_not_object () =
  let json = {|["url", "https://example.com"]|} in
  match Mirror.parse_request json with
  | Error e -> check bool "is invalid url error" true (is_mirror_invalid_url e)
  | Ok _ -> fail "expected Error"

let test_parse_request_invalid_json () =
  let json = {|{not valid json}|} in
  match Mirror.parse_request json with
  | Error e -> check bool "is invalid url error" true (is_mirror_invalid_url e)
  | Ok _ -> fail "expected Error"

let test_parse_request_empty_url () =
  let json = {|{"url": ""}|} in
  match Mirror.parse_request json with
  | Ok req -> check string "empty url parsed" "" req.url
  | Error _ -> fail "expected Ok (validation is separate)"

(* validate_url tests *)
let test_validate_url_https () =
  match Mirror.validate_url "https://example.com/blob.pdf" with
  | Ok () -> ()
  | Error _ -> fail "expected Ok"

let test_validate_url_http () =
  match Mirror.validate_url "http://example.com/blob.pdf" with
  | Ok () -> ()
  | Error _ -> fail "expected Ok"

let test_validate_url_ftp () =
  match Mirror.validate_url "ftp://example.com/blob.pdf" with
  | Error e -> check bool "is invalid url error" true (is_mirror_invalid_url e)
  | Ok () -> fail "expected Error"

let test_validate_url_no_scheme () =
  match Mirror.validate_url "example.com/blob.pdf" with
  | Error e -> check bool "is invalid url error" true (is_mirror_invalid_url e)
  | Ok () -> fail "expected Error"

let test_validate_url_too_short () =
  match Mirror.validate_url "http://" with
  | Error e -> check bool "is invalid url error" true (is_mirror_invalid_url e)
  | Ok () -> fail "expected Error"

let test_validate_url_empty () =
  match Mirror.validate_url "" with
  | Error e -> check bool "is invalid url error" true (is_mirror_invalid_url e)
  | Ok () -> fail "expected Error"

let test_validate_url_case_insensitive () =
  match Mirror.validate_url "HTTPS://example.com/blob.pdf" with
  | Ok () -> ()
  | Error _ -> fail "expected Ok (case insensitive)"

(* extension_from_url tests *)
let test_extension_from_url_pdf () =
  check (option string) "pdf extension" (Some "pdf")
    (Mirror.extension_from_url "https://example.com/file.pdf")

let test_extension_from_url_with_query () =
  check (option string) "extension with query" (Some "jpg")
    (Mirror.extension_from_url "https://example.com/image.jpg?size=large")

let test_extension_from_url_with_fragment () =
  check (option string) "extension with fragment" (Some "png")
    (Mirror.extension_from_url "https://example.com/image.png#section")

let test_extension_from_url_with_both () =
  check (option string) "extension with query and fragment" (Some "gif")
    (Mirror.extension_from_url "https://example.com/image.gif?v=1#top")

let test_extension_from_url_no_extension () =
  check (option string) "no extension" None
    (Mirror.extension_from_url "https://example.com/file")

let test_extension_from_url_hash_only () =
  check (option string) "hash-based url" None
    (Mirror.extension_from_url "https://example.com/abc123def456")

let test_extension_from_url_dot_in_path () =
  check (option string) "dot in path" (Some "pdf")
    (Mirror.extension_from_url "https://cdn.example.com/v1.0/file.pdf")

let test_extension_from_url_multiple_dots () =
  check (option string) "multiple dots" (Some "gz")
    (Mirror.extension_from_url "https://example.com/archive.tar.gz")

(* ===== SSRF Protection Tests ===== *)

let is_ssrf_blocked = function
  | Domain.Mirror_ssrf_blocked _ -> true
  | _ -> false

let is_ok = function Ok () -> true | Error _ -> false
let is_error_str = function Error _ -> true | Ok () -> false

(* extract_host tests *)
let test_extract_host_https () =
  match Mirror.extract_host "https://example.com/path" with
  | Ok host -> check string "host" "example.com" host
  | Error _ -> fail "expected Ok"

let test_extract_host_http () =
  match Mirror.extract_host "http://example.com/path" with
  | Ok host -> check string "host" "example.com" host
  | Error _ -> fail "expected Ok"

let test_extract_host_with_port () =
  match Mirror.extract_host "https://example.com:8080/path" with
  | Ok host -> check string "host" "example.com" host
  | Error _ -> fail "expected Ok"

let test_extract_host_ipv4 () =
  match Mirror.extract_host "http://192.168.1.1/path" with
  | Ok host -> check string "host" "192.168.1.1" host
  | Error _ -> fail "expected Ok"

let test_extract_host_ipv4_with_port () =
  match Mirror.extract_host "http://192.168.1.1:8080/path" with
  | Ok host -> check string "host" "192.168.1.1" host
  | Error _ -> fail "expected Ok"

let test_extract_host_ipv6_bracket () =
  match Mirror.extract_host "http://[::1]:8080/path" with
  | Ok host -> check string "host" "::1" host
  | Error _ -> fail "expected Ok"

let test_extract_host_ipv6_no_port () =
  match Mirror.extract_host "http://[2001:db8::1]/path" with
  | Ok host -> check string "host" "2001:db8::1" host
  | Error _ -> fail "expected Ok"

let test_extract_host_no_path () =
  match Mirror.extract_host "https://example.com" with
  | Ok host -> check string "host" "example.com" host
  | Error _ -> fail "expected Ok"

let test_extract_host_with_userinfo () =
  match Mirror.extract_host "https://user:pass@example.com/path" with
  | Ok host -> check string "host" "example.com" host
  | Error _ -> fail "expected Ok"

let test_extract_host_with_query () =
  match Mirror.extract_host "https://example.com?query=1" with
  | Ok host -> check string "host" "example.com" host
  | Error _ -> fail "expected Ok"

let test_extract_host_empty () =
  match Mirror.extract_host "http:///path" with
  | Error _ -> ()
  | Ok _ -> fail "expected Error for empty host"

let test_extract_host_bad_scheme () =
  match Mirror.extract_host "ftp://example.com" with
  | Error _ -> ()
  | Ok _ -> fail "expected Error for bad scheme"

(* parse_ipv4 tests *)
let test_parse_ipv4_valid () =
  match Mirror.parse_ipv4 "192.168.1.1" with
  | Some a ->
    check int "o0" 192 a.(0); check int "o1" 168 a.(1);
    check int "o2" 1 a.(2); check int "o3" 1 a.(3)
  | None -> fail "expected Some"

let test_parse_ipv4_loopback () =
  match Mirror.parse_ipv4 "127.0.0.1" with
  | Some a ->
    check int "o0" 127 a.(0); check int "o1" 0 a.(1);
    check int "o2" 0 a.(2); check int "o3" 1 a.(3)
  | None -> fail "expected Some"

let test_parse_ipv4_zeros () =
  match Mirror.parse_ipv4 "0.0.0.0" with
  | Some a ->
    check int "o0" 0 a.(0); check int "o1" 0 a.(1);
    check int "o2" 0 a.(2); check int "o3" 0 a.(3)
  | None -> fail "expected Some"

let test_parse_ipv4_too_few () =
  check bool "None" true (Option.is_none (Mirror.parse_ipv4 "192.168.1"))

let test_parse_ipv4_too_many () =
  check bool "None" true (Option.is_none (Mirror.parse_ipv4 "192.168.1.1.1"))

let test_parse_ipv4_out_of_range () =
  check bool "None" true (Option.is_none (Mirror.parse_ipv4 "256.1.1.1"))

let test_parse_ipv4_negative () =
  check bool "None" true (Option.is_none (Mirror.parse_ipv4 "-1.1.1.1"))

let test_parse_ipv4_not_number () =
  check bool "None" true (Option.is_none (Mirror.parse_ipv4 "abc.1.1.1"))

let test_parse_ipv4_empty () =
  check bool "None" true (Option.is_none (Mirror.parse_ipv4 ""))

(* check_ipv4_safety tests *)
let test_ipv4_safe_public () =
  check bool "safe" true (is_ok (Mirror.check_ipv4_safety [|8;8;8;8|]))

let test_ipv4_safe_public_2 () =
  check bool "safe" true (is_ok (Mirror.check_ipv4_safety [|1;1;1;1|]))

let test_ipv4_block_loopback_1 () =
  check bool "blocked" true (is_error_str (Mirror.check_ipv4_safety [|127;0;0;1|]))

let test_ipv4_block_loopback_range () =
  check bool "blocked" true (is_error_str (Mirror.check_ipv4_safety [|127;255;255;255|]))

let test_ipv4_block_unspecified () =
  check bool "blocked" true (is_error_str (Mirror.check_ipv4_safety [|0;0;0;0|]))

let test_ipv4_block_reserved_0 () =
  check bool "blocked" true (is_error_str (Mirror.check_ipv4_safety [|0;1;2;3|]))

let test_ipv4_block_private_10 () =
  check bool "blocked" true (is_error_str (Mirror.check_ipv4_safety [|10;0;0;1|]))

let test_ipv4_block_private_10_high () =
  check bool "blocked" true (is_error_str (Mirror.check_ipv4_safety [|10;255;255;255|]))

let test_ipv4_block_private_172_16 () =
  check bool "blocked" true (is_error_str (Mirror.check_ipv4_safety [|172;16;0;1|]))

let test_ipv4_block_private_172_31 () =
  check bool "blocked" true (is_error_str (Mirror.check_ipv4_safety [|172;31;255;255|]))

let test_ipv4_safe_172_15 () =
  check bool "safe" true (is_ok (Mirror.check_ipv4_safety [|172;15;0;1|]))

let test_ipv4_safe_172_32 () =
  check bool "safe" true (is_ok (Mirror.check_ipv4_safety [|172;32;0;1|]))

let test_ipv4_block_private_192_168 () =
  check bool "blocked" true (is_error_str (Mirror.check_ipv4_safety [|192;168;0;1|]))

let test_ipv4_block_link_local () =
  check bool "blocked" true (is_error_str (Mirror.check_ipv4_safety [|169;254;1;1|]))

let test_ipv4_block_cloud_metadata () =
  check bool "blocked" true (is_error_str (Mirror.check_ipv4_safety [|169;254;169;254|]))

(* parse_ipv6_groups tests *)
let test_parse_ipv6_loopback () =
  match Mirror.parse_ipv6_groups "::1" with
  | Some g ->
    check int "len" 8 (Array.length g);
    check int "g7" 1 g.(7);
    check int "g0" 0 g.(0)
  | None -> fail "expected Some"

let test_parse_ipv6_unspecified () =
  match Mirror.parse_ipv6_groups "::" with
  | Some g ->
    check int "len" 8 (Array.length g);
    Array.iteri (fun i v -> check int (Printf.sprintf "g%d" i) 0 v) g
  | None -> fail "expected Some"

let test_parse_ipv6_full () =
  match Mirror.parse_ipv6_groups "2001:db8:0:0:0:0:0:1" with
  | Some g ->
    check int "g0" 0x2001 g.(0);
    check int "g1" 0xdb8 g.(1);
    check int "g7" 1 g.(7)
  | None -> fail "expected Some"

let test_parse_ipv6_compressed () =
  match Mirror.parse_ipv6_groups "2001:db8::1" with
  | Some g ->
    check int "g0" 0x2001 g.(0);
    check int "g1" 0xdb8 g.(1);
    check int "g7" 1 g.(7);
    (* middle groups should be 0 *)
    check int "g2" 0 g.(2);
    check int "g6" 0 g.(6)
  | None -> fail "expected Some"

let test_parse_ipv6_link_local () =
  match Mirror.parse_ipv6_groups "fe80::1" with
  | Some g ->
    check int "g0" 0xfe80 g.(0);
    check int "g7" 1 g.(7)
  | None -> fail "expected Some"

let test_parse_ipv6_ipv4_mapped () =
  match Mirror.parse_ipv6_groups "::ffff:192.168.1.1" with
  | Some g ->
    check int "len" 8 (Array.length g);
    check int "g5" 0xffff g.(5);
    check int "g6" (192 * 256 + 168) g.(6);
    check int "g7" (1 * 256 + 1) g.(7)
  | None -> fail "expected Some"

let test_parse_ipv6_invalid_too_many () =
  check bool "None" true
    (Option.is_none (Mirror.parse_ipv6_groups "1:2:3:4:5:6:7:8:9"))

let test_parse_ipv6_invalid_empty () =
  check bool "None" true (Option.is_none (Mirror.parse_ipv6_groups ""))

let test_parse_ipv6_invalid_double_double_colon () =
  check bool "None" true
    (Option.is_none (Mirror.parse_ipv6_groups "1::2::3"))

(* check_ipv6_safety tests *)
let test_ipv6_safe_public () =
  check bool "safe" true
    (is_ok (Mirror.check_ipv6_safety [|0x2001;0xdb8;0;0;0;0;0;1|]))

let test_ipv6_block_loopback () =
  check bool "blocked" true
    (is_error_str (Mirror.check_ipv6_safety [|0;0;0;0;0;0;0;1|]))

let test_ipv6_block_unspecified () =
  check bool "blocked" true
    (is_error_str (Mirror.check_ipv6_safety [|0;0;0;0;0;0;0;0|]))

let test_ipv6_block_link_local () =
  check bool "blocked" true
    (is_error_str (Mirror.check_ipv6_safety [|0xfe80;0;0;0;0;0;0;1|]))

let test_ipv6_block_link_local_febf () =
  check bool "blocked" true
    (is_error_str (Mirror.check_ipv6_safety [|0xfebf;0;0;0;0;0;0;1|]))

let test_ipv6_block_unique_local_fc () =
  check bool "blocked" true
    (is_error_str (Mirror.check_ipv6_safety [|0xfc00;0;0;0;0;0;0;1|]))

let test_ipv6_block_unique_local_fd () =
  check bool "blocked" true
    (is_error_str (Mirror.check_ipv6_safety [|0xfd00;0;0;0;0;0;0;1|]))

let test_ipv6_block_ipv4_mapped_private () =
  (* ::ffff:192.168.1.1 *)
  let g6 = 192 * 256 + 168 and g7 = 1 * 256 + 1 in
  check bool "blocked" true
    (is_error_str (Mirror.check_ipv6_safety [|0;0;0;0;0;0xffff;g6;g7|]))

let test_ipv6_block_ipv4_mapped_loopback () =
  (* ::ffff:127.0.0.1 *)
  let g6 = 127 * 256 + 0 and g7 = 0 * 256 + 1 in
  check bool "blocked" true
    (is_error_str (Mirror.check_ipv6_safety [|0;0;0;0;0;0xffff;g6;g7|]))

let test_ipv6_safe_ipv4_mapped_public () =
  (* ::ffff:8.8.8.8 *)
  let g6 = 8 * 256 + 8 and g7 = 8 * 256 + 8 in
  check bool "safe" true
    (is_ok (Mirror.check_ipv6_safety [|0;0;0;0;0;0xffff;g6;g7|]))

let test_ipv6_block_ipv4_mapped_metadata () =
  (* ::ffff:169.254.169.254 *)
  let g6 = 169 * 256 + 254 and g7 = 169 * 256 + 254 in
  check bool "blocked" true
    (is_error_str (Mirror.check_ipv6_safety [|0;0;0;0;0;0xffff;g6;g7|]))

(* validate_ip_string tests *)
let test_validate_ip_safe_ipv4 () =
  check bool "safe" true (is_ok (Mirror.validate_ip_string "8.8.8.8"))

let test_validate_ip_block_ipv4_loopback () =
  check bool "blocked" true (is_error_str (Mirror.validate_ip_string "127.0.0.1"))

let test_validate_ip_block_ipv4_private () =
  check bool "blocked" true (is_error_str (Mirror.validate_ip_string "10.0.0.1"))

let test_validate_ip_safe_ipv6 () =
  check bool "safe" true (is_ok (Mirror.validate_ip_string "2001:db8::1"))

let test_validate_ip_block_ipv6_loopback () =
  check bool "blocked" true (is_error_str (Mirror.validate_ip_string "::1"))

let test_validate_ip_block_ipv6_unspecified () =
  check bool "blocked" true (is_error_str (Mirror.validate_ip_string "::"))

let test_validate_ip_invalid_string () =
  check bool "error" true (is_error_str (Mirror.validate_ip_string "not-an-ip"))

(* validate_url_ssrf tests *)
let test_ssrf_public_url () =
  match Mirror.validate_url_ssrf "https://example.com/file.pdf" with
  | Ok () -> ()
  | Error _ -> fail "expected Ok"

let test_ssrf_public_ip () =
  match Mirror.validate_url_ssrf "http://8.8.8.8/file" with
  | Ok () -> ()
  | Error _ -> fail "expected Ok"

let test_ssrf_block_loopback_ip () =
  match Mirror.validate_url_ssrf "http://127.0.0.1/file" with
  | Error e -> check bool "ssrf blocked" true (is_ssrf_blocked e)
  | Ok () -> fail "expected Error"

let test_ssrf_block_private_10 () =
  match Mirror.validate_url_ssrf "http://10.0.0.1/file" with
  | Error e -> check bool "ssrf blocked" true (is_ssrf_blocked e)
  | Ok () -> fail "expected Error"

let test_ssrf_block_private_192 () =
  match Mirror.validate_url_ssrf "http://192.168.1.1/file" with
  | Error e -> check bool "ssrf blocked" true (is_ssrf_blocked e)
  | Ok () -> fail "expected Error"

let test_ssrf_block_private_172 () =
  match Mirror.validate_url_ssrf "http://172.16.0.1/file" with
  | Error e -> check bool "ssrf blocked" true (is_ssrf_blocked e)
  | Ok () -> fail "expected Error"

let test_ssrf_block_cloud_metadata () =
  match Mirror.validate_url_ssrf "http://169.254.169.254/latest/meta-data/" with
  | Error e -> check bool "ssrf blocked" true (is_ssrf_blocked e)
  | Ok () -> fail "expected Error"

let test_ssrf_block_link_local () =
  match Mirror.validate_url_ssrf "http://169.254.1.1/file" with
  | Error e -> check bool "ssrf blocked" true (is_ssrf_blocked e)
  | Ok () -> fail "expected Error"

let test_ssrf_block_ipv6_loopback () =
  match Mirror.validate_url_ssrf "http://[::1]/file" with
  | Error e -> check bool "ssrf blocked" true (is_ssrf_blocked e)
  | Ok () -> fail "expected Error"

let test_ssrf_block_ipv6_link_local () =
  match Mirror.validate_url_ssrf "http://[fe80::1]/file" with
  | Error e -> check bool "ssrf blocked" true (is_ssrf_blocked e)
  | Ok () -> fail "expected Error"

let test_ssrf_block_ipv4_mapped_loopback () =
  match Mirror.validate_url_ssrf "http://[::ffff:127.0.0.1]/file" with
  | Error e -> check bool "ssrf blocked" true (is_ssrf_blocked e)
  | Ok () -> fail "expected Error"

let test_ssrf_block_localhost () =
  match Mirror.validate_url_ssrf "http://localhost/file" with
  | Error e -> check bool "ssrf blocked" true (is_ssrf_blocked e)
  | Ok () -> fail "expected Error"

let test_ssrf_block_localhost_upper () =
  match Mirror.validate_url_ssrf "http://LOCALHOST/file" with
  | Error e -> check bool "ssrf blocked" true (is_ssrf_blocked e)
  | Ok () -> fail "expected Error"

let test_ssrf_block_unspecified () =
  match Mirror.validate_url_ssrf "http://0.0.0.0/file" with
  | Error e -> check bool "ssrf blocked" true (is_ssrf_blocked e)
  | Ok () -> fail "expected Error"

let test_ssrf_block_ftp_scheme () =
  match Mirror.validate_url_ssrf "ftp://example.com/file" with
  | Error e -> check bool "invalid url" true (is_mirror_invalid_url e)
  | Ok () -> fail "expected Error"

let test_ssrf_hostname_passes_pure_check () =
  (* Non-localhost hostnames pass the pure check; DNS validation is in shell *)
  match Mirror.validate_url_ssrf "http://internal.corp/file" with
  | Ok () -> ()
  | Error _ -> fail "expected Ok (DNS check is in shell layer)"

(* Zone identifier IPv6 bypass tests *)
let test_ssrf_block_ipv6_zone_id () =
  (* fe80::1%25en0 is link-local with zone identifier — must be blocked *)
  match Mirror.validate_url_ssrf "http://[fe80::1%25en0]/file" with
  | Error e -> check bool "ssrf blocked" true (is_ssrf_blocked e)
  | Ok () -> fail "expected Error: zone identifier IPv6 should be rejected"

let test_ssrf_block_ipv6_zone_id_loopback () =
  (* ::1%25lo0 — loopback with zone identifier *)
  match Mirror.validate_url_ssrf "http://[::1%25lo0]/file" with
  | Error e -> check bool "ssrf blocked" true (is_ssrf_blocked e)
  | Ok () -> fail "expected Error: zone identifier IPv6 should be rejected"

let test_ssrf_block_bracket_garbage () =
  (* Bracket host that isn't a valid IP at all *)
  match Mirror.validate_url_ssrf "http://[not-an-ip]/file" with
  | Error e -> check bool "ssrf blocked" true (is_ssrf_blocked e)
  | Ok () -> fail "expected Error: unparseable bracket host should be rejected"

let test_ssrf_block_bracket_hostname () =
  (* Bracket host that looks like a hostname — still reject *)
  match Mirror.validate_url_ssrf "http://[evil.com]/file" with
  | Error e -> check bool "ssrf blocked" true (is_ssrf_blocked e)
  | Ok () -> fail "expected Error: bracket hostname should be rejected"

let tests = [
  (* parse_request *)
  test_case "parse_request valid" `Quick test_parse_request_valid;
  test_case "parse_request http url" `Quick test_parse_request_http_url;
  test_case "parse_request missing url" `Quick test_parse_request_missing_url;
  test_case "parse_request url not string" `Quick test_parse_request_url_not_string;
  test_case "parse_request not object" `Quick test_parse_request_not_object;
  test_case "parse_request invalid json" `Quick test_parse_request_invalid_json;
  test_case "parse_request empty url" `Quick test_parse_request_empty_url;
  (* validate_url *)
  test_case "validate_url https" `Quick test_validate_url_https;
  test_case "validate_url http" `Quick test_validate_url_http;
  test_case "validate_url ftp" `Quick test_validate_url_ftp;
  test_case "validate_url no scheme" `Quick test_validate_url_no_scheme;
  test_case "validate_url too short" `Quick test_validate_url_too_short;
  test_case "validate_url empty" `Quick test_validate_url_empty;
  test_case "validate_url case insensitive" `Quick test_validate_url_case_insensitive;
  (* extension_from_url *)
  test_case "extension_from_url pdf" `Quick test_extension_from_url_pdf;
  test_case "extension_from_url with query" `Quick test_extension_from_url_with_query;
  test_case "extension_from_url with fragment" `Quick test_extension_from_url_with_fragment;
  test_case "extension_from_url with both" `Quick test_extension_from_url_with_both;
  test_case "extension_from_url no extension" `Quick test_extension_from_url_no_extension;
  test_case "extension_from_url hash only" `Quick test_extension_from_url_hash_only;
  test_case "extension_from_url dot in path" `Quick test_extension_from_url_dot_in_path;
  test_case "extension_from_url multiple dots" `Quick test_extension_from_url_multiple_dots;
  (* extract_host *)
  test_case "extract_host https" `Quick test_extract_host_https;
  test_case "extract_host http" `Quick test_extract_host_http;
  test_case "extract_host with port" `Quick test_extract_host_with_port;
  test_case "extract_host ipv4" `Quick test_extract_host_ipv4;
  test_case "extract_host ipv4 with port" `Quick test_extract_host_ipv4_with_port;
  test_case "extract_host ipv6 bracket" `Quick test_extract_host_ipv6_bracket;
  test_case "extract_host ipv6 no port" `Quick test_extract_host_ipv6_no_port;
  test_case "extract_host no path" `Quick test_extract_host_no_path;
  test_case "extract_host with userinfo" `Quick test_extract_host_with_userinfo;
  test_case "extract_host with query" `Quick test_extract_host_with_query;
  test_case "extract_host empty" `Quick test_extract_host_empty;
  test_case "extract_host bad scheme" `Quick test_extract_host_bad_scheme;
  (* parse_ipv4 *)
  test_case "parse_ipv4 valid" `Quick test_parse_ipv4_valid;
  test_case "parse_ipv4 loopback" `Quick test_parse_ipv4_loopback;
  test_case "parse_ipv4 zeros" `Quick test_parse_ipv4_zeros;
  test_case "parse_ipv4 too few" `Quick test_parse_ipv4_too_few;
  test_case "parse_ipv4 too many" `Quick test_parse_ipv4_too_many;
  test_case "parse_ipv4 out of range" `Quick test_parse_ipv4_out_of_range;
  test_case "parse_ipv4 negative" `Quick test_parse_ipv4_negative;
  test_case "parse_ipv4 not number" `Quick test_parse_ipv4_not_number;
  test_case "parse_ipv4 empty" `Quick test_parse_ipv4_empty;
  (* check_ipv4_safety *)
  test_case "ipv4 safe public" `Quick test_ipv4_safe_public;
  test_case "ipv4 safe public 2" `Quick test_ipv4_safe_public_2;
  test_case "ipv4 block loopback" `Quick test_ipv4_block_loopback_1;
  test_case "ipv4 block loopback range" `Quick test_ipv4_block_loopback_range;
  test_case "ipv4 block unspecified" `Quick test_ipv4_block_unspecified;
  test_case "ipv4 block reserved 0/8" `Quick test_ipv4_block_reserved_0;
  test_case "ipv4 block private 10" `Quick test_ipv4_block_private_10;
  test_case "ipv4 block private 10 high" `Quick test_ipv4_block_private_10_high;
  test_case "ipv4 block private 172.16" `Quick test_ipv4_block_private_172_16;
  test_case "ipv4 block private 172.31" `Quick test_ipv4_block_private_172_31;
  test_case "ipv4 safe 172.15" `Quick test_ipv4_safe_172_15;
  test_case "ipv4 safe 172.32" `Quick test_ipv4_safe_172_32;
  test_case "ipv4 block private 192.168" `Quick test_ipv4_block_private_192_168;
  test_case "ipv4 block link-local" `Quick test_ipv4_block_link_local;
  test_case "ipv4 block cloud metadata" `Quick test_ipv4_block_cloud_metadata;
  (* parse_ipv6_groups *)
  test_case "parse_ipv6 loopback" `Quick test_parse_ipv6_loopback;
  test_case "parse_ipv6 unspecified" `Quick test_parse_ipv6_unspecified;
  test_case "parse_ipv6 full" `Quick test_parse_ipv6_full;
  test_case "parse_ipv6 compressed" `Quick test_parse_ipv6_compressed;
  test_case "parse_ipv6 link-local" `Quick test_parse_ipv6_link_local;
  test_case "parse_ipv6 ipv4-mapped" `Quick test_parse_ipv6_ipv4_mapped;
  test_case "parse_ipv6 invalid too many" `Quick test_parse_ipv6_invalid_too_many;
  test_case "parse_ipv6 invalid empty" `Quick test_parse_ipv6_invalid_empty;
  test_case "parse_ipv6 invalid double ::" `Quick test_parse_ipv6_invalid_double_double_colon;
  (* check_ipv6_safety *)
  test_case "ipv6 safe public" `Quick test_ipv6_safe_public;
  test_case "ipv6 block loopback" `Quick test_ipv6_block_loopback;
  test_case "ipv6 block unspecified" `Quick test_ipv6_block_unspecified;
  test_case "ipv6 block link-local" `Quick test_ipv6_block_link_local;
  test_case "ipv6 block link-local febf" `Quick test_ipv6_block_link_local_febf;
  test_case "ipv6 block unique-local fc" `Quick test_ipv6_block_unique_local_fc;
  test_case "ipv6 block unique-local fd" `Quick test_ipv6_block_unique_local_fd;
  test_case "ipv6 block ipv4-mapped private" `Quick test_ipv6_block_ipv4_mapped_private;
  test_case "ipv6 block ipv4-mapped loopback" `Quick test_ipv6_block_ipv4_mapped_loopback;
  test_case "ipv6 safe ipv4-mapped public" `Quick test_ipv6_safe_ipv4_mapped_public;
  test_case "ipv6 block ipv4-mapped metadata" `Quick test_ipv6_block_ipv4_mapped_metadata;
  (* validate_ip_string *)
  test_case "validate_ip safe ipv4" `Quick test_validate_ip_safe_ipv4;
  test_case "validate_ip block ipv4 loopback" `Quick test_validate_ip_block_ipv4_loopback;
  test_case "validate_ip block ipv4 private" `Quick test_validate_ip_block_ipv4_private;
  test_case "validate_ip safe ipv6" `Quick test_validate_ip_safe_ipv6;
  test_case "validate_ip block ipv6 loopback" `Quick test_validate_ip_block_ipv6_loopback;
  test_case "validate_ip block ipv6 unspecified" `Quick test_validate_ip_block_ipv6_unspecified;
  test_case "validate_ip invalid string" `Quick test_validate_ip_invalid_string;
  (* validate_url_ssrf *)
  test_case "ssrf public url" `Quick test_ssrf_public_url;
  test_case "ssrf public ip" `Quick test_ssrf_public_ip;
  test_case "ssrf block loopback ip" `Quick test_ssrf_block_loopback_ip;
  test_case "ssrf block private 10" `Quick test_ssrf_block_private_10;
  test_case "ssrf block private 192" `Quick test_ssrf_block_private_192;
  test_case "ssrf block private 172" `Quick test_ssrf_block_private_172;
  test_case "ssrf block cloud metadata" `Quick test_ssrf_block_cloud_metadata;
  test_case "ssrf block link-local" `Quick test_ssrf_block_link_local;
  test_case "ssrf block ipv6 loopback" `Quick test_ssrf_block_ipv6_loopback;
  test_case "ssrf block ipv6 link-local" `Quick test_ssrf_block_ipv6_link_local;
  test_case "ssrf block ipv4-mapped loopback" `Quick test_ssrf_block_ipv4_mapped_loopback;
  test_case "ssrf block localhost" `Quick test_ssrf_block_localhost;
  test_case "ssrf block LOCALHOST" `Quick test_ssrf_block_localhost_upper;
  test_case "ssrf block unspecified" `Quick test_ssrf_block_unspecified;
  test_case "ssrf block ftp scheme" `Quick test_ssrf_block_ftp_scheme;
  test_case "ssrf hostname passes pure" `Quick test_ssrf_hostname_passes_pure_check;
  (* Zone identifier / bracket bypass *)
  test_case "ssrf block ipv6 zone id" `Quick test_ssrf_block_ipv6_zone_id;
  test_case "ssrf block ipv6 zone id loopback" `Quick test_ssrf_block_ipv6_zone_id_loopback;
  test_case "ssrf block bracket garbage" `Quick test_ssrf_block_bracket_garbage;
  test_case "ssrf block bracket hostname" `Quick test_ssrf_block_bracket_hostname;
]
