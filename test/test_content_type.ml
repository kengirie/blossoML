open Alcotest
open Blossom_core

(* Helper to check parse success and media_type *)
let check_media_type input expected =
  match Content_type.parse input with
  | Ok ct -> check string "media_type" expected (Content_type.media_type ct)
  | Error e -> fail (Content_type.error_to_string e)

(* Helper to check parse success and parameters *)
let check_parameters input expected_params =
  match Content_type.parse input with
  | Ok ct ->
      let params = Content_type.parameters ct in
      check int "param count" (List.length expected_params) (List.length params);
      List.iter2 (fun (ek, ev) (ak, av) ->
        check string "param key" ek ak;
        check string "param value" ev av
      ) expected_params params
  | Error e -> fail (Content_type.error_to_string e)

(* Helper to check normalize_media_type *)
let check_normalize input expected =
  match Content_type.normalize_media_type input with
  | Ok mt -> check string "normalized" expected mt
  | Error e -> fail (Content_type.error_to_string e)

(* Helper to check parse failure *)
let check_parse_error input =
  check bool "should fail" true (Result.is_error (Content_type.parse input))

(* === Basic parsing tests === *)

let test_parse_simple () =
  check_media_type "image/png" "image/png"

let test_parse_application () =
  check_media_type "application/octet-stream" "application/octet-stream"

let test_parse_text_plain () =
  check_media_type "text/plain" "text/plain"

(* === Case normalization tests === *)

let test_parse_uppercase () =
  check_media_type "IMAGE/PNG" "image/png"

let test_parse_mixed_case () =
  check_media_type "Text/HTML" "text/html"

let test_parse_uppercase_application () =
  check_media_type "APPLICATION/JSON" "application/json"

(* === Whitespace handling tests === *)

let test_parse_with_leading_space () =
  check_media_type " image/png" "image/png"

let test_parse_with_trailing_space () =
  check_media_type "image/png " "image/png"

let test_parse_with_spaces_around_slash () =
  check_media_type "image / png" "image/png"

let test_parse_with_tabs () =
  check_media_type "\timage/png\t" "image/png"

(* === Parameter parsing tests === *)

let test_parse_with_charset () =
  check_media_type "text/html; charset=UTF-8" "text/html";
  check_parameters "text/html; charset=UTF-8" [("charset", "UTF-8")]

let test_parse_with_charset_lowercase () =
  check_parameters "text/html; CHARSET=UTF-8" [("charset", "UTF-8")]

let test_parse_with_multiple_params () =
  check_parameters "multipart/form-data; boundary=----WebKit; charset=utf-8"
    [("boundary", "----WebKit"); ("charset", "utf-8")]

let test_parse_with_quoted_param () =
  check_parameters "multipart/form-data; boundary=\"----boundary string\""
    [("boundary", "----boundary string")]

let test_parse_param_with_escaped_quote () =
  check_parameters "text/plain; filename=\"file\\\"name.txt\""
    [("filename", "file\"name.txt")]

let test_parse_param_spaces_around_equals () =
  check_parameters "text/html; charset = UTF-8" [("charset", "UTF-8")]

(* === normalize_media_type tests === *)

let test_normalize_simple () =
  check_normalize "image/png" "image/png"

let test_normalize_uppercase () =
  check_normalize "IMAGE/PNG" "image/png"

let test_normalize_with_params () =
  check_normalize "image/png; version=1" "image/png"

let test_normalize_with_charset () =
  check_normalize "text/html; charset=UTF-8" "text/html"

let test_normalize_complex () =
  check_normalize "TEXT/HTML; charset=UTF-8; boundary=something" "text/html"

(* === Error handling tests === *)

let test_parse_empty () =
  check_parse_error ""

let test_parse_invalid_no_subtype () =
  check_parse_error "image"

let test_parse_invalid_no_type () =
  check_parse_error "/png"

let test_parse_invalid_double_slash () =
  check_parse_error "image//png"

let test_parse_invalid_special_chars () =
  check_parse_error "text/pl@in"

let test_parse_invalid_control_chars () =
  check_parse_error "text/pl\x00ain"

(* === is_valid tests === *)

let test_is_valid_true () =
  check bool "valid" true (Content_type.is_valid "image/png")

let test_is_valid_with_params () =
  check bool "valid with params" true (Content_type.is_valid "text/html; charset=UTF-8")

let test_is_valid_false_empty () =
  check bool "invalid empty" false (Content_type.is_valid "")

let test_is_valid_false_malformed () =
  check bool "invalid malformed" false (Content_type.is_valid "not-a-content-type")

(* === to_string tests === *)

let test_to_string_simple () =
  match Content_type.parse "image/png" with
  | Ok ct -> check string "to_string" "image/png" (Content_type.to_string ct)
  | Error _ -> fail "parse failed"

let test_to_string_with_params () =
  match Content_type.parse "text/html; charset=UTF-8" with
  | Ok ct -> check string "to_string" "text/html; charset=UTF-8" (Content_type.to_string ct)
  | Error _ -> fail "parse failed"

(* === Policy integration scenario tests === *)

let test_policy_scenario_parameterized_allowed () =
  (* Scenario: policy allows "image/png", input is "image/png; version=1" *)
  let allowed = "image/png" in
  let input = "image/png; version=1" in
  match Content_type.normalize_media_type input with
  | Ok normalized -> check bool "should be allowed" true (normalized = allowed)
  | Error _ -> fail "parse failed"

let test_policy_scenario_case_insensitive () =
  (* Scenario: policy allows "image/png", input is "IMAGE/PNG" *)
  let allowed = "image/png" in
  let input = "IMAGE/PNG" in
  match Content_type.normalize_media_type input with
  | Ok normalized -> check bool "should be allowed" true (normalized = allowed)
  | Error _ -> fail "parse failed"

let test_policy_scenario_not_allowed () =
  (* Scenario: policy allows "image/png", input is "image/jpeg" *)
  let allowed = "image/png" in
  let input = "image/jpeg" in
  match Content_type.normalize_media_type input with
  | Ok normalized -> check bool "should not be allowed" false (normalized = allowed)
  | Error _ -> fail "parse failed"

(* === Test suite === *)

let tests = [
  (* Basic parsing *)
  test_case "parse simple image/png" `Quick test_parse_simple;
  test_case "parse application/octet-stream" `Quick test_parse_application;
  test_case "parse text/plain" `Quick test_parse_text_plain;

  (* Case normalization *)
  test_case "parse uppercase IMAGE/PNG" `Quick test_parse_uppercase;
  test_case "parse mixed case Text/HTML" `Quick test_parse_mixed_case;
  test_case "parse uppercase APPLICATION/JSON" `Quick test_parse_uppercase_application;

  (* Whitespace handling *)
  test_case "parse with leading space" `Quick test_parse_with_leading_space;
  test_case "parse with trailing space" `Quick test_parse_with_trailing_space;
  test_case "parse with spaces around slash" `Quick test_parse_with_spaces_around_slash;
  test_case "parse with tabs" `Quick test_parse_with_tabs;

  (* Parameter parsing *)
  test_case "parse with charset" `Quick test_parse_with_charset;
  test_case "parse with uppercase CHARSET" `Quick test_parse_with_charset_lowercase;
  test_case "parse with multiple params" `Quick test_parse_with_multiple_params;
  test_case "parse with quoted param" `Quick test_parse_with_quoted_param;
  test_case "parse with escaped quote in param" `Quick test_parse_param_with_escaped_quote;
  test_case "parse param with spaces around =" `Quick test_parse_param_spaces_around_equals;

  (* normalize_media_type *)
  test_case "normalize simple" `Quick test_normalize_simple;
  test_case "normalize uppercase" `Quick test_normalize_uppercase;
  test_case "normalize with params" `Quick test_normalize_with_params;
  test_case "normalize with charset" `Quick test_normalize_with_charset;
  test_case "normalize complex" `Quick test_normalize_complex;

  (* Error handling *)
  test_case "parse empty string" `Quick test_parse_empty;
  test_case "parse invalid no subtype" `Quick test_parse_invalid_no_subtype;
  test_case "parse invalid no type" `Quick test_parse_invalid_no_type;
  test_case "parse invalid double slash" `Quick test_parse_invalid_double_slash;
  test_case "parse invalid special chars" `Quick test_parse_invalid_special_chars;
  test_case "parse invalid control chars" `Quick test_parse_invalid_control_chars;

  (* is_valid *)
  test_case "is_valid true" `Quick test_is_valid_true;
  test_case "is_valid with params" `Quick test_is_valid_with_params;
  test_case "is_valid false empty" `Quick test_is_valid_false_empty;
  test_case "is_valid false malformed" `Quick test_is_valid_false_malformed;

  (* to_string *)
  test_case "to_string simple" `Quick test_to_string_simple;
  test_case "to_string with params" `Quick test_to_string_with_params;

  (* Policy integration scenarios *)
  test_case "policy: parameterized header allowed" `Quick test_policy_scenario_parameterized_allowed;
  test_case "policy: case insensitive match" `Quick test_policy_scenario_case_insensitive;
  test_case "policy: different type not allowed" `Quick test_policy_scenario_not_allowed;
]
