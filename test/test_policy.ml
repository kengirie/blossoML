open Alcotest
open Blossom_core

let test_check_size_valid () =
  let policy = Policy.default_policy in
  check bool "size within limit is ok" true
    (Result.is_ok (Policy.check_size ~policy 1000))

let test_check_size_too_large () =
  let policy = { Policy.max_size = 1000; allowed_mime_types = [] } in
  check bool "size over limit is error" true
    (Result.is_error (Policy.check_size ~policy 2000))

let test_check_size_negative () =
  let policy = Policy.default_policy in
  check bool "negative size is error" true
    (Result.is_error (Policy.check_size ~policy (-1)))

let test_check_mime_empty () =
  let policy = Policy.default_policy in
  check bool "empty mime is error" true
    (Result.is_error (Policy.check_mime_type ~policy ""))

let test_check_mime_allowed_all () =
  let policy = { Policy.max_size = 1000; allowed_mime_types = [] } in
  check bool "any mime is ok when list is empty" true
    (Result.is_ok (Policy.check_mime_type ~policy "image/png"))

let test_check_mime_allowed_specific () =
  let policy = { Policy.max_size = 1000; allowed_mime_types = ["image/png"; "image/jpeg"] } in
  check bool "allowed mime is ok" true
    (Result.is_ok (Policy.check_mime_type ~policy "image/png"))

let test_check_mime_not_allowed () =
  let policy = { Policy.max_size = 1000; allowed_mime_types = ["image/png"] } in
  check bool "not allowed mime is error" true
    (Result.is_error (Policy.check_mime_type ~policy "image/jpeg"))

let test_check_upload_valid () =
  let policy = Policy.default_policy in
  check bool "valid upload is ok" true
    (Result.is_ok (Policy.check_upload_policy ~policy ~size:1000 ~mime:"image/png"))

let test_check_upload_size_error () =
  let policy = { Policy.max_size = 100; allowed_mime_types = [] } in
  check bool "upload with size error fails" true
    (Result.is_error (Policy.check_upload_policy ~policy ~size:200 ~mime:"image/png"))

let test_check_upload_mime_error () =
  let policy = { Policy.max_size = 1000; allowed_mime_types = ["image/png"] } in
  check bool "upload with mime error fails" true
    (Result.is_error (Policy.check_upload_policy ~policy ~size:100 ~mime:"image/jpeg"))

(* === New tests for Content-Type normalization (Issue #1) === *)

(* Parameterized Content-Type should be accepted *)
let test_check_mime_with_charset () =
  let policy = { Policy.max_size = 1000; allowed_mime_types = ["text/html"] } in
  check bool "text/html with charset is ok" true
    (Result.is_ok (Policy.check_mime_type ~policy "text/html; charset=UTF-8"))

let test_check_mime_with_params () =
  let policy = { Policy.max_size = 1000; allowed_mime_types = ["image/png"] } in
  check bool "image/png with version param is ok" true
    (Result.is_ok (Policy.check_mime_type ~policy "image/png; version=1"))

(* Case-insensitive matching *)
let test_check_mime_uppercase () =
  let policy = { Policy.max_size = 1000; allowed_mime_types = ["image/png"] } in
  check bool "IMAGE/PNG matches image/png" true
    (Result.is_ok (Policy.check_mime_type ~policy "IMAGE/PNG"))

let test_check_mime_mixed_case () =
  let policy = { Policy.max_size = 1000; allowed_mime_types = ["text/html"] } in
  check bool "Text/HTML matches text/html" true
    (Result.is_ok (Policy.check_mime_type ~policy "Text/HTML"))

(* Invalid Content-Type should be rejected *)
let test_check_mime_invalid_format () =
  let policy = { Policy.max_size = 1000; allowed_mime_types = [] } in
  check bool "invalid format text/?? is error" true
    (Result.is_error (Policy.check_mime_type ~policy "text/??"))

let test_check_mime_no_subtype () =
  let policy = { Policy.max_size = 1000; allowed_mime_types = [] } in
  check bool "missing subtype is error" true
    (Result.is_error (Policy.check_mime_type ~policy "text"))

(* Combined test: uppercase with params *)
let test_check_mime_uppercase_with_params () =
  let policy = { Policy.max_size = 1000; allowed_mime_types = ["image/png"] } in
  check bool "IMAGE/PNG; charset=utf-8 matches image/png" true
    (Result.is_ok (Policy.check_mime_type ~policy "IMAGE/PNG; charset=utf-8"))

(* check_upload_policy with normalization *)
let test_check_upload_with_params () =
  let policy = { Policy.max_size = 1000; allowed_mime_types = ["image/png"] } in
  check bool "upload with parameterized mime is ok" true
    (Result.is_ok (Policy.check_upload_policy ~policy ~size:100 ~mime:"image/png; version=1"))

let test_check_upload_uppercase () =
  let policy = { Policy.max_size = 1000; allowed_mime_types = ["application/json"] } in
  check bool "upload with uppercase mime is ok" true
    (Result.is_ok (Policy.check_upload_policy ~policy ~size:100 ~mime:"APPLICATION/JSON"))

let tests = [
  test_case "check_size valid" `Quick test_check_size_valid;
  test_case "check_size too large" `Quick test_check_size_too_large;
  test_case "check_size negative" `Quick test_check_size_negative;
  test_case "check_mime_type empty" `Quick test_check_mime_empty;
  test_case "check_mime_type allowed all" `Quick test_check_mime_allowed_all;
  test_case "check_mime_type allowed specific" `Quick test_check_mime_allowed_specific;
  test_case "check_mime_type not allowed" `Quick test_check_mime_not_allowed;
  test_case "check_upload_policy valid" `Quick test_check_upload_valid;
  test_case "check_upload_policy size error" `Quick test_check_upload_size_error;
  test_case "check_upload_policy mime error" `Quick test_check_upload_mime_error;
  (* New tests for Issue #1 *)
  test_case "check_mime_type with charset param" `Quick test_check_mime_with_charset;
  test_case "check_mime_type with version param" `Quick test_check_mime_with_params;
  test_case "check_mime_type uppercase" `Quick test_check_mime_uppercase;
  test_case "check_mime_type mixed case" `Quick test_check_mime_mixed_case;
  test_case "check_mime_type invalid format" `Quick test_check_mime_invalid_format;
  test_case "check_mime_type no subtype" `Quick test_check_mime_no_subtype;
  test_case "check_mime_type uppercase with params" `Quick test_check_mime_uppercase_with_params;
  test_case "check_upload_policy with params" `Quick test_check_upload_with_params;
  test_case "check_upload_policy uppercase" `Quick test_check_upload_uppercase;
]
