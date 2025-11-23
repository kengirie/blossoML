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
]
