open Alcotest
open Blossom_core

let read ~max_size body = Blossom_shell.Request_body.read_limited ~max_size body

(* Piaf のボディ読み込みは Eio のスケジューラ上で動かす必要がある *)
let eio f () = Eio_main.run (fun _env -> f ())

(* Body.of_string_stream は Content-Length 不明（チャンク転送相当）のボディを作る *)
let chunked chunks =
  let stream, push = Piaf.Stream.create (List.length chunks + 1) in
  List.iter (fun c -> push (Some c)) chunks;
  push None;
  Piaf.Body.of_string_stream stream

let test_within_limit () =
  match read ~max_size:10 (Piaf.Body.of_string "hello") with
  | Ok s -> check string "body" "hello" s
  | Error _ -> fail "expected Ok"

let test_exactly_limit () =
  match read ~max_size:5 (Piaf.Body.of_string "hello") with
  | Ok s -> check string "body" "hello" s
  | Error _ -> fail "expected Ok"

let test_fixed_length_over_limit () =
  match read ~max_size:4 (Piaf.Body.of_string "hello") with
  | Error (Domain.Payload_too_large (actual, max)) ->
      check int "actual" 5 actual;
      check int "max" 4 max
  | _ -> fail "expected Payload_too_large"

let test_chunked_within_limit () =
  match read ~max_size:10 (chunked ["ab"; "cd"; "ef"]) with
  | Ok s -> check string "body" "abcdef" s
  | Error _ -> fail "expected Ok"

let test_chunked_over_limit () =
  match read ~max_size:5 (chunked ["abc"; "def"; "ghi"]) with
  | Error (Domain.Payload_too_large (_, max)) -> check int "max" 5 max
  | _ -> fail "expected Payload_too_large"

let test_empty () =
  match read ~max_size:10 Piaf.Body.empty with
  | Ok s -> check string "body" "" s
  | Error _ -> fail "expected Ok"

let tests = [
  test_case "within limit" `Quick (eio test_within_limit);
  test_case "exactly at limit" `Quick (eio test_exactly_limit);
  test_case "fixed length over limit" `Quick (eio test_fixed_length_over_limit);
  test_case "chunked within limit" `Quick (eio test_chunked_within_limit);
  test_case "chunked over limit" `Quick (eio test_chunked_over_limit);
  test_case "empty body" `Quick (eio test_empty);
]
