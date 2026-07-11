open Alcotest
open Blossom_core

(** parse_resultの比較・表示用testable *)
let parse_result : Range.parse_result testable =
  let pp fmt = function
    | Range.Satisfiable { start; end_ } ->
        Format.fprintf fmt "Satisfiable { start = %d; end_ = %d }" start end_
    | Range.Unsatisfiable -> Format.fprintf fmt "Unsatisfiable"
    | Range.Not_applicable -> Format.fprintf fmt "Not_applicable"
  in
  testable pp ( = )

let check_parse name header ~total_size expected =
  check parse_result name expected (Range.parse header ~total_size)

(* --- 正常系 --- *)

let test_closed_range () =
  check_parse "bytes=0-499" "bytes=0-499" ~total_size:1000
    (Range.Satisfiable { start = 0; end_ = 499 })

let test_middle_range () =
  check_parse "bytes=500-999" "bytes=500-999" ~total_size:1000
    (Range.Satisfiable { start = 500; end_ = 999 })

let test_open_range () =
  check_parse "bytes=500-" "bytes=500-" ~total_size:1000
    (Range.Satisfiable { start = 500; end_ = 999 })

let test_suffix_range () =
  check_parse "bytes=-500" "bytes=-500" ~total_size:1000
    (Range.Satisfiable { start = 500; end_ = 999 })

let test_single_byte () =
  check_parse "bytes=0-0" "bytes=0-0" ~total_size:1000
    (Range.Satisfiable { start = 0; end_ = 0 })

let test_last_byte () =
  check_parse "bytes=999-999" "bytes=999-999" ~total_size:1000
    (Range.Satisfiable { start = 999; end_ = 999 })

let test_case_insensitive_unit () =
  check_parse "BYTES=0-499" "BYTES=0-499" ~total_size:1000
    (Range.Satisfiable { start = 0; end_ = 499 })

let test_whitespace_trim () =
  check_parse " bytes=0-499 " " bytes=0-499 " ~total_size:1000
    (Range.Satisfiable { start = 0; end_ = 499 })

(* --- RFC 7233のクランプ挙動 --- *)

let test_end_clamped_to_size () =
  (* endがサイズ超過の場合は末尾にクランプ *)
  check_parse "bytes=500-99999" "bytes=500-99999" ~total_size:1000
    (Range.Satisfiable { start = 500; end_ = 999 })

let test_suffix_larger_than_size () =
  (* suffix長がサイズ超過の場合は全体を返す *)
  check_parse "bytes=-99999" "bytes=-99999" ~total_size:1000
    (Range.Satisfiable { start = 0; end_ = 999 })

(* --- 416 Unsatisfiable --- *)

let test_start_beyond_size () =
  check_parse "bytes=1000-" "bytes=1000-" ~total_size:1000 Range.Unsatisfiable

let test_start_beyond_size_closed () =
  check_parse "bytes=1000-2000" "bytes=1000-2000" ~total_size:1000 Range.Unsatisfiable

let test_start_greater_than_end () =
  check_parse "bytes=500-100" "bytes=500-100" ~total_size:1000 Range.Unsatisfiable

let test_suffix_zero () =
  check_parse "bytes=-0" "bytes=-0" ~total_size:1000 Range.Unsatisfiable

let test_empty_spec () =
  check_parse "bytes=" "bytes=" ~total_size:1000 Range.Unsatisfiable

let test_dash_only () =
  check_parse "bytes=-" "bytes=-" ~total_size:1000 Range.Unsatisfiable

let test_non_numeric () =
  check_parse "bytes=abc-def" "bytes=abc-def" ~total_size:1000 Range.Unsatisfiable

let test_empty_blob () =
  (* サイズ0のblobにはどんなrangeも適用不可 *)
  check_parse "bytes=0-0 (empty blob)" "bytes=0-0" ~total_size:0 Range.Unsatisfiable

let test_suffix_empty_blob () =
  check_parse "bytes=-1 (empty blob)" "bytes=-1" ~total_size:0 Range.Unsatisfiable

(* --- 無視するケース（200で全体を返す） --- *)

let test_multiple_ranges () =
  (* 複数rangeは非対応: ヘッダーを無視 *)
  check_parse "bytes=0-1,5-9" "bytes=0-1,5-9" ~total_size:1000 Range.Not_applicable

let test_unknown_unit () =
  check_parse "items=0-499" "items=0-499" ~total_size:1000 Range.Not_applicable

let test_garbage_header () =
  check_parse "garbage" "garbage" ~total_size:1000 Range.Not_applicable

(* --- length --- *)

let test_length () =
  check int "length of 0-499" 500 (Range.length { Range.start = 0; end_ = 499 });
  check int "length of single byte" 1 (Range.length { Range.start = 42; end_ = 42 })

let tests = [
  test_case "closed range" `Quick test_closed_range;
  test_case "middle range" `Quick test_middle_range;
  test_case "open range" `Quick test_open_range;
  test_case "suffix range" `Quick test_suffix_range;
  test_case "single byte" `Quick test_single_byte;
  test_case "last byte" `Quick test_last_byte;
  test_case "case-insensitive unit" `Quick test_case_insensitive_unit;
  test_case "whitespace trim" `Quick test_whitespace_trim;
  test_case "end clamped to size" `Quick test_end_clamped_to_size;
  test_case "suffix larger than size" `Quick test_suffix_larger_than_size;
  test_case "start beyond size (open)" `Quick test_start_beyond_size;
  test_case "start beyond size (closed)" `Quick test_start_beyond_size_closed;
  test_case "start greater than end" `Quick test_start_greater_than_end;
  test_case "suffix zero" `Quick test_suffix_zero;
  test_case "empty spec" `Quick test_empty_spec;
  test_case "dash only" `Quick test_dash_only;
  test_case "non-numeric" `Quick test_non_numeric;
  test_case "empty blob" `Quick test_empty_blob;
  test_case "suffix on empty blob" `Quick test_suffix_empty_blob;
  test_case "multiple ranges ignored" `Quick test_multiple_ranges;
  test_case "unknown unit ignored" `Quick test_unknown_unit;
  test_case "garbage header ignored" `Quick test_garbage_header;
  test_case "length" `Quick test_length;
]
