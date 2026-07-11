(** BUD-01: Rangeリクエストヘッダーのパースと検証（RFC 7233/9110 単一range対応）

    純粋関数のみ。I/Oは行わない。 *)

(** バイトレンジ（両端を含む） *)
type t = {
  start : int;
  end_ : int;  (* inclusive *)
}

(** パース結果
    - Satisfiable: 有効なrange → 206 Partial Content
    - Unsatisfiable: bytes形式だが不正または範囲外 → 416 Range Not Satisfiable
    - Not_applicable: bytes以外の単位や複数range → ヘッダーを無視して200で全体を返す
      （RFC 9110: 理解できないrange単位はMUST ignore） *)
type parse_result =
  | Satisfiable of t
  | Unsatisfiable
  | Not_applicable

(** rangeのバイト長 *)
let length { start; end_ } = end_ - start + 1

(** 文字列が空でなく数字のみで構成されているか *)
let is_digits s =
  String.length s > 0
  && String.for_all (fun c -> c >= '0' && c <= '9') s

(** ASCII小文字化（Rangeヘッダーの単位は大文字小文字を区別しない） *)
let lowercase_ascii_prefix s len =
  if String.length s < len then None
  else Some (String.lowercase_ascii (String.sub s 0 len))

(** Rangeヘッダー値をパースする

    対応形式（単一rangeのみ）:
    - "bytes=0-499"  : 先頭500バイト
    - "bytes=500-"   : 500バイト目から末尾まで
    - "bytes=-500"   : 末尾500バイト（suffix range）

    RFC 7233に準拠した挙動:
    - end が total_size を超える場合は末尾にクランプ
    - suffix長が total_size を超える場合は全体を返す
    - start >= total_size は Unsatisfiable（416）
    - suffix長 0（"bytes=-0"）は Unsatisfiable（416）
    - 複数range（"bytes=0-1,5-9"）は Not_applicable（無視して200） *)
let parse header ~total_size =
  let header = String.trim header in
  let prefix_len = String.length "bytes=" in
  match lowercase_ascii_prefix header prefix_len with
  | Some "bytes=" ->
      let spec = String.sub header prefix_len (String.length header - prefix_len) in
      let spec = String.trim spec in
      if String.contains spec ',' then
        (* 複数rangeは非対応: ヘッダーを無視して全体を返す *)
        Not_applicable
      else
        (match String.index_opt spec '-' with
         | None -> Unsatisfiable
         | Some i ->
             let first = String.sub spec 0 i in
             let second = String.sub spec (i + 1) (String.length spec - i - 1) in
             (match is_digits first, is_digits second with
              | false, false -> Unsatisfiable
              | false, true ->
                  (* suffix range: bytes=-N → 末尾Nバイト *)
                  (match int_of_string_opt second with
                   | None -> Unsatisfiable  (* オーバーフロー等 *)
                   | Some n ->
                       if n <= 0 || total_size <= 0 then Unsatisfiable
                       else
                         let start = max 0 (total_size - n) in
                         Satisfiable { start; end_ = total_size - 1 })
              | true, false ->
                  (* open range: bytes=N- → Nバイト目から末尾まで *)
                  (match int_of_string_opt first with
                   | None -> Unsatisfiable
                   | Some start ->
                       if start >= total_size then Unsatisfiable
                       else Satisfiable { start; end_ = total_size - 1 })
              | true, true ->
                  (* closed range: bytes=N-M *)
                  (match int_of_string_opt first, int_of_string_opt second with
                   | Some start, Some end_ ->
                       if start > end_ || start >= total_size then Unsatisfiable
                       else Satisfiable { start; end_ = min end_ (total_size - 1) }
                   | _ -> Unsatisfiable)))
  | _ ->
      (* bytes以外のrange単位は無視する（RFC 9110） *)
      Not_applicable
