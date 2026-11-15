(** リクエストボディをサイズ上限付きで読み込む *)

open Blossom_core

exception Too_large of int

(** [body] を最大 [max_size] バイトまで文字列として読み込む。

    - Content-Length が上限を超える場合は読み込まずに拒否する
    - Content-Length が上限以下ならそのまま読み込む
      （HTTP層が Content-Length を超えるバイトを渡さないため、上限内に収まる）
    - チャンク転送など長さ不明の場合は、読み込み中の累計で打ち切る

    上限超過は [Payload_too_large]、受信エラーは [Storage_error] を返す。 *)
let read_limited ~max_size body =
  let of_piaf = function
    | Ok s -> Ok s
    | Error e -> Error (Domain.Storage_error (Piaf.Error.to_string e))
  in
  match Piaf.Body.length body with
  | `Fixed n when n > Int64.of_int max_size ->
      Error (Domain.Payload_too_large (Int64.to_int n, max_size))
  | `Fixed _ -> of_piaf (Piaf.Body.to_string body)
  | `Chunked | `Close_delimited | `Error _ | `Unknown ->
      let buf = Buffer.create 1024 in
      match
        Piaf.Body.iter_string body ~f:(fun chunk ->
          let size = Buffer.length buf + String.length chunk in
          if size > max_size then raise (Too_large size);
          Buffer.add_string buf chunk)
      with
      | result -> of_piaf (Result.map (fun () -> Buffer.contents buf) result)
      | exception Too_large size -> Error (Domain.Payload_too_large (size, max_size))
