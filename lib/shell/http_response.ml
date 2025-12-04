(** HTTPレスポンス生成の純粋関数モジュール

    このモジュールはHTTPレスポンス生成ロジックを純粋関数として提供します。
    すべての関数は副作用を持たず、ユニットテストが可能です。
*)

open Piaf
open Blossom_core

(** レスポンスの種類を表すバリアント型 *)
type response_kind =
  | Success_blob of { data: string; mime_type: string; size: int }
    (** Blobデータの取得成功 *)
  | Success_metadata of { mime_type: string; size: int }
    (** Blobメタデータの取得成功（HEADリクエスト用） *)
  | Success_upload of Domain.blob_descriptor
    (** Blobアップロード成功 *)
  | Cors_preflight
    (** CORSプリフライトレスポンス *)
  | Error_not_found of string
    (** 404 Not Found *)
  | Error_unauthorized of string
    (** 401 Unauthorized *)
  | Error_bad_request of string
    (** 400 Bad Request *)
  | Error_internal of string
    (** 500 Internal Server Error *)

(** CORSヘッダーをレスポンスに追加する純粋関数

    @koa/cors 相当の全面適用:
    - Access-Control-Allow-Origin: *
    - Access-Control-Allow-Methods: * (全メソッド許可)
    - Access-Control-Allow-Headers: Authorization, Content-Type, Content-Length, *
    - Access-Control-Expose-Headers: * (全ヘッダー公開)
    - Access-Control-Max-Age: 86400 (プリフライトキャッシュ24時間)
*)
let add_cors_headers response =
  let headers = Response.headers response in
  let headers =
    headers
    |> fun h -> Headers.remove h "access-control-allow-origin"
    |> fun h -> Headers.remove h "access-control-allow-methods"
    |> fun h -> Headers.remove h "access-control-allow-headers"
    |> fun h -> Headers.remove h "access-control-expose-headers"
    |> fun h -> Headers.remove h "access-control-max-age"
    |> fun h -> Headers.add h "Access-Control-Allow-Origin" "*"
    |> fun h -> Headers.add h "Access-Control-Allow-Methods" "*"
    |> fun h -> Headers.add h "Access-Control-Allow-Headers" "Authorization, Content-Type, Content-Length, *"
    |> fun h -> Headers.add h "Access-Control-Expose-Headers" "*"
    |> fun h -> Headers.add h "Access-Control-Max-Age" "86400"
  in
  Response.create
    ~version:response.version
    ~headers
    ~body:response.body
    response.status

(** blob descriptorをJSON文字列に変換する純粋関数 *)
let descriptor_to_json descriptor =
  `Assoc [
    ("url", `String descriptor.Domain.url);
    ("sha256", `String descriptor.sha256);
    ("size", `Int descriptor.size);
    ("type", `String descriptor.mime_type);
    ("uploaded", `Int (Int64.to_int descriptor.uploaded));
  ]
  |> Yojson.Basic.to_string

(** レスポンスの種類から実際のHTTPレスポンスを生成する純粋関数

    この関数はパターンマッチを使用してすべてのresponse_kindを網羅的に処理します。
    新しいレスポンス種別を追加した場合、コンパイラが未処理のケースを警告します。
*)
let create = function
  | Success_blob { data; mime_type; size } ->
      let headers = Headers.of_list [
        ("Content-Type", mime_type);
        ("Content-Length", string_of_int size);
      ] in
      Response.create ~headers ~body:(Body.of_string data) `OK

  | Success_metadata { mime_type; size } ->
      let headers = Headers.of_list [
        ("Content-Type", mime_type);
        ("Content-Length", string_of_int size);
      ] in
      Response.create ~headers `OK

  | Success_upload descriptor ->
      let json = descriptor_to_json descriptor in
      Response.of_string ~body:json `OK

  | Cors_preflight ->
      let headers = Headers.of_list [
        ("Access-Control-Allow-Origin", "*");
        ("Access-Control-Allow-Methods", "*");
        ("Access-Control-Allow-Headers", "Authorization, Content-Type, Content-Length, *");
        ("Access-Control-Expose-Headers", "*");
        ("Access-Control-Max-Age", "86400");
      ] in
      Response.create ~headers `No_content

  | Error_not_found message ->
      let headers = Headers.of_list [("X-Reason", message)] in
      Response.of_string ~headers ~body:message `Not_found

  | Error_unauthorized message ->
      let headers = Headers.of_list [("X-Reason", message)] in
      Response.of_string ~headers ~body:message `Unauthorized

  | Error_bad_request message ->
      let headers = Headers.of_list [("X-Reason", message)] in
      Response.of_string ~headers ~body:message `Bad_request

  | Error_internal message ->
      let headers = Headers.of_list [("X-Reason", message)] in
      Response.of_string ~headers ~body:message `Internal_server_error
