(** ファイルシステム操作の抽象インターフェース *)

open Blossom_core

(** ストリーミング保存の結果 *)
type save_result = {
  sha256: string;
  size: int;
  first_chunk: string option;
}

(** ファイルシステム操作 *)
module type S = sig
  (** ファイルシステム操作のコンテキスト（Eio.Path.tなど） *)
  type t

  (** ストリーミングでファイルを保存しながらSHA256を計算する
      max_size: 最大サイズ（バイト）。超過時はエラーを返し一時ファイルを削除する *)
  val save :
    t ->
    body:Piaf.Body.t ->
    max_size:int ->
    (save_result, Domain.error) result

  (** ストリーミングでファイルを取得する *)
  val get :
    sw:Eio.Switch.t ->
    t ->
    path:string ->
    size:int ->
    (Piaf.Body.t, Domain.error) result

  (** ファイルが存在するか確認する
      - Ok true: 存在する
      - Ok false: Not_found
      - Error: その他のエラー（Permission denied等） *)
  val exists : t -> path:string -> (bool, Domain.error) result

  (** ファイルのメタデータを取得（サイズを返す） *)
  val stat : t -> path:string -> (int, Domain.error) result

  (** ファイルを削除する *)
  val unlink : t -> path:string -> (unit, Domain.error) result

  (** ファイルをリネームする *)
  val rename : t -> src:string -> dst:string -> (unit, Domain.error) result
end
