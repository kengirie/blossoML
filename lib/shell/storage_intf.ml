(** ファイルシステム操作の抽象インターフェース *)

open Blossom_core

(** ストリーミング保存の結果 *)
type save_result = {
  sha256: string;
  size: int;
  first_chunk: string option;
}

(** ストリーミング配信の進捗通知（停止監視用、[Stall_guard] を参照） *)
type progress = {
  taken : unit -> unit;      (** 送信側が次のチャンクを取りに来た *)
  finished : unit -> unit;   (** 配信が終わった（完了・開始失敗） *)
  truncated : unit -> unit;  (** 配信が予定より早く終わった（読み込みエラー等） *)
  resume : unit -> bool;     (** [on_idle] でファイルを閉じた後に開き直してよいか（false なら中止） *)
}

(** 配信1件を監視対象に登録する。[on_idle] は送信側がしばらく次のチャンクを
    取りに来ないときに呼ばれ、実装はファイルを閉じる（再開時は [resume] を
    確認して開き直す）。同時配信数の上限などで受け付けられない場合は [Error 理由] *)
type watch = on_idle:(unit -> unit) -> (progress, string) result

(** 監視しない場合の [watch]（テスト用など） *)
let no_watch : watch = fun ~on_idle:_ ->
  Ok { taken = ignore; finished = ignore; truncated = ignore; resume = (fun () -> true) }

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

  (** ストリーミングでファイルを取得する。
      [sw] には接続ごとのスイッチを渡すこと。[watch] で配信の進捗を通知する *)
  val get :
    sw:Eio.Switch.t ->
    watch:watch ->
    t ->
    path:string ->
    size:int ->
    (Piaf.Body.t, Domain.error) result

  (** ストリーミングでファイルの一部を取得する（BUD-01 Rangeリクエスト用）
      offset: 読み込み開始位置（バイト）
      length: 読み込むバイト数 *)
  val get_range :
    sw:Eio.Switch.t ->
    watch:watch ->
    t ->
    path:string ->
    offset:int ->
    length:int ->
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
