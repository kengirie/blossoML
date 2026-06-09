(** データベース操作の抽象インターフェース *)

open Blossom_core

(** メタデータDB操作のシグネチャ *)
module type S = sig
  (** DB接続/プールの型 *)
  type t

  (** Blobメタデータを保存する *)
  val save :
    t ->
    sha256:string ->
    size:int ->
    mime_type:string ->
    (unit, Domain.error) result

  (** Blobメタデータを取得する *)
  val get :
    t ->
    sha256:string ->
    (Domain.blob_descriptor, Domain.error) result

  (** Blobを論理削除する（status = 'deleted' に更新） *)
  val delete :
    t ->
    sha256:string ->
    (unit, Domain.error) result

  (** Blobに所有者を追加する（重複時は何もしない） *)
  val add_owner :
    t ->
    sha256:string ->
    pubkey:string ->
    (unit, Domain.error) result

  (** 指定pubkeyがBlobの所有者かどうか判定する *)
  val has_owner :
    t ->
    sha256:string ->
    pubkey:string ->
    (bool, Domain.error) result

  (** Blobから所有者を削除する *)
  val remove_owner :
    t ->
    sha256:string ->
    pubkey:string ->
    (unit, Domain.error) result

  (** Blobの所有者数を取得する *)
  val count_owners :
    t ->
    sha256:string ->
    (int, Domain.error) result

  (** Blobの全所有者を取得する *)
  val list_owners :
    t ->
    sha256:string ->
    (string list, Domain.error) result

  (** 指定pubkeyが所有するBlobのリストを取得する（BUD-12）。
      [since]/[until] は uploaded_at の閉区間フィルタ（包含）。
      [cursor] は (uploaded_at, sha256) の組で、それより降順で「後ろ」のものを返す。
      [limit] は返す最大件数。
      結果は uploaded_at 降順、同値時は sha256 降順でソートされる。 *)
  val list_by_pubkey :
    t ->
    pubkey:string ->
    since:int64 ->
    until:int64 ->
    cursor:(int64 * string) option ->
    limit:int ->
    (Domain.blob_descriptor list, Domain.error) result

  (** BUD-09: NIP-56 レポートを1件保存する。
      同じ (event_id, sha256) 組は重複保存しない。 *)
  val save_report :
    t ->
    event_id:string ->
    sha256:string ->
    reporter_pubkey:string ->
    report_type:string ->
    content:string ->
    e_tag:string option ->
    p_tag:string option ->
    raw_event_json:string ->
    event_created_at:int64 ->
    received_at:int64 ->
    (unit, Domain.error) result
end
