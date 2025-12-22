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
end
