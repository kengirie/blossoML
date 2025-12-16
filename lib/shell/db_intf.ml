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
    uploader:string ->
    (unit, Domain.error) result

  (** Blobメタデータを取得する *)
  val get :
    t ->
    sha256:string ->
    (Domain.blob_descriptor, Domain.error) result

  (** Blobのアップローダーpubkeyを取得する *)
  val get_uploader :
    t ->
    sha256:string ->
    (string option, Domain.error) result

  (** Blobを論理削除する（status = 'deleted' に更新） *)
  val delete :
    t ->
    sha256:string ->
    (unit, Domain.error) result
end
