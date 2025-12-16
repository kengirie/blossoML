(** Blobストレージサービス - StorageとDBを組み合わせた高レベルAPI *)

open Blossom_core

(** Blob_serviceのシグネチャ *)
module type S = sig
  type storage
  type db

  (** Blobを保存する（ストリーミング受信 → SHA256計算 → ファイル保存 → DB保存） *)
  val save :
    storage:storage ->
    db:db ->
    body:Piaf.Body.t ->
    mime_type:string ->
    uploader:string ->
    (string * int * string, Domain.error) result  (* sha256, size, mime_type *)

  (** Blobを取得する（DB取得 → ストリーミング返却） *)
  val get :
    sw:Eio.Switch.t ->
    storage:storage ->
    db:db ->
    sha256:string ->
    (Piaf.Body.t * Domain.blob_descriptor, Domain.error) result

  (** メタデータのみ取得（HEADリクエスト用） *)
  val get_metadata :
    storage:storage ->
    db:db ->
    sha256:string ->
    (Domain.blob_descriptor, Domain.error) result

  (** Blobを削除する（所有者検証 → DB論理削除 → ファイル物理削除） *)
  val delete :
    storage:storage ->
    db:db ->
    sha256:string ->
    pubkey:string ->
    (unit, Domain.error) result
end

(** Blob_serviceファンクタ *)
module Make (Storage : Storage_intf.S) (Db : Db_intf.S) :
  S with type storage = Storage.t and type db = Db.t = struct

  type storage = Storage.t
  type db = Db.t

  let save ~storage ~db ~body ~mime_type ~uploader =
    (* ストレージに保存 *)
    match Storage.save storage ~body with
    | Error e -> Error e
    | Ok result ->
        (* MIME type がデフォルト値の場合、先頭チャンクから検出を試みる *)
        let final_mime_type =
          if mime_type = "application/octet-stream" then
            match result.first_chunk with
            | Some chunk ->
                (match Mime_detect.detect_from_bytes chunk with
                 | Some detected -> detected
                 | None -> mime_type)
            | None -> mime_type
          else
            mime_type
        in

        (* DBにメタデータを保存 *)
        match Db.save db ~sha256:result.sha256 ~size:result.size ~mime_type:final_mime_type ~uploader with
        | Ok () -> Ok (result.sha256, result.size, final_mime_type)
        | Error e ->
            (* DB保存失敗時はファイルも削除 *)
            let _ = Storage.unlink storage ~path:result.sha256 in
            Error e

  let get ~sw ~storage ~db ~sha256 =
    match Db.get db ~sha256 with
    | Ok metadata ->
        (* ファイルの存在確認 *)
        (match Storage.exists storage ~path:sha256 with
         | Error e -> Error e
         | Ok false -> Error (Domain.Blob_not_found sha256)
         | Ok true ->
             (* ストリーミングで取得 *)
             match Storage.get ~sw storage ~path:sha256 ~size:metadata.size with
             | Error e -> Error e
             | Ok body -> Ok (body, metadata))
    | Error (Domain.Blob_not_found _) ->
        (* DBにない場合もファイルがあればストリーミングで返す（後方互換性） *)
        (match Storage.exists storage ~path:sha256 with
         | Error e -> Error e
         | Ok false -> Error (Domain.Blob_not_found sha256)
         | Ok true ->
             (* サイズを取得 *)
             match Storage.stat storage ~path:sha256 with
             | Error e -> Error e
             | Ok size ->
                 (* ストリーミングで取得 *)
                 match Storage.get ~sw storage ~path:sha256 ~size with
                 | Error e -> Error e
                 | Ok body ->
                     Ok (body, {
                       Domain.sha256 = sha256;
                       size;
                       mime_type = "application/octet-stream";
                       uploaded = 0L;
                       url = "/";
                     }))
    | Error e -> Error e

  let get_metadata ~storage ~db ~sha256 =
    match Db.get db ~sha256 with
    | Ok metadata ->
        (* ファイルの存在確認 *)
        (match Storage.exists storage ~path:sha256 with
         | Error e -> Error e
         | Ok false -> Error (Domain.Blob_not_found sha256)
         | Ok true -> Ok metadata)
    | Error e -> Error e

  let delete ~storage ~db ~sha256 ~pubkey =
    (* 1. アップローダー情報を取得し所有者検証 *)
    match Db.get_uploader db ~sha256 with
    | Error e -> Error e
    | Ok uploader_opt ->
        (* アップローダーが記録されていて、リクエスト者と異なる場合は拒否 *)
        (match uploader_opt with
         | Some uploader when uploader <> pubkey ->
             Error (Domain.Storage_error "Not authorized to delete this blob")
         | _ ->
             (* 2. DB論理削除 *)
             match Db.delete db ~sha256 with
             | Error e -> Error e
             | Ok () ->
                 (* 3. ファイル物理削除（エラーは無視してDBの状態を優先） *)
                 let _ = Storage.unlink storage ~path:sha256 in
                 Ok ())
end
