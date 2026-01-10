(** Blobストレージサービス - StorageとDBを組み合わせた高レベルAPI *)

open Blossom_core

(** Blob_serviceのシグネチャ *)
module type S = sig
  type storage
  type db

  (** Blobを保存する（ストリーミング受信 → SHA256計算 → ファイル保存 → DB保存 → 所有者追加）
      max_size: 最大サイズ（バイト）。超過時はエラーを返す *)
  val save :
    storage:storage ->
    db:db ->
    body:Piaf.Body.t ->
    mime_type:string ->
    uploader:string ->
    max_size:int ->
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

  (** Blobを削除する（所有者検証 → 所有関係削除 → 所有者0ならDB削除→物理削除） *)
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

  let save ~storage ~db ~body ~mime_type ~uploader ~max_size =
    (* ストレージに保存（サイズ制限付き） *)
    match Storage.save storage ~body ~max_size with
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

        (* DBにメタデータを保存（既存blobの場合は何もしない） *)
        match Db.save db ~sha256:result.sha256 ~size:result.size ~mime_type:final_mime_type with
        | Error e ->
            (* DB保存失敗時はファイルも削除 *)
            let _ = Storage.unlink storage ~path:result.sha256 in
            Error e
        | Ok () ->
            (* 所有者を追加（重複時は何もしない） *)
            match Db.add_owner db ~sha256:result.sha256 ~pubkey:uploader with
            | Ok () -> Ok (result.sha256, result.size, final_mime_type)
            | Error e ->
                (* 所有者追加失敗時はファイルも削除 *)
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
    (* Check if blob exists in DB (404 if not) *)
    match Db.get db ~sha256 with
    | Error e -> Error e
    | Ok metadata ->
        (* Verify caller is an owner *)
        match Db.has_owner db ~sha256 ~pubkey with
        | Error e -> Error e
        | Ok false ->
            Error (Domain.Forbidden "Not authorized to delete this blob")
        | Ok true ->
        (* Remove ownership relation *)
        match Db.remove_owner db ~sha256 ~pubkey with
        | Error e -> Error e
        | Ok () ->
            (* Check remaining owners *)
            match Db.count_owners db ~sha256 with
            | Error e -> Error e
            | Ok count when count > 0 ->
                (* Other owners exist; only ownership was removed *)
                Ok ()
            | Ok _ ->
                (* No owners remain; delete the blob entirely.
                   Delete from DB first, then unlink file.
                   If either fails, restore DB state. *)
                (match Db.delete db ~sha256 with
                 | Error e ->
                     (* Db.delete failed; restore ownership *)
                     let _ = Db.add_owner db ~sha256 ~pubkey in
                     Eio.traceln "WARNING: Db.delete failed for %s, restored owner: %s" sha256
                       (match e with Domain.Storage_error msg -> msg | _ -> "Unknown error");
                     Error e
                 | Ok () ->
                     (* DB deleted; now unlink file *)
                     (match Storage.unlink storage ~path:sha256 with
                      | Ok () -> Ok ()
                      | Error (Domain.Blob_not_found _) ->
                          (* File already gone; success *)
                          Ok ()
                      | Error e ->
                          (* Unlink failed; restore DB entry and ownership *)
                          let _ = Db.save db ~sha256 ~size:metadata.size ~mime_type:metadata.mime_type in
                          let _ = Db.add_owner db ~sha256 ~pubkey in
                          Eio.traceln "WARNING: Failed to unlink file %s, restored DB entry: %s" sha256
                            (match e with
                             | Domain.Storage_error msg -> msg
                             | Domain.Forbidden msg -> msg
                             | _ -> "Unknown error");
                          Error e))
end
