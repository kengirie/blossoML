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

  (** Blobを削除する（所有者検証 → 所有関係削除 → 所有者0なら論理削除＋物理削除） *)
  val delete :
    storage:storage ->
    db:db ->
    sha256:string ->
    pubkey:string ->
    (unit, Domain.error) result

  (** リモートURLからBlobをミラーリングする（BUD-04） *)
  val mirror :
    sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    storage:storage ->
    db:db ->
    url:string ->
    uploader:string ->
    max_size:int ->
    (string * int * string, Domain.error) result  (* sha256, size, mime_type *)
end

(** Blob_serviceファンクタ *)
module Make (Storage : Storage_intf.S) (Db : Db_intf.S) :
  S with type storage = Storage.t and type db = Db.t = struct

  type storage = Storage.t
  type db = Db.t

  let save ~storage ~db ~body ~mime_type:fallback_mime_type ~uploader ~max_size =
    (* ストレージに保存（サイズ制限付き） *)
    match Storage.save storage ~body ~max_size with
    | Error e -> Error e
    | Ok result ->
        (* MIME type は常にバイト検査を優先し、検出できない場合のみフォールバックを使用 *)
        let final_mime_type =
          match result.first_chunk with
          | Some chunk ->
              (match Mime_detect.detect_from_bytes chunk with
               | Some detected -> detected
               | None -> fallback_mime_type)
          | None -> fallback_mime_type
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
    | Error (Domain.Blob_not_found _) ->
        (* DBにない場合もファイルがあればメタデータを返す（後方互換性） *)
        (match Storage.exists storage ~path:sha256 with
         | Error e -> Error e
         | Ok false -> Error (Domain.Blob_not_found sha256)
         | Ok true ->
             match Storage.stat storage ~path:sha256 with
             | Error e -> Error e
             | Ok size ->
                 Ok {
                   Domain.sha256 = sha256;
                   size;
                   mime_type = "application/octet-stream";
                   uploaded = 0L;
                   url = "/";
                 })
    | Error e -> Error e

  let delete ~storage ~db ~sha256 ~pubkey =
    (* 1. まずBlobが存在するか確認（存在しない場合は404） *)
    match Db.get db ~sha256 with
    | Error (Domain.Blob_not_found _) ->
        (* DBにない場合もファイルがあるか確認（後方互換性） *)
        (match Storage.exists storage ~path:sha256 with
         | Error e -> Error e
         | Ok false -> Error (Domain.Blob_not_found sha256)
         | Ok true ->
             (* ファイルはあるがDBにない場合は削除を許可（orphaned file） *)
             let _ = Storage.unlink storage ~path:sha256 in
             Ok ())
    | Error e -> Error e
    | Ok _ ->
        (* 2. 所有者かどうか確認 *)
        match Db.has_owner db ~sha256 ~pubkey with
        | Error e -> Error e
        | Ok false ->
            (* 所有者でない場合は拒否 *)
            Error (Domain.Forbidden "Not authorized to delete this blob")
        | Ok true ->
        (* 3. 所有関係を削除 *)
        match Db.remove_owner db ~sha256 ~pubkey with
        | Error e -> Error e
        | Ok () ->
            (* 4. 残りの所有者数を確認 *)
            match Db.count_owners db ~sha256 with
            | Error e -> Error e
            | Ok count when count > 0 ->
                (* 他の所有者がいる場合は所有関係の削除のみで終了 *)
                Ok ()
            | Ok _ ->
                (* 所有者がいなくなった場合はblob自体を削除 *)
                (* DB論理削除 *)
                match Db.delete db ~sha256 with
                | Error e -> Error e
                | Ok () ->
                    (* ファイル物理削除 *)
                    match Storage.unlink storage ~path:sha256 with
                    | Ok () -> Ok ()
                    | Error (Domain.Blob_not_found _) ->
                        (* ファイルが既に存在しない場合は成功扱い *)
                        Ok ()
                    | Error e ->
                        (* エラーをログ出力して調査用に記録 *)
                        Eio.traceln "WARNING: Failed to unlink file %s: %s" sha256
                          (match e with
                           | Domain.Storage_error msg -> msg
                           | Domain.Forbidden msg -> msg
                           | _ -> "Unknown error");
                        Error e

  let mirror ~sw ~env ~storage ~db ~url ~uploader ~max_size =
    (* 1. リモートURLからblobをダウンロード *)
    match Http_client.fetch ~sw ~env url with
    | Error e -> Error e
    | Ok fetch_result ->
        (* 2. MIME type はバイト検査で決定される（saveで実行）
           フォールバックとして application/octet-stream を渡す
           BUD-04仕様: Content-Type/URL拡張子はバイト検査失敗時のフォールバックとして使用可能だが、
           現在はバイト検査を優先し、検出できない場合のみ octet-stream を使用 *)
        let fallback_mime_type = "application/octet-stream" in
        (* 3. 既存のsave関数を使用して保存（バイト検査はsave内で実行） *)
        save ~storage ~db ~body:fetch_result.body ~mime_type:fallback_mime_type ~uploader ~max_size
end
