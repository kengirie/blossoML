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

  (** Blobの一部を取得する（BUD-01 Rangeリクエスト用）
      offset/lengthは呼び出し側で検証済みであること（Range.parseを使用） *)
  val get_range :
    sw:Eio.Switch.t ->
    storage:storage ->
    db:db ->
    sha256:string ->
    offset:int ->
    length:int ->
    (Piaf.Body.t, Domain.error) result

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

  open Syntax

  (** ファイルの存在確認。存在しない場合はBlob_not_foundを返す *)
  let ensure_file_exists storage ~sha256 =
    let* exists = Storage.exists storage ~path:sha256 in
    if exists then Ok () else Error (Domain.Blob_not_found sha256)

  (** DBにない場合のフォールバック: ファイルがあればstatからdescriptorを合成（後方互換性） *)
  let fallback_descriptor storage ~sha256 =
    let* () = ensure_file_exists storage ~sha256 in
    let* size = Storage.stat storage ~path:sha256 in
    Ok {
      Domain.sha256 = sha256;
      size;
      mime_type = "application/octet-stream";
      uploaded = 0L;
      url = "/";
    }

  let save ~storage ~db ~body ~mime_type:fallback_mime_type ~uploader ~max_size =
    (* ストレージに保存（サイズ制限付き） *)
    let* result = Storage.save storage ~body ~max_size in
    (* MIME type は常にバイト検査を優先し、検出できない場合のみフォールバックを使用 *)
    let final_mime_type =
      Option.bind result.first_chunk Mime_detect.detect_from_bytes
      |> Option.value ~default:fallback_mime_type
    in
    (* DB操作失敗時はファイルも削除してエラーを返す *)
    let cleanup_on_error = function
      | Ok v -> Ok v
      | Error e ->
          let _ = Storage.unlink storage ~path:result.sha256 in
          Error e
    in
    (* DBにメタデータを保存（既存blobの場合は何もしない） *)
    let* () =
      cleanup_on_error
        (Db.save db ~sha256:result.sha256 ~size:result.size ~mime_type:final_mime_type)
    in
    (* 所有者を追加（重複時は何もしない） *)
    let* () = cleanup_on_error (Db.add_owner db ~sha256:result.sha256 ~pubkey:uploader) in
    Ok (result.sha256, result.size, final_mime_type)

  let get_metadata ~storage ~db ~sha256 =
    match Db.get db ~sha256 with
    | Ok metadata ->
        let* () = ensure_file_exists storage ~sha256 in
        Ok metadata
    | Error (Domain.Blob_not_found _) ->
        (* DBにない場合もファイルがあればメタデータを返す（後方互換性） *)
        fallback_descriptor storage ~sha256
    | Error e -> Error e

  let get ~sw ~storage ~db ~sha256 =
    (* メタデータ取得（ファイルの存在確認込み） → ストリーミング返却 *)
    let* metadata = get_metadata ~storage ~db ~sha256 in
    let* body = Storage.get ~sw storage ~path:sha256 ~size:metadata.Domain.size in
    Ok (body, metadata)

  let get_range ~sw ~storage ~db:_ ~sha256 ~offset ~length =
    (* メタデータ検証（Range境界の検証）は呼び出し側でget_metadata + Range.parseにより実施済み。
       ここではファイルの存在確認とストリーミング取得のみ行う *)
    let* () = ensure_file_exists storage ~sha256 in
    Storage.get_range ~sw storage ~path:sha256 ~offset ~length

  let delete ~storage ~db ~sha256 ~pubkey =
    (* 1. まずBlobが存在するか確認（存在しない場合は404） *)
    match Db.get db ~sha256 with
    | Error (Domain.Blob_not_found _) ->
        (* DBにない場合もファイルがあるか確認（後方互換性）
           ファイルはあるがDBにない場合は削除を許可（orphaned file） *)
        let* () = ensure_file_exists storage ~sha256 in
        let _ = Storage.unlink storage ~path:sha256 in
        Ok ()
    | Error e -> Error e
    | Ok _ ->
        (* 2. 所有者かどうか確認（所有者でない場合は拒否） *)
        let* is_owner = Db.has_owner db ~sha256 ~pubkey in
        let* () =
          if is_owner then Ok ()
          else Error (Domain.Forbidden "Not authorized to delete this blob")
        in
        (* 3. 所有関係を削除 *)
        let* () = Db.remove_owner db ~sha256 ~pubkey in
        (* 4. 残りの所有者数を確認 *)
        let* count = Db.count_owners db ~sha256 in
        if count > 0 then
          (* 他の所有者がいる場合は所有関係の削除のみで終了 *)
          Ok ()
        else begin
          (* 所有者がいなくなった場合はblob自体を削除（DB論理削除 → ファイル物理削除） *)
          let* () = Db.delete db ~sha256 in
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
        end

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
