(** Eioベースのファイルシステム実装 *)

open Blossom_core

(** Eio実装 *)
module Impl : Storage_intf.S with type t = Eio.Fs.dir_ty Eio.Path.t = struct
  type t = Eio.Fs.dir_ty Eio.Path.t

  (** ストリーミング保存時の内部状態 *)
  type stream_state = {
    ctx: Digestif.SHA256.ctx;
    size: int;
    first_chunk: string option;
  }

  (** Eioの例外をドメインエラーに変換するヘルパー *)
  let catch_eio_error ~path f =
    try Ok (f ())
    with
    | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
        Error (Domain.Blob_not_found path)
    | Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) ->
        Error (Domain.Storage_error "Permission denied")
    | exn ->
        Error (Domain.Storage_error (Printexc.to_string exn))

  (** サイズ超過を示す例外 *)
  exception Size_limit_exceeded of int

  (** 空ファイルのSHA-256ハッシュ（定数） *)
  let empty_sha256 = Digestif.SHA256.(to_hex (digest_string ""))

  (** ファイルの存在確認（内部用） *)
  let file_exists_internal path =
    try
      let _ = Eio.Path.stat ~follow:true path in
      Ok true
    with
    | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
        Ok false
    | Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) ->
        Error (Domain.Storage_error "Permission denied")
    | exn ->
        Error (Domain.Storage_error (Printexc.to_string exn))

  (** 空ファイルを保存する（冪等） *)
  let save_empty dir =
    let final_path = Eio.Path.(dir / empty_sha256) in
    (* 既存ファイルチェック（冪等性） *)
    match file_exists_internal final_path with
    | Error e -> Error e
    | Ok true ->
        (* 既に存在する場合は成功扱い *)
        Ok {
          Storage_intf.sha256 = empty_sha256;
          size = 0;
          first_chunk = None;
        }
    | Ok false ->
        try
          Eio.Path.with_open_out ~create:(`Or_truncate 0o644) final_path ignore;
          Ok {
            Storage_intf.sha256 = empty_sha256;
            size = 0;
            first_chunk = None;
          }
        with exn ->
          Error (Domain.Storage_error (Printexc.to_string exn))

  let save dir ~body ~max_size =
    (* Content-Length: 0 の場合は即座に空ファイル処理（ブロック回避） *)
    match Piaf.Body.length body with
    | `Fixed 0L -> save_empty dir
    | _ ->
        (* 一時ファイル名を生成 *)
        let temp_id = Printf.sprintf "tmp_%d_%d" (Random.int 1000000) (Random.int 1000000) in
        let temp_path = Eio.Path.(dir / temp_id) in

        (* 一時ファイルに書き込みながらハッシュ計算（ストリーミング） *)
        let write_result =
          try
            Ok (Eio.Path.with_open_out ~create:(`Or_truncate 0o644) temp_path (fun file ->
              let init = { ctx = Digestif.SHA256.init (); size = 0; first_chunk = None } in
              Piaf.Body.fold_string body ~init ~f:(fun state chunk ->
                let new_size = state.size + String.length chunk in
                (* サイズ制限チェック *)
                if new_size > max_size then
                  raise (Size_limit_exceeded new_size);
                (* ファイルに書き込み *)
                Eio.Flow.copy_string chunk file;
                (* 状態を更新 *)
                {
                  ctx = Digestif.SHA256.feed_string state.ctx chunk;
                  size = new_size;
                  first_chunk = if Option.is_none state.first_chunk then Some chunk else state.first_chunk;
                }
              )
            ))
          with
          | Size_limit_exceeded size ->
              (* サイズ超過時は一時ファイルを削除 *)
              (try Eio.Path.unlink temp_path with _ -> ());
              Error (Domain.Payload_too_large (size, max_size))
          | exn ->
              (* 一時ファイルを削除 *)
              (try Eio.Path.unlink temp_path with _ -> ());
              Error (Domain.Storage_error (Printexc.to_string exn))
        in

        match write_result with
        | Error e -> Error e
        | Ok (Error e) ->
            (* Piaf.Body.fold_string のエラー *)
            (try Eio.Path.unlink temp_path with _ -> ());
            Error (Domain.Storage_error (Piaf.Error.to_string e))
        | Ok (Ok state) ->
            (* 空ボディだった場合（Chunked/Unknown で空が送られたケース） *)
            if state.size = 0 then begin
              (try Eio.Path.unlink temp_path with _ -> ());
              save_empty dir
            end else
              let hash = Digestif.SHA256.(to_hex (get state.ctx)) in
              let final_path = Eio.Path.(dir / hash) in
              (* 同じハッシュのファイルが既に存在するかチェック *)
              match file_exists_internal final_path with
              | Error e ->
                  (* stat失敗時は一時ファイルを削除してエラーを返す *)
                  (try Eio.Path.unlink temp_path with _ -> ());
                  Error e
              | Ok true ->
                  (* 既存ファイルがある場合は一時ファイルを削除して成功扱い（冪等性） *)
                  (try Eio.Path.unlink temp_path with _ -> ());
                  Ok {
                    Storage_intf.sha256 = hash;
                    size = state.size;
                    first_chunk = state.first_chunk;
                  }
              | Ok false ->
                  (* 一時ファイルを最終パスにリネーム *)
                  (try
                    Eio.Path.rename temp_path final_path;
                    Ok {
                      Storage_intf.sha256 = hash;
                      size = state.size;
                      first_chunk = state.first_chunk;
                    }
                  with exn ->
                    (try Eio.Path.unlink temp_path with _ -> ());
                    Error (Domain.Storage_error (Printexc.to_string exn)))

  (** クライアントが応答を受け取らなくなったことを示す例外 *)
  exception Client_stalled of string

  (** 送信待ちのチャンクが受け取られないまま経過してよい最大秒数。
      これを超えたら接続を打ち切る *)
  let stall_timeout = 30.0

  let get_range ~sw dir ~path ~offset ~length =
    let chunk_size = 16384 in (* 16KB chunks *)
    let full_path = Eio.Path.(dir / path) in
    (* ファイルを開く（エラーを早期に検出） *)
    try
      let file = Eio.Path.open_in ~sw full_path in
      let stream, push = Piaf.Stream.create 2 in
      (* 送信キューへの追加。[stall_timeout] 秒以内に受け取られなければ false。

         クライアントが応答の途中で切断すると、Piaf 0.2.0 / httpun 0.2.0 では
         本文送信側が書き込み完了を永久に待ち続け、接続（ソケット）が解放されない。
         そのため受け取りが止まったら接続スイッチ [sw] を失敗させ、止まった
         ファイバーごと接続を片付ける（Piaf は接続ごとのスイッチの失敗を
         ログに記録して他の接続の処理を続ける） *)
      let push_or_stall item =
        Eio.Fiber.first
          (fun () -> push item; true)
          (fun () -> Eio_unix.sleep stall_timeout; false)
      in
      (* 別ファイバーでoffsetからpreadしながらストリームにプッシュ
         （offsetより前のバイトはディスクから読まない） *)
      Eio.Fiber.fork ~sw (fun () ->
        Fun.protect ~finally:(fun () -> Eio.Flow.close file) (fun () ->
          let rec read_loop pos remaining =
            if remaining <= 0 then
              push None (* ストリーム終了 *)
            else
              let to_read = min chunk_size remaining in
              let buf = Cstruct.create to_read in
              match Eio.File.pread file ~file_offset:(Optint.Int63.of_int pos) [buf] with
              | 0 -> push None
              | n ->
                  if push_or_stall (Some (Cstruct.to_string ~len:n buf)) then
                    read_loop (pos + n) (remaining - n)
                  else begin
                    Eio.traceln "Client stalled for %.0fs while streaming %s, closing connection"
                      stall_timeout path;
                    Eio.Switch.fail sw (Client_stalled path)
                  end
              | exception End_of_file -> push None
          in
          read_loop offset length
        )
      );
      Ok (Piaf.Body.of_string_stream ~length:(`Fixed (Int64.of_int length)) stream)
    with
    | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
        Error (Domain.Blob_not_found path)
    | Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) ->
        Error (Domain.Storage_error "Permission denied")
    | exn ->
        Error (Domain.Storage_error (Printexc.to_string exn))

  let get ~sw dir ~path ~size =
    get_range ~sw dir ~path ~offset:0 ~length:size

  let exists dir ~path =
    let full_path = Eio.Path.(dir / path) in
    try
      let _ = Eio.Path.stat ~follow:true full_path in
      Ok true
    with
    | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
        Ok false
    | Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) ->
        Error (Domain.Storage_error "Permission denied")
    | exn ->
        Error (Domain.Storage_error (Printexc.to_string exn))

  let stat dir ~path =
    let full_path = Eio.Path.(dir / path) in
    catch_eio_error ~path (fun () ->
      let s = Eio.Path.stat ~follow:true full_path in
      Optint.Int63.to_int s.size
    )

  let unlink dir ~path =
    let full_path = Eio.Path.(dir / path) in
    catch_eio_error ~path (fun () ->
      Eio.Path.unlink full_path
    )

  let rename dir ~src ~dst =
    let src_path = Eio.Path.(dir / src) in
    let dst_path = Eio.Path.(dir / dst) in
    catch_eio_error ~path:src (fun () ->
      Eio.Path.rename src_path dst_path
    )
end
