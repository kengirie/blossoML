open Blossom_core

module Db = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let create_blobs_table =
    (unit ->. unit)
    @@ {sql|
      CREATE TABLE IF NOT EXISTS blobs (
        sha256 TEXT(64) PRIMARY KEY,
        uploaded_at INTEGER NOT NULL,
        status TEXT NOT NULL DEFAULT 'stored' CHECK (status IN ('stored','deleted','quarantined')),
        remote_url TEXT,
        mime_type TEXT,
        size INTEGER
      )
    |sql}

  let create_blobs_uploaded_at_index =
    (unit ->. unit)
    @@ {sql|
      CREATE INDEX IF NOT EXISTS blobs_uploaded_at ON blobs(uploaded_at)
    |sql}

  let create_blob_owners_table =
    (unit ->. unit)
    @@ {sql|
      CREATE TABLE IF NOT EXISTS blob_owners (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        sha256 TEXT(64) NOT NULL,
        pubkey TEXT(64) NOT NULL,
        added_at INTEGER NOT NULL,
        UNIQUE(sha256, pubkey),
        FOREIGN KEY (sha256) REFERENCES blobs(sha256)
      )
    |sql}

  let create_blob_owners_sha256_index =
    (unit ->. unit)
    @@ {sql|
      CREATE INDEX IF NOT EXISTS blob_owners_sha256 ON blob_owners(sha256)
    |sql}

  let create_blob_owners_pubkey_index =
    (unit ->. unit)
    @@ {sql|
      CREATE INDEX IF NOT EXISTS blob_owners_pubkey ON blob_owners(pubkey)
    |sql}

  let save_blob =
    (t3 string (option string) (option int64) ->. unit)
    @@ {sql|
      INSERT INTO blobs (sha256, uploaded_at, mime_type, size)
      VALUES ($1, strftime('%s', 'now'), $2, $3)
      ON CONFLICT(sha256) DO NOTHING
    |sql}

  let get_blob =
    (string ->? t4 string int64 (option string) (option int64))
    @@ {sql|
      SELECT sha256, uploaded_at, mime_type, size
      FROM blobs
      WHERE sha256 = $1 AND status = 'stored'
    |sql}

  let delete_blob =
    (string ->. unit)
    @@ {sql|
      UPDATE blobs SET status = 'deleted' WHERE sha256 = $1
    |sql}

  let add_owner =
    (t2 string string ->. unit)
    @@ {sql|
      INSERT INTO blob_owners (sha256, pubkey, added_at)
      VALUES ($1, $2, strftime('%s', 'now'))
      ON CONFLICT(sha256, pubkey) DO NOTHING
    |sql}

  let has_owner =
    (t2 string string ->? int)
    @@ {sql|
      SELECT 1 FROM blob_owners WHERE sha256 = $1 AND pubkey = $2
    |sql}

  let remove_owner =
    (t2 string string ->. unit)
    @@ {sql|
      DELETE FROM blob_owners WHERE sha256 = $1 AND pubkey = $2
    |sql}

  let count_owners =
    (string ->! int)
    @@ {sql|
      SELECT COUNT(*) FROM blob_owners WHERE sha256 = $1
    |sql}

  let list_owners =
    (string ->* string)
    @@ {sql|
      SELECT pubkey FROM blob_owners WHERE sha256 = $1
    |sql}
end

type t = (Caqti_eio.connection, Caqti_error.t) Caqti_eio.Pool.t

let init ~env ~sw ~dir =
  let db_path = Eio.Path.(dir / "blossom.db") in
  let uri = Uri.of_string ("sqlite3:" ^ (Eio.Path.native_exn db_path)) in

  match Caqti_eio_unix.connect_pool ~sw ~stdenv:(env :> Caqti_eio.stdenv) uri with
  | Error e -> Error (Domain.Storage_error (Caqti_error.show e))
  | Ok pool ->
      let init_result =
        Caqti_eio.Pool.use (fun (module C : Caqti_eio.CONNECTION) ->
          Result.bind (C.exec Db.create_blobs_table ()) @@ fun () ->
          Result.bind (C.exec Db.create_blobs_uploaded_at_index ()) @@ fun () ->
          Result.bind (C.exec Db.create_blob_owners_table ()) @@ fun () ->
          Result.bind (C.exec Db.create_blob_owners_sha256_index ()) @@ fun () ->
          C.exec Db.create_blob_owners_pubkey_index ()
        ) pool
      in
      match init_result with
      | Ok () -> Ok pool
      | Error e -> Error (Domain.Storage_error (Caqti_error.show e))

let save (pool : t) ~sha256 ~size ~mime_type =
  let result =
    Caqti_eio.Pool.use (fun (module C : Caqti_eio.CONNECTION) ->
      C.exec Db.save_blob (sha256, Some mime_type, Some (Int64.of_int size))
    ) pool
  in
  match result with
  | Ok () -> Ok ()
  | Error e -> Error (Domain.Storage_error (Caqti_error.show e))

let get (pool : t) ~sha256 =
  let result =
    Caqti_eio.Pool.use (fun (module C : Caqti_eio.CONNECTION) ->
      C.find_opt Db.get_blob sha256
    ) pool
  in
  match result with
  | Ok (Some (sha, uploaded_at, mime, size)) ->
      Ok {
        Domain.sha256 = sha;
        size = Option.value ~default:0 (Option.map Int64.to_int size);
        mime_type = Option.value ~default:"application/octet-stream" mime;
        uploaded = uploaded_at;
        url = "/"; (* URL construction is handled by Http_server *)
      }
  | Ok None -> Error (Domain.Blob_not_found sha256)
  | Error e -> Error (Domain.Storage_error (Caqti_error.show e))

let delete (pool : t) ~sha256 =
  let result =
    Caqti_eio.Pool.use (fun (module C : Caqti_eio.CONNECTION) ->
      C.exec Db.delete_blob sha256
    ) pool
  in
  match result with
  | Ok () -> Ok ()
  | Error e -> Error (Domain.Storage_error (Caqti_error.show e))

let add_owner (pool : t) ~sha256 ~pubkey =
  let result =
    Caqti_eio.Pool.use (fun (module C : Caqti_eio.CONNECTION) ->
      C.exec Db.add_owner (sha256, pubkey)
    ) pool
  in
  match result with
  | Ok () -> Ok ()
  | Error e -> Error (Domain.Storage_error (Caqti_error.show e))

let has_owner (pool : t) ~sha256 ~pubkey =
  let result =
    Caqti_eio.Pool.use (fun (module C : Caqti_eio.CONNECTION) ->
      C.find_opt Db.has_owner (sha256, pubkey)
    ) pool
  in
  match result with
  | Ok (Some _) -> Ok true
  | Ok None -> Ok false
  | Error e -> Error (Domain.Storage_error (Caqti_error.show e))

let remove_owner (pool : t) ~sha256 ~pubkey =
  let result =
    Caqti_eio.Pool.use (fun (module C : Caqti_eio.CONNECTION) ->
      C.exec Db.remove_owner (sha256, pubkey)
    ) pool
  in
  match result with
  | Ok () -> Ok ()
  | Error e -> Error (Domain.Storage_error (Caqti_error.show e))

let count_owners (pool : t) ~sha256 =
  let result =
    Caqti_eio.Pool.use (fun (module C : Caqti_eio.CONNECTION) ->
      C.find Db.count_owners sha256
    ) pool
  in
  match result with
  | Ok count -> Ok count
  | Error e -> Error (Domain.Storage_error (Caqti_error.show e))

let list_owners (pool : t) ~sha256 =
  let result =
    Caqti_eio.Pool.use (fun (module C : Caqti_eio.CONNECTION) ->
      C.collect_list Db.list_owners sha256
    ) pool
  in
  match result with
  | Ok owners -> Ok owners
  | Error e -> Error (Domain.Storage_error (Caqti_error.show e))

(** Db_intf.S を満たすモジュール *)
module Impl : Db_intf.S with type t = t = struct
  type nonrec t = t
  let save = save
  let get = get
  let delete = delete
  let add_owner = add_owner
  let has_owner = has_owner
  let remove_owner = remove_owner
  let count_owners = count_owners
  let list_owners = list_owners
end
