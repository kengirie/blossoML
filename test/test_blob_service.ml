(** Blob_service unit tests *)

open Blossom_core
open Blossom_shell

(** Mock Storage state for testing *)
module MockStorageState = struct
  let files : (string, int) Hashtbl.t = Hashtbl.create 16
  (* Configurable unlink behavior: None = success, Some error = fail with that error *)
  let unlink_error : Domain.error option ref = ref None
  let reset () =
    Hashtbl.clear files;
    unlink_error := None
  let add_file ~path ~size = Hashtbl.replace files path size
  let set_unlink_error err = unlink_error := Some err
  let clear_unlink_error () = unlink_error := None
end

(** Mock Storage implementation for testing *)
module MockStorage : Storage_intf.S with type t = unit = struct
  type t = unit

  let save _ ~body:_ ~max_size:_ =
    failwith "MockStorage.save not implemented"

  let get ~sw:_ _ ~path:_ ~size:_ =
    failwith "MockStorage.get not implemented"

  let exists _ ~path =
    Ok (Hashtbl.mem MockStorageState.files path)

  let stat _ ~path =
    match Hashtbl.find_opt MockStorageState.files path with
    | Some size -> Ok size
    | None -> Error (Domain.Blob_not_found path)

  let unlink _ ~path =
    match !(MockStorageState.unlink_error) with
    | Some err -> Error err
    | None ->
        if Hashtbl.mem MockStorageState.files path then begin
          Hashtbl.remove MockStorageState.files path;
          Ok ()
        end else
          Error (Domain.Blob_not_found path)

  let rename _ ~src:_ ~dst:_ =
    failwith "MockStorage.rename not implemented"
end

(** Mock DB state for testing *)
module MockDbState = struct
  let blobs : (string, Domain.blob_descriptor) Hashtbl.t = Hashtbl.create 16
  let owners : (string, string list) Hashtbl.t = Hashtbl.create 16
  (* Configurable delete behavior: None = success, Some error = fail with that error *)
  let delete_error : Domain.error option ref = ref None

  let reset () =
    Hashtbl.clear blobs;
    Hashtbl.clear owners;
    delete_error := None

  let add_blob ~sha256 ~size ~mime_type =
    Hashtbl.replace blobs sha256 {
      Domain.sha256;
      size;
      mime_type;
      uploaded = 0L;
      url = "/";
    }

  let add_owner ~sha256 ~pubkey =
    let current = match Hashtbl.find_opt owners sha256 with
      | Some list -> list
      | None -> []
    in
    if not (List.mem pubkey current) then
      Hashtbl.replace owners sha256 (pubkey :: current)

  let has_blob sha256 = Hashtbl.mem blobs sha256

  let has_owner ~sha256 ~pubkey =
    match Hashtbl.find_opt owners sha256 with
    | Some list -> List.mem pubkey list
    | None -> false

  let set_delete_error err = delete_error := Some err
  let clear_delete_error () = delete_error := None
end

(** Mock DB implementation for testing *)
module MockDb : Db_intf.S with type t = unit = struct
  type t = unit

  let save _ ~sha256 ~size ~mime_type =
    MockDbState.add_blob ~sha256 ~size ~mime_type;
    Ok ()

  let get _ ~sha256 =
    match Hashtbl.find_opt MockDbState.blobs sha256 with
    | Some desc -> Ok desc
    | None -> Error (Domain.Blob_not_found sha256)

  let delete _ ~sha256 =
    match !(MockDbState.delete_error) with
    | Some err -> Error err
    | None ->
        Hashtbl.remove MockDbState.blobs sha256;
        Ok ()

  let add_owner _ ~sha256 ~pubkey =
    let current = match Hashtbl.find_opt MockDbState.owners sha256 with
      | Some list -> list
      | None -> []
    in
    if not (List.mem pubkey current) then
      Hashtbl.replace MockDbState.owners sha256 (pubkey :: current);
    Ok ()

  let has_owner _ ~sha256 ~pubkey =
    match Hashtbl.find_opt MockDbState.owners sha256 with
    | Some list -> Ok (List.mem pubkey list)
    | None -> Ok false

  let remove_owner _ ~sha256 ~pubkey =
    match Hashtbl.find_opt MockDbState.owners sha256 with
    | Some list ->
        let filtered = List.filter (fun p -> p <> pubkey) list in
        if filtered = [] then
          Hashtbl.remove MockDbState.owners sha256
        else
          Hashtbl.replace MockDbState.owners sha256 filtered;
        Ok ()
    | None -> Ok ()

  let count_owners _ ~sha256 =
    match Hashtbl.find_opt MockDbState.owners sha256 with
    | Some list -> Ok (List.length list)
    | None -> Ok 0

  let list_owners _ ~sha256 =
    match Hashtbl.find_opt MockDbState.owners sha256 with
    | Some list -> Ok list
    | None -> Ok []
end

module BlobService = Blob_service.Make(MockStorage)(MockDb)

let test_get_returns_not_found_when_db_lacks_entry () =
  MockStorageState.reset ();
  MockDbState.reset ();
  (* Add file to storage but NOT to DB *)
  let hash = "abc123def456abc123def456abc123def456abc123def456abc123def456abc1" in
  MockStorageState.add_file ~path:hash ~size:100;
  Eio_main.run (fun _ ->
    Eio.Switch.run (fun sw ->
      match BlobService.get ~sw ~storage:() ~db:() ~sha256:hash with
      | Error (Domain.Blob_not_found h) ->
          Alcotest.(check string) "hash matches" hash h
      | Error e ->
          Alcotest.fail (Printf.sprintf "Unexpected error: %s"
            (match e with
             | Domain.Storage_error msg -> msg
             | _ -> "Unknown"))
      | Ok _ ->
          Alcotest.fail "Expected Blob_not_found but got Ok"
    )
  )

let test_get_metadata_returns_not_found_when_db_lacks_entry () =
  MockStorageState.reset ();
  MockDbState.reset ();
  (* Add file to storage but NOT to DB *)
  let hash = "abc123def456abc123def456abc123def456abc123def456abc123def456abc1" in
  MockStorageState.add_file ~path:hash ~size:100;
  match BlobService.get_metadata ~storage:() ~db:() ~sha256:hash with
  | Error (Domain.Blob_not_found h) ->
      Alcotest.(check string) "hash matches" hash h
  | Error e ->
      Alcotest.fail (Printf.sprintf "Unexpected error: %s"
        (match e with
         | Domain.Storage_error msg -> msg
         | _ -> "Unknown"))
  | Ok _ ->
      Alcotest.fail "Expected Blob_not_found but got Ok"

let test_delete_returns_not_found_when_db_lacks_entry () =
  MockStorageState.reset ();
  MockDbState.reset ();
  (* Add file to storage but NOT to DB *)
  let hash = "abc123def456abc123def456abc123def456abc123def456abc123def456abc1" in
  let pubkey = "somepubkey1234567890" in
  MockStorageState.add_file ~path:hash ~size:100;
  match BlobService.delete ~storage:() ~db:() ~sha256:hash ~pubkey with
  | Error (Domain.Blob_not_found h) ->
      Alcotest.(check string) "hash matches" hash h
  | Error e ->
      Alcotest.fail (Printf.sprintf "Unexpected error: %s"
        (match e with
         | Domain.Storage_error msg -> msg
         | Domain.Forbidden msg -> msg
         | _ -> "Unknown"))
  | Ok () ->
      Alcotest.fail "Expected Blob_not_found but got Ok"

let test_delete_restores_db_on_unlink_failure () =
  MockStorageState.reset ();
  MockDbState.reset ();
  let hash = "abc123def456abc123def456abc123def456abc123def456abc123def456abc1" in
  let pubkey = "owner_pubkey_12345" in
  (* Set up: file in storage, blob in DB with owner *)
  MockStorageState.add_file ~path:hash ~size:100;
  MockDbState.add_blob ~sha256:hash ~size:100 ~mime_type:"text/plain";
  MockDbState.add_owner ~sha256:hash ~pubkey;
  (* Configure storage to fail on unlink *)
  MockStorageState.set_unlink_error (Domain.Storage_error "Simulated unlink failure");
  (* Attempt delete *)
  match BlobService.delete ~storage:() ~db:() ~sha256:hash ~pubkey with
  | Ok () ->
      Alcotest.fail "Expected error but got Ok"
  | Error (Domain.Storage_error msg) ->
      (* Verify error message *)
      Alcotest.(check string) "error message" "Simulated unlink failure" msg;
      (* Verify DB state was restored *)
      Alcotest.(check bool) "blob restored in DB" true (MockDbState.has_blob hash);
      Alcotest.(check bool) "owner restored in DB" true (MockDbState.has_owner ~sha256:hash ~pubkey)
  | Error e ->
      Alcotest.fail (Printf.sprintf "Unexpected error type: %s"
        (match e with
         | Domain.Blob_not_found _ -> "Blob_not_found"
         | Domain.Forbidden msg -> "Forbidden: " ^ msg
         | _ -> "Unknown"))

let test_delete_succeeds_when_file_already_gone () =
  MockStorageState.reset ();
  MockDbState.reset ();
  let hash = "abc123def456abc123def456abc123def456abc123def456abc123def456abc1" in
  let pubkey = "owner_pubkey_12345" in
  (* Set up: NO file in storage, but blob in DB with owner *)
  MockDbState.add_blob ~sha256:hash ~size:100 ~mime_type:"text/plain";
  MockDbState.add_owner ~sha256:hash ~pubkey;
  (* Attempt delete - should succeed because Blob_not_found from unlink is treated as success *)
  match BlobService.delete ~storage:() ~db:() ~sha256:hash ~pubkey with
  | Ok () ->
      (* Verify DB was cleaned up *)
      Alcotest.(check bool) "blob removed from DB" false (MockDbState.has_blob hash);
      Alcotest.(check bool) "owner removed from DB" false (MockDbState.has_owner ~sha256:hash ~pubkey)
  | Error e ->
      Alcotest.fail (Printf.sprintf "Expected success but got error: %s"
        (match e with
         | Domain.Storage_error msg -> msg
         | Domain.Blob_not_found _ -> "Blob_not_found"
         | Domain.Forbidden msg -> "Forbidden: " ^ msg
         | _ -> "Unknown"))

let test_delete_succeeds_and_removes_file () =
  MockStorageState.reset ();
  MockDbState.reset ();
  let hash = "abc123def456abc123def456abc123def456abc123def456abc123def456abc1" in
  let pubkey = "owner_pubkey_12345" in
  (* Set up: file in storage, blob in DB with owner *)
  MockStorageState.add_file ~path:hash ~size:100;
  MockDbState.add_blob ~sha256:hash ~size:100 ~mime_type:"text/plain";
  MockDbState.add_owner ~sha256:hash ~pubkey;
  (* Attempt delete - should succeed *)
  match BlobService.delete ~storage:() ~db:() ~sha256:hash ~pubkey with
  | Ok () ->
      (* Verify file was removed from storage *)
      Alcotest.(check bool) "file removed from storage" false (Hashtbl.mem MockStorageState.files hash);
      (* Verify DB was cleaned up *)
      Alcotest.(check bool) "blob removed from DB" false (MockDbState.has_blob hash);
      Alcotest.(check bool) "owner removed from DB" false (MockDbState.has_owner ~sha256:hash ~pubkey)
  | Error e ->
      Alcotest.fail (Printf.sprintf "Expected success but got error: %s"
        (match e with
         | Domain.Storage_error msg -> msg
         | Domain.Blob_not_found _ -> "Blob_not_found"
         | Domain.Forbidden msg -> "Forbidden: " ^ msg
         | _ -> "Unknown"))

let test_delete_restores_owner_on_db_delete_failure () =
  MockStorageState.reset ();
  MockDbState.reset ();
  let hash = "abc123def456abc123def456abc123def456abc123def456abc123def456abc1" in
  let pubkey = "owner_pubkey_12345" in
  (* Set up: file in storage, blob in DB with owner *)
  MockStorageState.add_file ~path:hash ~size:100;
  MockDbState.add_blob ~sha256:hash ~size:100 ~mime_type:"text/plain";
  MockDbState.add_owner ~sha256:hash ~pubkey;
  (* Configure DB to fail on delete *)
  MockDbState.set_delete_error (Domain.Storage_error "Simulated DB delete failure");
  (* Attempt delete *)
  match BlobService.delete ~storage:() ~db:() ~sha256:hash ~pubkey with
  | Ok () ->
      Alcotest.fail "Expected error but got Ok"
  | Error (Domain.Storage_error msg) ->
      (* Verify error message *)
      Alcotest.(check string) "error message" "Simulated DB delete failure" msg;
      (* Verify owner was restored (blob metadata is never removed since Db.delete failed) *)
      Alcotest.(check bool) "blob still in DB" true (MockDbState.has_blob hash);
      Alcotest.(check bool) "owner restored in DB" true (MockDbState.has_owner ~sha256:hash ~pubkey);
      (* Verify file was NOT unlinked *)
      Alcotest.(check bool) "file still in storage" true (Hashtbl.mem MockStorageState.files hash)
  | Error e ->
      Alcotest.fail (Printf.sprintf "Unexpected error type: %s"
        (match e with
         | Domain.Blob_not_found _ -> "Blob_not_found"
         | Domain.Forbidden msg -> "Forbidden: " ^ msg
         | _ -> "Unknown"))

let tests = [
  "get returns Blob_not_found when DB lacks entry", `Quick, test_get_returns_not_found_when_db_lacks_entry;
  "get_metadata returns Blob_not_found when DB lacks entry", `Quick, test_get_metadata_returns_not_found_when_db_lacks_entry;
  "delete returns Blob_not_found when DB lacks entry", `Quick, test_delete_returns_not_found_when_db_lacks_entry;
  "delete restores DB on unlink failure", `Quick, test_delete_restores_db_on_unlink_failure;
  "delete succeeds when file already gone", `Quick, test_delete_succeeds_when_file_already_gone;
  "delete succeeds and removes file", `Quick, test_delete_succeeds_and_removes_file;
  "delete restores owner on DB delete failure", `Quick, test_delete_restores_owner_on_db_delete_failure;
]
