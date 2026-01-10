open Cmdliner

let cert_arg =
  let doc = "Path to SSL certificate" in
  Arg.(value & opt (some string) None & info ["cert"] ~doc)

let key_arg =
  let doc = "Path to SSL private key" in
  Arg.(value & opt (some string) None & info ["key"] ~doc)

let port_arg =
  let doc = "Port to listen on" in
  Arg.(value & opt int 8082 & info ["port"] ~doc)

let host_arg =
  let doc = "Host to bind to (e.g., localhost, 0.0.0.0)" in
  Arg.(value & opt string "localhost" & info ["host"] ~doc)

let base_url_arg =
  let doc = "Base URL for blob URLs in responses (e.g., https://example.com:8082)" in
  Arg.(value & opt (some string) None & info ["base-url"] ~doc)

(** Storage mode: direct (default) or reader-guarded (delays unlink while reads in progress).
    CLI argument takes precedence; falls back to BLOSSOM_STORAGE env var if not specified. *)
let storage_arg =
  let doc = "Storage mode: 'direct' for immediate file operations (default), 'reader-guarded' to delay unlink while reads are in progress (useful for CIFS/Azure Files mounts). Overrides BLOSSOM_STORAGE env var." in
  let storage_type = Arg.enum [("direct", Some false); ("reader-guarded", Some true)] in
  Arg.(value & opt storage_type None & info ["storage"] ~docv:"MODE" ~doc)

let run_server cert key port host base_url storage_opt =
  (* CLI argument takes precedence; fall back to BLOSSOM_STORAGE env var *)
  let use_reader_guard = match storage_opt with
    | Some v -> v
    | None -> Sys.getenv_opt "BLOSSOM_STORAGE" = Some "reader-guarded"
  in
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let clock = Eio.Stdenv.clock env in
      let data_dir = Eio.Path.(Eio.Stdenv.cwd env / "data") in
      let db_dir = Eio.Path.(Eio.Stdenv.cwd env / "db") in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 data_dir;
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 db_dir;

      Eio.traceln "Initializing database...";
      match Blossom_shell.Blossom_db.init ~env ~sw ~dir:db_dir with
      | Error e ->
          let msg = match e with
            | Blossom_core.Domain.Storage_error msg -> msg
            | _ -> "Unknown error"
          in
          Eio.traceln "Database initialization failed: %s" msg;
          exit 1
      | Ok db ->
          let effective_base_url = match base_url with
            | Some url -> url
            | None ->
                let scheme = if Option.is_some cert then "https" else "http" in
                Printf.sprintf "%s://localhost:%d" scheme port
          in
          let storage_mode_str = if use_reader_guard then "reader-guarded" else "direct" in
          Eio.traceln "Starting Blossom server on %s:%d (base URL: %s, storage: %s)" host port effective_base_url storage_mode_str;
          Blossom_shell.Http_server.start ~sw ~env ~port ~host ~clock ~data_dir ~db ~base_url:effective_base_url ~use_reader_guard ?cert ?key ();
          (* Keep the server running *)
          Eio.Fiber.await_cancel ()
    )
  )

let server_cmd =
  let doc = "Blossom Server - A file storage server with Nostr authentication" in
  let info = Cmd.info "blossoML" ~version:"0.1.0" ~doc in
  Cmd.v info Term.(const run_server $ cert_arg $ key_arg $ port_arg $ host_arg $ base_url_arg $ storage_arg)

let () = exit (Cmd.eval server_cmd)
