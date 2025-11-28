let () =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let port = 8082 in
      let clock = Eio.Stdenv.clock env in
      let dir = Eio.Path.(Eio.Stdenv.cwd env / "data") in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dir;

      Eio.traceln "Initializing database...";
      match Blossom_shell.Blossom_db.init ~env ~sw ~dir with
      | Error (Blossom_core.Domain.Storage_error msg) ->
          Eio.traceln "Database initialization failed: %s" msg;
          exit 1
      | Error _ ->
          Eio.traceln "Database initialization failed: Unknown error";
          exit 1
      | Ok db ->
          Eio.traceln "Starting Blossom server on port %d" port;
          Blossom_shell.Http_server.start ~sw ~env ~port ~clock ~dir ~db;
          (* Keep the server running *)
          Eio.Fiber.await_cancel ()
    )
  )
