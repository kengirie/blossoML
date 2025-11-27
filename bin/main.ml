let () =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let port = 8082 in
      let dir = Eio.Path.(Eio.Stdenv.cwd env / "data") in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dir;

      Printf.printf "Initializing database...\n%!";
      match Blossom_shell.Blossom_db.init ~env ~sw ~dir with
      | Error (Blossom_core.Domain.Storage_error msg) ->
          Printf.eprintf "Database initialization failed: %s\n%!" msg;
          exit 1
      | Error _ ->
          Printf.eprintf "Database initialization failed: Unknown error\n%!";
          exit 1
      | Ok db ->
          Printf.printf "Starting Blossom server on port %d\n%!" port;
          Blossom_shell.Http_server.start ~sw ~env ~port ~dir ~db;
          (* Keep the server running *)
          Eio.Fiber.await_cancel ()
    )
  )
