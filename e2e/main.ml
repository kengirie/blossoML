(** E2E test runner for Blossom server. *)

let run_test ~sw ~env (name, test_fn) =
  Eio.traceln "  [RUN] %s... @?" name;
  try
    test_fn ~sw ~env;
    Eio.traceln "\027[32mOK\027[0m"
  with e ->
    Eio.traceln "\027[31mFAILED\027[0m\n    %s" (Printexc.to_string e);
    raise e

let () =
  Random.self_init ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  Eio.traceln "E2E Tests for Blossom Server";
  Eio.traceln "Base URL: %s\n" E2e.Config.base_url;

  Eio.traceln "Upload Tests:";
  List.iter (run_test ~sw ~env) E2e.Test_upload.tests;

  Eio.traceln "\nGET/HEAD Tests:";
  List.iter (run_test ~sw ~env) E2e.Test_get.tests;

  Eio.traceln "\nDelete Tests:";
  List.iter (run_test ~sw ~env) E2e.Test_delete.tests;

  Eio.traceln "\nCORS Tests:";
  List.iter (run_test ~sw ~env) E2e.Test_cors.tests;

  Eio.traceln "\nHEAD /upload Tests (BUD-06):";
  List.iter (run_test ~sw ~env) E2e.Test_head_upload.tests;

  Eio.traceln "\nMirror Tests (BUD-04):";
  List.iter (run_test ~sw ~env) E2e.Test_mirror.tests;

  Eio.traceln "\nDone."
