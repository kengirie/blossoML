(** E2E tests for CORS functionality. *)

(** Helper to get header value (case-insensitive) *)
let get_header headers name =
  let name_lower = String.lowercase_ascii name in
  List.find_opt (fun (k, _) -> String.lowercase_ascii k = name_lower) headers
  |> Option.map snd

(** Helper to assert header exists with expected value *)
let assert_header headers name expected =
  match get_header headers name with
  | None -> failwith (Printf.sprintf "Missing header: %s" name)
  | Some v when v <> expected ->
      failwith (Printf.sprintf "Header %s: expected '%s', got '%s'" name expected v)
  | Some _ -> ()

(** Helper to assert header exists (any value) *)
let assert_header_exists headers name =
  match get_header headers name with
  | None -> failwith (Printf.sprintf "Missing header: %s" name)
  | Some _ -> ()

(** Test: OPTIONS /upload returns 204 with CORS headers *)
let test_options_upload ~sw ~env =
  let base_url = Config.base_url in
  let url = base_url ^ "/upload" in

  let result = Http_client.options ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("OPTIONS request failed: " ^ e)
  | Ok response ->
      if response.status <> 204 then
        failwith (Printf.sprintf "Expected 204, got %d" response.status);

      (* Verify CORS headers *)
      assert_header response.headers "access-control-allow-origin" "*";
      assert_header response.headers "access-control-allow-methods" "*";
      assert_header_exists response.headers "access-control-allow-headers";
      assert_header response.headers "access-control-expose-headers" "*";
      assert_header response.headers "access-control-max-age" "86400"

(** Test: OPTIONS on blob path returns 204 with CORS headers *)
let test_options_blob_path ~sw ~env =
  let base_url = Config.base_url in
  let fake_hash = "0000000000000000000000000000000000000000000000000000000000000000" in
  let url = base_url ^ "/" ^ fake_hash in

  let result = Http_client.options ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("OPTIONS request failed: " ^ e)
  | Ok response ->
      if response.status <> 204 then
        failwith (Printf.sprintf "Expected 204, got %d" response.status);

      (* Verify CORS headers *)
      assert_header response.headers "access-control-allow-origin" "*"

(** Test: GET response includes CORS headers *)
let test_get_includes_cors_headers ~sw ~env =
  let base_url = Config.base_url in
  let fake_hash = "0000000000000000000000000000000000000000000000000000000000000000" in
  let url = base_url ^ "/" ^ fake_hash in

  let result = Http_client.get ~sw ~env ~url () in

  match result with
  | Error e -> failwith ("GET request failed: " ^ e)
  | Ok response ->
      (* Even 404 responses should have CORS headers *)
      assert_header response.headers "access-control-allow-origin" "*";
      assert_header response.headers "access-control-expose-headers" "*"

(** Test: Error responses include CORS headers *)
let test_error_response_includes_cors_headers ~sw ~env =
  let base_url = Config.base_url in
  let url = base_url ^ "/upload" in

  (* PUT without auth should return 401, but still with CORS headers *)
  let result = Http_client.put ~sw ~env ~url ~body:"test" () in

  match result with
  | Error e -> failwith ("PUT request failed: " ^ e)
  | Ok response ->
      if response.status <> 401 then
        failwith (Printf.sprintf "Expected 401, got %d" response.status);

      (* Error responses should also have CORS headers *)
      assert_header response.headers "access-control-allow-origin" "*";
      assert_header response.headers "access-control-expose-headers" "*"

(** Test: Preflight with custom headers *)
let test_preflight_with_custom_headers ~sw ~env =
  let base_url = Config.base_url in
  let url = base_url ^ "/upload" in

  (* Simulate browser preflight with custom headers *)
  let headers = [
    ("Access-Control-Request-Method", "PUT");
    ("Access-Control-Request-Headers", "Authorization, Content-Type");
    ("Origin", "http://example.com");
  ] in

  let result = Http_client.options ~sw ~env ~url ~headers () in

  match result with
  | Error e -> failwith ("OPTIONS request failed: " ^ e)
  | Ok response ->
      if response.status <> 204 then
        failwith (Printf.sprintf "Expected 204, got %d" response.status);

      (* Should allow the requested headers *)
      assert_header_exists response.headers "access-control-allow-headers"

(** All tests *)
let tests = [
  ("OPTIONS /upload returns 204 with CORS headers", test_options_upload);
  ("OPTIONS on blob path returns 204", test_options_blob_path);
  ("GET response includes CORS headers", test_get_includes_cors_headers);
  ("Error response includes CORS headers", test_error_response_includes_cors_headers);
  ("Preflight with custom headers", test_preflight_with_custom_headers);
]
