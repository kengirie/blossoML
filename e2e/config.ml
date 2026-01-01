(** E2E test configuration *)

let base_url =
  match Sys.getenv_opt "E2E_BASE_URL" with
  | Some url -> url
  | None -> "http://localhost:8082"

let timeout_seconds = 30.0
