(** E2E test configuration *)

let base_url =
  match Sys.getenv_opt "E2E_BASE_URL" with
  | Some url -> url
  | None -> "http://localhost:8082"

(** External Blossom blob URL for mirror happy-path tests.
    Example: E2E_MIRROR_URL=https://blossom.example.com/<sha256>
    When unset, mirror happy-path tests are skipped. *)
let mirror_url = Sys.getenv_opt "E2E_MIRROR_URL"

let timeout_seconds = 30.0
