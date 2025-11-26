# Repository Guidelines

## Project Structure & Module Organization
The OCaml sources live under `lib/`, split between `core/` (policy, auth, BIP-340, integrity logic) and `shell/` (HTTP server and storage adapters). The HTTP executable lands in `bin/main.ml`, while reusable test helpers and suites sit in `test/`. Scripts for end-to-end checks (`test_phase*.sh`, `test_cors.sh`, `test_auth_integration.sh`) and sample payloads in `data/` support manual verification.

## Build, Test, and Development Commands
- `opam install . --deps-only` — install compiler switches and library dependencies declared in `ocaml-nostr-blossom.opam`.
- `dune build` — compile the server and libraries, producing `_build/default/bin/main.exe`.
- `dune exec bin/main.exe` — run the HTTP server locally with in-memory storage.
- `dune runtest` — execute all Alcotest suites in `test/`.

## Coding Style & Naming Conventions
Use OCaml’s conventional two-space indentation and keep modules in `UpperCamelCase` (e.g., `Blossom_core`), values/functions in `snake_case`, and constructors in `CamelCase`. Prefer small, pure functions in `lib/core` and isolate side effects inside `lib/shell`. When touching HTTP code, keep header constants and route names descriptive (`"X-Reason"`, `upload_endpoint`). Run `dune fmt` (ocamlformat) before committing when available.

## Testing Guidelines
Unit tests rely on Alcotest; add suites by extending `test/test_runner.ml`. Follow the naming pattern `test_<module>.ml` and expose a `tests` list of `test_case`s. Quick checks belong in `` `Quick `` mode, while property-style or I/O heavy scenarios should use `` `Slow ``. For integration regressions, duplicate the existing shell scripts, keep filenames prefixed with `test_`, and document prerequisites at the top of the script. Aim to keep new logic covered by at least one Alcotest assertion or shell script path.

## Commit & Pull Request Guidelines
History shows Conventional-style messages (`feat:`, `fix:`, `chore:`). Keep subject lines imperative and under ~72 characters, then provide context in the body if behavior or APIs changed. Pull requests should link to the relevant design doc (`architecture_design.md`, `implementation_plan.md`) or issue, describe user-visible changes, list manual test commands (`dune runtest`, script invocations), and include screenshots/log excerpts when modifying HTTP responses.

## Security & Configuration Tips
Keep policy changes centralized in `lib/core/policy.ml`; explain new limits or cap-table rules in the PR text. Never hard-code secrets—`local_storage` is intended for dev only. When running scripts that touch the filesystem, target the `data/` directory so artifacts stay contained and out of version control.
