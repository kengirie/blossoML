# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

blossoML is a Blossom server implementation in OCaml. Blossom is a file storage protocol that uses Nostr for authentication via BIP-340 Schnorr signatures.

## Build Commands

```bash
opam install . --deps-only    # Install dependencies
dune build                     # Build the project
dune exec blossoML -- --port 8082  # Run the server
```

Run unit tests (requires libsecp256k1):
```bash
export DYLD_INSERT_LIBRARIES=/opt/homebrew/opt/secp256k1/lib/libsecp256k1.dylib && eval $(opam env) && dune runtest
```

To run E2E tests (requires running server on localhost:8082):
```bash
dune exec e2e/main.exe
```

## Architecture

The codebase follows a core/shell architecture pattern:

**lib/core/** - Pure domain logic, no I/O side effects:
- `bip340.ml` - BIP-340 Schnorr signature verification using libsecp256k1
- `nostr_event.ml` - Nostr event parsing and validation
- `auth.ml` - Authentication logic for Blossom protocol
- `policy.ml` - Access control policies (allowed pubkeys, size limits)
- `integrity.ml` - SHA256 blob integrity checking
- `mime_detect.ml` - MIME type detection using Conan
- `content_type.ml` - Content-Type header parsing
- `domain.ml` - Core domain types and errors

**lib/shell/** - I/O and infrastructure:
- `http_server.ml` - Piaf HTTP server with Blossom endpoints
- `http_response.ml` - HTTP response builders
- `blob_service.ml` - Blob upload/download orchestration
- `storage_eio.ml` - Filesystem storage using Eio
- `blossom_db.ml` - SQLite database via Caqti
- `db_intf.ml`, `storage_intf.ml` - Interfaces

**bin/main.ml** - Server entry point with Cmdliner CLI

**test/** - Alcotest unit tests (pattern: `test_<module>.ml`)

**e2e/** - End-to-end integration tests

## Testing Guidelines

Unit tests use Alcotest. Add new test suites by:
1. Creating `test/test_<module>.ml` with a `tests` list
2. Adding the module to `test/dune` modules list
3. Registering in `test/test_runner.ml`

## Coding Conventions

- Two-space indentation
- Modules: `UpperCamelCase`, values/functions: `snake_case`
- Keep pure logic in `lib/core`, side effects in `lib/shell`
- Policy changes should be centralized in `lib/core/policy.ml`
- Commit messages: Conventional style, one line (`feat:`, `fix:`, `chore:`)
