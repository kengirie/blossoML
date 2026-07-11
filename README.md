# blossoML

blossoML is yet another [Blossom](https://github.com/hzrd149/blossom) server implementation written in OCaml.

### Endpoints

- [x] `GET /<sha256>` - Get blob ([BUD-01](https://github.com/hzrd149/blossom/blob/master/buds/01.md))
- [x] `HEAD /<sha256>` - Has blob ([BUD-01](https://github.com/hzrd149/blossom/blob/master/buds/01.md))
- [x] `PUT /upload` - Upload blob ([BUD-02](https://github.com/hzrd149/blossom/blob/master/buds/02.md))
- [x] `HEAD /upload` - Upload requirements ([BUD-06](https://github.com/hzrd149/blossom/blob/master/buds/06.md))
- [x] `PUT /mirror` - Mirror blob ([BUD-04](https://github.com/hzrd149/blossom/blob/master/buds/04.md))
- [x] `DELETE /<sha256>` - Delete blob ([BUD-12](https://github.com/hzrd149/blossom/blob/master/buds/12.md))
- [x] `GET /list/<pubkey>` - List blobs ([BUD-12](https://github.com/hzrd149/blossom/blob/master/buds/12.md)) _(unrecommended)_
- [ ] `HEAD /media` - Media optimization info ([BUD-05](https://github.com/hzrd149/blossom/blob/master/buds/05.md))
- [ ] `PUT /media` - Media optimization ([BUD-05](https://github.com/hzrd149/blossom/blob/master/buds/05.md))
- [x] `PUT /report` - Blob report ([BUD-09](https://github.com/hzrd149/blossom/blob/master/buds/09.md))

### Other BUDs

- [x] Nostr Authorization (`kind:24242`) ([BUD-11](https://github.com/hzrd149/blossom/blob/master/buds/11.md))
- [ ] Nostr File Metadata Tags (`nip94` in blob descriptor) ([BUD-08](https://github.com/hzrd149/blossom/blob/master/buds/08.md))
- [ ] Payment Required (`402` + `X-Cashu` / `X-Lightning`) ([BUD-07](https://github.com/hzrd149/blossom/blob/master/buds/07.md))

BUD-03 (User Server List) and BUD-10 (Blossom URI Schema) are client-side specifications and do not apply to server implementations.

## Technology Stack

The OCaml libraries that blossoML depends on:

- HTTP server: [Piaf](https://github.com/anmonteiro/piaf)
- Concurrent I/O: [Eio](https://github.com/ocaml-multicore/eio)
- Database: SQLite via [Caqti](https://github.com/paurkedal/ocaml-caqti)
- Hashing: [Digestif](https://github.com/mirage/digestif) for SHA256
- BIP-340 Schnorr signature verification: [secp256k1](https://github.com/dakk/secp256k1-ml)
- MIME detection: [Conan](https://github.com/mirage/conan)

## Quick Start

### Prerequisites

- OCaml 5.x
- opam
- libsecp256k1

### Installation

```bash
# Install dependencies
opam install . --deps-only

# Build
dune build

# Run
dune exec blossoML -- --port 8080
```

### Command Line Options

```
--host      Host to bind to (default: localhost)
            Use "0.0.0.0" for external access
--port      Port to listen on
--cert      Path to SSL certificate (optional)
--key       Path to SSL private key (optional)
--base-url  Base URL for blob URLs in responses
            (e.g., https://example.com:8080)
```

### Examples

```bash
# Local development
dune exec blossoML -- --host localhost --port 8080

```

## How to Run Tests

```bash
dune test
```

## Acknowledgements
This project was inspired by [Piaf](https://github.com/anmonteiro/piaf) and [Waq](https://github.com/ushitora-anqou/waq).
## License

MIT.
