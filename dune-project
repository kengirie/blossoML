(lang dune 3.20)

(name blossoML)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name blossoML)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  piaf
  eio
  eio_main
  digestif
  checkseum
  alcotest
  angstrom
  conan
  conan-database
  yojson
  base64
  ctypes
  ctypes-foreign
  secp256k1-internal
  caqti
  caqti-driver-sqlite3
  caqti-eio)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
