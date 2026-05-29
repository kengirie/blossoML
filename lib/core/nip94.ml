(** NIP-94 File Metadata tag generation (BUD-08).

    Pure logic that converts a [Domain.blob_descriptor] into the KV pairs
    described in https://github.com/nostr-protocol/nips/blob/master/94.md.

    Only fields the server can derive from the blob itself are emitted:
    [url], [m] (mime), [x] (sha256), [ox] (original sha256), [size].

    [ox] is set to the same value as [x] because this server stores blobs
    verbatim and never transforms them. *)

let tags_of_descriptor (d : Domain.blob_descriptor) : (string * string) list =
  [
    ("url", d.url);
    ("m", d.mime_type);
    ("x", d.sha256);
    ("ox", d.sha256);
    ("size", string_of_int d.size);
  ]
