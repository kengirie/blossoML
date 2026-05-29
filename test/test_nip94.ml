open Alcotest
open Blossom_core

let sample_descriptor : Domain.blob_descriptor = {
  url = "https://cdn.example.com/abcdef";
  sha256 = "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789";
  size = 184292;
  mime_type = "application/pdf";
  uploaded = 1725909682L;
}

let find_tag tags key =
  match List.assoc_opt key tags with
  | Some v -> v
  | None -> Alcotest.failf "missing %s tag" key

let test_tags_basic () =
  let tags = Nip94.tags_of_descriptor sample_descriptor in
  check int "tag count" 5 (List.length tags);
  check string "url tag" sample_descriptor.url (find_tag tags "url");
  check string "m tag" sample_descriptor.mime_type (find_tag tags "m");
  check string "x tag" sample_descriptor.sha256 (find_tag tags "x");
  check string "ox tag (= x for untransformed blobs)" sample_descriptor.sha256 (find_tag tags "ox");
  check string "size tag" "184292" (find_tag tags "size")

let test_tags_size_is_string () =
  let d = { sample_descriptor with size = 0 } in
  let tags = Nip94.tags_of_descriptor d in
  check string "size 0 stringified" "0" (find_tag tags "size")

let tests = [
  test_case "tags_of_descriptor returns url/m/x/size" `Quick test_tags_basic;
  test_case "size value is stringified" `Quick test_tags_size_is_string;
]
