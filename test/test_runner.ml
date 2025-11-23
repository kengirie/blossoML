let () =
  Alcotest.run "Blossom Core" [
    "Integrity", Test_integrity.tests;
    "Policy", Test_policy.tests;
  ]
