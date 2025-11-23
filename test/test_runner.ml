let () =
  Alcotest.run "Blossom Core" [
    "Integrity", Test_integrity.tests;
    "Policy", Test_policy.tests;
    "Auth", Test_auth.tests;
    "BIP340", Test_bip340.tests;
  ]
