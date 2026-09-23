let () =
  Alcotest.run "Mollymawk API Function & Data Format Tests"
    [
      ("Auth & Registration", Test_auth.tests);
      ("User Administration", Test_admin.tests);
      ("API Tokens", Test_tokens.tests);
      ("Unikernel Operations", Test_unikernel.tests);
      ("Layouts and Views", Test_views.tests);
      ("Block Operations", Test_block.tests);
      ("Albatross Operations", Test_albatross.tests);
      ("Monitoring & Scaling", Test_scaling.tests);
      ("Email Operations", Test_email.tests);
      ("Policy Operations", Test_policy.tests);
    ]
