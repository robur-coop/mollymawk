let () =
  Alcotest.run "Mollymawk API Function & Data Format Tests"
    [
      ("User Administration", Test_admin.tests);
      ("Middleware Cookies", Test_middleware.tests);
      ("User Authentication", Test_auth.tests);
      ("Views", Test_views.tests);
      ("Unikernel", Test_unikernel.tests);
      ("Block Devices", Test_block.tests);
      ("Albatross Configuration", Test_albatross.tests);
      ("Unikernel Scaling & Monitoring", Test_scaling.tests);
      ("Email Settings & Verification", Test_email.tests);
      ("User Policy Management", Test_policy.tests);
    ]
