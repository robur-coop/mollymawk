let () =
  Alcotest.run "Mollymawk API Function & Data Format Tests"
    [ ("Auth & Registration", Test_auth.tests) ]
