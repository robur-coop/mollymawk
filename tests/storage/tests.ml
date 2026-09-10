open Test_utils

let check_deprecated_version () =
  let expected =
    `Msg
      "expected version 10, found version 8. note: version [1 - 8] is now \
       deprecated."
  in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should fail to start for a deprecated version check"
      (Error expected)
      (Storage.t_of_json (mock_storage ~version:8 ())))

let check_valid_version () =
  let expected = ([], [], None) in
  Alcotest.(
    check (result storage_t msg_t) "mollymawk should start for a valid version"
      (Ok expected)
      (Storage.t_of_json (mock_storage ~version:9 ())))

let check_invalid_version () =
  let expected =
    `Msg
      "expected version 10, found version 1000. note: version [1 - 8] is now \
       deprecated."
  in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should fail to start when version is invalid" (Error expected)
      (Storage.t_of_json (mock_storage ~version:1000 ())))

let check_email_config_in_v9 () =
  let expected = ([], [], Some mock_email) in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should start with email config in v9" (Ok expected)
      (Storage.t_of_json (mock_storage ~version:9 ~email:(Some mock_email) ())))

let check_no_email_config_in_v9 () =
  let expected = ([], [], None) in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should start even with no email config in v9" (Ok expected)
      (Storage.t_of_json (mock_storage ~version:9 ())))

let check_email_config_in_v10 () =
  let expected = ([], [], Some mock_email) in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should start with email config in v10" (Ok expected)
      (Storage.t_of_json (mock_storage ~version:10 ~email:(Some mock_email) ())))

let check_no_email_config_in_v10 () =
  let expected = ([], [], None) in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should start even with no email config in v10" (Ok expected)
      (Storage.t_of_json (mock_storage ~version:10 ())))

let check_no_albatross_config_in_v9 () =
  let expected = ([], [], None) in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should start even with no albatross config in v9" (Ok expected)
      (Storage.t_of_json (mock_storage ~version:9 ())))

let check_no_albatross_config_in_v10 () =
  let expected = ([], [], None) in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should start even with no albatross config in v10"
      (Ok expected)
      (Storage.t_of_json (mock_storage ~version:10 ())))

let check_valid_albatross_config_in_v9 () =
  let expected = ([], [ mock_albatross_config ], None) in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should start with a valid albatross config in v9" (Ok expected)
      (Storage.t_of_json
         (mock_storage ~version:9 ~configuration:[ mock_albatross_config ] ())))

let check_valid_albatross_config_in_v10 () =
  let expected = ([], [ mock_albatross_config ], None) in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should start with a valid albatross config in v10"
      (Ok expected)
      (Storage.t_of_json
         (mock_storage ~version:10 ~configuration:[ mock_albatross_config ] ())))

let check_invalid_private_key_in_albatross_config () =
  let expected = `Msg "certificate and private key do not match" in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should not start if the certificate and private key in the \
       albatross config don't match"
      (Error expected)
      (Storage.t_of_json
         (mock_storage ~version:10
            ~configuration:
              [
                {
                  mock_albatross_config with
                  certificate = certificate_exn second_private_key;
                };
              ]
            ())))

let check_missing_certificate_in_albatross_config () =
  let expected = `Msg "No certificate" in
  let bad_json =
    `Assoc
      [
        ("version", `Int 10);
        ("users", `List []);
        ( "configuration",
          `List
            [
              `Assoc
                [
                  ("name", `String "default");
                  ("certificate", `String "");
                  ("private_key", `String "");
                  ("server_ip", `String "10.0.0.1");
                  ("server_port", `Int 25);
                  ("updated_at", `String "2026-08-21 20:02:17-00:00");
                ];
            ] );
        ("email", `Null);
      ]
  in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should not start if there is no certificate in the albatross \
       config "
      (Error expected)
      (Storage.t_of_json bad_json))

let check_missing_private_key_in_albatross_config () =
  let expected = `Msg "No private key" in
  let bad_json =
    `Assoc
      [
        ("version", `Int 10);
        ("users", `List []);
        ( "configuration",
          `List
            [
              `Assoc
                [
                  ("name", `String "default");
                  ( "certificate",
                    `String
                      "-----BEGIN CERTIFICATE-----\n\
                       MIG1MGmgAwIBAgILAOLhFMcl4xlKt5YwBQYDK2VwMAAwHhcNNzAwMTAxMDAwMDAw\n\
                       WhcNMjYwODIxMjEzNjA4WjAAMCowBQYDK2VwAyEAKkL+FItGwbO0WWbhPR6DtCJX\n\
                       wDxNWnAnuTdVpdd+aSowBQYDK2VwA0EAmFZ10FnNhq2kYLzFObcw0P2uwyPfdnAg\n\
                       DFLzoFIPoYlE98spkELRNeMpkxMbRsd4G2XYbrwdnwFOc9B+faX+Dw==\n\
                       -----END CERTIFICATE-----\n" );
                  ("private_key", `String "");
                  ("server_ip", `String "10.0.0.1");
                  ("server_port", `Int 25);
                  ("updated_at", `String "2026-08-21 20:02:17-00:00");
                ];
            ] );
        ("email", `Null);
      ]
  in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should not start if there is no private_key in the albatross \
       config "
      (Error expected)
      (Storage.t_of_json bad_json))

(** This test currently will not pass.*)
let check_multiple_valid_albatross_configs_with_same_name () =
  let expected = `Msg "Duplicated albatross configurations" in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should fail to start if two albatross configs have the same \
       name"
      (Error expected)
      (Storage.t_of_json
         (mock_storage ~version:10
            ~configuration:[ mock_albatross_config; mock_albatross_config ]
            ())))

let check_multiple_valid_albatross_configs_with_different_names () =
  let expected =
    ( [],
      [
        mock_albatross_config;
        {
          mock_albatross_config with
          Configuration.name = label_of_string_exn "default-2";
        };
      ],
      None )
  in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should fail to start if two albatross configs have the same \
       name"
      (Ok expected)
      (Storage.t_of_json
         (mock_storage ~version:10
            ~configuration:
              [
                mock_albatross_config;
                {
                  mock_albatross_config with
                  Configuration.name = label_of_string_exn "default-2";
                };
              ]
            ())))

let check_disk_dump () =
  let json = Utils.Json.from_string raw_dump in
  match Storage.t_of_json (json_of_string_exn json) with
  | Ok (users, configs, email) ->
      Alcotest.(check int "4 users in dump" 4 (List.length users));
      Alcotest.(check int "1 configuration in dump" 1 (List.length configs));
      Alcotest.(check bool "Email config present" true (Option.is_some email));
      let serialized = Storage.t_to_json users configs email in
      Alcotest.(
        check (result storage_t msg_t) "roundtrip serialization matches"
          (Ok (users, configs, email))
          (Storage.t_of_json serialized))
  | Error (`Msg err) -> Alcotest.fail err

let version_tests =
  [
    ("Deprecated version", `Quick, check_deprecated_version);
    ("Accepted version", `Quick, check_valid_version);
    ("Invalid version", `Quick, check_invalid_version);
  ]

let email_config_tests =
  [
    ("Email configuration in v9", `Quick, check_email_config_in_v9);
    ("No email configuration in v9", `Quick, check_no_email_config_in_v9);
    ("Email configuration in v10", `Quick, check_email_config_in_v10);
    ("No email configuration in v10", `Quick, check_no_email_config_in_v10);
  ]

let disk_dump_tests =
  [ ("Test with live data from a disk dump", `Quick, check_disk_dump) ]

let albatross_config_tests =
  [
    ("No albatross configuration in v9", `Quick, check_no_albatross_config_in_v9);
    ( "No albatross configuration in v10",
      `Quick,
      check_no_albatross_config_in_v10 );
    ( "Valid albatross configuration in v9",
      `Quick,
      check_valid_albatross_config_in_v9 );
    ( "Valid albatross configuration in v10",
      `Quick,
      check_valid_albatross_config_in_v10 );
    ( "Incompatible private key and certificate combination",
      `Quick,
      check_invalid_private_key_in_albatross_config );
    ("Empty certificate", `Quick, check_missing_certificate_in_albatross_config);
    ("Empty private_key", `Quick, check_missing_private_key_in_albatross_config);
    ( "Multiple albatross configurations with the same name",
      `Quick,
      check_multiple_valid_albatross_configs_with_same_name );
    ( "Multiple albatross configurations with the different names",
      `Quick,
      check_multiple_valid_albatross_configs_with_different_names );
  ]

let tests =
  [
    ("Version tests", version_tests);
    ("Email config tests", email_config_tests);
    ("Albatross config tests", albatross_config_tests);
    ("Disk dump tests", disk_dump_tests);
  ]

let () = Alcotest.run "Mollymawk data serialization tests for storage" tests
