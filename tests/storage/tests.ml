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
  let expected = (Utils.UM.empty, Utils.LM.empty, None) in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should start for a valid version 10" (Ok expected)
      (Storage.t_of_json (mock_storage ~version:10 ())));
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should start for a valid version 9" (Ok expected)
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
  let expected = (Utils.UM.empty, Utils.LM.empty, Some mock_email) in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should start with email config in v9" (Ok expected)
      (Storage.t_of_json (mock_storage ~version:9 ~email:(Some mock_email) ())))

let check_no_email_config_in_v9 () =
  let expected = (Utils.UM.empty, Utils.LM.empty, None) in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should start even with no email config in v9" (Ok expected)
      (Storage.t_of_json (mock_storage ~version:9 ())))

let check_email_config_in_v10 () =
  let expected = (Utils.UM.empty, Utils.LM.empty, Some mock_email) in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should start with email config in v10" (Ok expected)
      (Storage.t_of_json (mock_storage ~version:10 ~email:(Some mock_email) ())))

let check_no_email_config_in_v10 () =
  let expected = (Utils.UM.empty, Utils.LM.empty, None) in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should start even with no email config in v10" (Ok expected)
      (Storage.t_of_json (mock_storage ~version:10 ())))

let check_no_albatross_config_in_v9 () =
  let expected = (Utils.UM.empty, Utils.LM.empty, None) in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should start even with no albatross config in v9" (Ok expected)
      (Storage.t_of_json (mock_storage ~version:9 ())))

let check_no_albatross_config_in_v10 () =
  let expected = (Utils.UM.empty, Utils.LM.empty, None) in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should start even with no albatross config in v10"
      (Ok expected)
      (Storage.t_of_json (mock_storage ~version:10 ())))

let check_valid_albatross_config_in_v9 () =
  let expected =
    ( Utils.UM.empty,
      Utils.LM.singleton mock_albatross_config.name mock_albatross_config,
      None )
  in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should start with a valid albatross config in v9" (Ok expected)
      (Storage.t_of_json
         (mock_storage ~version:9
            ~configuration:
              (Utils.LM.singleton mock_albatross_config.name
                 mock_albatross_config)
            ())))

let check_valid_albatross_config_in_v10 () =
  let expected =
    ( Utils.UM.empty,
      Utils.LM.singleton mock_albatross_config.name mock_albatross_config,
      None )
  in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should start with a valid albatross config in v10"
      (Ok expected)
      (Storage.t_of_json
         (mock_storage ~version:10
            ~configuration:
              (Utils.LM.singleton mock_albatross_config.name
                 mock_albatross_config)
            ())))

let check_invalid_private_key_in_albatross_config () =
  let expected = `Msg "certificate and private key do not match" in
  let bad_cfg =
    {
      mock_albatross_config with
      certificate = certificate_exn second_private_key;
    }
  in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should not start if the certificate and private key in the \
       albatross config don't match"
      (Error expected)
      (Storage.t_of_json
         (mock_storage ~version:10
            ~configuration:(Utils.LM.singleton bad_cfg.name bad_cfg)
            ())))

let check_missing_certificate_in_albatross_config () =
  let expected = `Msg "No certificate" in
  let bad_json =
    Albatross_json.j_obj
      [
        ("version", Jsont.Json.int 10);
        ("users", Jsont.Json.list []);
        ( "configuration",
          Jsont.Json.list
            [
              Albatross_json.j_obj
                [
                  ("name", Jsont.Json.string "default");
                  ("certificate", Jsont.Json.string "");
                  ("private_key", Jsont.Json.string "");
                  ("server_ip", Jsont.Json.string "10.0.0.1");
                  ("server_port", Jsont.Json.int 25);
                  ("updated_at", Jsont.Json.string "2026-08-21 20:02:17-00:00");
                ];
            ] );
        ("email", Jsont.Json.null ());
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
    Albatross_json.j_obj
      [
        ("version", Jsont.Json.int 10);
        ("users", Jsont.Json.list []);
        ( "configuration",
          Jsont.Json.list
            [
              Albatross_json.j_obj
                [
                  ("name", Jsont.Json.string "default");
                  ( "certificate",
                    Jsont.Json.string
                      "-----BEGIN CERTIFICATE-----\n\
                       MIG1MGmgAwIBAgILAOLhFMcl4xlKt5YwBQYDK2VwMAAwHhcNNzAwMTAxMDAwMDAw\n\
                       WhcNMjYwODIxMjEzNjA4WjAAMCowBQYDK2VwAyEAKkL+FItGwbO0WWbhPR6DtCJX\n\
                       wDxNWnAnuTdVpdd+aSowBQYDK2VwA0EAmFZ10FnNhq2kYLzFObcw0P2uwyPfdnAg\n\
                       DFLzoFIPoYlE98spkELRNeMpkxMbRsd4G2XYbrwdnwFOc9B+faX+Dw==\n\
                       -----END CERTIFICATE-----\n" );
                  ("private_key", Jsont.Json.string "");
                  ("server_ip", Jsont.Json.string "10.0.0.1");
                  ("server_port", Jsont.Json.int 25);
                  ("updated_at", Jsont.Json.string "2026-08-21 20:02:17-00:00");
                ];
            ] );
        ("email", Jsont.Json.null ());
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
  let one_cfg_json =
    match
      Models.Configuration.to_json
        (Utils.LM.singleton mock_albatross_config.name mock_albatross_config)
    with
    | Jsont.Array ([ c ], _) -> c
    | _ -> failwith "unexpected json"
  in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should fail to start if two albatross configs have the same \
       name"
      (Error expected)
      (Storage.t_of_json
         (Albatross_json.j_obj
            [
              ("version", Jsont.Json.int 10);
              ("users", Jsont.Json.list []);
              ("configuration", Jsont.Json.list [ one_cfg_json; one_cfg_json ]);
              ("email", Jsont.Json.null ());
            ])))

let check_multiple_valid_albatross_configs_with_different_names () =
  let cfg2 =
    {
      mock_albatross_config with
      Models.Configuration.name = label_of_string_exn "default-2";
    }
  in
  let configs =
    Utils.LM.empty
    |> Utils.LM.add mock_albatross_config.name mock_albatross_config
    |> Utils.LM.add cfg2.name cfg2
  in
  let expected = (Utils.UM.empty, configs, None) in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should fail to start if two albatross configs have the same \
       name"
      (Ok expected)
      (Storage.t_of_json (mock_storage ~version:10 ~configuration:configs ())))

let check_disk_dump () =
  let json = Utils.Json.from_string raw_dump in
  match Storage.t_of_json (json_of_string_exn json) with
  | Ok (users, configs, email) ->
      Alcotest.(check int "3 users in dump" 3 (Utils.UM.cardinal users));
      Alcotest.(
        check int "1 configuration in dump" 1 (Utils.LM.cardinal configs));
      Alcotest.(check bool "Email config present" true (Option.is_some email));
      let _serialized = Storage.t_to_json users configs email in
      ();
      Alcotest.(
        check (result storage_t msg_t) "roundtrip serialization matches"
          (Ok (users, configs, email))
          (Storage.t_of_json (json_of_string_exn json)))
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

let check_cookies_persisted_in_v10 () =
  let user = make_mock_user ~name:"user1" ~email:"user1@robur.coop" () in
  let json =
    Storage.t_to_json ~version:10
      (Utils.UM.singleton user.uuid user)
      Utils.LM.empty None
  in
  match json with
  | Jsont.Object (xs, _) -> (
      match Utils.Json.get "users" xs with
      | Some (Jsont.Array ([ Jsont.Object (u_fields, _) ], _)) ->
          Alcotest.(
            check bool "cookies field present in JSON in v10" true
              (Option.is_some (Utils.Json.get "cookies" u_fields)))
      | _ -> Alcotest.fail "Expected single user in users list")
  | _ -> Alcotest.fail "Expected assoc in storage json"

let check_discard_malformed_unikernel_update () =
  let valid_uk_uuid = Models.User.generate_uuid () in
  let valid_update : Models.User.unikernel_update =
    {
      name = label_of_string_exn "my-app";
      job = "my-job";
      uuid = valid_uk_uuid;
      config =
        {
          typ = `Solo5;
          compressed = false;
          image = "";
          fail_behaviour = `Quit;
          add_name = true;
          startup = None;
          cpuids = Vmm_core.IS.singleton 0;
          memory = 32;
          block_devices = [];
          bridges = [];
          argv = None;
          numcpus = 1;
          linux_boot_partition = None;
        };
      timestamp = Ptime.epoch;
    }
  in
  let user = make_mock_user ~name:"user1" ~email:"user1@robur.coop" () in
  let user =
    {
      user with
      unikernel_updates = Utils.LM.singleton valid_update.name valid_update;
    }
  in
  let json =
    Storage.t_to_json (Utils.UM.singleton user.uuid user) Utils.LM.empty None
  in
  let malformed_update_json =
    Albatross_json.j_obj
      [
        ("name", Jsont.Json.string "malformed-app");
        ("job", Jsont.Json.string "bad-job");
        ("uuid", Jsont.Json.string "");
        ("config", Albatross_json.config_to_json valid_update.config);
        ( "timestamp",
          Jsont.Json.string
            (Utils.TimeHelper.string_of_ptime valid_update.timestamp) );
      ]
  in
  let injected_json =
    match json with
    | Jsont.Object (fields, meta) ->
        let updated_fields =
          List.map
            (function
              | name, Jsont.Array ([ Jsont.Object (u_fields, u_meta) ], a_meta)
                when fst name = "users" ->
                  let u_fields' =
                    List.map
                      (function
                        | u_name, Jsont.Array (updates, up_meta)
                          when fst u_name = "unikernel_updates" ->
                            ( u_name,
                              Jsont.Array
                                (malformed_update_json :: updates, up_meta) )
                        | other -> other)
                      u_fields
                  in
                  ( name,
                    Jsont.Array ([ Jsont.Object (u_fields', u_meta) ], a_meta)
                  )
              | other -> other)
            fields
        in
        Jsont.Object (updated_fields, meta)
    | _ -> Alcotest.fail "Unexpected json structure"
  in
  match Storage.t_of_json injected_json with
  | Ok (users, _, _) when Utils.UM.cardinal users = 1 ->
      let loaded_user = snd (Utils.UM.choose users) in
      Alcotest.(
        check int "Malformed update discarded, only 1 valid update loaded" 1
          (Utils.LM.cardinal loaded_user.unikernel_updates));
      Alcotest.(
        check bool "Valid update present" true
          (Utils.LM.mem valid_update.name loaded_user.unikernel_updates))
  | Ok _ -> Alcotest.fail "Expected 1 user"
  | Error (`Msg err) -> Alcotest.fail err

let cookie_tests =
  [
    ("Cookies are persisted in v10", `Quick, check_cookies_persisted_in_v10);
    ( "Discard malformed unikernel updates",
      `Quick,
      check_discard_malformed_unikernel_update );
  ]

let tests =
  [
    ("Version tests", version_tests);
    ("Email config tests", email_config_tests);
    ("Albatross config tests", albatross_config_tests);
    ("Cookie map migration tests", cookie_tests);
    ("Disk dump tests", disk_dump_tests);
  ]

let () = Alcotest.run "Mollymawk data serialization tests for storage" tests
