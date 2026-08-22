Mirage_crypto_rng_unix.use_default ();;

let of_string_exn f =
  match f with Ok a -> a | Error (`Msg err) -> failwith err

let label_of_string_exn label =
  of_string_exn (Vmm_core.Name.Label.of_string label)

let email_of_string_exn email = of_string_exn (Mrmime.Mailbox.of_string email)

let signing_request_exn pk =
  match X509.Signing_request.create [] pk with
  | Ok c -> c
  | Error _ -> failwith "invalid signing request"

let private_key = X509.Private_key.generate `ED25519
let second_private_key = X509.Private_key.generate `ED25519

let certificate_exn pk =
  match
    X509.Signing_request.sign (signing_request_exn pk) ~valid_from:Ptime.epoch
      ~valid_until:(Mirage_ptime.now ()) pk []
  with
  | Ok c -> c
  | Error _ -> failwith "invalid certificate"

let msg_t =
  let pp ppf (`Msg s) = Fmt.string ppf s in
  Alcotest.testable pp (fun (`Msg a) (`Msg b) -> String.equal a b)

let pp_storage ppf (users, configuration, email) =
  Fmt.pf ppf "%a" Yojson.Basic.pp (Storage.t_to_json users configuration email)

let eq_config (config_1 : Configuration.t list)
    (config_2 : Configuration.t list) =
  match (config_1, config_2) with
  | [], [] -> true
  | c1 :: _, c2 :: _ ->
      Vmm_core.Name.Label.equal c1.name c2.name
      && Ipaddr.compare c1.server_ip c2.server_ip = 0
      && c1.server_port = c2.server_port
      && Ptime.equal c1.updated_at c2.updated_at
      && String.equal
           (X509.Public_key.fingerprint
              (X509.Certificate.public_key c1.certificate))
           (X509.Public_key.fingerprint
              (X509.Certificate.public_key c2.certificate))
      && String.equal
           (X509.Public_key.fingerprint
              (X509.Private_key.public c1.private_key))
           (X509.Public_key.fingerprint
              (X509.Private_key.public c2.private_key))
  | _ -> false

let cmp_option f o1 o2 =
  match (o1, o2) with
  | None, None -> true
  | None, _ -> false
  | Some _, None -> false
  | Some a, Some b -> f a b

let eq_users (users_1 : User_model.user list) (users_2 : User_model.user list) =
  match (users_1, users_2) with
  | [], [] -> true
  | u1 :: _, u2 :: _ ->
      Vmm_core.Name.Label.equal u1.name u2.name
      && Mrmime.Mailbox.equal u1.email u2.email
      && String.equal u1.password u2.password
      && String.equal u1.uuid u2.uuid
      && u1.active = u2.active
      && u1.super_user = u2.super_user
      && Ptime.equal u1.updated_at u2.updated_at
      && Ptime.equal u1.created_at u2.created_at
      && cmp_option Ptime.equal u1.email_verified u2.email_verified
      && cmp_option Uuidm.equal u1.email_verification_uuid
           u2.email_verification_uuid
  | _ -> false

let eq_emails (e1 : Utils.Email.t) (e2 : Utils.Email.t) =
  Ipaddr.compare e1.server e2.server = 0
  && e1.port = e2.port
  && String.equal e1.base_url e2.base_url
  && Mrmime.Mailbox.equal e1.from_email e2.from_email
  && cmp_option Mrmime.Mailbox.equal e1.to_email e2.to_email

let eq_storage (u1, c1, e1) (u2, c2, e2) =
  eq_users u1 u2 && eq_config c1 c2 && cmp_option eq_emails e1 e2

let storage_t = Alcotest.testable pp_storage eq_storage

let mock_storage ?(version = 10) ?(users = []) ?(configuration = [])
    ?(email = None) () =
  Storage.t_to_json ~version users configuration email

let mock_email =
  {
    Utils.Email.server = Ipaddr.of_string_exn "10.0.0.1";
    port = 56;
    from_email = email_of_string_exn "test@robur.coop";
    base_url = "robur.coop";
    to_email = None;
  }

let mock_albatross_config =
  {
    Configuration.name = label_of_string_exn "default";
    server_ip = Ipaddr.of_string_exn "10.0.0.1";
    server_port = 25;
    updated_at = Ptime.epoch;
    private_key;
    certificate = certificate_exn private_key;
  }

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
      "expected version 10, found version 11. note: version [1 - 8] is now \
       deprecated."
  in
  Alcotest.(
    check (result storage_t msg_t)
      "mollymawk should fail to start when version is invalid" (Error expected)
      (Storage.t_of_json (mock_storage ~version:11 ())))

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
  ]

let tests =
  [
    ("Version tests", version_tests);
    ("Email config tests", email_config_tests);
    ("Albatross config tests", albatross_config_tests);
  ]

let () = Alcotest.run "Mollymawk data serialization tests for storage" tests
