open Test_utils
open Mock_devices
open Lwt.Infix

let make_register_request ?(csrf = "valid-csrf-token-1234") ~name ~email
    ~password () =
  let json_body =
    Fmt.str
      {|{ "name": "%s", "email": "%s", "password": "%s", "form_csrf": "%s" }|}
      name email password csrf
  in
  make_post_request ~path:"/api/register" ~body:json_body ~csrf_token:csrf ()

let check_valid_registration () =
  let password = "SecretPassword123!" in
  let user =
    make_mock_user ~name:"test" ~email:"test@robur.coop" ~password ()
  in
  let cookie = List.hd user.cookies in
  Alcotest.(check string)
    "User name matches" "test"
    (Configuration.name_to_str user.name);
  Alcotest.(check bool) "User is active" true user.active;
  Alcotest.(check bool) "User is not superuser" false user.super_user;
  Alcotest.(check bool) "Password is encrypted" true (user.password <> password);
  Alcotest.(check string)
    "Cookie has session name" User_model.session_cookie cookie.name;
  Alcotest.(check bool) "Cookie has value" true (String.length cookie.value > 0)

let check_registration_with_no_name () =
  Alcotest.check_raises "Registration with empty name fails"
    (Failure "invalid label (only [a-zA-Z0-9-.] allowed, 1 to 63 chars)")
    (fun () ->
      ignore
        (make_mock_user ~name:"" ~email:"test@robur.coop"
           ~password:"SecretPassword123!" ()))

let check_registration_with_no_email () =
  Alcotest.check_raises "Registration with empty email fails"
    (Failure "Invalid email address: \"\"") (fun () ->
      ignore
        (make_mock_user ~name:"test" ~email:"" ~password:"SecretPassword123!" ()))

let check_duplicate_user () =
  let existing_user = make_mock_user ~name:"test" ~email:"test@robur.coop" () in
  let users = [ existing_user ] in

  let dup_name = label_of_string_exn "test" in
  let dup_email = email_of_string_exn "test@robur.coop" in
  let new_name = label_of_string_exn "user" in
  let new_email = email_of_string_exn "user@robur.coop" in

  Alcotest.(check bool)
    "Existing user with name found" true
    (Option.is_some (Storage.find_by_name users dup_name));

  Alcotest.(check bool)
    "Existing user with email found" true
    (Option.is_some (Storage.find_by_email users dup_email));

  Alcotest.(check bool)
    "Unique name not found" true
    (Option.is_none (Storage.find_by_name users new_name));
  Alcotest.(check bool)
    "Unique email not found" true
    (Option.is_none (Storage.find_by_email users new_email))

let check_email_validation () =
  Alcotest.(check bool)
    "Accepts valid standard email" true
    (Utils.Email.validate_email "user@robur.coop");
  Alcotest.(check bool)
    "Accepts valid subdomain email" true
    (Utils.Email.validate_email "user@sub.robur.coop");
  Alcotest.(check bool)
    "Rejects missing domain" false
    (Utils.Email.validate_email "user@");
  Alcotest.(check bool)
    "Rejects missing user" false
    (Utils.Email.validate_email "@robur.coop");
  Alcotest.(check bool)
    "Rejects plain string" false
    (Utils.Email.validate_email "not-an-email")

let check_password_validation () =
  (*TODO: passwords should be at least 8 characters *)
  Alcotest.(check bool)
    "Rejects password shorter than 8 chars" true
    (User_model.password_validation "short");
  Alcotest.(check bool)
    "Accepts 8-character password" true
    (User_model.password_validation "12345678");
  Alcotest.(check bool)
    "Accepts complex password" true
    (User_model.password_validation "SuperSecureP@ssw0rd!2026")

let check_successful_login () =
  let user = make_mock_user ~name:"test" ~email:"test@robur.coop" () in
  let now = Mirage_ptime.now () in
  match
    User_model.login_user ~email:user.email ~password:"Password123!"
      ~user_agent:(Some "Alcotest") (Some user) now
  with
  | Ok (updated_user, cookie) ->
      Alcotest.(check string)
        "Logged in user matches" "test"
        (Configuration.name_to_str updated_user.name);
      Alcotest.(check string)
        "Session cookie name" User_model.session_cookie cookie.name;
      Alcotest.(check bool)
        "Cookie is recorded on user" true
        (List.exists
           (fun (c : User_model.cookie) -> String.equal c.value cookie.value)
           updated_user.cookies)
  | Error (`Msg err) -> failwith err

let check_failed_login_wrong_password () =
  let user = make_mock_user ~name:"test" ~email:"test@robur.coop" () in
  let now = Mirage_ptime.now () in
  match
    User_model.login_user ~email:user.email ~password:"WrongPassword"
      ~user_agent:(Some "Alcotest") (Some user) now
  with
  | Ok _ -> failwith "Expected login failure with wrong password"
  | Error (`Msg err) ->
      Alcotest.(check string) "Error message" "Invalid email or password." err

let check_email_token_verification () =
  let user = make_mock_user ~name:"test" ~email:"test@robur.coop" () in
  let token = User_model.generate_uuid () in
  let token_str = Uuidm.to_string token in
  let user_with_token =
    User_model.update_user user ~email_verification_uuid:(Some token) ()
  in
  let now = Mirage_ptime.now () in
  match User_model.verify_email_token (Some user_with_token) token_str now with
  | Ok verified_user ->
      Alcotest.(check bool)
        "Email is verified" true
        (User_model.is_email_verified verified_user);
      Alcotest.(check bool)
        "Verification token is cleared" true
        (verified_user.email_verification_uuid = None)
  | Error (`Msg err) -> failwith err

let check_registration_endpoint () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let raw_http_request =
        make_register_request ~name:"test" ~email:"test@robur.coop"
          ~password:"SecretPassword123!" ()
      in
      query_endpoint (make_app_request_handler store) raw_http_request
      >>= fun response_str ->
      Printf.printf "Response:\n%s\n%!" response_str;
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" response_str);

      Alcotest.(check bool)
        "Response contains redirect to dashboard" true
        (String.includes ~affix:"location: /dashboard"
           (String.lowercase_ascii response_str));

      Alcotest.(check bool)
        "Response body contains user name test" true
        (String.includes ~affix:"\"name\":\"test\"" response_str);

      Alcotest.(check int)
        "User stored in database" 1
        (List.length store.Storage.users);
      let saved_user = List.hd store.Storage.users in
      Alcotest.(check string)
        "Saved user name matches" "test"
        (Configuration.name_to_str saved_user.name);
      Alcotest.(check bool) "First user is superuser" true saved_user.super_user;
      Alcotest.(check bool) "First user is active" true saved_user.active;

      Lwt.return_unit )

let check_duplicate_registration_endpoint () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req1 =
        make_register_request ~name:"test2" ~email:"test2@robur.coop"
          ~password:"SecretPassword123!" ()
      in
      query_endpoint (make_app_request_handler store) req1 >>= fun resp1 ->
      Printf.printf "Response 1:\n%s\n%!" resp1;
      Alcotest.(check bool)
        "First registration succeeds" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp1);

      let req2 =
        make_register_request ~name:"test2" ~email:"test3@robur.coop"
          ~password:"SecretPassword123!" ()
      in
      query_endpoint (make_app_request_handler store) req2 >>= fun resp2 ->
      Printf.printf "Response 2:\n%s\n%!" resp2;
      Alcotest.(check bool)
        "Duplicate name is 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp2);
      Alcotest.(check bool)
        "Duplicate name error message" true
        (String.includes ~affix:"A user with this name already exist." resp2);

      Lwt.return_unit )

let check_registration_endpoint_bad_email () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req =
        make_register_request ~name:"testuser" ~email:"testuser@"
          ~password:"SecretPassword123!" ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Printf.printf "Response:\n%s\n%!" resp;
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error message indicates invalid email" true
        (String.includes ~affix:"Invalid email address." resp);
      Lwt.return_unit )

let check_registration_endpoint_empty_name () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req =
        make_register_request ~name:"" ~email:"test@robur.coop"
          ~password:"SecretPassword123!" ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Printf.printf "Response:\n%s\n%!" resp;
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error message indicates all fields must be filled" true
        (String.includes ~affix:"All fields must be filled." resp);
      Lwt.return_unit )

let check_registration_endpoint_bad_name () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req =
        make_register_request ~name:"test user" ~email:"test@robur.coop"
          ~password:"SecretPassword123!" ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Printf.printf "Response:\n%s\n%!" resp;
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error message indicates invalid label" true
        (String.includes ~affix:"invalid label" resp);
      Lwt.return_unit )

let tests =
  [
    ("Valid registration", `Quick, check_valid_registration);
    ("Registration with no name", `Quick, check_registration_with_no_name);
    ("Registration with no email", `Quick, check_registration_with_no_email);
    ("Check duplicate user", `Quick, check_duplicate_user);
    ("Email validation formats", `Quick, check_email_validation);
    ("Password validation length", `Quick, check_password_validation);
    ("Successful user login", `Quick, check_successful_login);
    ( "Failed login with wrong password",
      `Quick,
      check_failed_login_wrong_password );
    ("Email token verification", `Quick, check_email_token_verification);
    ("Register a user", `Quick, check_registration_endpoint);
    ( "Reject duplicate registration",
      `Quick,
      check_duplicate_registration_endpoint );
    ( "Register endpoint with bad email",
      `Quick,
      check_registration_endpoint_bad_email );
    ( "Register endpoint with empty name",
      `Quick,
      check_registration_endpoint_empty_name );
    ( "Register endpoint with bad name (whitespace)",
      `Quick,
      check_registration_endpoint_bad_name );
  ]
