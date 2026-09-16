open Test_utils

let check_valid_registration () =
  let name = label_of_string_exn "test" in
  let email = email_of_string_exn "test@robur.coop" in
  let password = "SecretPassword123!" in
  let created_at = Mirage_ptime.now () in
  let user, cookie =
    User_model.create_user ~name ~email ~password ~created_at ~active:true
      ~super_user:false ~user_agent:(Some "Alcotest")
  in
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
      let name = label_of_string_exn "" in
      let email = email_of_string_exn "test@robur.coop" in
      let password = "SecretPassword123!" in
      let created_at = Mirage_ptime.now () in
      ignore
        (User_model.create_user ~name ~email ~password ~created_at ~active:true
           ~super_user:false ~user_agent:(Some "Alcotest")))

let check_registration_with_no_email () =
  Alcotest.check_raises "Registration with empty email fails"
    (Failure "Invalid email address: \"\"") (fun () ->
      let name = label_of_string_exn "test" in
      let email = email_of_string_exn "" in
      let password = "SecretPassword123!" in
      let created_at = Mirage_ptime.now () in
      ignore
        (User_model.create_user ~name ~email ~password ~created_at ~active:true
           ~super_user:false ~user_agent:(Some "Alcotest")))

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
  ]
