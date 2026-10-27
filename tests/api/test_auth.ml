open Test_utils
open Mock_devices

let make_register_request ?(session_cookie = Test_utils.session_cookie)
    ?(csrf = Test_utils.make_csrf_token ~session_cookie ()) ~name ~email
    ~password () =
  let json_body =
    Fmt.str
      {|{ "name": "%s", "email": "%s", "password": "%s", "molly_csrf": "%s" }|}
      name email password csrf
  in
  make_post_request ~path:"/api/register" ~body:json_body ~session_cookie
    ~csrf_token:csrf ()

let check_valid_registration () =
  let password = "SecretPassword123!" in
  let user, cookie =
    Models.User.create_user
      ~name:(label_of_string_exn "test")
      ~email:(email_of_string_exn "test@robur.coop")
      ~password ~created_at:(Ptime_clock.now ()) ~active:true ~super_user:false
      ~user_agent:(Some "Alcotest-client")
  in
  Alcotest.(check string)
    "User name matches" "test"
    (Models.Configuration.name_to_str user.name);
  Alcotest.(check bool) "User is active" true user.active;
  Alcotest.(check bool) "User is not superuser" false user.super_user;
  Alcotest.(check bool) "Password is encrypted" true (user.password <> password);
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
  let store =
    Storage.create
      ~users:(Utils.UM.singleton existing_user.uuid existing_user)
      ()
  in

  let dup_name = label_of_string_exn "test" in
  let dup_email = email_of_string_exn "test@robur.coop" in
  let new_name = label_of_string_exn "user" in
  let new_email = email_of_string_exn "user@robur.coop" in

  Alcotest.(check bool)
    "Existing user with name found" true
    (Option.is_some (Storage.find_by_name store dup_name));

  Alcotest.(check bool)
    "Existing user with email found" true
    (Option.is_some (Storage.find_by_email store dup_email));

  Alcotest.(check bool)
    "Unique name not found" true
    (Option.is_none (Storage.find_by_name store new_name));
  Alcotest.(check bool)
    "Unique email not found" true
    (Option.is_none (Storage.find_by_email store new_email))

let check_email_validation () =
  Alcotest.(check bool)
    "Accepts valid standard email" true
    (Result.is_ok (Models.Email.validate_email "user@robur.coop"));
  Alcotest.(check bool)
    "Accepts valid subdomain email" true
    (Result.is_ok (Models.Email.validate_email "user@sub.robur.coop"));
  Alcotest.(check bool)
    "Rejects missing domain" true
    (Result.is_error (Models.Email.validate_email "user@"));
  Alcotest.(check bool)
    "Rejects missing user" true
    (Result.is_error (Models.Email.validate_email "@robur.coop"));
  Alcotest.(check bool)
    "Rejects plain string" true
    (Result.is_error (Models.Email.validate_email "not-an-email"))

let check_password_validation () =
  Alcotest.(check bool)
    "Rejects password shorter than 8 chars" false
    (Models.User.password_validation "short");
  Alcotest.(check bool)
    "Accepts 8-character password" true
    (Models.User.password_validation "12345678");
  Alcotest.(check bool)
    "Accepts complex password" true
    (Models.User.password_validation "SuperSecureP@ssw0rd!2026")

let check_successful_login () =
  let user = make_mock_user ~name:"test" ~email:"test@robur.coop" () in
  let now = Ptime_clock.now () in
  match
    Models.User.login_user ~email:user.email ~password:"Password123!"
      ~user_agent:(Some "Alcotest") (Some user) now
  with
  | Ok (updated_user, cookie) ->
      Alcotest.(check string)
        "Logged in user matches" "test"
        (Models.Configuration.name_to_str updated_user.name);
      Alcotest.(check bool)
        "Session cookie has non-empty value" true
        (String.length cookie.value > 0)
  | Error (`Msg err) -> failwith err

let check_failed_login_wrong_password () =
  let user = make_mock_user ~name:"test" ~email:"test@robur.coop" () in
  let now = Ptime_clock.now () in
  match
    Models.User.login_user ~email:user.email ~password:"WrongPassword"
      ~user_agent:(Some "Alcotest") (Some user) now
  with
  | Ok _ -> failwith "Expected login failure with wrong password"
  | Error (`Msg err) ->
      Alcotest.(check string) "Error message" "Invalid email or password." err

let check_email_token_verification () =
  let user = make_mock_user ~name:"test" ~email:"test@robur.coop" () in
  let token = Models.User.generate_uuid () in
  let token_str = Uuidm.to_string token in
  let now = Ptime_clock.now () in
  let user_with_token =
    Models.User.update_user user ~email_verification_uuid:(Some token)
      ~updated_at:now ()
  in
  match Models.User.verify_email_token (Some user_with_token) token_str now with
  | Ok verified_user ->
      Alcotest.(check bool)
        "Email is verified" true
        (Models.User.is_email_verified verified_user);
      Alcotest.(check bool)
        "Verification token is cleared" true
        (verified_user.email_verification_uuid = None)
  | Error (`Msg err) -> failwith err

let check_registration_endpoint () =
  let store = init_mock_store () in
  let raw_http_request =
    make_register_request ~name:"test" ~email:"test@robur.coop"
      ~password:"SecretPassword123!" ()
  in
  let response_str = query_endpoint raw_http_request in
  Alcotest.(check bool)
    "Response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" response_str);

  Alcotest.(check bool)
    "Response contains redirect to dashboard" true
    (String.includes ~affix:"location: /dashboard"
       (String.lowercase_ascii response_str));

  Alcotest.(check bool)
    "Response body contains registration success message" true
    (String.includes ~affix:"Registration successful" response_str);

  Alcotest.(check int)
    "User stored in database" 1
    (Utils.UM.cardinal store.Storage.users);
  let saved_user = snd (Utils.UM.choose store.Storage.users) in
  Alcotest.(check string)
    "Saved user name matches" "test"
    (Models.Configuration.name_to_str saved_user.name);
  Alcotest.(check bool) "First user is superuser" true saved_user.super_user;
  Alcotest.(check bool) "First user is active" true saved_user.active

let check_duplicate_registration_endpoint () =
  let _store = init_mock_store () in
  let req1 =
    make_register_request ~name:"test2" ~email:"test2@robur.coop"
      ~password:"SecretPassword123!" ()
  in
  let resp1 = query_endpoint req1 in
  Alcotest.(check bool)
    "First registration succeeds" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp1);

  let req2 =
    make_register_request ~name:"test2" ~email:"test3@robur.coop"
      ~password:"SecretPassword123!" ()
  in
  let resp2 = query_endpoint req2 in
  Alcotest.(check bool)
    "Duplicate name is 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp2);
  Alcotest.(check bool)
    "Duplicate name error message" true
    (String.includes ~affix:"A user with this name already exist." resp2)

let check_registration_endpoint_bad_email () =
  let _store = init_mock_store () in
  let req =
    make_register_request ~name:"testuser" ~email:"testuser@"
      ~password:"SecretPassword123!" ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Error message indicates invalid email" true
    (String.includes ~affix:"Invalid email address." resp)

let check_registration_endpoint_empty_name () =
  let _store = init_mock_store () in
  let req =
    make_register_request ~name:"" ~email:"test@robur.coop"
      ~password:"SecretPassword123!" ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Error message indicates all fields must be filled" true
    (String.includes ~affix:"All fields must be filled." resp)

let check_registration_endpoint_bad_name () =
  let _store = init_mock_store () in
  let req =
    make_register_request ~name:"test user" ~email:"test@robur.coop"
      ~password:"SecretPassword123!" ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Error message indicates invalid label" true
    (String.includes ~affix:"invalid label" resp)

let check_login_endpoint () =
  let store = init_mock_store () in
  let user, _session, _csrf = setup_user store in
  let csrf = Test_utils.make_csrf_token () in
  let body =
    Fmt.str
      {|{ "email": "test@robur.coop", "password": "Password123!", "molly_csrf": "%s" }|}
      csrf
  in
  let req =
    make_post_request ~path:"/api/login" ~body
      ~session_cookie:Test_utils.session_cookie ~csrf_token:csrf ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Sets session cookie" true
    (String.includes ~affix:"Set-Cookie: molly_session=" resp);
  Alcotest.(check bool)
    "Redirects to dashboard" true
    (String.includes ~affix:"location: /dashboard"
       (String.lowercase_ascii resp));
  Alcotest.(check bool)
    "Response body contains login success message" true
    (String.includes ~affix:"Login successful" resp);
  Alcotest.(check bool)
    "Session recorded in storage" true
    (not (Utils.SM.is_empty (Controller.Session.user_sessions user.uuid)))

let check_logout_endpoint () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body = Fmt.str {|{ "molly_csrf": "%s" }|} csrf_token in
  let req =
    make_post_request ~path:"/logout" ~body ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Logout success message" true
    (String.includes ~affix:"Logout successful" resp);
  Alcotest.(check bool)
    "Session cookie removed after logout" true
    (Option.is_none (Controller.Session.find_session session_cookie))

let check_update_password_endpoint () =
  let store = init_mock_store () in
  let user, session_cookie, csrf_token =
    setup_user ~password:"OldPassword123!" store
  in
  let body =
    Fmt.str
      {|{ "current_password": "OldPassword123!", "new_password": "NewSecretPassword123!", "confirm_password": "NewSecretPassword123!", "molly_csrf": "%s" }|}
      csrf_token
  in
  let req =
    make_post_request ~path:"/account/password/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Password updated message" true
    (String.includes ~affix:"Updated password successfully" resp);
  let updated_user = Option.get (Storage.find_by_uuid store user.uuid) in
  let expected_hash =
    Models.User.hash_password ~password:"NewSecretPassword123!" ~uuid:user.uuid
  in
  Alcotest.(check bool)
    "Stored password hash matches new password" true
    (String.equal updated_user.password expected_hash)

let check_close_sessions_endpoint () =
  let store = init_mock_store () in
  let user, session_cookie_1, _csrf = setup_user store in
  let login_csrf = Test_utils.make_csrf_token () in
  let login_body =
    Fmt.str
      {|{ "email": "test@robur.coop", "password": "Password123!", "molly_csrf": "%s" }|}
      login_csrf
  in
  let login_req =
    make_post_request ~path:"/api/login" ~body:login_body
      ~session_cookie:Test_utils.session_cookie ~csrf_token:login_csrf ()
  in
  let _login_resp = query_endpoint login_req in
  let user_sessions = Controller.Session.user_sessions user.uuid in
  let session_cookie_2 =
    (Utils.SM.fold
       (fun _ (c : Models.User.cookie) acc ->
         if not (String.equal c.value session_cookie_1) then Some c else acc)
       user_sessions None
    |> Option.get)
      .value
  in
  let csrf_token =
    Test_utils.make_csrf_token ~session_cookie:session_cookie_1 ()
  in
  let body = Fmt.str {|{ "molly_csrf": "%s" }|} csrf_token in
  let req =
    make_post_request ~path:"/api/account/sessions/close" ~body
      ~session_cookie:session_cookie_1 ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Closed all sessions message" true
    (String.includes ~affix:"Closed all sessions successfully" resp);
  let updated_sessions = Controller.Session.user_sessions user.uuid in
  Alcotest.(check bool)
    "Current session still active" true
    (Utils.SM.mem session_cookie_1 updated_sessions);
  Alcotest.(check bool)
    "Other session closed" false
    (Utils.SM.mem session_cookie_2 updated_sessions)

let check_close_session_endpoint () =
  let store = init_mock_store () in
  let user, session_cookie_1, _csrf = setup_user store in
  let login_csrf = Test_utils.make_csrf_token () in
  let login_body =
    Fmt.str
      {|{ "email": "test@robur.coop", "password": "Password123!", "molly_csrf": "%s" }|}
      login_csrf
  in
  let login_req =
    make_post_request ~path:"/api/login" ~body:login_body
      ~session_cookie:Test_utils.session_cookie ~csrf_token:login_csrf ()
  in
  let _login_resp = query_endpoint login_req in
  let user_sessions = Controller.Session.user_sessions user.uuid in
  let session_cookie_2 =
    (Utils.SM.fold
       (fun _ (c : Models.User.cookie) acc ->
         if not (String.equal c.value session_cookie_1) then Some c else acc)
       user_sessions None
    |> Option.get)
      .value
  in
  let csrf_token =
    Test_utils.make_csrf_token ~session_cookie:session_cookie_1 ()
  in
  let body =
    Fmt.str {|{ "session_value": "%s", "molly_csrf": "%s" }|} session_cookie_2
      csrf_token
  in
  let req =
    make_post_request ~path:"/api/account/session/close" ~body
      ~session_cookie:session_cookie_1 ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Session closed message" true
    (String.includes ~affix:"Session closed successfully" resp);
  let updated_sessions = Controller.Session.user_sessions user.uuid in
  Alcotest.(check bool)
    "Current session still active" true
    (Utils.SM.mem session_cookie_1 updated_sessions);
  Alcotest.(check bool)
    "Targeted session closed" false
    (Utils.SM.mem session_cookie_2 updated_sessions)

let check_csrf_double_submit_success () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Fmt.str
      {|{ "current_password": "Password123!", "new_password": "NewSecretPassword123!", "confirm_password": "NewSecretPassword123!", "molly_csrf": "%s" }|}
      csrf_token
  in
  let req =
    make_post_request ~path:"/account/password/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Valid double-submit CSRF succeeds" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp)

let check_csrf_tampered_rejected () =
  let store = init_mock_store () in
  let _user, session_cookie, _csrf = setup_user store in
  let tampered_csrf = "tampered.bad_signature" in
  let body =
    Fmt.str
      {|{ "current_password": "Password123!", "new_password": "NewSecretPassword123!", "confirm_password": "NewSecretPassword123!", "molly_csrf": "%s" }|}
      tampered_csrf
  in
  let req =
    make_post_request ~path:"/account/password/update" ~body ~session_cookie
      ~csrf_token:tampered_csrf ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Tampered CSRF fails with 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Error message indicates CSRF mismatch error" true
    (String.includes ~affix:"CSRF token mismatch error" resp)

let check_csrf_mismatch_rejected () =
  let store = init_mock_store () in
  let _user, session_cookie, _csrf = setup_user store in
  let submitted_token =
    Test_utils.make_csrf_token ~session_cookie:"other-session-cookie" ()
  in
  let body =
    Fmt.str
      {|{ "current_password": "Password123!", "new_password": "NewSecretPassword123!", "confirm_password": "NewSecretPassword123!", "molly_csrf": "%s" }|}
      submitted_token
  in
  let req =
    make_post_request ~path:"/account/password/update" ~body ~session_cookie
      ~csrf_token:submitted_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Mismatched CSRF tokens fail with 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Error message indicates CSRF mismatch error" true
    (String.includes ~affix:"CSRF token mismatch error" resp)

let check_csrf_missing_cookie_rejected () =
  let store = init_mock_store () in
  let _user, _session_cookie, csrf_token = setup_user store in
  let body =
    Fmt.str
      {|{ "email": "test@robur.coop", "password": "Password123!", "molly_csrf": "%s" }|}
      csrf_token
  in
  let req =
    make_post_request ~path:"/api/login" ~body ~session_cookie:"" ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Missing CSRF cookie fails with 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Error message indicates CSRF mismatch error" true
    (String.includes ~affix:"CSRF token mismatch error" resp)

let check_login_prunes_expired_cookies () =
  let store = init_mock_store () in
  let user, session_cookie_1, _csrf = setup_user store in
  let now = Utils.now () in
  let two_days_ago =
    Option.get (Ptime.sub_span now (Ptime.Span.of_int_s (86400 * 2)))
  in
  let expired_cookie =
    Models.User.generate_cookie ~expires_in:86400 two_days_ago None ()
  in
  Controller.Session.add_session user.uuid expired_cookie;
  Alcotest.(check bool)
    "Expired cookie is in sessions before login" true
    (Option.is_some (Controller.Session.find_session expired_cookie.value));
  let login_csrf = Test_utils.make_csrf_token () in
  let login_body =
    Fmt.str
      {|{ "email": "test@robur.coop", "password": "Password123!", "molly_csrf": "%s" }|}
      login_csrf
  in
  let login_req =
    make_post_request ~path:"/api/login" ~body:login_body
      ~session_cookie:Test_utils.session_cookie ~csrf_token:login_csrf ()
  in
  let resp = query_endpoint login_req in
  Alcotest.(check bool)
    "Login succeeds" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Expired cookie was pruned after login" false
    (Option.is_some (Controller.Session.find_session expired_cookie.value));
  Alcotest.(check bool)
    "Valid session_cookie_1 is retained" true
    (Option.is_some (Controller.Session.find_session session_cookie_1))

let check_update_cookie_usage_updates_last_access () =
  let store = init_mock_store () in
  let _user, session_cookie, _csrf = setup_user store in
  let initial_cookie =
    Option.get (Controller.Session.find_session session_cookie) |> snd
  in
  let later =
    Option.get
      (Ptime.add_span initial_cookie.last_access (Ptime.Span.of_int_s 300))
  in
  Controller.Session.update_cookie_usage initial_cookie (Some "New-User-Agent")
    later;
  let updated_cookie =
    Option.get (Controller.Session.find_session session_cookie) |> snd
  in
  Alcotest.(check bool)
    "Last access timestamp updated" true
    (Ptime.equal updated_cookie.last_access later);
  Alcotest.(check (option string))
    "User agent updated" (Some "New-User-Agent") updated_cookie.user_agent

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
    ("Login endpoint", `Quick, check_login_endpoint);
    ("Logout endpoint", `Quick, check_logout_endpoint);
    ("Update password endpoint", `Quick, check_update_password_endpoint);
    ("Close all other sessions endpoint", `Quick, check_close_sessions_endpoint);
    ("Close specific session endpoint", `Quick, check_close_session_endpoint);
    ( "CSRF valid double-submit succeeds",
      `Quick,
      check_csrf_double_submit_success );
    ("CSRF tampered token rejected", `Quick, check_csrf_tampered_rejected);
    ("CSRF mismatched tokens rejected", `Quick, check_csrf_mismatch_rejected);
    ("CSRF missing cookie rejected", `Quick, check_csrf_missing_cookie_rejected);
    ( "Login prunes expired session cookies",
      `Quick,
      check_login_prunes_expired_cookies );
    ( "Update cookie usage updates last_access and user_agent",
      `Quick,
      check_update_cookie_usage_updates_last_access );
  ]
