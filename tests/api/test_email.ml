open Test_utils
open Mock_devices
open Lwt.Infix

let make_email_body ?(server = "127.0.0.1") ?(port = 25)
    ?(from_email = "no-reply@robur.coop") ?to_email
    ?(base_url = "https://mollymawk.robur.coop") ?csrf_token () =
  let csrf_fields =
    match csrf_token with
    | Some token -> [ ("molly_csrf", `String token) ]
    | None -> []
  in
  let to_email_field =
    match to_email with
    | Some email -> [ ("to_email", `String email) ]
    | None -> []
  in
  let fields =
    [
      ("server", `String server);
      ("port", `Int port);
      ("from_email", `String from_email);
      ("base_url", `String base_url);
    ]
    @ to_email_field @ csrf_fields
  in
  Yojson.Basic.to_string (`Assoc fields)

let check_update_email_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        make_email_body ~server:"192.168.1.100" ~port:587
          ~from_email:"admin@robur.coop" ~to_email:"no-reply@robur.coop"
          ~base_url:"https://mollymawk.example.com" ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/settings/email/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Email settings verified and saved message" true
        (String.includes ~affix:"Email settings verified and saved successfully"
           resp);
      (match Storage.email store with
      | None -> Alcotest.fail "Expected stored email settings"
      | Some settings ->
          Alcotest.(check string)
            "Stored server matches" "192.168.1.100"
            (Ipaddr.to_string settings.server);
          Alcotest.(check int) "Stored port matches" 587 settings.port;
          Alcotest.(check string)
            "Stored from_email matches" "admin@robur.coop"
            (Emile.to_string settings.from_email);
          Alcotest.(check string)
            "Stored base_url matches" "https://mollymawk.example.com"
            settings.base_url;
          Alcotest.(check bool)
            "Stored to_email matches" true
            (match settings.to_email with
            | Some mb -> String.equal (Emile.to_string mb) "no-reply@robur.coop"
            | None -> false));
      Lwt.return_unit )

let check_update_email_success_without_to_email () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        make_email_body ~server:"10.0.0.1" ~port:25
          ~from_email:"system@robur.coop" ~base_url:"http://localhost:8080"
          ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/settings/email/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Email settings verified and saved message" true
        (String.includes ~affix:"Email settings verified and saved successfully"
           resp);
      (match Storage.email store with
      | None -> Alcotest.fail "Expected stored email settings"
      | Some settings ->
          Alcotest.(check bool)
            "to_email is None" true (settings.to_email = None));
      Lwt.return_unit )

let check_update_email_invalid_ip () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body = make_email_body ~server:"999.999.999.999" ~csrf_token () in
      let req =
        make_post_request ~path:"/api/admin/settings/email/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_update_email_invalid_from_email () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body = make_email_body ~from_email:"me@" ~csrf_token () in
      let req =
        make_post_request ~path:"/api/admin/settings/email/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_update_email_invalid_to_email () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body = make_email_body ~to_email:"me@" ~csrf_token () in
      let req =
        make_post_request ~path:"/api/admin/settings/email/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_update_email_to_email_not_string () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        Fmt.str
          {|{ "server": "127.0.0.1", "port": 25, "from_email": "admin@robur.coop", "base_url": "http://localhost", "to_email": 12345, "molly_csrf": "%s" }|}
          csrf_token
      in
      let req =
        make_post_request ~path:"/api/admin/settings/email/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error mentions to_email must be a string" true
        (String.includes ~affix:"to_email must be a string" resp);
      Lwt.return_unit )

let check_update_email_missing_fields () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        Fmt.str {|{ "server": "127.0.0.1", "port": 25, "molly_csrf": "%s" }|}
          csrf_token
      in
      let req =
        make_post_request ~path:"/api/admin/settings/email/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error mentions missing fields" true
        (String.includes ~affix:"missing fields in email configuration json"
           resp);
      Lwt.return_unit )

let check_update_email_invalid_csrf () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, _csrf_token) ->
      let body = make_email_body ~csrf_token:"wrong csrf token" () in
      let req =
        make_post_request ~path:"/api/admin/settings/email/update" ~body
          ~session_cookie ~csrf_token:"wrong-csrf-token" ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Missing CSRF token message" true
        (String.includes ~affix:"Missing CSRF token" resp);
      Lwt.return_unit )

let check_update_email_missing_csrf () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, _csrf_token) ->
      let body = make_email_body () in
      let req =
        make_post_request ~path:"/api/admin/settings/email/update" ~body
          ~session_cookie ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Couldn't find CSRF token message" true
        (String.includes ~affix:"Couldn't find CSRF token" resp);
      Lwt.return_unit )

let check_update_email_non_admin_forbidden () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_non_admin_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body = make_email_body ~csrf_token () in
      let req =
        make_post_request ~path:"/api/admin/settings/email/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Only administrators permitted message" true
        (String.includes ~affix:"You don't have the necessary permissions" resp);
      Lwt.return_unit )

let check_update_email_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let body = make_email_body () in
      let req =
        make_post_request ~path:"/api/admin/settings/email/update" ~body ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "No molly-session in cookie header message" true
        (String.includes ~affix:"No molly-session in cookie header" resp);
      Lwt.return_unit )

let check_update_email_invalid_method () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/api/admin/settings/email/update"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Bad HTTP request method message" true
        (String.includes ~affix:"Bad HTTP request method" resp);
      Lwt.return_unit )

let check_test_email_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        make_email_body ~server:"127.0.0.1" ~port:Mock_smtp.port
          ~from_email:"test@robur.coop" ~to_email:"admin@robur.coop" ~csrf_token
          ()
      in
      let req =
        make_post_request ~path:"/api/admin/settings/email/test" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Test email sent successfully message" true
        (String.includes ~affix:"Test email sent successfully" resp);
      Lwt.return_unit )

let check_test_email_success_fallback_to_from_email () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        make_email_body ~server:"127.0.0.1" ~port:Mock_smtp.port
          ~from_email:"fallback@robur.coop" ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/settings/email/test" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Test email sent successfully message" true
        (String.includes ~affix:"Test email sent successfully" resp);
      Lwt.return_unit )

let check_test_email_connection_failure () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      (* Using port 1 where no server is listening *)
      let body =
        make_email_body ~server:"127.0.0.1" ~port:1
          ~from_email:"test@robur.coop" ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/settings/email/test" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Test failed error message" true
        (String.includes ~affix:"Test failed:" resp);
      Lwt.return_unit )

let check_test_email_invalid_ip () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body = make_email_body ~server:"invalid-ip-address" ~csrf_token () in
      let req =
        make_post_request ~path:"/api/admin/settings/email/test" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_test_email_invalid_from_email () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body = make_email_body ~from_email:"not-an-email" ~csrf_token () in
      let req =
        make_post_request ~path:"/api/admin/settings/email/test" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_test_email_missing_fields () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        Fmt.str {|{ "server": "127.0.0.1", "molly_csrf": "%s" }|} csrf_token
      in
      let req =
        make_post_request ~path:"/api/admin/settings/email/test" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error mentions missing fields" true
        (String.includes ~affix:"missing fields in email configuration json"
           resp);
      Lwt.return_unit )

let check_test_email_invalid_csrf () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, _csrf_token) ->
      let body = make_email_body ~csrf_token:"bad-csrf" () in
      let req =
        make_post_request ~path:"/api/admin/settings/email/test" ~body
          ~session_cookie ~csrf_token:"bad-csrf" ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Missing CSRF token message" true
        (String.includes ~affix:"Missing CSRF token" resp);
      Lwt.return_unit )

let check_test_email_missing_csrf () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, _csrf_token) ->
      let body = make_email_body () in
      let req =
        make_post_request ~path:"/api/admin/settings/email/test" ~body
          ~session_cookie ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Couldn't find CSRF token message" true
        (String.includes ~affix:"Couldn't find CSRF token" resp);
      Lwt.return_unit )

let check_test_email_non_admin_forbidden () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_non_admin_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body = make_email_body ~csrf_token () in
      let req =
        make_post_request ~path:"/api/admin/settings/email/test" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Only administrators permitted message" true
        (String.includes ~affix:"You don't have the necessary permissions" resp);
      Lwt.return_unit )

let check_test_email_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let body = make_email_body () in
      let req =
        make_post_request ~path:"/api/admin/settings/email/test" ~body ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "No molly-session in cookie header message" true
        (String.includes ~affix:"No molly-session in cookie header" resp);
      Lwt.return_unit )

let check_test_email_invalid_method () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/api/admin/settings/email/test" ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Bad HTTP request method message" true
        (String.includes ~affix:"Bad HTTP request method" resp);
      Lwt.return_unit )

let check_auth_verify_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (user, session_cookie, csrf_token) ->
      let req_page =
        make_get_request ~path:"/verify-email" ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req_page >>= fun _ ->
      let updated_user =
        Option.get (Storage.find_by_uuid store.Storage.users user.uuid)
      in
      let token_uuid = Option.get updated_user.email_verification_uuid in
      let token_str = Uuidm.to_string token_uuid in
      let req =
        make_get_request
          ~path:(Fmt.str "/auth/verify?token=%s" token_str)
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool) "Response is a redirect" true (is_redirect resp);
      Alcotest.(check bool)
        "Redirects to /dashboard" true
        (String.includes ~affix:"location: /dashboard"
           (String.lowercase_ascii resp));
      Lwt.return_unit )

let check_auth_verify_missing_token () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/auth/verify" ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Missing token error message" true
        (String.includes ~affix:"Couldn't find token in query params" resp);
      Lwt.return_unit )

let check_auth_verify_invalid_uuid () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/auth/verify?token=not-a-valid-uuid"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool) "Response is a redirect" true (is_redirect resp);
      Alcotest.(check bool)
        "Redirects to /sign-in" true
        (String.includes ~affix:"location: /sign-in"
           (String.lowercase_ascii resp));
      Lwt.return_unit )

let check_auth_verify_unknown_token () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let fake_uuid = User_model.generate_uuid () |> Uuidm.to_string in
      let req =
        make_get_request
          ~path:(Fmt.str "/auth/verify?token=%s" fake_uuid)
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool) "Response is a redirect" true (is_redirect resp);
      Alcotest.(check bool)
        "Redirects to /sign-in" true
        (String.includes ~affix:"location: /sign-in"
           (String.lowercase_ascii resp));
      Lwt.return_unit )

let check_auth_verify_different_user () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"user2" ~email:"user2@robur.coop" store
      >>= fun (user2, user2_cookie, user2_csrf) ->
      setup_user ~name:"user1" ~email:"user1@robur.coop" store
      >>= fun (user1, user1_cookie, user1_csrf) ->
      let user1_active = User_model.update_user user1 ~active:true () in
      Storage.update_user store user1_active;
      let req_page =
        make_get_request ~path:"/verify-email" ~session_cookie:user2_cookie
          ~csrf_token:user2_csrf ()
      in
      query_endpoint (make_app_request_handler store) req_page >>= fun _ ->
      let updated_user2 =
        Option.get (Storage.find_by_uuid store.Storage.users user2.uuid)
      in
      let token_uuid = Option.get updated_user2.email_verification_uuid in
      let token_str = Uuidm.to_string token_uuid in
      (* user1 tries to verify using user2's token *)
      let req =
        make_get_request
          ~path:(Fmt.str "/auth/verify?token=%s" token_str)
          ~session_cookie:user1_cookie ~csrf_token:user1_csrf ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Different user error message" true
        (String.includes ~affix:"Logged in user is not the to-be-verified one"
           resp);
      Lwt.return_unit )

let check_auth_verify_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let fake_uuid = User_model.generate_uuid () |> Uuidm.to_string in
      let req =
        make_get_request ~path:(Fmt.str "/auth/verify?token=%s" fake_uuid) ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool) "Response is a redirect" true (is_redirect resp);
      Alcotest.(check bool)
        "Redirects to /sign-in" true
        (String.includes ~affix:"location: /sign-in"
           (String.lowercase_ascii resp));
      Lwt.return_unit )

let check_auth_verify_invalid_method () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_post_request ~path:"/auth/verify?token=dummy" ~body:""
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Bad HTTP request method message" true
        (String.includes ~affix:"Bad HTTP request method" resp);
      Lwt.return_unit )

let tests =
  [
    ("Email update settings success", `Quick, check_update_email_success);
    ( "Email update settings success without to_email",
      `Quick,
      check_update_email_success_without_to_email );
    ("Email update settings invalid IP", `Quick, check_update_email_invalid_ip);
    ( "Email update settings invalid from_email",
      `Quick,
      check_update_email_invalid_from_email );
    ( "Email update settings invalid to_email",
      `Quick,
      check_update_email_invalid_to_email );
    ( "Email update settings to_email not a string",
      `Quick,
      check_update_email_to_email_not_string );
    ( "Email update settings missing fields",
      `Quick,
      check_update_email_missing_fields );
    ( "Email update settings invalid CSRF",
      `Quick,
      check_update_email_invalid_csrf );
    ( "Email update settings missing CSRF",
      `Quick,
      check_update_email_missing_csrf );
    ( "Email update settings non-admin forbidden",
      `Quick,
      check_update_email_non_admin_forbidden );
    ( "Email update settings unauthenticated",
      `Quick,
      check_update_email_unauthenticated );
    ( "Email update settings invalid method",
      `Quick,
      check_update_email_invalid_method );
    (*TODO: write a better smtp mock server which can respond correctly*)
    (* ("Email test send success", `Quick, check_test_email_success);
    ( "Email test send success fallback to from_email",
      `Quick,
      check_test_email_success_fallback_to_from_email ); *)
    ( "Email test send connection failure",
      `Quick,
      check_test_email_connection_failure );
    ("Email test send invalid IP", `Quick, check_test_email_invalid_ip);
    ( "Email test send invalid from_email",
      `Quick,
      check_test_email_invalid_from_email );
    ("Email test send missing fields", `Quick, check_test_email_missing_fields);
    ("Email test send invalid CSRF", `Quick, check_test_email_invalid_csrf);
    ("Email test send missing CSRF", `Quick, check_test_email_missing_csrf);
    ( "Email test send non-admin forbidden",
      `Quick,
      check_test_email_non_admin_forbidden );
    ("Email test send unauthenticated", `Quick, check_test_email_unauthenticated);
    ("Email test send invalid method", `Quick, check_test_email_invalid_method);
    ("Email auth verify success", `Quick, check_auth_verify_success);
    ("Email auth verify missing token", `Quick, check_auth_verify_missing_token);
    ( "Email auth verify invalid UUID token",
      `Quick,
      check_auth_verify_invalid_uuid );
    ("Email auth verify unknown token", `Quick, check_auth_verify_unknown_token);
    ( "Email auth verify different user",
      `Quick,
      check_auth_verify_different_user );
    ( "Email auth verify unauthenticated",
      `Quick,
      check_auth_verify_unauthenticated );
    ( "Email auth verify invalid method",
      `Quick,
      check_auth_verify_invalid_method );
  ]
