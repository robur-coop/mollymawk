open Test_utils
open Mock_devices
open Lwt.Infix

let check_landing_page () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Alcotest.(check bool)
        "Contains landing page title" true
        (String.includes ~affix:"Deploy unikernels with ease" resp);
      Lwt.return_unit )

let check_sign_in_page () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/sign-in" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Alcotest.(check bool)
        "Contains sign-in form action" true
        (String.includes ~affix:"/api/login" resp);
      Lwt.return_unit )

let check_sign_up_page () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/sign-up" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Alcotest.(check bool)
        "Contains sign-up form action" true
        (String.includes ~affix:"/api/register" resp);
      Lwt.return_unit )

let check_static_javascript () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/main.js" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/javascript" true
        (String.includes ~affix:"text/javascript" resp);
      Alcotest.(check bool)
        "Contains js content" true
        (String.includes ~affix:"/* some js */" resp);
      Lwt.return_unit )

let check_static_stylesheet () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/style.css" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/css" true
        (String.includes ~affix:"text/css" resp);
      Alcotest.(check bool)
        "Contains css content" true
        (String.includes ~affix:"/* some css */" resp);
      Lwt.return_unit )

let check_grafana_dashboard_json () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/grafana.json" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is application/json" true
        (String.includes ~affix:"application/json" resp);
      Lwt.return_unit )

let check_static_image () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/images/robur.png" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is image/png" true
        (String.includes ~affix:"image/png" resp);
      Alcotest.(check bool)
        "Contains image content" true
        (String.includes ~affix:"robur" resp);
      Lwt.return_unit )

let check_static_image_molly_bird () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/images/molly_bird.jpeg" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is image/jpeg" true
        (String.includes ~affix:"image/jpeg" resp);
      Alcotest.(check bool)
        "Contains image content" true
        (String.includes ~affix:"molly" resp);
      Lwt.return_unit )

let check_static_image_albatross () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/images/albatross_1.png" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is image/jpeg" true
        (String.includes ~affix:"image/jpeg" resp);
      Alcotest.(check bool)
        "Contains image content" true
        (String.includes ~affix:"albatross" resp);
      Lwt.return_unit )

let check_static_image_dashboard () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/images/dashboard_1.png" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is image/png" true
        (String.includes ~affix:"image/png" resp);
      Alcotest.(check bool)
        "Contains image content" true
        (String.includes ~affix:"dashboard" resp);
      Lwt.return_unit )

let check_static_image_mirage () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/images/mirage_os_1.png" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is image/jpeg" true
        (String.includes ~affix:"image/jpeg" resp);
      Alcotest.(check bool)
        "Contains image content" true
        (String.includes ~affix:"mirage" resp);
      Lwt.return_unit )

let check_dashboard_authenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/dashboard" ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Lwt.return_unit )

let check_dashboard_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/dashboard" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to sign-in" true
        (is_redirect resp || String.includes ~affix:"/sign-in" resp);
      Lwt.return_unit )

let check_account_page_authenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/account" ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Lwt.return_unit )

let check_account_page_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/account" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to sign-in" true
        (is_redirect resp || String.includes ~affix:"/sign-in" resp);
      Lwt.return_unit )

let check_tokens_page_authenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/tokens" ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Lwt.return_unit )

let check_tokens_page_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/tokens" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to sign-in" true
        (is_redirect resp || String.includes ~affix:"/sign-in" resp);
      Lwt.return_unit )

let check_select_instance_authenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/select/instance?callback=/dashboard"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Alcotest.(check bool)
        "Contains instance selection content" true
        (String.includes ~affix:"Select an instance" resp);
      Lwt.return_unit )

let check_select_instance_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/select/instance" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to sign-in" true
        (is_redirect resp || String.includes ~affix:"/sign-in" resp);
      Lwt.return_unit )

let check_albatross_instances_redirect () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/albatross/instances" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to select instance" true
        (is_redirect resp && String.includes ~affix:"/select/instance" resp);
      Lwt.return_unit )

let check_usage_authenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/usage?instance=default" ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Lwt.return_unit )

let check_usage_missing_instance () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/usage" ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to instance selector" true
        (is_redirect resp && String.includes ~affix:"/select/instance" resp);
      Lwt.return_unit )

let check_usage_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/usage?instance=default" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to sign-in" true
        (is_redirect resp || String.includes ~affix:"/sign-in" resp);
      Lwt.return_unit )

let check_unikernel_info_authenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request
          ~path:"/unikernel/info?instance=default&unikernel=hello"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Lwt.return_unit )

let check_unikernel_info_name_casing () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let handler = make_app_request_handler store in
      let req_lower =
        make_get_request
          ~path:"/unikernel/info?instance=default&unikernel=hello"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint handler req_lower >>= fun resp_lower ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK for lowercase unikernel name" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp_lower);
      Alcotest.(check bool)
        "Response references unikernel hello" true
        (String.includes ~affix:"unikernel=hello" resp_lower);
      let req_cap =
        make_get_request
          ~path:"/unikernel/info?instance=default&unikernel=Hello"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint handler req_cap >>= fun resp_cap ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK for capitalized unikernel name" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp_cap);
      Alcotest.(check bool)
        "Hello references the same unikernel hello" true
        (String.includes ~affix:"unikernel=hello" resp_cap);
      let req_upper =
        make_get_request
          ~path:"/unikernel/info?instance=default&unikernel=HELLO"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint handler req_upper >>= fun resp_upper ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK for uppercase unikernel name" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp_upper);
      Alcotest.(check bool)
        "HELLO references the same unikernel hello" true
        (String.includes ~affix:"unikernel=hello" resp_upper);
      let req_nonexistent =
        make_get_request
          ~path:"/unikernel/info?instance=default&unikernel=other"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint handler req_nonexistent >>= fun resp_nonexistent ->
      Alcotest.(check bool)
        "Response has HTTP 404 Not Found for non-matching unikernel name" true
        (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp_nonexistent);
      Lwt.return_unit )

let check_unikernel_info_missing_unikernel () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/unikernel/info?instance=default"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_unikernel_info_missing_instance () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/unikernel/info?unikernel=hello" ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to instance selector" true
        (is_redirect resp && String.includes ~affix:"/select/instance" resp);
      Lwt.return_unit )

let check_unikernel_info_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req =
        make_get_request
          ~path:"/unikernel/info?instance=default&unikernel=hello" ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to sign-in" true
        (is_redirect resp || String.includes ~affix:"/sign-in" resp);
      Lwt.return_unit )

let check_unikernel_console_viewer_authenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request
          ~path:"/unikernel/console?instance=default&unikernel=hello"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Lwt.return_unit )

let check_unikernel_console_viewer_missing_unikernel () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/unikernel/console?instance=default"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_unikernel_console_viewer_missing_instance () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/unikernel/console?unikernel=hello"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to instance selector" true
        (is_redirect resp && String.includes ~affix:"/select/instance" resp);
      Lwt.return_unit )

let check_unikernel_console_viewer_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req =
        make_get_request
          ~path:"/unikernel/console?instance=default&unikernel=hello" ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to sign-in" true
        (is_redirect resp || String.includes ~affix:"/sign-in" resp);
      Lwt.return_unit )

let check_unikernel_deploy_authenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (user, session_cookie, csrf_token) ->
      let policies = make_default_policies ~domain:user.name () in
      let req =
        make_get_request ~path:"/unikernel/deploy?instance=default"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler ~policies store) req
      >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Lwt.return_unit )

let check_unikernel_deploy_missing_instance () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/unikernel/deploy" ~session_cookie ~csrf_token
          ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to instance selector" true
        (is_redirect resp && String.includes ~affix:"/select/instance" resp);
      Lwt.return_unit )

let check_unikernel_deploy_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req =
        make_get_request ~path:"/unikernel/deploy?instance=default" ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to sign-in" true
        (is_redirect resp || String.includes ~affix:"/sign-in" resp);
      Lwt.return_unit )

let check_unikernel_update_missing_unikernel () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/unikernel/update?instance=default"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_unikernel_update_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req =
        make_get_request
          ~path:"/unikernel/update?instance=default&unikernel=hello" ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to sign-in" true
        (is_redirect resp || String.includes ~affix:"/sign-in" resp);
      Lwt.return_unit )

let check_unikernel_update_compare_changes_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req =
        make_get_request
          ~path:
            "/unikernel/update/compare-changes?instance=default&unikernel=hello"
          ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to sign-in" true
        (is_redirect resp || String.includes ~affix:"/sign-in" resp);
      Lwt.return_unit )

let check_unikernel_update_compare_changes_missing_instance () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request
          ~path:"/unikernel/update/compare-changes?unikernel=hello"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to select instance" true
        (is_redirect resp || String.includes ~affix:"/select/instance" resp);
      Lwt.return_unit )

let check_unikernel_update_compare_changes_missing_unikernel () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request
          ~path:"/unikernel/update/compare-changes?instance=default"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_unikernel_update_compare_changes_bad_method () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_post_request
          ~path:
            "/unikernel/update/compare-changes?instance=default&unikernel=hello"
          ~body:"" ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_admin_users_superuser () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/admin/users" ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Lwt.return_unit )

let check_admin_users_non_admin () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store >>= fun _ ->
      (*this second user created will not be an admin*)
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/admin/users" ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects non-admin to sign-in" true
        (is_redirect resp || String.includes ~affix:"/sign-in" resp);
      Lwt.return_unit )

let check_admin_users_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/admin/users" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects unauthenticated to sign-in" true
        (is_redirect resp || String.includes ~affix:"/sign-in" resp);
      Lwt.return_unit )

let check_admin_user_profile_superuser () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (user, session_cookie, csrf_token) ->
      let req =
        make_get_request
          ~path:("/admin/user/profile?uuid=" ^ user.uuid)
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Lwt.return_unit )

let check_admin_user_profile_not_found () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request
          ~path:"/admin/user/profile?uuid=00000000-0000-0000-0000-000000000000"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 404 Not Found" true
        (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp);
      Lwt.return_unit )

let check_admin_user_profile_missing_uuid () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/admin/user/profile" ~session_cookie ~csrf_token
          ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_admin_user_unikernels_superuser () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (user, session_cookie, csrf_token) ->
      let req =
        make_get_request
          ~path:("/admin/user/unikernels?uuid=" ^ user.uuid)
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Lwt.return_unit )

let check_admin_user_unikernels_missing_uuid () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/admin/user/unikernels" ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_admin_user_policy_superuser () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (user, session_cookie, csrf_token) ->
      let req =
        make_get_request
          ~path:("/admin/user/policy?uuid=" ^ user.uuid)
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Lwt.return_unit )

let check_admin_user_policy_missing_uuid () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/admin/user/policy" ~session_cookie ~csrf_token
          ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_admin_user_policy_edit_superuser () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (user, session_cookie, csrf_token) ->
      let policies = make_default_policies ~domain:user.name () in
      let req =
        make_get_request
          ~path:("/admin/u/policy/edit?uuid=" ^ user.uuid ^ "&instance=default")
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler ~policies store) req
      >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Lwt.return_unit )

let check_admin_user_policy_edit_missing_uuid () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/admin/u/policy/edit?instance=default"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_admin_user_policy_edit_missing_instance () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (user, session_cookie, csrf_token) ->
      let req =
        make_get_request
          ~path:("/admin/u/policy/edit?uuid=" ^ user.uuid)
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to instance selector" true
        (is_redirect resp && String.includes ~affix:"/select/instance" resp);
      Lwt.return_unit )

let check_admin_settings_albatross_superuser () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/admin/settings/albatross" ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Lwt.return_unit )

let check_admin_settings_albatross_non_admin () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store >>= fun _ ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/admin/settings/albatross" ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects non-admin to sign-in" true
        (is_redirect resp || String.includes ~affix:"/sign-in" resp);
      Lwt.return_unit )

let check_admin_settings_email_superuser () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/admin/settings/email" ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Lwt.return_unit )

let check_admin_settings_email_non_admin () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store >>= fun _ ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/admin/settings/email" ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects non-admin to sign-in" true
        (is_redirect resp || String.includes ~affix:"/sign-in" resp);
      Lwt.return_unit )

let check_admin_albatross_errors_superuser () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/admin/albatross/errors?instance=default"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Lwt.return_unit )

let check_admin_albatross_errors_missing_instance () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/admin/albatross/errors" ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to instance selector" true
        (is_redirect resp && String.includes ~affix:"/select/instance" resp);
      Lwt.return_unit )

let check_verify_email_page () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/verify-email" ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Alcotest.(check bool)
        "Contains X-MOLLY-CSRF header" true
        (String.includes ~affix:"X-MOLLY-CSRF" resp);
      Alcotest.(check bool)
        "Contains verify email title" true
        (String.includes ~affix:"Verify Email" resp);
      let updated_user = Option.get (Storage.find_by_uuid store user.uuid) in
      Alcotest.(check bool)
        "User has email_verification_uuid assigned" true
        (Option.is_some updated_user.email_verification_uuid);
      Lwt.return_unit )

let check_verify_email_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/verify-email" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool) "Response is a redirect" true (is_redirect resp);
      Alcotest.(check bool)
        "Redirects to /sign-in" true
        (String.includes ~affix:"location: /sign-in"
           (String.lowercase_ascii resp));
      Lwt.return_unit )

let check_verify_email_invalid_method () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_post_request ~path:"/verify-email" ~body:"" ~session_cookie
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

let check_page_not_found () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/some/nonexistent/page" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Contains page not found message" true
        (String.includes ~affix:"This page cannot be found" resp);
      Lwt.return_unit )

let check_error_layout_html_escaping () =
  let status =
    {
      Utils.Status.code = 400;
      title = "Bad Request";
      data =
        `String "<script>alert('xss')</script> <img src=x onerror=alert(1)>";
      success = false;
    }
  in
  let rendered_html =
    Format.asprintf "%a" (Tyxml_html.pp_elt ()) (Error_page.error_layout status)
  in
  Alcotest.(check bool)
    "HTML does not contain unescaped script tag" false
    (String.includes ~affix:"<script>" rendered_html);
  Alcotest.(check bool)
    "HTML does not contain unescaped img tag" false
    (String.includes ~affix:"<img" rendered_html);
  Alcotest.(check bool)
    "HTML contains escaped script entity" true
    (String.includes ~affix:"&lt;script&gt;alert('xss')&lt;/script&gt;"
       rendered_html);
  Alcotest.(check bool)
    "HTML contains escaped img entity" true
    (String.includes ~affix:"&lt;img src=x onerror=alert(1)&gt;" rendered_html)

let check_status_to_json_escaping () =
  let injection_payload =
    "<script>alert(\"xss\")</script>\n\r\t\" \"><img src=x onerror=alert(1)>"
  in
  let status =
    {
      Utils.Status.code = 400;
      title = "Bad Request";
      data = `String injection_payload;
      success = false;
    }
  in
  let json_str = Utils.Status.to_json status in
  let parsed =
    match Utils.Json.from_string json_str with
    | Ok j -> j
    | Error (`Msg e) -> Alcotest.fail ("Failed to parse JSON: " ^ e)
  in
  let data_str =
    match parsed with
    | `Assoc kvs -> (
        match List.assoc_opt "data" kvs with
        | Some (`String s) -> s
        | _ -> Alcotest.fail "Missing data field")
    | _ -> Alcotest.fail "Expected Assoc"
  in
  Alcotest.(check string)
    "Decoded data string matches original injection payload exactly"
    injection_payload data_str;
  Alcotest.(check bool)
    "No stray backslashes in data" false
    (String.includes ~affix:"\\\"" data_str)

let check_api_error_injection_handling () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let injection_name = "<script>alert('xss')</script>\"" in
      let json_body =
        Fmt.str
          {|{ "name": %s, "email": "test@robur.coop", "password": "Password123!", "form_csrf": "test" }|}
          (Yojson.Basic.to_string (`String injection_name))
      in
      let req =
        make_post_request ~path:"/api/register" ~body:json_body
          ~csrf_token:"test" ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response is HTTP 400" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Response content-type is application/json" true
        (String.includes ~affix:"content-type: application/json"
           (String.lowercase_ascii resp));
      Alcotest.(check bool)
        "Response body does not contain unescaped script tag" false
        (String.includes ~affix:"<script>" resp);
      Lwt.return_unit )

let check_api_malformed_json_error_escaping () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req =
        make_post_request ~path:"/api/register"
          ~body:"{ \"invalid\": \"<script>alert(1)</script>\", broken: }"
          ~csrf_token:"test" ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response is HTTP 400" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Response body does not contain unescaped script tag" false
        (String.includes ~affix:"<script>" resp);
      Lwt.return_unit )

let check_unauthenticated_redirect_with_target () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let target1 = "/account" in
      let req1 = make_get_request ~path:target1 () in
      query_endpoint (make_app_request_handler store) req1 >>= fun resp1 ->
      Alcotest.(check bool) "Response is redirect" true (is_redirect resp1);
      Alcotest.(check bool)
        "Redirects to sign-in with encoded redirect param" true
        (String.includes
           ~affix:("location: /sign-in?redirect=" ^ Uri.pct_encode target1)
           resp1);

      let target2 = "/tokens" in
      let req2 = make_get_request ~path:target2 () in
      query_endpoint (make_app_request_handler store) req2 >>= fun resp2 ->
      Alcotest.(check bool) "Response is redirect" true (is_redirect resp2);
      Alcotest.(check bool)
        "Redirects to sign-in with encoded tokens path" true
        (String.includes
           ~affix:("location: /sign-in?redirect=" ^ Uri.pct_encode target2)
           resp2);

      let target3 = "/usage?instance=default" in
      let req3 = make_get_request ~path:target3 () in
      query_endpoint (make_app_request_handler store) req3 >>= fun resp3 ->
      Alcotest.(check bool) "Response is redirect" true (is_redirect resp3);
      Alcotest.(check bool)
        "Redirects to sign-in with encoded query parameters" true
        (String.includes
           ~affix:("location: /sign-in?redirect=" ^ Uri.pct_encode target3)
           resp3);

      let target4 = "/unikernel/info?instance=default&unikernel=hello" in
      let req4 = make_get_request ~path:target4 () in
      query_endpoint (make_app_request_handler store) req4 >>= fun resp4 ->
      Alcotest.(check bool) "Response is redirect" true (is_redirect resp4);
      Alcotest.(check bool)
        "Redirects to sign-in with encoded multi-param query" true
        (String.includes
           ~affix:("location: /sign-in?redirect=" ^ Uri.pct_encode target4)
           resp4);
      Lwt.return_unit )

let check_unauthenticated_redirect_excluded_routes () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/dashboard" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool) "Response is redirect" true (is_redirect resp);
      Alcotest.(check bool)
        "Redirect location is exactly /sign-in" true
        (String.includes ~affix:"location: /sign-in\r\n" resp
        || String.includes ~affix:"location: /sign-in\n" resp);
      Alcotest.(check bool)
        "Does not include redirect query param" false
        (String.includes ~affix:"redirect=" resp);
      Lwt.return_unit )

let check_sign_in_page_redirect_handling () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/sign-in?redirect=%2Faccount" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Contains URLSearchParams search query extraction" true
        (String.includes ~affix:"URLSearchParams(window.location.search)" resp);
      Alcotest.(check bool)
        "Contains redirect URL extraction with fallback" true
        (String.includes ~affix:"urlParams.get('redirect') || '/dashboard'" resp);
      Alcotest.(check bool)
        "Contains window.location.replace" true
        (String.includes ~affix:"window.location.replace(redirectUrl)" resp);
      Lwt.return_unit )

let tests =
  [
    ("Landing page (/)", `Quick, check_landing_page);
    ("Sign-in page (/sign-in)", `Quick, check_sign_in_page);
    ("Sign-up page (/sign-up)", `Quick, check_sign_up_page);
    ("Static javascript (/main.js)", `Quick, check_static_javascript);
    ("Static stylesheet (/style.css)", `Quick, check_static_stylesheet);
    ( "Grafana dashboard JSON (/grafana.json)",
      `Quick,
      check_grafana_dashboard_json );
    ("Static image (/images/robur.png)", `Quick, check_static_image);
    ( "Static image (/images/molly_bird.jpeg)",
      `Quick,
      check_static_image_molly_bird );
    ( "Static image (/images/albatross_1.png)",
      `Quick,
      check_static_image_albatross );
    ( "Static image (/images/dashboard_1.png)",
      `Quick,
      check_static_image_dashboard );
    ("Static image (/images/mirage_os_1.png)", `Quick, check_static_image_mirage);
    ( "Dashboard (/dashboard) authenticated",
      `Quick,
      check_dashboard_authenticated );
    ( "Dashboard (/dashboard) unauthenticated redirects",
      `Quick,
      check_dashboard_unauthenticated );
    ( "User account (/account) authenticated",
      `Quick,
      check_account_page_authenticated );
    ( "User account (/account) unauthenticated redirects",
      `Quick,
      check_account_page_unauthenticated );
    ( "Tokens view (/tokens) authenticated",
      `Quick,
      check_tokens_page_authenticated );
    ( "Tokens view (/tokens) unauthenticated redirects",
      `Quick,
      check_tokens_page_unauthenticated );
    ( "Select instance (/select/instance) authenticated",
      `Quick,
      check_select_instance_authenticated );
    ( "Select instance (/select/instance) unauthenticated redirects",
      `Quick,
      check_select_instance_unauthenticated );
    ( "Albatross instances (/albatross/instances) redirects",
      `Quick,
      check_albatross_instances_redirect );
    ( "Usage (/usage) authenticated with instance",
      `Quick,
      check_usage_authenticated );
    ( "Usage (/usage) missing instance redirects",
      `Quick,
      check_usage_missing_instance );
    ( "Usage (/usage) unauthenticated redirects",
      `Quick,
      check_usage_unauthenticated );
    ( "Unikernel info (/unikernel/info) authenticated",
      `Quick,
      check_unikernel_info_authenticated );
    ("Unikernel name casing", `Quick, check_unikernel_info_name_casing);
    ( "Unikernel info (/unikernel/info) missing unikernel",
      `Quick,
      check_unikernel_info_missing_unikernel );
    ( "Unikernel info (/unikernel/info) missing instance redirects",
      `Quick,
      check_unikernel_info_missing_instance );
    ( "Unikernel info (/unikernel/info) unauthenticated redirects",
      `Quick,
      check_unikernel_info_unauthenticated );
    ( "Unikernel console viewer (/unikernel/console) authenticated",
      `Quick,
      check_unikernel_console_viewer_authenticated );
    ( "Unikernel console viewer (/unikernel/console) missing unikernel",
      `Quick,
      check_unikernel_console_viewer_missing_unikernel );
    ( "Unikernel console viewer (/unikernel/console) missing instance redirects",
      `Quick,
      check_unikernel_console_viewer_missing_instance );
    ( "Unikernel console viewer (/unikernel/console) unauthenticated redirects",
      `Quick,
      check_unikernel_console_viewer_unauthenticated );
    ( "Unikernel deploy (/unikernel/deploy) authenticated with policy",
      `Quick,
      check_unikernel_deploy_authenticated );
    ( "Unikernel deploy (/unikernel/deploy) missing instance redirects",
      `Quick,
      check_unikernel_deploy_missing_instance );
    ( "Unikernel deploy (/unikernel/deploy) unauthenticated redirects",
      `Quick,
      check_unikernel_deploy_unauthenticated );
    ( "Unikernel update (/unikernel/update) missing unikernel",
      `Quick,
      check_unikernel_update_missing_unikernel );
    ( "Unikernel update (/unikernel/update) unauthenticated redirects",
      `Quick,
      check_unikernel_update_unauthenticated );
    ( "Unikernel compare changes unauthenticated redirects",
      `Quick,
      check_unikernel_update_compare_changes_unauthenticated );
    ( "Unikernel compare changes missing instance redirects",
      `Quick,
      check_unikernel_update_compare_changes_missing_instance );
    ( "Unikernel compare changes missing unikernel",
      `Quick,
      check_unikernel_update_compare_changes_missing_unikernel );
    ( "Unikernel compare changes bad method",
      `Quick,
      check_unikernel_update_compare_changes_bad_method );
    ( "Admin users (/admin/users) as superuser",
      `Quick,
      check_admin_users_superuser );
    ( "Admin users (/admin/users) non-admin redirects",
      `Quick,
      check_admin_users_non_admin );
    ( "Admin users (/admin/users) unauthenticated redirects",
      `Quick,
      check_admin_users_unauthenticated );
    ( "Admin user profile (/admin/user/profile) as superuser",
      `Quick,
      check_admin_user_profile_superuser );
    ( "Admin user profile (/admin/user/profile) non-existent uuid",
      `Quick,
      check_admin_user_profile_not_found );
    ( "Admin user profile (/admin/user/profile) missing uuid",
      `Quick,
      check_admin_user_profile_missing_uuid );
    ( "Admin user unikernels (/admin/user/unikernels) as superuser",
      `Quick,
      check_admin_user_unikernels_superuser );
    ( "Admin user unikernels (/admin/user/unikernels) missing uuid",
      `Quick,
      check_admin_user_unikernels_missing_uuid );
    ( "Admin user policy (/admin/user/policy) as superuser",
      `Quick,
      check_admin_user_policy_superuser );
    ( "Admin user policy (/admin/user/policy) missing uuid",
      `Quick,
      check_admin_user_policy_missing_uuid );
    ( "Admin user policy edit (/admin/u/policy/edit) as superuser",
      `Quick,
      check_admin_user_policy_edit_superuser );
    ( "Admin user policy edit (/admin/u/policy/edit) missing uuid",
      `Quick,
      check_admin_user_policy_edit_missing_uuid );
    ( "Admin user policy edit (/admin/u/policy/edit) missing instance redirects",
      `Quick,
      check_admin_user_policy_edit_missing_instance );
    ( "Admin settings albatross (/admin/settings/albatross) as superuser",
      `Quick,
      check_admin_settings_albatross_superuser );
    ( "Admin settings albatross (/admin/settings/albatross) non-admin redirects",
      `Quick,
      check_admin_settings_albatross_non_admin );
    ( "Admin settings email (/admin/settings/email) as superuser",
      `Quick,
      check_admin_settings_email_superuser );
    ( "Admin settings email (/admin/settings/email) non-admin redirects",
      `Quick,
      check_admin_settings_email_non_admin );
    ( "Admin albatross errors (/admin/albatross/errors) as superuser",
      `Quick,
      check_admin_albatross_errors_superuser );
    ( "Admin albatross errors (/admin/albatross/errors) missing instance \
       redirects",
      `Quick,
      check_admin_albatross_errors_missing_instance );
    ("Verify email page (/verify-email)", `Quick, check_verify_email_page);
    ( "Verify email page unauthenticated redirects",
      `Quick,
      check_verify_email_unauthenticated );
    ( "Verify email page invalid method",
      `Quick,
      check_verify_email_invalid_method );
    ("Page not found (unknown path)", `Quick, check_page_not_found);
    ( "Error layout HTML escaping against injection",
      `Quick,
      check_error_layout_html_escaping );
    ( "Status JSON escaping against injection",
      `Quick,
      check_status_to_json_escaping );
    ("API error injection handling", `Quick, check_api_error_injection_handling);
    ( "API malformed JSON error escaping",
      `Quick,
      check_api_malformed_json_error_escaping );
    ( "Unauthenticated redirect includes encoded target",
      `Quick,
      check_unauthenticated_redirect_with_target );
    ( "Unauthenticated redirect for excluded routes omits target",
      `Quick,
      check_unauthenticated_redirect_excluded_routes );
    ( "Sign-in page contains client redirect handling script",
      `Quick,
      check_sign_in_page_redirect_handling );
  ]
