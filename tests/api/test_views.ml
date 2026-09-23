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
    ("Page not found (unknown path)", `Quick, check_page_not_found);
  ]
