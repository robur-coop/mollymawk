open Test_utils
open Mock_devices
open Lwt.Infix

let hello_hvt = "mock binary data for unikernel create test"
let default_cfg = {|{"typ": "solo5", "cpuids": [0], "memory": 32}|}

let check_unikernels_info_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/api/unikernels" ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Response contains unikernel info from success instance" true
        (String.includes ~affix:"Unikernel Information" resp);
      Alcotest.(check bool)
        "Response contains error from failing instance" true
        (String.includes ~affix:"albatross failure" resp);
      Lwt.return_unit )

let check_unikernels_info_with_token () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user_with_token store >>= fun (_user, token) ->
      let req = make_get_request ~path:"/api/unikernels" ~token () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Response contains unikernel info" true
        (String.includes ~affix:"Unikernel Information" resp);
      Lwt.return_unit )

let check_unikernels_info_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/api/unikernels" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Response indicates user not found" true
        (String.includes ~affix:"User not found" resp);
      Lwt.return_unit )

let check_unikernel_create_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts =
        [
          ("albatross_instance", "default");
          ("unikernel_name", "hello");
          ("unikernel_config", default_cfg);
          ("unikernel_force_create", "false");
          ("molly_csrf", csrf_token);
          ("deploy_mode", "manual");
        ]
      in
      let file_part =
        ("binary", "hello-key.hvt", "application/octet-stream", hello_hvt)
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          ~session_cookie ~csrf_token "/api/unikernel/create"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Response contains success message" true
        (String.includes ~affix:"unikernel created" resp);
      Lwt.return_unit )

let check_unikernel_force_create_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts =
        [
          ("albatross_instance", "default");
          ("unikernel_name", "hello");
          ("unikernel_config", default_cfg);
          ("unikernel_force_create", "true");
          ("molly_csrf", csrf_token);
          ("deploy_mode", "manual");
        ]
      in
      let file_part =
        ("binary", "hello-key.hvt", "application/octet-stream", hello_hvt)
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          ~session_cookie ~csrf_token "/api/unikernel/create"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Response contains success message" true
        (String.includes ~affix:"unikernel force created" resp);
      Lwt.return_unit )

let check_unikernel_create_with_token () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user_with_token store >>= fun (_user, token) ->
      let parts =
        [
          ("albatross_instance", "default");
          ("unikernel_name", "hello");
          ("unikernel_config", default_cfg);
          ("unikernel_force_create", "false");
          ("molly_csrf", "dummy_csrf");
          ("deploy_mode", "manual");
        ]
      in
      let file_part =
        ("binary", "hello-key.hvt", "application/octet-stream", hello_hvt)
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          ~token "/api/unikernel/create"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Response contains success message" true
        (String.includes ~affix:"unikernel created" resp);
      Lwt.return_unit )

let check_unikernel_create_missing_fields () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts =
        [
          ("albatross_instance", "default");
          ("unikernel_config", default_cfg);
          ("unikernel_force_create", "false");
          ("molly_csrf", csrf_token);
          ("deploy_mode", "manual");
        ]
      in
      let file_part =
        ("binary", "hello-key.hvt", "application/octet-stream", hello_hvt)
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          ~session_cookie ~csrf_token "/api/unikernel/create"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Response indicates missing fields" true
        (String.includes ~affix:"One or more required fields are missing" resp);
      Lwt.return_unit )

let check_unikernel_create_unknown_instance () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts =
        [
          ("albatross_instance", "nonexistent-instance");
          ("unikernel_name", "hello");
          ("unikernel_config", default_cfg);
          ("unikernel_force_create", "false");
          ("molly_csrf", csrf_token);
          ("deploy_mode", "manual");
        ]
      in
      let file_part =
        ("binary", "hello-key.hvt", "application/octet-stream", hello_hvt)
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          ~session_cookie ~csrf_token "/api/unikernel/create"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 404 Not Found" true
        (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp);
      Alcotest.(check bool)
        "Response indicates error finding instance" true
        (String.includes ~affix:"Error finding albatross instance" resp);
      Lwt.return_unit )

let check_unikernel_create_invalid_config () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts =
        [
          ("albatross_instance", "default");
          ("unikernel_name", "hello");
          ("unikernel_config", "{ invalid json }");
          ("unikernel_force_create", "false");
          ("molly_csrf", csrf_token);
          ("deploy_mode", "manual");
        ]
      in
      let file_part =
        ("binary", "hello-key.hvt", "application/octet-stream", hello_hvt)
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          ~session_cookie ~csrf_token "/api/unikernel/create"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Response indicates invalid unikernel arguments" true
        (String.includes ~affix:"Invalid unikernel arguments" resp);
      Lwt.return_unit )

let check_unikernel_create_invalid_csrf () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, _csrf_token) ->
      let parts =
        [
          ("albatross_instance", "default");
          ("unikernel_name", "hello");
          ("unikernel_config", default_cfg);
          ("unikernel_force_create", "false");
          ("molly_csrf", "wrong_csrf_token");
          ("deploy_mode", "manual");
        ]
      in
      let file_part =
        ("binary", "hello-key.hvt", "application/octet-stream", hello_hvt)
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          ~session_cookie ~csrf_token:"different_csrf_cookie"
          "/api/unikernel/create"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Response indicates invalid CSRF" true
        (String.includes ~affix:"CSRF token" resp);
      Lwt.return_unit )

let check_unikernel_create_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let parts =
        [
          ("albatross_instance", "default");
          ("unikernel_name", "hello");
          ("unikernel_config", default_cfg);
          ("unikernel_force_create", "false");
          ("molly_csrf", "any_csrf");
          ("deploy_mode", "manual");
        ]
      in
      let file_part =
        ("binary", "hello-key.hvt", "application/octet-stream", hello_hvt)
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          "/api/unikernel/create"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Response indicates user not found" true
        (String.includes ~affix:"User not found" resp);
      Lwt.return_unit )

let check_unikernel_create_albatross_failure () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts =
        [
          ("albatross_instance", "failing");
          ("unikernel_name", "hello");
          ("unikernel_config", default_cfg);
          ("unikernel_force_create", "false");
          ("molly_csrf", csrf_token);
          ("deploy_mode", "manual");
        ]
      in
      let file_part =
        ("binary", "hello-key.hvt", "application/octet-stream", hello_hvt)
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          ~session_cookie ~csrf_token "/api/unikernel/create"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 500 Internal Server Error" true
        (String.starts_with ~prefix:"HTTP/1.1 500 Internal Server Error" resp);
      Alcotest.(check bool)
        "Response contains failure message" true
        (String.includes ~affix:"albatross failure" resp);
      Lwt.return_unit )

let check_unikernel_destroy_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        Fmt.str
          {|{"name": "hello", "albatross_instance": "default", "molly_csrf": "%s"}|}
          csrf_token
      in
      let req =
        make_post_request ~path:"/api/unikernel/destroy" ~body ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Destroy response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Lwt.return_unit )

let check_unikernel_destroy_failure () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        Fmt.str
          {|{"name": "hello", "albatross_instance": "failing", "molly_csrf": "%s"}|}
          csrf_token
      in
      let req =
        make_post_request ~path:"/api/unikernel/destroy" ~body ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Destroy failure response has HTTP 500 Internal Server Error" true
        (String.starts_with ~prefix:"HTTP/1.1 500 Internal Server Error" resp);
      Alcotest.(check bool)
        "Destroy failure message indicates albatross failure" true
        (String.includes ~affix:"albatross failure" resp);
      Lwt.return_unit )

let check_unikernel_restart_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body = Fmt.str {|{"molly_csrf": "%s"}|} csrf_token in
      let req =
        make_post_request
          ~path:"/api/unikernel/restart?instance=default&unikernel=hello" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Restart response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Lwt.return_unit )

let check_unikernel_restart_failure () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body = Fmt.str {|{"molly_csrf": "%s"}|} csrf_token in
      let req =
        make_post_request
          ~path:"/api/unikernel/restart?instance=failing&unikernel=hello" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Restart failure response has HTTP 500 Internal Server Error" true
        (String.starts_with ~prefix:"HTTP/1.1 500 Internal Server Error" resp);
      Alcotest.(check bool)
        "Restart failure message indicates albatross failure" true
        (String.includes ~affix:"albatross failure" resp);
      Lwt.return_unit )

let tests =
  [
    ("Unikernels list / info (session)", `Quick, check_unikernels_info_success);
    ("Unikernels list / info (token)", `Quick, check_unikernels_info_with_token);
    ( "Unikernels list / info (unauthenticated)",
      `Quick,
      check_unikernels_info_unauthenticated );
    ( "Successful unikernel create (manual deploy with .hvt binary)",
      `Quick,
      check_unikernel_create_success );
    ( "Successful unikernel force create",
      `Quick,
      check_unikernel_force_create_success );
    ( "Successful unikernel create with Bearer token",
      `Quick,
      check_unikernel_create_with_token );
    ( "Reject create when required fields are missing",
      `Quick,
      check_unikernel_create_missing_fields );
    ( "Reject create when Albatross instance is unknown",
      `Quick,
      check_unikernel_create_unknown_instance );
    ( "Reject create when unikernel config JSON is invalid",
      `Quick,
      check_unikernel_create_invalid_config );
    ( "Reject create when CSRF token is invalid",
      `Quick,
      check_unikernel_create_invalid_csrf );
    ( "Reject create when unauthenticated",
      `Quick,
      check_unikernel_create_unauthenticated );
    ( "Handle Albatross create error response",
      `Quick,
      check_unikernel_create_albatross_failure );
    ("Successful unikernel destroy", `Quick, check_unikernel_destroy_success);
    ("Handle Albatross destroy error", `Quick, check_unikernel_destroy_failure);
    ("Successful unikernel restart", `Quick, check_unikernel_restart_success);
    ("Handle Albatross restart error", `Quick, check_unikernel_restart_failure);
  ]
