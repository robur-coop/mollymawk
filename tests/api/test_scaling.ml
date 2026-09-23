open Test_utils
open Mock_devices
open Lwt.Infix

let check_monitoring_status_session () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request
          ~path:"/api/unikernel/monitoring/status?unikernel=hello"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Monitoring status response has HTTP 400 Bad Request on connection \
         failure"
        true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Monitoring status error mentions connection failure" true
        (String.includes ~affix:"Failed to connect" resp);
      Lwt.return_unit )

let check_monitoring_status_token () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user_with_token store >>= fun (_user, token) ->
      let req =
        make_get_request
          ~path:"/api/unikernel/monitoring/status?unikernel=hello" ~token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Monitoring status with token response has HTTP 400 Bad Request on \
         connection failure"
        true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Monitoring status error mentions connection failure" true
        (String.includes ~affix:"Failed to connect" resp);
      Lwt.return_unit )

let check_monitoring_status_missing_unikernel () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/api/unikernel/monitoring/status"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Monitoring status missing unikernel has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_monitoring_status_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req =
        make_get_request
          ~path:"/api/unikernel/monitoring/status?unikernel=hello" ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Monitoring status unauthenticated has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_monitoring_status_bad_method () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_post_request
          ~path:"/api/unikernel/monitoring/status?unikernel=hello" ~body:""
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Monitoring status bad method has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_monitoring_update_missing_fields () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts = [ ("molly_csrf", csrf_token) ] in
      let req =
        make_multipart_request ~parts ~session_cookie ~csrf_token
          "/api/unikernel/monitoring/update"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Monitoring update missing fields has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Monitoring update error mentions missing fields" true
        (String.includes ~affix:"Missing unikernel name or command in request."
           resp);
      Lwt.return_unit )

let check_monitoring_update_malformed_command () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts =
        [
          ("molly_csrf", csrf_token);
          ("unikernel_name", "hello");
          ("command", "invalid command");
        ]
      in
      let req =
        make_multipart_request ~parts ~session_cookie ~csrf_token
          "/api/unikernel/monitoring/update"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Monitoring update malformed command has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Monitoring update error mentions malformated command" true
        (String.includes ~affix:"Command is malformated" resp);
      Lwt.return_unit )

let check_monitoring_update_valid_command_connection_failure () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts =
        [
          ("molly_csrf", csrf_token);
          ("unikernel_name", "hello");
          ("command", "L*:debug");
        ]
      in
      let req =
        make_multipart_request ~parts ~session_cookie ~csrf_token
          "/api/unikernel/monitoring/update"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Monitoring update valid command connection failure has HTTP 400 Bad \
         Request"
        true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Monitoring update error mentions connection error" true
        (String.includes ~affix:"Failed to connect" resp);
      Lwt.return_unit )

let check_monitoring_update_with_token () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user_with_token store >>= fun (_user, token) ->
      let parts = [ ("unikernel_name", "hello"); ("command", "M*:enable") ] in
      let req =
        make_multipart_request ~parts ~token "/api/unikernel/monitoring/update"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Monitoring update with token connection failure has HTTP 400 Bad \
         Request"
        true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Monitoring update error mentions connection error" true
        (String.includes ~affix:"Failed to connect" resp);
      Lwt.return_unit )

let check_monitoring_update_invalid_csrf () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, _csrf_token) ->
      let parts =
        [
          ("molly_csrf", "wrong-csrf");
          ("unikernel_name", "hello");
          ("command", "L*:info");
        ]
      in
      let req =
        make_multipart_request ~parts ~session_cookie ~csrf_token:"wrong-csrf"
          "/api/unikernel/monitoring/update"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Monitoring update invalid CSRF has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_monitoring_update_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let parts =
        [
          ("molly_csrf", "csrf");
          ("unikernel_name", "hello");
          ("command", "L*:info");
        ]
      in
      let req =
        make_multipart_request ~parts "/api/unikernel/monitoring/update"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Monitoring update unauthenticated has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_monitoring_update_bad_method () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/api/unikernel/monitoring/update"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Monitoring update bad method has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_scaling_update_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (user, session_cookie, csrf_token) ->
      let policies = make_default_policies ~domain:user.name ~unikernels:5 () in
      let parts =
        [
          ("molly_csrf", csrf_token);
          ("should_scale", "true");
          ("max_instances", "3");
        ]
      in
      let req =
        make_multipart_request ~parts ~session_cookie ~csrf_token
          "/api/unikernel/scaling/update?instance=default&unikernel=hello"
      in
      query_endpoint (make_app_request_handler ~policies store) req
      >>= fun resp ->
      Alcotest.(check bool)
        "Scaling update success has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Scaling update message indicates success" true
        (String.includes ~affix:"Unikernel scaling policy updated successfully."
           resp);
      match Storage.find_by_uuid store.Storage.users user.uuid with
      | None -> Alcotest.fail "User not found in storage"
      | Some u ->
          Alcotest.(check int)
            "User scaling policies count is 1" 1
            (List.length u.scaling_policies);
          let p = List.hd u.scaling_policies in
          Alcotest.(check int) "Max instances is 3" 3 p.max_instances;
          Lwt.return_unit )

let check_scaling_update_with_token () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user_with_token store >>= fun (user, token) ->
      let policies = make_default_policies ~domain:user.name ~unikernels:5 () in
      let parts = [ ("should_scale", "true"); ("max_instances", "2") ] in
      let req =
        make_multipart_request ~parts ~token
          "/api/unikernel/scaling/update?instance=default&unikernel=hello"
      in
      query_endpoint (make_app_request_handler ~policies store) req
      >>= fun resp ->
      Alcotest.(check bool)
        "Scaling update with token has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      match Storage.find_by_uuid store.Storage.users user.uuid with
      | None -> Alcotest.fail "User not found in storage"
      | Some u ->
          Alcotest.(check int)
            "User scaling policies count is 1" 1
            (List.length u.scaling_policies);
          let p = List.hd u.scaling_policies in
          Alcotest.(check int) "Max instances is 2" 2 p.max_instances;
          Lwt.return_unit )

let check_scaling_update_exceeds_max_allowed () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (user, session_cookie, csrf_token) ->
      let policies = make_default_policies ~domain:user.name ~unikernels:5 () in
      let parts =
        [
          ("molly_csrf", csrf_token);
          ("should_scale", "true");
          ("max_instances", "10");
        ]
      in
      let req =
        make_multipart_request ~parts ~session_cookie ~csrf_token
          "/api/unikernel/scaling/update?instance=default&unikernel=hello"
      in
      query_endpoint (make_app_request_handler ~policies store) req
      >>= fun resp ->
      Alcotest.(check bool)
        "Scaling update exceeds limit has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Scaling update error message mentions maximum clones allowed" true
        (String.includes ~affix:"Error: you can spawn a maximum of 5 clones."
           resp);
      Lwt.return_unit )

let check_scaling_update_remove_when_max_instances_is_one () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (user, session_cookie, csrf_token) ->
      let initial_policy : User_model.unikernel_scaling_policy =
        {
          name = label_of_string_exn "hello";
          primary_albatross_instance = label_of_string_exn "default";
          max_instances = 3;
        }
      in
      let user = { user with scaling_policies = [ initial_policy ] } in
      Storage.update_user store user;
      let parts =
        [
          ("molly_csrf", csrf_token);
          ("should_scale", "true");
          ("max_instances", "1");
        ]
      in
      let req =
        make_multipart_request ~parts ~session_cookie ~csrf_token
          "/api/unikernel/scaling/update?instance=default&unikernel=hello"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Scaling update max_instances=1 has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      match Storage.find_by_uuid store.Storage.users user.uuid with
      | None -> Alcotest.fail "User not found in storage"
      | Some u ->
          Alcotest.(check int)
            "Scaling policies list is now empty" 0
            (List.length u.scaling_policies);
          Lwt.return_unit )

let check_scaling_update_remove_when_should_scale_unchecked () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (user, session_cookie, csrf_token) ->
      let initial_policy : User_model.unikernel_scaling_policy =
        {
          name = label_of_string_exn "hello";
          primary_albatross_instance = label_of_string_exn "default";
          max_instances = 3;
        }
      in
      let user = { user with scaling_policies = [ initial_policy ] } in
      Storage.update_user store user;
      let parts = [ ("molly_csrf", csrf_token) ] in
      let req =
        make_multipart_request ~parts ~session_cookie ~csrf_token
          "/api/unikernel/scaling/update?instance=default&unikernel=hello"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Scaling update should_scale unchecked has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      match Storage.find_by_uuid store.Storage.users user.uuid with
      | None -> Alcotest.fail "User not found in storage"
      | Some u ->
          Alcotest.(check int)
            "Scaling policies list is now empty" 0
            (List.length u.scaling_policies);
          Lwt.return_unit )

let check_scaling_update_noop_when_no_existing_policy () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts = [ ("molly_csrf", csrf_token) ] in
      let req =
        make_multipart_request ~parts ~session_cookie ~csrf_token
          "/api/unikernel/scaling/update?instance=default&unikernel=hello"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Scaling update noop has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Scaling update message indicates nothing to do" true
        (String.includes
           ~affix:
             "No scaling policy exist for this unikernel. Nothing to do here."
           resp);
      Lwt.return_unit )

let check_scaling_update_missing_max_instances () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts = [ ("molly_csrf", csrf_token); ("should_scale", "true") ] in
      let req =
        make_multipart_request ~parts ~session_cookie ~csrf_token
          "/api/unikernel/scaling/update?instance=default&unikernel=hello"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Scaling update missing max_instances has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Scaling update error message mentions missing max clones" true
        (String.includes ~affix:"Missing max number of clones to spawn." resp);
      Lwt.return_unit )

let check_scaling_update_invalid_max_instances () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts =
        [
          ("molly_csrf", csrf_token);
          ("should_scale", "true");
          ("max_instances", "0");
        ]
      in
      let req =
        make_multipart_request ~parts ~session_cookie ~csrf_token
          "/api/unikernel/scaling/update?instance=default&unikernel=hello"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Scaling update invalid max_instances has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Scaling update error message mentions max instances > 1" true
        (String.includes ~affix:"Max instances must be greater than 1." resp);
      Lwt.return_unit )

let check_scaling_update_missing_instance () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts =
        [
          ("molly_csrf", csrf_token);
          ("should_scale", "true");
          ("max_instances", "3");
        ]
      in
      let req =
        make_multipart_request ~parts ~session_cookie ~csrf_token
          "/api/unikernel/scaling/update?unikernel=hello"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Scaling update missing instance redirects to select instance" true
        (is_redirect resp || String.includes ~affix:"/select/instance" resp);
      Lwt.return_unit )

let check_scaling_update_unknown_instance () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts =
        [
          ("molly_csrf", csrf_token);
          ("should_scale", "true");
          ("max_instances", "3");
        ]
      in
      let req =
        make_multipart_request ~parts ~session_cookie ~csrf_token
          "/api/unikernel/scaling/update?instance=nonexistent&unikernel=hello"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Scaling update unknown instance has HTTP 404 Not Found" true
        (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp);
      Lwt.return_unit )

let check_scaling_update_missing_unikernel () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts =
        [
          ("molly_csrf", csrf_token);
          ("should_scale", "true");
          ("max_instances", "3");
        ]
      in
      let req =
        make_multipart_request ~parts ~session_cookie ~csrf_token
          "/api/unikernel/scaling/update?instance=default"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Scaling update missing unikernel has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_scaling_update_invalid_csrf () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, _csrf_token) ->
      let parts =
        [
          ("molly_csrf", "invalid-csrf");
          ("should_scale", "true");
          ("max_instances", "3");
        ]
      in
      let req =
        make_multipart_request ~parts ~session_cookie ~csrf_token:"invalid-csrf"
          "/api/unikernel/scaling/update?instance=default&unikernel=hello"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Scaling update invalid CSRF has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_scaling_update_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let parts =
        [
          ("molly_csrf", "csrf");
          ("should_scale", "true");
          ("max_instances", "3");
        ]
      in
      let req =
        make_multipart_request ~parts
          "/api/unikernel/scaling/update?instance=default&unikernel=hello"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Scaling update unauthenticated has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_scaling_update_bad_method () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request
          ~path:"/api/unikernel/scaling/update?instance=default&unikernel=hello"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Scaling update bad method has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let tests =
  [
    ( "Monitoring status (session) handles connection failure",
      `Quick,
      check_monitoring_status_session );
    ( "Monitoring status (token) handles connection failure",
      `Quick,
      check_monitoring_status_token );
    ( "Monitoring status missing unikernel",
      `Quick,
      check_monitoring_status_missing_unikernel );
    ( "Monitoring status unauthenticated",
      `Quick,
      check_monitoring_status_unauthenticated );
    ("Monitoring status bad method", `Quick, check_monitoring_status_bad_method);
    ( "Monitoring update missing fields",
      `Quick,
      check_monitoring_update_missing_fields );
    ( "Monitoring update malformed command",
      `Quick,
      check_monitoring_update_malformed_command );
    ( "Monitoring update valid command connection failure",
      `Quick,
      check_monitoring_update_valid_command_connection_failure );
    ("Monitoring update with token", `Quick, check_monitoring_update_with_token);
    ( "Monitoring update invalid CSRF",
      `Quick,
      check_monitoring_update_invalid_csrf );
    ( "Monitoring update unauthenticated",
      `Quick,
      check_monitoring_update_unauthenticated );
    ("Monitoring update bad method", `Quick, check_monitoring_update_bad_method);
    ( "Scaling update successful policy update",
      `Quick,
      check_scaling_update_success );
    ( "Scaling update successful with token",
      `Quick,
      check_scaling_update_with_token );
    ( "Scaling update exceeds max allowed instances",
      `Quick,
      check_scaling_update_exceeds_max_allowed );
    ( "Scaling update remove policy when max_instances=1",
      `Quick,
      check_scaling_update_remove_when_max_instances_is_one );
    ( "Scaling update remove policy when should_scale unchecked",
      `Quick,
      check_scaling_update_remove_when_should_scale_unchecked );
    ( "Scaling update noop when no policy and should_scale unchecked",
      `Quick,
      check_scaling_update_noop_when_no_existing_policy );
    ( "Scaling update missing max instances",
      `Quick,
      check_scaling_update_missing_max_instances );
    ( "Scaling update invalid max instances",
      `Quick,
      check_scaling_update_invalid_max_instances );
    ( "Scaling update missing instance redirects",
      `Quick,
      check_scaling_update_missing_instance );
    ( "Scaling update unknown instance",
      `Quick,
      check_scaling_update_unknown_instance );
    ( "Scaling update missing unikernel",
      `Quick,
      check_scaling_update_missing_unikernel );
    ("Scaling update invalid CSRF", `Quick, check_scaling_update_invalid_csrf);
    ( "Scaling update unauthenticated",
      `Quick,
      check_scaling_update_unauthenticated );
    ("Scaling update bad method", `Quick, check_scaling_update_bad_method);
  ]
