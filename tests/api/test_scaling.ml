open Test_utils
open Mock_devices

let check_monitoring_status_session () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request ~path:"/api/unikernel/monitoring/status?unikernel=hello"
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Monitoring status response has HTTP 400 Bad Request on connection failure"
    true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Monitoring status error mentions connection failure" true
    (String.includes ~affix:"Failed to connect" resp)

let check_monitoring_status_missing_unikernel () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request ~path:"/api/unikernel/monitoring/status" ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Monitoring status missing unikernel has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_monitoring_status_unauthenticated () =
  let _store = init_mock_store () in
  let req =
    make_get_request ~path:"/api/unikernel/monitoring/status?unikernel=hello" ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Monitoring status unauthenticated has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_monitoring_status_bad_method () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_post_request ~path:"/api/unikernel/monitoring/status?unikernel=hello"
      ~body:"" ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Monitoring status bad method has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_monitoring_update_missing_fields () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body = Fmt.str {|{"molly_csrf": "%s"}|} csrf_token in
  let req =
    make_post_request ~path:"/api/unikernel/monitoring/update" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Monitoring update missing fields has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Monitoring update error mentions missing fields" true
    (String.includes ~affix:"Missing unikernel name or command in request." resp)

let check_monitoring_update_malformed_command () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Fmt.str
      {|{"molly_csrf": "%s", "unikernel_name": "hello", "command": "invalid command"}|}
      csrf_token
  in
  let req =
    make_post_request ~path:"/api/unikernel/monitoring/update" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Monitoring update malformed command has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Monitoring update error mentions malformated command" true
    (String.includes ~affix:"Command is malformated" resp)

let check_monitoring_update_valid_command_connection_failure () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Fmt.str
      {|{"molly_csrf": "%s", "unikernel_name": "hello", "command": "L*:debug"}|}
      csrf_token
  in
  let req =
    make_post_request ~path:"/api/unikernel/monitoring/update" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Monitoring update valid command connection failure has HTTP 400 Bad \
     Request"
    true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Monitoring update error mentions connection error" true
    (String.includes ~affix:"Failed to connect" resp)

let check_monitoring_update_invalid_csrf () =
  let store = init_mock_store () in
  let _user, session_cookie, _csrf_token = setup_user store in
  let body =
    {|{"molly_csrf": "wrong-csrf", "unikernel_name": "hello", "command": "L*:info"}|}
  in
  let req =
    make_post_request ~path:"/api/unikernel/monitoring/update" ~body
      ~session_cookie ~csrf_token:"wrong-csrf" ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Monitoring update invalid CSRF has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_monitoring_update_unauthenticated () =
  let _store = init_mock_store () in
  let body =
    {|{"molly_csrf": "csrf", "unikernel_name": "hello", "command": "L*:info"}|}
  in
  let req =
    make_post_request ~path:"/api/unikernel/monitoring/update" ~body ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Monitoring update unauthenticated has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_monitoring_update_bad_method () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request ~path:"/api/unikernel/monitoring/update" ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Monitoring update bad method has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_scaling_update_success () =
  let store = init_mock_store () in
  let user, session_cookie, csrf_token = setup_user store in
  let policies = make_default_policies ~domain:user.name ~unikernels:5 () in
  let body =
    Fmt.str {|{"molly_csrf": "%s", "should_scale": true, "max_instances": 3}|}
      csrf_token
  in
  let req =
    make_post_request
      ~path:"/api/unikernel/scaling/update?instance=default&unikernel=hello"
      ~body ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint ~policies req in
  Alcotest.(check bool)
    "Scaling update success has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Scaling update message indicates success" true
    (String.includes ~affix:"Unikernel scaling policy updated successfully."
       resp);
  match Storage.find_by_uuid store user.uuid with
  | None -> Alcotest.fail "User not found in storage"
  | Some u ->
      Alcotest.(check int)
        "User scaling policies count is 1" 1
        (Models.User.Scaling_policy_map.cardinal u.scaling_policies);
      let p = snd (Models.User.Scaling_policy_map.choose u.scaling_policies) in
      Alcotest.(check int) "Max instances is 3" 3 p.max_instances

let check_scaling_update_exceeds_max_allowed () =
  let store = init_mock_store () in
  let user, session_cookie, csrf_token = setup_user store in
  let policies = make_default_policies ~domain:user.name ~unikernels:5 () in
  let body =
    Fmt.str {|{"molly_csrf": "%s", "should_scale": true, "max_instances": 10}|}
      csrf_token
  in
  let req =
    make_post_request
      ~path:"/api/unikernel/scaling/update?instance=default&unikernel=hello"
      ~body ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint ~policies req in
  Alcotest.(check bool)
    "Scaling update exceeds limit has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Scaling update error message mentions maximum clones allowed" true
    (String.includes ~affix:"Error: you can spawn a maximum of 5 clones." resp)

let check_scaling_update_remove_when_max_instances_is_one () =
  let store = init_mock_store () in
  let user, session_cookie, csrf_token = setup_user store in
  let initial_policy : Models.User.unikernel_scaling_policy =
    {
      name = label_of_string_exn "hello";
      primary_albatross_instance = label_of_string_exn "default";
      max_instances = 3;
    }
  in
  let user =
    {
      user with
      scaling_policies =
        Models.User.Scaling_policy_map.singleton
          (initial_policy.name, initial_policy.primary_albatross_instance)
          initial_policy;
    }
  in
  Storage.update_user store user;
  let body =
    Fmt.str {|{"molly_csrf": "%s", "should_scale": true, "max_instances": 1}|}
      csrf_token
  in
  let req =
    make_post_request
      ~path:"/api/unikernel/scaling/update?instance=default&unikernel=hello"
      ~body ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Scaling update max_instances=1 has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  match Storage.find_by_uuid store user.uuid with
  | None -> Alcotest.fail "User not found in storage"
  | Some u ->
      Alcotest.(check int)
        "Scaling policies list is now empty" 0
        (Models.User.Scaling_policy_map.cardinal u.scaling_policies)

let check_scaling_update_remove_when_should_scale_unchecked () =
  let store = init_mock_store () in
  let user, session_cookie, csrf_token = setup_user store in
  let initial_policy : Models.User.unikernel_scaling_policy =
    {
      name = label_of_string_exn "hello";
      primary_albatross_instance = label_of_string_exn "default";
      max_instances = 3;
    }
  in
  let user =
    {
      user with
      scaling_policies =
        Models.User.Scaling_policy_map.singleton
          (initial_policy.name, initial_policy.primary_albatross_instance)
          initial_policy;
    }
  in
  Storage.update_user store user;
  let body = Fmt.str {|{"molly_csrf": "%s"}|} csrf_token in
  let req =
    make_post_request
      ~path:"/api/unikernel/scaling/update?instance=default&unikernel=hello"
      ~body ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Scaling update should_scale unchecked has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  match Storage.find_by_uuid store user.uuid with
  | None -> Alcotest.fail "User not found in storage"
  | Some u ->
      Alcotest.(check int)
        "Scaling policies list is now empty" 0
        (Models.User.Scaling_policy_map.cardinal u.scaling_policies)

let check_scaling_update_noop_when_no_existing_policy () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body = Fmt.str {|{"molly_csrf": "%s"}|} csrf_token in
  let req =
    make_post_request
      ~path:"/api/unikernel/scaling/update?instance=default&unikernel=hello"
      ~body ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Scaling update noop has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Scaling update message indicates nothing to do" true
    (String.includes
       ~affix:"No scaling policy exist for this unikernel. Nothing to do here."
       resp)

let check_scaling_update_missing_max_instances () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Fmt.str {|{"molly_csrf": "%s", "should_scale": true}|} csrf_token
  in
  let req =
    make_post_request
      ~path:"/api/unikernel/scaling/update?instance=default&unikernel=hello"
      ~body ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Scaling update missing max_instances has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Scaling update error message mentions missing max clones" true
    (String.includes ~affix:"Missing max number of clones to spawn." resp)

let check_scaling_update_invalid_max_instances () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Fmt.str {|{"molly_csrf": "%s", "should_scale": true, "max_instances": 0}|}
      csrf_token
  in
  let req =
    make_post_request
      ~path:"/api/unikernel/scaling/update?instance=default&unikernel=hello"
      ~body ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Scaling update invalid max_instances has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Scaling update error message mentions max instances > 1" true
    (String.includes ~affix:"Max instances must be greater than 1." resp)

let check_scaling_update_missing_instance () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Fmt.str {|{"molly_csrf": "%s", "should_scale": true, "max_instances": 3}|}
      csrf_token
  in
  let req =
    make_post_request ~path:"/api/unikernel/scaling/update?unikernel=hello"
      ~body ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Scaling update missing instance redirects to select instance" true
    (is_redirect resp || String.includes ~affix:"/select/instance" resp)

let check_scaling_update_unknown_instance () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Fmt.str {|{"molly_csrf": "%s", "should_scale": true, "max_instances": 3}|}
      csrf_token
  in
  let req =
    make_post_request
      ~path:"/api/unikernel/scaling/update?instance=nonexistent&unikernel=hello"
      ~body ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Scaling update unknown instance has HTTP 404 Not Found" true
    (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp)

let check_scaling_update_missing_unikernel () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Fmt.str {|{"molly_csrf": "%s", "should_scale": true, "max_instances": 3}|}
      csrf_token
  in
  let req =
    make_post_request ~path:"/api/unikernel/scaling/update?instance=default"
      ~body ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Scaling update missing unikernel has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_scaling_update_invalid_csrf () =
  let store = init_mock_store () in
  let _user, session_cookie, _csrf_token = setup_user store in
  let body =
    {|{"molly_csrf": "invalid-csrf", "should_scale": true, "max_instances": 3}|}
  in
  let req =
    make_post_request
      ~path:"/api/unikernel/scaling/update?instance=default&unikernel=hello"
      ~body ~session_cookie ~csrf_token:"invalid-csrf" ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Scaling update invalid CSRF has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_scaling_update_unauthenticated () =
  let _store = init_mock_store () in
  let body =
    {|{"molly_csrf": "csrf", "should_scale": true, "max_instances": 3}|}
  in
  let req =
    make_post_request
      ~path:"/api/unikernel/scaling/update?instance=default&unikernel=hello"
      ~body ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Scaling update unauthenticated has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_scaling_update_bad_method () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request
      ~path:"/api/unikernel/scaling/update?instance=default&unikernel=hello"
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Scaling update bad method has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let tests =
  [
    ( "Monitoring status (session) handles connection failure",
      `Quick,
      check_monitoring_status_session );
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
