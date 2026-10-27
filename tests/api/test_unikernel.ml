open Test_utils
open Mock_devices

let hello_hvt = "mock binary data for unikernel create test"
let default_cfg = {|{"typ": "solo5", "cpuids": [0], "memory": 32}|}

let check_unikernels_info_success () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request ~path:"/api/unikernels" ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Response contains unikernel info from success instance" true
    (String.includes ~affix:"Unikernel Information" resp);
  Alcotest.(check bool)
    "Response contains error from failing instance" true
    (String.includes ~affix:"albatross failure" resp)

let check_unikernels_info_unauthenticated () =
  let _store = init_mock_store () in
  let req = make_get_request ~path:"/api/unikernels" () in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Response indicates user not found" true
    (String.includes ~affix:"User not found" resp)

let check_unikernel_create_success () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
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
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Response contains success message" true
    (String.includes ~affix:"unikernel created" resp)

let check_unikernel_force_create_success () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
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
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Response contains success message" true
    (String.includes ~affix:"unikernel force created" resp)

let check_unikernel_create_missing_fields () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
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
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Response indicates missing fields" true
    (String.includes ~affix:"One or more required fields are missing" resp)

let check_unikernel_create_unknown_instance () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
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
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 404 Not Found" true
    (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp);
  Alcotest.(check bool)
    "Response indicates error finding instance" true
    (String.includes ~affix:"Error finding albatross instance" resp)

let check_unikernel_create_invalid_config () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
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
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Response indicates invalid unikernel arguments" true
    (String.includes ~affix:"Invalid unikernel arguments" resp)

let check_unikernel_create_invalid_csrf () =
  let store = init_mock_store () in
  let _user, session_cookie, _csrf_token = setup_user store in
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
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Response indicates invalid CSRF" true
    (String.includes ~affix:"CSRF token" resp)

let check_unikernel_create_unauthenticated () =
  let _store = init_mock_store () in
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
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Response indicates user not found" true
    (String.includes ~affix:"User not found" resp)

let check_unikernel_create_albatross_failure () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
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
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 500 Internal Server Error" true
    (String.starts_with ~prefix:"HTTP/1.1 500 Internal Server Error" resp);
  Alcotest.(check bool)
    "Response contains failure message" true
    (String.includes ~affix:"albatross failure" resp)

let check_unikernel_destroy_success () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Fmt.str
      {|{"name": "hello", "albatross_instance": "default", "molly_csrf": "%s"}|}
      csrf_token
  in
  let req =
    make_post_request ~path:"/api/unikernel/destroy" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Destroy response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp)

let check_unikernel_destroy_failure () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Fmt.str
      {|{"name": "hello", "albatross_instance": "failing", "molly_csrf": "%s"}|}
      csrf_token
  in
  let req =
    make_post_request ~path:"/api/unikernel/destroy" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Destroy failure response has HTTP 500 Internal Server Error" true
    (String.starts_with ~prefix:"HTTP/1.1 500 Internal Server Error" resp);
  Alcotest.(check bool)
    "Destroy failure message indicates albatross failure" true
    (String.includes ~affix:"albatross failure" resp)

let check_unikernel_restart_success () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body = Fmt.str {|{"molly_csrf": "%s"}|} csrf_token in
  let req =
    make_post_request
      ~path:"/api/unikernel/restart?instance=default&unikernel=hello" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Restart response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp)

let check_unikernel_restart_failure () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body = Fmt.str {|{"molly_csrf": "%s"}|} csrf_token in
  let req =
    make_post_request
      ~path:"/api/unikernel/restart?instance=failing&unikernel=hello" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Restart failure response has HTTP 500 Internal Server Error" true
    (String.starts_with ~prefix:"HTTP/1.1 500 Internal Server Error" resp);
  Alcotest.(check bool)
    "Restart failure message indicates albatross failure" true
    (String.includes ~affix:"albatross failure" resp)

let check_unikernel_console_success () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request
      ~path:"/api/unikernel/console?instance=default&unikernel=hello"
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Console response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Console response has text/event-stream" true
    (String.includes ~affix:"text/event-stream" resp)

let check_unikernel_console_unauthenticated () =
  let _store = init_mock_store () in
  let req =
    make_get_request
      ~path:"/api/unikernel/console?instance=default&unikernel=hello" ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Console unauthenticated has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_unikernel_console_missing_instance () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request ~path:"/api/unikernel/console?unikernel=hello"
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Console missing instance redirects to select instance" true
    (is_redirect resp || String.includes ~affix:"/select/instance" resp)

let check_unikernel_console_unknown_instance () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request
      ~path:"/api/unikernel/console?instance=nonexistent&unikernel=hello"
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Console unknown instance has HTTP 404 Not Found" true
    (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp)

let check_unikernel_console_missing_unikernel () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request ~path:"/api/unikernel/console?instance=default"
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Console missing unikernel has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_unikernel_console_bad_method () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_post_request
      ~path:"/api/unikernel/console?instance=default&unikernel=hello" ~body:""
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Console bad method has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_unikernel_update_missing_fields () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body = Fmt.str {|{"molly_csrf": "%s"}|} csrf_token in
  let req =
    make_post_request ~path:"/api/unikernel/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Update missing fields has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Update missing fields message" true
    (String.includes ~affix:"Failed to parse JSON:" resp)

let check_unikernel_update_unknown_instance () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Fmt.str
      {|{
        "molly_csrf": "%s",
        "albatross_instance": "nonexistent",
        "job": "hello-job",
        "to_be_updated_unikernel": "uuid-new",
        "currently_running_unikernel": "uuid-old",
        "unikernel_name": "hello"
      }|}
      csrf_token
  in
  let req =
    make_post_request ~path:"/api/unikernel/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Update unknown instance has HTTP 404 Not Found" true
    (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp)

let check_unikernel_update_invalid_instance_name () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Fmt.str
      {|{
        "molly_csrf": "%s",
        "albatross_instance": "",
        "job": "hello-job",
        "to_be_updated_unikernel": "uuid-new",
        "currently_running_unikernel": "uuid-old",
        "unikernel_name": "hello"
      }|}
      csrf_token
  in
  let req =
    make_post_request ~path:"/api/unikernel/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Update invalid instance name has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_unikernel_update_invalid_unikernel_name () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Fmt.str
      {|{
        "molly_csrf": "%s",
        "albatross_instance": "default",
        "job": "hello-job",
        "to_be_updated_unikernel": "uuid-new",
        "currently_running_unikernel": "uuid-old",
        "unikernel_name": ""
      }|}
      csrf_token
  in
  let req =
    make_post_request ~path:"/api/unikernel/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Update invalid unikernel name has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_unikernel_update_invalid_arguments_json () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Fmt.str
      {|{
        "molly_csrf": "%s",
        "albatross_instance": "default",
        "job": "hello-job",
        "to_be_updated_unikernel": "uuid-new",
        "currently_running_unikernel": "uuid-old",
        "unikernel_name": "hello",
        "unikernel_arguments": "not-a-valid-config-object"
      }|}
      csrf_token
  in
  let req =
    make_post_request ~path:"/api/unikernel/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Update invalid arguments JSON has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Error message indicates JSON issue" true
    (String.includes ~affix:"Error with Unikernel Arguments Json" resp)

let check_unikernel_update_invalid_csrf () =
  let store = init_mock_store () in
  let _user, session_cookie, _csrf_token = setup_user store in
  let body =
    {|{
        "molly_csrf": "invalid-csrf-token",
        "albatross_instance": "default",
        "job": "hello-job",
        "to_be_updated_unikernel": "uuid-new",
        "currently_running_unikernel": "uuid-old",
        "unikernel_name": "hello"
      }|}
  in
  let req =
    make_post_request ~path:"/api/unikernel/update" ~body ~session_cookie
      ~csrf_token:"invalid-csrf-token" ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Update invalid CSRF has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_unikernel_update_unauthenticated () =
  let _store = init_mock_store () in
  let body =
    {|{
        "molly_csrf": "csrf",
        "albatross_instance": "default",
        "job": "hello-job",
        "to_be_updated_unikernel": "uuid-new",
        "currently_running_unikernel": "uuid-old",
        "unikernel_name": "hello"
      }|}
  in
  let req = make_post_request ~path:"/api/unikernel/update" ~body () in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Update unauthenticated has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_unikernel_update_bad_method () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request ~path:"/api/unikernel/update" ~session_cookie ~csrf_token
      ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Update bad method has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_unikernel_rollback_missing_fields () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body = Fmt.str {|{"molly_csrf": "%s"}|} csrf_token in
  let req =
    make_post_request ~path:"/api/unikernel/rollback" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Rollback missing fields has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Rollback message indicates failed to parse JSON" true
    (String.includes ~affix:"Failed to parse JSON:" resp)

let check_unikernel_rollback_unknown_instance () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Fmt.str
      {|{"molly_csrf": "%s", "albatross_instance": "nonexistent", "unikernel_name": "hello"}|}
      csrf_token
  in
  let req =
    make_post_request ~path:"/api/unikernel/rollback" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Rollback unknown instance has HTTP 404 Not Found" true
    (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp)

let check_unikernel_rollback_no_update_record () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Fmt.str
      {|{"molly_csrf": "%s", "albatross_instance": "default", "unikernel_name": "hello"}|}
      csrf_token
  in
  let req =
    make_post_request ~path:"/api/unikernel/rollback" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Rollback without update record has HTTP 404 Not Found" true
    (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp);
  Alcotest.(check bool)
    "Rollback message indicates build info not found" true
    (String.includes ~affix:"Could not find the build information" resp)

let check_unikernel_rollback_expired_window () =
  let store = init_mock_store () in
  let user, session_cookie, csrf_token = setup_user store in
  let dummy_cfg : Vmm_core.Unikernel.config =
    {
      typ = `Solo5;
      compressed = false;
      image = "";
      fail_behaviour = `Quit;
      add_name = true;
      startup = None;
      cpuids = Vmm_core.IS.singleton 0;
      memory = 32;
      block_devices = [];
      bridges = [];
      argv = None;
      numcpus = 1;
      linux_boot_partition = None;
    }
  in
  let expired_update : Models.User.unikernel_update =
    {
      name = label_of_string_exn "hello";
      job = "hello-job";
      uuid = Models.User.generate_uuid ();
      config = dummy_cfg;
      timestamp =
        Option.get (Ptime.sub_span (Utils.now ()) (Ptime.Span.of_int_s 700));
    }
  in
  let user =
    {
      user with
      unikernel_updates = Utils.LM.singleton expired_update.name expired_update;
    }
  in
  Storage.update_user store user;
  let body =
    Fmt.str
      {|{"molly_csrf": "%s", "albatross_instance": "default", "unikernel_name": "hello"}|}
      csrf_token
  in
  let req =
    make_post_request ~path:"/api/unikernel/rollback" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Rollback expired window has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Rollback message indicates 10 minutes limit" true
    (String.includes ~affix:"after 10 minutes of an update" resp)

let check_unikernel_rollback_invalid_csrf () =
  let store = init_mock_store () in
  let _user, session_cookie, _csrf_token = setup_user store in
  let body =
    {|{"molly_csrf": "invalid-csrf", "albatross_instance": "default", "unikernel_name": "hello"}|}
  in
  let req =
    make_post_request ~path:"/api/unikernel/rollback" ~body ~session_cookie
      ~csrf_token:"invalid-csrf" ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Rollback invalid CSRF has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_unikernel_rollback_unauthenticated () =
  let _store = init_mock_store () in
  let body =
    {|{"molly_csrf": "csrf", "albatross_instance": "default", "unikernel_name": "hello"}|}
  in
  let req = make_post_request ~path:"/api/unikernel/rollback" ~body () in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Rollback unauthenticated has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_unikernel_rollback_bad_method () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request ~path:"/api/unikernel/rollback" ~session_cookie ~csrf_token
      ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Rollback bad method has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let tests =
  [
    ("Unikernels list / info (session)", `Quick, check_unikernels_info_success);
    ( "Unikernels list / info (unauthenticated)",
      `Quick,
      check_unikernels_info_unauthenticated );
    ( "Successful unikernel create (manual deploy with .hvt binary)",
      `Quick,
      check_unikernel_create_success );
    ( "Successful unikernel force create",
      `Quick,
      check_unikernel_force_create_success );
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
    ( "Successful unikernel console (session)",
      `Quick,
      check_unikernel_console_success );
    ( "Reject console when unauthenticated",
      `Quick,
      check_unikernel_console_unauthenticated );
    ( "Console missing instance redirects",
      `Quick,
      check_unikernel_console_missing_instance );
    ( "Reject console when instance unknown",
      `Quick,
      check_unikernel_console_unknown_instance );
    ( "Reject console when unikernel missing",
      `Quick,
      check_unikernel_console_missing_unikernel );
    ( "Reject console with bad method",
      `Quick,
      check_unikernel_console_bad_method );
    ( "Reject update when required fields are missing",
      `Quick,
      check_unikernel_update_missing_fields );
    ( "Reject update when Albatross instance is unknown",
      `Quick,
      check_unikernel_update_unknown_instance );
    ( "Reject update when instance name is invalid",
      `Quick,
      check_unikernel_update_invalid_instance_name );
    ( "Reject update when unikernel name is invalid",
      `Quick,
      check_unikernel_update_invalid_unikernel_name );
    ( "Reject update when arguments JSON is invalid",
      `Quick,
      check_unikernel_update_invalid_arguments_json );
    ( "Reject update when CSRF token is invalid",
      `Quick,
      check_unikernel_update_invalid_csrf );
    ( "Reject update when unauthenticated",
      `Quick,
      check_unikernel_update_unauthenticated );
    ("Reject update with bad method", `Quick, check_unikernel_update_bad_method);
    ( "Reject rollback when required fields are missing",
      `Quick,
      check_unikernel_rollback_missing_fields );
    ( "Reject rollback when Albatross instance is unknown",
      `Quick,
      check_unikernel_rollback_unknown_instance );
    ( "Reject rollback when no update record exists",
      `Quick,
      check_unikernel_rollback_no_update_record );
    ( "Reject rollback when update window has expired",
      `Quick,
      check_unikernel_rollback_expired_window );
    ( "Reject rollback when CSRF token is invalid",
      `Quick,
      check_unikernel_rollback_invalid_csrf );
    ( "Reject rollback when unauthenticated",
      `Quick,
      check_unikernel_rollback_unauthenticated );
    ( "Reject rollback with bad method",
      `Quick,
      check_unikernel_rollback_bad_method );
  ]
