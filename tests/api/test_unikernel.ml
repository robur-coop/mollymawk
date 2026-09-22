open Test_utils
open Mock_devices
open Lwt.Infix

let hello_hvt = "mock binary data for unikernel create test"
let default_cfg = {|{"typ": "solo5", "cpuids": [0], "memory": 32}|}

let check_unikernel_create_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let _user, session_cookie, csrf_token = setup_admin_user store in
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

let tests =
  [
    ( "Successful unikernel create (manual deploy with .hvt binary)",
      `Quick,
      check_unikernel_create_success );
  ]
