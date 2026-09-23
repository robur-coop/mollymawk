open Test_utils
open Mock_devices
open Lwt.Infix

let make_policy_body ?user_uuid ?(instance = "default") ?(unikernels = 5)
    ?(memory = 512) ?(block = 0) ?(cpuids = "0") ?(bridges = "service")
    ?csrf_token () =
  let csrf_fields =
    match csrf_token with
    | Some token -> [ ("molly_csrf", `String token) ]
    | None -> []
  in
  let uuid_field =
    match user_uuid with Some u -> [ ("user_uuid", `String u) ] | None -> []
  in
  let fields =
    uuid_field
    @ [
        ("albatross_instance", `String instance);
        ("unikernels", `Int unikernels);
        ("memory", `Int memory);
        ("block", `Int block);
        ("cpuids", `String cpuids);
        ("bridges", `String bridges);
      ]
    @ csrf_fields
  in
  Yojson.Basic.to_string (`Assoc fields)

let check_update_policy_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store
      >>= fun (_admin, session_cookie, csrf_token) ->
      setup_user ~name:"test" ~email:"test@robur.coop" store
      >>= fun (test, _, _) ->
      let policies = make_default_policies ~domain:test.name () in
      let handler = make_app_request_handler ~policies store in
      let body =
        make_policy_body ~user_uuid:test.uuid ~instance:"default" ~unikernels:5
          ~memory:512 ~block:0 ~cpuids:"0" ~bridges:"service" ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/u/policy/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint handler req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Response contains allowed_unikernels" true
        (String.includes ~affix:"\"allowed_unikernels\":5" resp);
      Alcotest.(check bool)
        "Response contains allowed_memory" true
        (String.includes ~affix:"\"allowed_memory\":512" resp);
      Alcotest.(check bool)
        "Response contains allowed_block_size" true
        (String.includes ~affix:"\"allowed_block_size\":0" resp);
      Lwt.return_unit )

let check_update_policy_with_block_size_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store
      >>= fun (_admin, session_cookie, csrf_token) ->
      setup_user ~name:"test" ~email:"test@robur.coop" store
      >>= fun (test, _, _) ->
      let policies =
        make_default_policies ~domain:test.name ~block:(Some 4096) ()
      in
      let handler = make_app_request_handler ~policies store in
      let body =
        make_policy_body ~user_uuid:test.uuid ~instance:"default" ~unikernels:4
          ~memory:1024 ~block:2048 ~cpuids:"0,1" ~bridges:"service" ~csrf_token
          ()
      in
      let req =
        make_post_request ~path:"/api/admin/u/policy/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint handler req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Response contains allowed_block_size: 2048" true
        (String.includes ~affix:"\"allowed_block_size\":2048" resp);
      Lwt.return_unit )

let check_update_policy_exceeds_root_unikernels () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store
      >>= fun (_admin, session_cookie, csrf_token) ->
      setup_user ~name:"ctest" ~email:"ctest@robur.coop" store
      >>= fun (ctest, _, _) ->
      let policies =
        make_default_policies ~domain:ctest.name ~unikernels:10 ()
      in
      let handler = make_app_request_handler ~policies store in
      let body =
        make_policy_body ~user_uuid:ctest.uuid ~instance:"default"
          ~unikernels:50 ~memory:512 ~block:0 ~cpuids:"0" ~bridges:"service"
          ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/u/policy/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint handler req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error mentions policy not smaller than root" true
        (String.includes ~affix:"Policy is not smaller than root policy:" resp);
      Lwt.return_unit )

let check_update_policy_exceeds_root_memory () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store
      >>= fun (_admin, session_cookie, csrf_token) ->
      setup_user ~name:"btest" ~email:"btest@robur.coop" store
      >>= fun (btest, _, _) ->
      let policies = make_default_policies ~domain:btest.name ~memory:1024 () in
      let handler = make_app_request_handler ~policies store in
      let body =
        make_policy_body ~user_uuid:btest.uuid ~instance:"default" ~unikernels:5
          ~memory:4096 ~block:0 ~cpuids:"0" ~bridges:"service" ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/u/policy/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint handler req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error mentions policy not smaller than root" true
        (String.includes ~affix:"Policy is not smaller than root policy:" resp);
      Lwt.return_unit )

let check_update_policy_unauthorized_bridge () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store
      >>= fun (_admin, session_cookie, csrf_token) ->
      setup_user ~name:"etest" ~email:"etest@robur.coop" store
      >>= fun (etest, _, _) ->
      let policies =
        make_default_policies ~domain:etest.name ~bridges:[ "service" ] ()
      in
      let handler = make_app_request_handler ~policies store in
      let body =
        make_policy_body ~user_uuid:etest.uuid ~instance:"default" ~unikernels:2
          ~memory:256 ~block:0 ~cpuids:"0" ~bridges:"unauthorized" ~csrf_token
          ()
      in
      let req =
        make_post_request ~path:"/api/admin/u/policy/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint handler req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error mentions policy not smaller than root" true
        (String.includes ~affix:"Policy is not smaller than root policy:" resp);
      Lwt.return_unit )

let check_update_policy_unauthorized_cpuid () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store
      >>= fun (_admin, session_cookie, csrf_token) ->
      setup_user ~name:"itest" ~email:"itest@robur.coop" store
      >>= fun (itest, _, _) ->
      let policies =
        make_default_policies ~domain:itest.name ~cpuids:[ 0; 1 ] ()
      in
      let handler = make_app_request_handler ~policies store in
      let body =
        make_policy_body ~user_uuid:itest.uuid ~instance:"default" ~unikernels:2
          ~memory:256 ~block:0 ~cpuids:"99" ~bridges:"service" ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/u/policy/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint handler req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error mentions policy not smaller than root" true
        (String.includes ~affix:"Policy is not smaller than root policy:" resp);
      Lwt.return_unit )

let check_update_policy_user_not_found () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store
      >>= fun (_admin, session_cookie, csrf_token) ->
      let non_existent_uuid = "00000000-0000-0000-0000-000000000000" in
      let body =
        make_policy_body ~user_uuid:non_existent_uuid ~instance:"default"
          ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/u/policy/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 404 Not Found" true
        (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp);
      Alcotest.(check bool)
        "Error mentions User not found" true
        (String.includes ~affix:"User not found" resp);
      Lwt.return_unit )

let check_update_policy_unknown_instance () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store
      >>= fun (_admin, session_cookie, csrf_token) ->
      setup_user ~name:"ktest" ~email:"ktest@robur.coop" store
      >>= fun (ktest, _, _) ->
      let body =
        make_policy_body ~user_uuid:ktest.uuid ~instance:"nonexistent"
          ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/u/policy/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 404 Not Found" true
        (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp);
      Alcotest.(check bool)
        "Error mentions instance not found" true
        (String.includes
           ~affix:"Couldn't find albatross instance with name: nonexistent" resp);
      Lwt.return_unit )

let check_update_policy_invalid_instance_name () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store
      >>= fun (_admin, session_cookie, csrf_token) ->
      setup_user ~name:"htest" ~email:"htest@robur.coop" store
      >>= fun (htest, _, _) ->
      let body =
        make_policy_body ~user_uuid:htest.uuid ~instance:"invalid label!"
          ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/u/policy/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error mentions couldn't convert name" true
        (String.includes ~affix:"Couldn't convert name to albatross instance:"
           resp);
      Lwt.return_unit )

let check_update_policy_missing_user_uuid () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store
      >>= fun (_admin, session_cookie, csrf_token) ->
      let body = make_policy_body ~instance:"default" ~csrf_token () in
      let req =
        make_post_request ~path:"/api/admin/u/policy/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error mentions unexpected fields" true
        (String.includes ~affix:"Update policy: Unexpected fields." resp);
      Lwt.return_unit )

let check_update_policy_missing_policy_fields () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store
      >>= fun (_admin, session_cookie, csrf_token) ->
      setup_user ~name:"ktest" ~email:"ktest@robur.coop" store
      >>= fun (ktest, _, _) ->
      let body =
        Fmt.str
          {|{ "user_uuid": "%s", "albatross_instance": "default", "molly_csrf": "%s" }|}
          ktest.uuid csrf_token
      in
      let req =
        make_post_request ~path:"/api/admin/u/policy/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error mentions policy unexpected types" true
        (String.includes ~affix:"policy: unexpected types" resp);
      Lwt.return_unit )

let check_update_policy_root_policy_null () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store
      >>= fun (_admin, session_cookie, csrf_token) ->
      setup_user ~name:"jtest" ~email:"jtest@robur.coop" store
      >>= fun (jtest, _, _) ->
      let handler = make_app_request_handler store in
      let body =
        make_policy_body ~user_uuid:jtest.uuid ~instance:"default" ~unikernels:2
          ~memory:256 ~block:0 ~cpuids:"0" ~bridges:"service" ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/u/policy/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint handler req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 500 Internal Server Error" true
        (String.starts_with ~prefix:"HTTP/1.1 500 Internal Server Error" resp);
      Alcotest.(check bool)
        "Error mentions Root policy is null" true
        (String.includes ~affix:"Root policy is null" resp);
      Lwt.return_unit )

let check_update_policy_albatross_failure () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store
      >>= fun (_admin, session_cookie, csrf_token) ->
      setup_user ~name:"qtest" ~email:"qtest@robur.coop" store
      >>= fun (qtest, _, _) ->
      let policies = make_default_policies ~domain:qtest.name () in
      let failing_lbl = label_of_string_exn "failing" in
      let unreachable_cfg =
        {
          mock_albatross_config with
          name = failing_lbl;
          server_ip = Ipaddr.of_string_exn "127.0.0.1";
          server_port = 1;
        }
      in
      let unreachable_instance : Albatross.t =
        {
          configuration = unreachable_cfg;
          policies;
          status = Albatross.Status.Online;
        }
      in
      let instances =
        App.Label_map.singleton failing_lbl unreachable_instance
      in
      let handler = make_app_request_handler ~policies ~instances store in
      let body =
        make_policy_body ~user_uuid:qtest.uuid ~instance:"failing" ~unikernels:2
          ~memory:256 ~block:0 ~cpuids:"0" ~bridges:"service" ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/u/policy/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint handler req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 500 Internal Server Error" true
        (String.starts_with ~prefix:"HTTP/1.1 500 Internal Server Error" resp);
      Alcotest.(check bool)
        "Error mentions error setting policy" true
        (String.includes ~affix:"error setting policy:" resp);
      Lwt.return_unit )

let check_update_policy_invalid_csrf () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store
      >>= fun (_admin, session_cookie, _csrf_token) ->
      let body =
        make_policy_body ~user_uuid:"dummy" ~instance:"default"
          ~csrf_token:"bad-csrf" ()
      in
      let req =
        make_post_request ~path:"/api/admin/u/policy/update" ~body
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

let check_update_policy_missing_csrf () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store
      >>= fun (_admin, session_cookie, _csrf_token) ->
      let body = make_policy_body ~user_uuid:"dummy" ~instance:"default" () in
      let req =
        make_post_request ~path:"/api/admin/u/policy/update" ~body
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

let check_update_policy_non_admin_forbidden () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_non_admin_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        make_policy_body ~user_uuid:"dummy" ~instance:"default" ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/u/policy/update" ~body
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

let check_update_policy_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let body = make_policy_body ~user_uuid:"dummy" ~instance:"default" () in
      let req = make_post_request ~path:"/api/admin/u/policy/update" ~body () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "No molly-session in cookie header message" true
        (String.includes ~affix:"No molly-session in cookie header" resp);
      Lwt.return_unit )

let check_update_policy_invalid_method () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user ~name:"admin" ~email:"admin@robur.coop" store
      >>= fun (_admin, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/api/admin/u/policy/update" ~session_cookie
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

let tests =
  [
    ("Policy update success", `Quick, check_update_policy_success);
    ( "Policy update with block size success",
      `Quick,
      check_update_policy_with_block_size_success );
    ( "Policy update exceeds root unikernels",
      `Quick,
      check_update_policy_exceeds_root_unikernels );
    ( "Policy update exceeds root memory",
      `Quick,
      check_update_policy_exceeds_root_memory );
    ( "Policy update unauthorized bridge",
      `Quick,
      check_update_policy_unauthorized_bridge );
    ( "Policy update unauthorized cpuid",
      `Quick,
      check_update_policy_unauthorized_cpuid );
    ("Policy update user not found", `Quick, check_update_policy_user_not_found);
    ( "Policy update unknown instance",
      `Quick,
      check_update_policy_unknown_instance );
    ( "Policy update invalid instance name",
      `Quick,
      check_update_policy_invalid_instance_name );
    ( "Policy update missing user uuid",
      `Quick,
      check_update_policy_missing_user_uuid );
    ( "Policy update missing policy fields",
      `Quick,
      check_update_policy_missing_policy_fields );
    ( "Policy update root policy null",
      `Quick,
      check_update_policy_root_policy_null );
    ( "Policy update albatross failure",
      `Quick,
      check_update_policy_albatross_failure );
    ("Policy update invalid CSRF", `Quick, check_update_policy_invalid_csrf);
    ("Policy update missing CSRF", `Quick, check_update_policy_missing_csrf);
    ( "Policy update non-admin forbidden",
      `Quick,
      check_update_policy_non_admin_forbidden );
    ( "Policy update unauthenticated",
      `Quick,
      check_update_policy_unauthenticated );
    ("Policy update invalid method", `Quick, check_update_policy_invalid_method);
  ]
