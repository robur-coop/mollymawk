open Test_utils
open Mock_devices

let make_policy_body ?user_uuid ?(instance = "default") ?(unikernels = 2)
    ?(block = 0) ?(cpuids = "0") ?(bridges = "service") ?(memory = 32)
    ?csrf_token () =
  let csrf_fields =
    match csrf_token with
    | Some token -> [ ("molly_csrf", Jsont.Json.string token) ]
    | None -> []
  in
  let uuid_field =
    match user_uuid with
    | Some u -> [ ("user_uuid", Jsont.Json.string u) ]
    | None -> []
  in
  let fields =
    uuid_field
    @ [
        ("albatross_instance", Jsont.Json.string instance);
        ("unikernels", Jsont.Json.int unikernels);
        ("memory", Jsont.Json.int memory);
        ("block", Jsont.Json.int block);
        ("cpuids", Jsont.Json.string cpuids);
        ("bridges", Jsont.Json.string bridges);
      ]
    @ csrf_fields
  in
  Utils.Json.to_string (Albatross_json.j_obj fields)

let check_update_policy_success () =
  let store = init_mock_store () in
  let _admin, session_cookie, csrf_token =
    setup_user ~name:"admin" ~email:"admin@robur.coop" store
  in
  let test, _, _ = setup_user ~name:"test" ~email:"test@robur.coop" store in
  let policies = make_default_policies ~domain:test.name () in
  let body =
    make_policy_body ~user_uuid:(Uuidm.to_string test.uuid) ~csrf_token ()
  in
  let req =
    make_post_request ~path:"/api/admin/u/policy/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint ~policies req in
  Alcotest.(check bool)
    "Response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Response contains success message" true
    (String.includes ~affix:"Updated policy successfully" resp)

let check_update_policy_with_block_size_success () =
  let store = init_mock_store () in
  let _admin, session_cookie, csrf_token =
    setup_user ~name:"admin" ~email:"admin@robur.coop" store
  in
  let test, _, _ = setup_user ~name:"test" ~email:"test@robur.coop" store in
  let policies =
    make_default_policies ~domain:test.name ~block:(Some 4096) ()
  in
  let body =
    make_policy_body
      ~user_uuid:(Uuidm.to_string test.uuid)
      ~block:2048 ~csrf_token ()
  in
  let req =
    make_post_request ~path:"/api/admin/u/policy/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint ~policies req in
  Alcotest.(check bool)
    "Response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Response contains success message" true
    (String.includes ~affix:"Updated policy successfully" resp)

let check_update_policy_exceeds_root_unikernels () =
  let store = init_mock_store () in
  let _admin, session_cookie, csrf_token =
    setup_user ~name:"admin" ~email:"admin@robur.coop" store
  in
  let ctest, _, _ = setup_user ~name:"ctest" ~email:"ctest@robur.coop" store in
  let policies = make_default_policies ~domain:ctest.name ~unikernels:10 () in
  let body =
    make_policy_body
      ~user_uuid:(Uuidm.to_string ctest.uuid)
      ~unikernels:50 ~csrf_token ()
  in
  let req =
    make_post_request ~path:"/api/admin/u/policy/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint ~policies req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Error mentions policy not smaller than root" true
    (String.includes ~affix:"Policy is not smaller than root policy:" resp)

let check_update_policy_exceeds_root_memory () =
  let store = init_mock_store () in
  let _admin, session_cookie, csrf_token =
    setup_user ~name:"admin" ~email:"admin@robur.coop" store
  in
  let btest, _, _ = setup_user ~name:"btest" ~email:"btest@robur.coop" store in
  let policies = make_default_policies ~domain:btest.name ~memory:1024 () in
  let body =
    make_policy_body
      ~user_uuid:(Uuidm.to_string btest.uuid)
      ~memory:4096 ~csrf_token ()
  in
  let req =
    make_post_request ~path:"/api/admin/u/policy/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint ~policies req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Error mentions policy not smaller than root" true
    (String.includes ~affix:"Policy is not smaller than root policy:" resp)

let check_update_policy_unauthorized_bridge () =
  let store = init_mock_store () in
  let _admin, session_cookie, csrf_token =
    setup_user ~name:"admin" ~email:"admin@robur.coop" store
  in
  let etest, _, _ = setup_user ~name:"etest" ~email:"etest@robur.coop" store in
  let policies = make_default_policies ~domain:etest.name () in
  let body =
    make_policy_body
      ~user_uuid:(Uuidm.to_string etest.uuid)
      ~bridges:"unauthorized" ~csrf_token ()
  in
  let req =
    make_post_request ~path:"/api/admin/u/policy/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint ~policies req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Error mentions policy not smaller than root" true
    (String.includes ~affix:"Policy is not smaller than root policy:" resp)

let check_update_policy_unauthorized_cpuid () =
  let store = init_mock_store () in
  let _admin, session_cookie, csrf_token =
    setup_user ~name:"admin" ~email:"admin@robur.coop" store
  in
  let itest, _, _ = setup_user ~name:"itest" ~email:"itest@robur.coop" store in
  let policies = make_default_policies ~domain:itest.name ~cpuids:[ 0; 1 ] () in
  let body =
    make_policy_body
      ~user_uuid:(Uuidm.to_string itest.uuid)
      ~cpuids:"99" ~csrf_token ()
  in
  let req =
    make_post_request ~path:"/api/admin/u/policy/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint ~policies req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Error mentions policy not smaller than root" true
    (String.includes ~affix:"Policy is not smaller than root policy:" resp)

let check_update_policy_user_not_found () =
  let store = init_mock_store () in
  let _admin, session_cookie, csrf_token =
    setup_user ~name:"admin" ~email:"admin@robur.coop" store
  in
  let non_existent_uuid = "00000000-0000-0000-0000-000000000000" in
  let body = make_policy_body ~user_uuid:non_existent_uuid ~csrf_token () in
  let req =
    make_post_request ~path:"/api/admin/u/policy/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 404 Not Found" true
    (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp);
  Alcotest.(check bool)
    "Error mentions User not found" true
    (String.includes ~affix:"User not found" resp)

let check_update_policy_unknown_instance () =
  let store = init_mock_store () in
  let _admin, session_cookie, csrf_token =
    setup_user ~name:"admin" ~email:"admin@robur.coop" store
  in
  let ktest, _, _ = setup_user ~name:"ktest" ~email:"ktest@robur.coop" store in
  let body =
    make_policy_body
      ~user_uuid:(Uuidm.to_string ktest.uuid)
      ~instance:"nonexistent" ~csrf_token ()
  in
  let req =
    make_post_request ~path:"/api/admin/u/policy/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 404 Not Found" true
    (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp);
  Alcotest.(check bool)
    "Error mentions instance not found" true
    (String.includes
       ~affix:"Couldn't find albatross instance with name: nonexistent" resp)

let check_update_policy_invalid_instance_name () =
  let store = init_mock_store () in
  let _admin, session_cookie, csrf_token =
    setup_user ~name:"admin" ~email:"admin@robur.coop" store
  in
  let htest, _, _ = setup_user ~name:"htest" ~email:"htest@robur.coop" store in
  let body =
    make_policy_body
      ~user_uuid:(Uuidm.to_string htest.uuid)
      ~instance:"invalid label!" ~csrf_token ()
  in
  let req =
    make_post_request ~path:"/api/admin/u/policy/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Error mentions couldn't convert name" true
    (String.includes ~affix:"Couldn't convert name to albatross instance:" resp)

let check_update_policy_missing_user_uuid () =
  let store = init_mock_store () in
  let _admin, session_cookie, csrf_token =
    setup_user ~name:"admin" ~email:"admin@robur.coop" store
  in
  let body = make_policy_body ~csrf_token () in
  let req =
    make_post_request ~path:"/api/admin/u/policy/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Error mentions failed to parse JSON" true
    (String.includes ~affix:"Failed to parse JSON:" resp)

let check_update_policy_missing_policy_fields () =
  let store = init_mock_store () in
  let _admin, session_cookie, csrf_token =
    setup_user ~name:"admin" ~email:"admin@robur.coop" store
  in
  let ktest, _, _ = setup_user ~name:"ktest" ~email:"ktest@robur.coop" store in
  let body =
    Fmt.str
      {|{ "user_uuid": "%s", "albatross_instance": "default", "molly_csrf": "%s" }|}
      (Uuidm.to_string ktest.uuid)
      csrf_token
  in
  let req =
    make_post_request ~path:"/api/admin/u/policy/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Error mentions failed to parse JSON" true
    (String.includes ~affix:"Failed to parse JSON:" resp)

let check_update_policy_root_policy_null () =
  let store = init_mock_store () in
  let _admin, session_cookie, csrf_token =
    setup_user ~name:"admin" ~email:"admin@robur.coop" store
  in
  let jtest, _, _ = setup_user ~name:"jtest" ~email:"jtest@robur.coop" store in
  let body =
    make_policy_body ~user_uuid:(Uuidm.to_string jtest.uuid) ~csrf_token ()
  in
  let req =
    make_post_request ~path:"/api/admin/u/policy/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 500 Internal Server Error" true
    (String.starts_with ~prefix:"HTTP/1.1 500 Internal Server Error" resp);
  Alcotest.(check bool)
    "Error mentions Root policy is null" true
    (String.includes ~affix:"Root policy is null" resp)

let check_update_policy_albatross_failure () =
  let store = init_mock_store () in
  let _admin, session_cookie, csrf_token =
    setup_user ~name:"admin" ~email:"admin@robur.coop" store
  in
  let qtest, _, _ = setup_user ~name:"qtest" ~email:"qtest@robur.coop" store in
  let policies = make_default_policies ~domain:qtest.name () in
  let body =
    make_policy_body
      ~user_uuid:(Uuidm.to_string qtest.uuid)
      ~instance:"failing" ~csrf_token ()
  in
  let req =
    make_post_request ~path:"/api/admin/u/policy/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint ~policies req in
  Alcotest.(check bool)
    "Response has HTTP 500 Internal Server Error" true
    (String.starts_with ~prefix:"HTTP/1.1 500 Internal Server Error" resp);
  Alcotest.(check bool)
    "Error mentions error setting policy" true
    (String.includes ~affix:"error setting policy:" resp)

let check_update_policy_invalid_csrf () =
  let store = init_mock_store () in
  let _admin, session_cookie, _csrf_token =
    setup_user ~name:"admin" ~email:"admin@robur.coop" store
  in
  let body = make_policy_body ~user_uuid:"dummy" ~csrf_token:"bad-csrf" () in
  let req =
    make_post_request ~path:"/api/admin/u/policy/update" ~body ~session_cookie
      ~csrf_token:"bad-csrf" ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_update_policy_missing_csrf () =
  let store = init_mock_store () in
  let _admin, session_cookie, _csrf_token =
    setup_user ~name:"admin" ~email:"admin@robur.coop" store
  in
  let body = make_policy_body ~user_uuid:"dummy" () in
  let req =
    make_post_request ~path:"/api/admin/u/policy/update" ~body ~session_cookie
      ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Failed to parse JSON error message" true
    (String.includes ~affix:"Failed to parse JSON:" resp)

let check_update_policy_non_admin_forbidden () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_non_admin_user store in
  let body = make_policy_body ~user_uuid:"dummy" ~csrf_token () in
  let req =
    make_post_request ~path:"/api/admin/u/policy/update" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Only administrators permitted message" true
    (String.includes ~affix:"You don't have the necessary permissions" resp)

let check_update_policy_unauthenticated () =
  let _store = init_mock_store () in
  let body = make_policy_body ~user_uuid:"dummy" () in
  let req = make_post_request ~path:"/api/admin/u/policy/update" ~body () in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "No molly-session in cookie header message" true
    (String.includes ~affix:"No molly-session in cookie header" resp)

let check_update_policy_invalid_method () =
  let store = init_mock_store () in
  let _admin, session_cookie, csrf_token =
    setup_user ~name:"admin" ~email:"admin@robur.coop" store
  in
  let req =
    make_get_request ~path:"/api/admin/u/policy/update" ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

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
