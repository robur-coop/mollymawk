open Test_utils
open Mock_devices
open Lwt.Infix

let valid_cert_pem =
  X509.Certificate.encode_pem mock_albatross_config.certificate

let valid_key_pem =
  X509.Private_key.encode_pem mock_albatross_config.private_key

let mismatched_cert_pem =
  X509.Certificate.encode_pem (certificate_exn second_private_key)

let make_albatross_body ?name ?cert ?key ?ip ?port ?csrf_token () =
  let success_cfg = Lwt_main.run Mock_albatross.success_config in
  let name = Option.value ~default:"secondary" name in
  let cert = Option.value ~default:valid_cert_pem cert in
  let key = Option.value ~default:valid_key_pem key in
  let ip = Option.value ~default:(Ipaddr.to_string success_cfg.server_ip) ip in
  let port = Option.value ~default:success_cfg.server_port port in
  let csrf_fields =
    match csrf_token with
    | Some token -> [ ("molly_csrf", `String token) ]
    | None -> []
  in
  let fields =
    [
      ("name", `String name);
      ("certificate", `String cert);
      ("private_key", `String key);
      ("server_ip", `String ip);
      ("server_port", `Int port);
    ]
    @ csrf_fields
  in
  Yojson.Basic.to_string (`Assoc fields)

let make_delete_body ?(name = "default") ?csrf_token () =
  let csrf_fields =
    match csrf_token with
    | Some token -> [ ("molly_csrf", `String token) ]
    | None -> []
  in
  let fields = ("name", `String name) :: csrf_fields in
  Yojson.Basic.to_string (`Assoc fields)

let check_retry_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/api/admin/albatross/retry?instance=default"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Re-initialization successful message" true
        (String.includes
           ~affix:"Re-initialization successful, instance is back online" resp);
      Lwt.return_unit )

let check_retry_failure () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/api/admin/albatross/retry?instance=failing"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 500 Internal Server Error" true
        (String.starts_with ~prefix:"HTTP/1.1 500 Internal Server Error" resp);
      Alcotest.(check bool)
        "Re-initialization failed message" true
        (String.includes ~affix:"Re-initialization failed" resp);
      Lwt.return_unit )

let check_retry_instance_not_found () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/api/admin/albatross/retry?instance=nonexistent"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 404 Not Found" true
        (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp);
      Alcotest.(check bool)
        "Not found message includes instance name" true
        (String.includes
           ~affix:"Couldn't find albatross instance with name: nonexistent" resp);
      Lwt.return_unit )

let check_retry_invalid_instance_name () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request
          ~path:"/api/admin/albatross/retry?instance=invalid%20name"
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Contains instance name error" true
        (String.includes ~affix:"Error with albatross instance name" resp);
      Lwt.return_unit )

let check_retry_missing_instance () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/api/admin/albatross/retry" ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to instance selector" true
        (is_redirect resp && String.includes ~affix:"/select/instance" resp);
      Lwt.return_unit )

let check_retry_non_admin_forbidden () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_non_admin_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/api/admin/albatross/retry?instance=default"
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

let check_retry_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req =
        make_get_request ~path:"/api/admin/albatross/retry?instance=default" ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "No molly-session in cookie header message" true
        (String.includes ~affix:"No molly-session in cookie header" resp);
      Lwt.return_unit )

let check_retry_invalid_method () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_post_request ~path:"/api/admin/albatross/retry?instance=default"
          ~body:"{}" ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Bad HTTP request method message" true
        (String.includes ~affix:"Bad HTTP request method" resp);
      Lwt.return_unit )

let check_create_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body = make_albatross_body ~csrf_token () in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/create" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Configuration updated successfully message" true
        (String.includes ~affix:"Configuration updated successfully" resp);
      let sec_name = label_of_string_exn "secondary" in
      Alcotest.(check bool)
        "Store now has secondary configuration" true
        (List.exists
           (fun (c : Configuration.t) ->
             Vmm_core.Name.Label.equal c.name sec_name)
           store.Storage.configurations);
      Lwt.return_unit )

let check_create_duplicate_rejected () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let success_cfg = Lwt_main.run Mock_albatross.success_config in
      store.Storage.configurations <- [ success_cfg ];
      let body =
        make_albatross_body
          ~name:(Configuration.name_to_str success_cfg.name)
          ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/create" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Already exists message" true
        (String.includes ~affix:"already exists" resp);
      Lwt.return_unit )

let check_create_mismatched_cert_and_key () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        make_albatross_body ~cert:mismatched_cert_pem ~key:valid_key_pem
          ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/create" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Mismatched cert and key error" true
        (String.includes ~affix:"certificate and private key do not match" resp);
      Lwt.return_unit )

let check_create_invalid_cert_pem () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body = make_albatross_body ~cert:"a wrong cert" ~csrf_token () in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/create" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_create_missing_fields () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        Yojson.Basic.to_string
          (`Assoc
             [
               ("name", `String "secondary"); ("molly_csrf", `String csrf_token);
             ])
      in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/create" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Unexpected types error message" true
        (String.includes ~affix:"unexpected types" resp);
      Lwt.return_unit )

let check_create_invalid_name () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body = make_albatross_body ~name:"invalid name" ~csrf_token () in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/create" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Invalid label error message" true
        (String.includes ~affix:"invalid label" resp);
      Lwt.return_unit )

let check_create_invalid_ip () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body = make_albatross_body ~ip:"999.999.999.999" ~csrf_token () in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/create" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Invalid IP message" true
        (String.includes ~affix:"not an IPv4 address" resp);
      Lwt.return_unit )

let check_create_connection_failure () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let failure_cfg = Lwt_main.run Mock_albatross.failure_config in
      let body =
        make_albatross_body ~name:"failing-node" ~port:failure_cfg.server_port
          ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/create" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 500 Internal Server Error" true
        (String.starts_with ~prefix:"HTTP/1.1 500 Internal Server Error" resp);
      Alcotest.(check bool)
        "Albatross failure message" true
        (String.includes ~affix:"albatross failure" resp);
      Lwt.return_unit )

let check_create_invalid_csrf () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, _csrf_token) ->
      let body = make_albatross_body ~csrf_token:"wrong_csrf" () in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/create" ~body
          ~session_cookie ~csrf_token:"wrong_csrf" ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_create_missing_csrf () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, _csrf_token) ->
      let body = make_albatross_body () in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/create" ~body
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

let check_create_non_admin_forbidden () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_non_admin_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body = make_albatross_body ~csrf_token () in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/create" ~body
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

let check_create_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let body = make_albatross_body () in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/create" ~body ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "No molly-session in cookie header message" true
        (String.includes ~affix:"No molly-session in cookie header" resp);
      Lwt.return_unit )

let check_create_invalid_method () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/api/admin/settings/albatross/create"
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

let check_update_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let success_cfg = Lwt_main.run Mock_albatross.success_config in
      store.Storage.configurations <- [ success_cfg ];
      let body =
        make_albatross_body
          ~name:(Configuration.name_to_str success_cfg.name)
          ~port:success_cfg.server_port ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Configuration updated successfully message" true
        (String.includes ~affix:"Configuration updated successfully" resp);
      Lwt.return_unit )

let check_update_nonexistent_rejected () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body = make_albatross_body ~name:"nonexistent" ~csrf_token () in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Not found message" true
        (String.includes ~affix:"not found" resp);
      Lwt.return_unit )

let check_update_mismatched_cert_and_key () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let success_cfg = Lwt_main.run Mock_albatross.success_config in
      store.Storage.configurations <- [ success_cfg ];
      let body =
        make_albatross_body
          ~name:(Configuration.name_to_str success_cfg.name)
          ~cert:mismatched_cert_pem ~key:valid_key_pem ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Mismatched cert and key error" true
        (String.includes ~affix:"certificate and private key do not match" resp);
      Lwt.return_unit )

let check_update_missing_fields () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        Yojson.Basic.to_string
          (`Assoc
             [ ("name", `String "default"); ("molly_csrf", `String csrf_token) ])
      in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Unexpected types error message" true
        (String.includes ~affix:"unexpected types" resp);
      Lwt.return_unit )

let check_update_connection_failure () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let success_cfg = Lwt_main.run Mock_albatross.success_config in
      let failure_cfg = Lwt_main.run Mock_albatross.failure_config in
      store.Storage.configurations <- [ success_cfg ];
      let body =
        make_albatross_body
          ~name:(Configuration.name_to_str success_cfg.name)
          ~port:failure_cfg.server_port ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/update" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 500 Internal Server Error" true
        (String.starts_with ~prefix:"HTTP/1.1 500 Internal Server Error" resp);
      Alcotest.(check bool)
        "Albatross failure message" true
        (String.includes ~affix:"albatross failure" resp);
      Lwt.return_unit )

let check_update_invalid_csrf () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, _csrf_token) ->
      let success_cfg = Lwt_main.run Mock_albatross.success_config in
      store.Storage.configurations <- [ success_cfg ];
      let body =
        make_albatross_body
          ~name:(Configuration.name_to_str success_cfg.name)
          ~csrf_token:"wrong_csrf" ()
      in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/update" ~body
          ~session_cookie ~csrf_token:"wrong_csrf" ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_update_non_admin_forbidden () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_non_admin_user store >>= fun (_user, session_cookie, csrf_token) ->
      let success_cfg = Lwt_main.run Mock_albatross.success_config in
      store.Storage.configurations <- [ success_cfg ];
      let body =
        make_albatross_body
          ~name:(Configuration.name_to_str success_cfg.name)
          ~csrf_token ()
      in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/update" ~body
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

let check_update_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let success_cfg = Lwt_main.run Mock_albatross.success_config in
      store.Storage.configurations <- [ success_cfg ];
      let body =
        make_albatross_body
          ~name:(Configuration.name_to_str success_cfg.name)
          ()
      in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/update" ~body ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "No molly-session in cookie header message" true
        (String.includes ~affix:"No molly-session in cookie header" resp);
      Lwt.return_unit )

let check_update_invalid_method () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/api/admin/settings/albatross/update"
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

let check_delete_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let success_cfg = Lwt_main.run Mock_albatross.success_config in
      store.Storage.configurations <- [ success_cfg ];
      let name_str = Configuration.name_to_str success_cfg.name in
      let body = make_delete_body ~name:name_str ~csrf_token () in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/delete" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Configuration delete successfully message" true
        (String.includes ~affix:"Configuration delete successfully" resp);
      Alcotest.(check bool)
        "Store configurations no longer contains deleted instance" false
        (List.exists
           (fun (c : Configuration.t) ->
             Vmm_core.Name.Label.equal c.name success_cfg.name)
           store.Storage.configurations);
      Lwt.return_unit )

let check_delete_missing_name_field () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        Yojson.Basic.to_string (`Assoc [ ("molly_csrf", `String csrf_token) ])
      in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/delete" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Unexpected fields error message" true
        (String.includes ~affix:"Delete albatross config: Unexpected fields"
           resp);
      Lwt.return_unit )

let check_delete_invalid_name_label () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body = make_delete_body ~name:"invalid name!" ~csrf_token () in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/delete" ~body
          ~session_cookie ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Invalid label error message" true
        (String.includes ~affix:"invalid label" resp);
      Lwt.return_unit )

let check_delete_invalid_csrf () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, _csrf_token) ->
      let body = make_delete_body ~name:"default" ~csrf_token:"wrong_csrf" () in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/delete" ~body
          ~session_cookie ~csrf_token:"wrong_csrf" ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Lwt.return_unit )

let check_delete_missing_csrf () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, _csrf_token) ->
      let body = make_delete_body ~name:"default" () in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/delete" ~body
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

let check_delete_non_admin_forbidden () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_non_admin_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body = make_delete_body ~name:"default" ~csrf_token () in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/delete" ~body
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

let check_delete_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let body = make_delete_body ~name:"default" () in
      let req =
        make_post_request ~path:"/api/admin/settings/albatross/delete" ~body ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "No molly-session in cookie header message" true
        (String.includes ~affix:"No molly-session in cookie header" resp);
      Lwt.return_unit )

let check_delete_invalid_method () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/api/admin/settings/albatross/delete"
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
    ("Albatross retry initialization success", `Quick, check_retry_success);
    ("Albatross retry initialization failure", `Quick, check_retry_failure);
    ( "Albatross retry initialization instance not found",
      `Quick,
      check_retry_instance_not_found );
    ( "Albatross retry initialization invalid instance name",
      `Quick,
      check_retry_invalid_instance_name );
    ( "Albatross retry initialization missing instance redirects",
      `Quick,
      check_retry_missing_instance );
    ( "Albatross retry initialization non-admin forbidden",
      `Quick,
      check_retry_non_admin_forbidden );
    ( "Albatross retry initialization unauthenticated",
      `Quick,
      check_retry_unauthenticated );
    ( "Albatross retry initialization invalid method",
      `Quick,
      check_retry_invalid_method );
    ("Albatross create configuration success", `Quick, check_create_success);
    ( "Albatross create configuration duplicate rejected",
      `Quick,
      check_create_duplicate_rejected );
    ( "Albatross create configuration mismatched cert and key",
      `Quick,
      check_create_mismatched_cert_and_key );
    ( "Albatross create configuration invalid cert PEM",
      `Quick,
      check_create_invalid_cert_pem );
    ( "Albatross create configuration missing fields",
      `Quick,
      check_create_missing_fields );
    ( "Albatross create configuration invalid name label",
      `Quick,
      check_create_invalid_name );
    ( "Albatross create configuration invalid IP address",
      `Quick,
      check_create_invalid_ip );
    ( "Albatross create configuration connection failure",
      `Quick,
      check_create_connection_failure );
    ( "Albatross create configuration invalid CSRF",
      `Quick,
      check_create_invalid_csrf );
    ( "Albatross create configuration missing CSRF",
      `Quick,
      check_create_missing_csrf );
    ( "Albatross create configuration non-admin forbidden",
      `Quick,
      check_create_non_admin_forbidden );
    ( "Albatross create configuration unauthenticated",
      `Quick,
      check_create_unauthenticated );
    ( "Albatross create configuration invalid method",
      `Quick,
      check_create_invalid_method );
    ("Albatross update configuration success", `Quick, check_update_success);
    ( "Albatross update configuration nonexistent rejected",
      `Quick,
      check_update_nonexistent_rejected );
    ( "Albatross update configuration mismatched cert and key",
      `Quick,
      check_update_mismatched_cert_and_key );
    ( "Albatross update configuration missing fields",
      `Quick,
      check_update_missing_fields );
    ( "Albatross update configuration connection failure",
      `Quick,
      check_update_connection_failure );
    ( "Albatross update configuration invalid CSRF",
      `Quick,
      check_update_invalid_csrf );
    ( "Albatross update configuration non-admin forbidden",
      `Quick,
      check_update_non_admin_forbidden );
    ( "Albatross update configuration unauthenticated",
      `Quick,
      check_update_unauthenticated );
    ( "Albatross update configuration invalid method",
      `Quick,
      check_update_invalid_method );
    ("Albatross delete configuration success", `Quick, check_delete_success);
    ( "Albatross delete configuration missing name field",
      `Quick,
      check_delete_missing_name_field );
    ( "Albatross delete configuration invalid name label",
      `Quick,
      check_delete_invalid_name_label );
    ( "Albatross delete configuration invalid CSRF",
      `Quick,
      check_delete_invalid_csrf );
    ( "Albatross delete configuration missing CSRF",
      `Quick,
      check_delete_missing_csrf );
    ( "Albatross delete configuration non-admin forbidden",
      `Quick,
      check_delete_non_admin_forbidden );
    ( "Albatross delete configuration unauthenticated",
      `Quick,
      check_delete_unauthenticated );
    ( "Albatross delete configuration invalid method",
      `Quick,
      check_delete_invalid_method );
  ]
