open Test_utils
open Mock_devices

let valid_cert_pem =
  X509.Certificate.encode_pem mock_albatross_config.certificate

let valid_key_pem =
  X509.Private_key.encode_pem mock_albatross_config.private_key

let mismatched_cert_pem =
  X509.Certificate.encode_pem (certificate_exn second_private_key)

let make_albatross_body ?name ?cert ?key ?ip ?port ?csrf_token () =
  let success_cfg = Mock_albatross.success_config in
  let name = Option.value ~default:"secondary" name in
  let cert = Option.value ~default:valid_cert_pem cert in
  let key = Option.value ~default:valid_key_pem key in
  let ip = Option.value ~default:(Ipaddr.to_string success_cfg.server_ip) ip in
  let port = Option.value ~default:success_cfg.server_port port in
  let csrf_fields =
    match csrf_token with
    | Some token -> [ ("molly_csrf", Jsont.Json.string token) ]
    | None -> []
  in
  let fields =
    [
      ("name", Jsont.Json.string name);
      ("certificate", Jsont.Json.string cert);
      ("private_key", Jsont.Json.string key);
      ("server_ip", Jsont.Json.string ip);
      ("server_port", Jsont.Json.int port);
    ]
    @ csrf_fields
  in
  Utils.Json.to_string (Albatross_json.j_obj fields)

let make_delete_body ?(name = "default") ?csrf_token () =
  let csrf_fields =
    match csrf_token with
    | Some token -> [ ("molly_csrf", Jsont.Json.string token) ]
    | None -> []
  in
  let fields = ("name", Jsont.Json.string name) :: csrf_fields in
  Utils.Json.to_string (Albatross_json.j_obj fields)

let check_retry_success () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request ~path:"/api/admin/albatross/retry?instance=default"
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Re-initialization successful message" true
    (String.includes
       ~affix:"Re-initialization successful, instance is back online" resp)

let check_retry_failure () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request ~path:"/api/admin/albatross/retry?instance=failing"
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 500 Internal Server Error" true
    (String.starts_with ~prefix:"HTTP/1.1 500 Internal Server Error" resp);
  Alcotest.(check bool)
    "Re-initialization failed message" true
    (String.includes ~affix:"Re-initialization failed" resp)

let check_retry_instance_not_found () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request ~path:"/api/admin/albatross/retry?instance=nonexistent"
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 404 Not Found" true
    (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp);
  Alcotest.(check bool)
    "Not found message includes instance name" true
    (String.includes
       ~affix:"Couldn't find albatross instance with name: nonexistent" resp)

let check_retry_invalid_instance_name () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request ~path:"/api/admin/albatross/retry?instance=invalid%20name"
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Contains instance name error" true
    (String.includes ~affix:"Error with albatross instance name" resp)

let check_retry_missing_instance () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request ~path:"/api/admin/albatross/retry" ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Redirects to instance selector" true
    (is_redirect resp && String.includes ~affix:"/select/instance" resp)

let check_retry_non_admin_forbidden () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_non_admin_user store in
  let req =
    make_get_request ~path:"/api/admin/albatross/retry?instance=default"
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Only administrators permitted message" true
    (String.includes ~affix:"You don't have the necessary permissions" resp)

let check_retry_unauthenticated () =
  let _store = init_mock_store () in
  let req =
    make_get_request ~path:"/api/admin/albatross/retry?instance=default" ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "No molly-session in cookie header message" true
    (String.includes ~affix:"No molly-session in cookie header" resp)

let check_retry_invalid_method () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_post_request ~path:"/api/admin/albatross/retry?instance=default"
      ~body:"{}" ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_create_success () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body = make_albatross_body ~csrf_token () in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/create" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Configuration updated successfully message" true
    (String.includes ~affix:"Configuration updated successfully" resp);
  let sec_name = label_of_string_exn "secondary" in
  Alcotest.(check bool)
    "Store now has secondary configuration" true
    (Utils.LM.mem sec_name store.Storage.configurations)

let check_create_duplicate_rejected () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let success_cfg = Mock_albatross.success_config in
  store.Storage.configurations <-
    Utils.LM.singleton success_cfg.name success_cfg;
  let body =
    make_albatross_body
      ~name:(Models.Configuration.name_to_str success_cfg.name)
      ~csrf_token ()
  in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/create" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Already exists message" true
    (String.includes ~affix:"already exists" resp)

let check_create_mismatched_cert_and_key () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    make_albatross_body ~cert:mismatched_cert_pem ~key:valid_key_pem ~csrf_token
      ()
  in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/create" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Mismatched cert and key error" true
    (String.includes ~affix:"certificate and private key do not match" resp)

let check_create_invalid_cert_pem () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body = make_albatross_body ~cert:"a wrong cert" ~csrf_token () in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/create" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_create_missing_fields () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Utils.Json.to_string
      (Albatross_json.j_obj
         [
           ("name", Jsont.Json.string "secondary");
           ("molly_csrf", Jsont.Json.string csrf_token);
         ])
  in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/create" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Failed to parse JSON error message" true
    (String.includes ~affix:"Failed to parse JSON:" resp)

let check_create_invalid_name () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body = make_albatross_body ~name:"invalid name" ~csrf_token () in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/create" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Invalid label error message" true
    (String.includes ~affix:"invalid label" resp)

let check_create_invalid_ip () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body = make_albatross_body ~ip:"999.999.999.999" ~csrf_token () in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/create" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Invalid IP message" true
    (String.includes ~affix:"not an IPv4 address" resp)

let check_create_connection_failure () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let failure_cfg = Mock_albatross.failure_config in
  let body =
    make_albatross_body ~name:"failing-node" ~port:failure_cfg.server_port
      ~csrf_token ()
  in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/create" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 500 Internal Server Error" true
    (String.starts_with ~prefix:"HTTP/1.1 500 Internal Server Error" resp);
  Alcotest.(check bool)
    "Albatross failure message" true
    (String.includes ~affix:"albatross failure" resp)

let check_create_invalid_csrf () =
  let store = init_mock_store () in
  let _user, session_cookie, _csrf_token = setup_user store in
  let body = make_albatross_body ~csrf_token:"wrong_csrf" () in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/create" ~body
      ~session_cookie ~csrf_token:"wrong_csrf" ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_create_missing_csrf () =
  let store = init_mock_store () in
  let _user, session_cookie, _csrf_token = setup_user store in
  let body = make_albatross_body () in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/create" ~body
      ~session_cookie ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Failed to parse JSON error message" true
    (String.includes ~affix:"Failed to parse JSON:" resp)

let check_create_non_admin_forbidden () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_non_admin_user store in
  let body = make_albatross_body ~csrf_token () in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/create" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Only administrators permitted message" true
    (String.includes ~affix:"You don't have the necessary permissions" resp)

let check_create_unauthenticated () =
  let _store = init_mock_store () in
  let body = make_albatross_body () in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/create" ~body ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "No molly-session in cookie header message" true
    (String.includes ~affix:"No molly-session in cookie header" resp)

let check_create_invalid_method () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request ~path:"/api/admin/settings/albatross/create"
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_update_success () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let success_cfg = Mock_albatross.success_config in
  store.Storage.configurations <-
    Utils.LM.singleton success_cfg.name success_cfg;
  let body =
    make_albatross_body
      ~name:(Models.Configuration.name_to_str success_cfg.name)
      ~port:success_cfg.server_port ~csrf_token ()
  in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/update" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Configuration updated successfully message" true
    (String.includes ~affix:"Configuration updated successfully" resp)

let check_update_nonexistent_rejected () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body = make_albatross_body ~name:"nonexistent" ~csrf_token () in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/update" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Not found message" true
    (String.includes ~affix:"not found" resp)

let check_update_mismatched_cert_and_key () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let success_cfg = Mock_albatross.success_config in
  store.Storage.configurations <-
    Utils.LM.singleton success_cfg.name success_cfg;
  let body =
    make_albatross_body
      ~name:(Models.Configuration.name_to_str success_cfg.name)
      ~cert:mismatched_cert_pem ~key:valid_key_pem ~csrf_token ()
  in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/update" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Mismatched cert and key error" true
    (String.includes ~affix:"certificate and private key do not match" resp)

let check_update_missing_fields () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Utils.Json.to_string
      (Albatross_json.j_obj
         [
           ("name", Jsont.Json.string "default");
           ("molly_csrf", Jsont.Json.string csrf_token);
         ])
  in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/update" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Failed to parse JSON error message" true
    (String.includes ~affix:"Failed to parse JSON:" resp)

let check_update_connection_failure () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let success_cfg = Mock_albatross.success_config in
  let failure_cfg = Mock_albatross.failure_config in
  store.Storage.configurations <-
    Utils.LM.singleton success_cfg.name success_cfg;
  let body =
    make_albatross_body
      ~name:(Models.Configuration.name_to_str success_cfg.name)
      ~port:failure_cfg.server_port ~csrf_token ()
  in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/update" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 500 Internal Server Error" true
    (String.starts_with ~prefix:"HTTP/1.1 500 Internal Server Error" resp);
  Alcotest.(check bool)
    "Albatross failure message" true
    (String.includes ~affix:"albatross failure" resp)

let check_update_invalid_csrf () =
  let store = init_mock_store () in
  let _user, session_cookie, _csrf_token = setup_user store in
  let success_cfg = Mock_albatross.success_config in
  store.Storage.configurations <-
    Utils.LM.singleton success_cfg.name success_cfg;
  let body =
    make_albatross_body
      ~name:(Models.Configuration.name_to_str success_cfg.name)
      ~csrf_token:"wrong_csrf" ()
  in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/update" ~body
      ~session_cookie ~csrf_token:"wrong_csrf" ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_update_non_admin_forbidden () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_non_admin_user store in
  let success_cfg = Mock_albatross.success_config in
  store.Storage.configurations <-
    Utils.LM.singleton success_cfg.name success_cfg;
  let body =
    make_albatross_body
      ~name:(Models.Configuration.name_to_str success_cfg.name)
      ~csrf_token ()
  in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/update" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Only administrators permitted message" true
    (String.includes ~affix:"You don't have the necessary permissions" resp)

let check_update_unauthenticated () =
  let store = init_mock_store () in
  let success_cfg = Mock_albatross.success_config in
  store.Storage.configurations <-
    Utils.LM.singleton success_cfg.name success_cfg;
  let body =
    make_albatross_body
      ~name:(Models.Configuration.name_to_str success_cfg.name)
      ()
  in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/update" ~body ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "No molly-session in cookie header message" true
    (String.includes ~affix:"No molly-session in cookie header" resp)

let check_update_invalid_method () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request ~path:"/api/admin/settings/albatross/update"
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_delete_success () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let success_cfg = Mock_albatross.success_config in
  store.Storage.configurations <-
    Utils.LM.singleton success_cfg.name success_cfg;
  let name_str = Models.Configuration.name_to_str success_cfg.name in
  let body = make_delete_body ~name:name_str ~csrf_token () in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/delete" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Configuration delete successfully message" true
    (String.includes ~affix:"Configuration delete successfully" resp);
  Alcotest.(check bool)
    "Store configurations no longer contains deleted instance" false
    (Utils.LM.mem success_cfg.name store.Storage.configurations)

let check_delete_missing_name_field () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body =
    Utils.Json.to_string
      (Albatross_json.j_obj [ ("molly_csrf", Jsont.Json.string csrf_token) ])
  in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/delete" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Failed to parse JSON error message" true
    (String.includes ~affix:"Failed to parse JSON:" resp)

let check_delete_invalid_name_label () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let body = make_delete_body ~name:"invalid name!" ~csrf_token () in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/delete" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Invalid label error message" true
    (String.includes ~affix:"invalid label" resp)

let check_delete_invalid_csrf () =
  let store = init_mock_store () in
  let _user, session_cookie, _csrf_token = setup_user store in
  let body = make_delete_body ~name:"default" ~csrf_token:"wrong_csrf" () in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/delete" ~body
      ~session_cookie ~csrf_token:"wrong_csrf" ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

let check_delete_missing_csrf () =
  let store = init_mock_store () in
  let _user, session_cookie, _csrf_token = setup_user store in
  let body = make_delete_body ~name:"default" () in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/delete" ~body
      ~session_cookie ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Failed to parse JSON error message" true
    (String.includes ~affix:"Failed to parse JSON:" resp)

let check_delete_non_admin_forbidden () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_non_admin_user store in
  let body = make_delete_body ~name:"default" ~csrf_token () in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/delete" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "Only administrators permitted message" true
    (String.includes ~affix:"You don't have the necessary permissions" resp)

let check_delete_unauthenticated () =
  let _store = init_mock_store () in
  let body = make_delete_body ~name:"default" () in
  let req =
    make_post_request ~path:"/api/admin/settings/albatross/delete" ~body ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
  Alcotest.(check bool)
    "No molly-session in cookie header message" true
    (String.includes ~affix:"No molly-session in cookie header" resp)

let check_delete_invalid_method () =
  let store = init_mock_store () in
  let _user, session_cookie, csrf_token = setup_user store in
  let req =
    make_get_request ~path:"/api/admin/settings/albatross/delete"
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 400 Bad Request" true
    (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp)

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
