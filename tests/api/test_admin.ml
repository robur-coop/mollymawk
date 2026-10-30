open Test_utils
open Mock_devices

let now = Ptime_clock.now ()

let make_admin =
  Models.User.create_user
    ~name:(label_of_string_exn "admin")
    ~email:(email_of_string_exn "admin@robur.coop")
    ~password:"AdminPassword123!" ~created_at:now ~active:true ~super_user:true
    ~user_agent:(Some "Alcotest-client")

let make_user ?(active = false) ?(super_user = false) () =
  Models.User.create_user
    ~name:(label_of_string_exn "test")
    ~email:(email_of_string_exn "test@robur.coop")
    ~password:"TestPassword123!" ~created_at:now ~active ~super_user
    ~user_agent:(Some "Alcotest-client")

let setup_admin_and_user ?(user_active = true) store =
  let admin, session_cookie = make_admin in
  let target_user, _ = make_user ~active:user_active () in
  Storage.add_user store admin;
  Storage.add_user store target_user;
  let csrf = make_csrf_token ~session_cookie:session_cookie.value () in
  (admin, session_cookie.value, csrf, target_user)

let setup_admin_only store =
  let admin, session_cookie = make_admin in
  Storage.add_user store admin;
  let csrf = make_csrf_token ~session_cookie:session_cookie.value () in
  (admin, session_cookie.value, csrf)

let make_admin_body ~uuid ~csrf_token =
  Fmt.str {|{ "uuid": "%s", "molly_csrf": "%s" }|} uuid csrf_token

let check_toggle_account_active () =
  let store = init_mock_store () in
  let _admin, session_cookie, csrf_token, test = setup_admin_and_user store in
  let body = make_admin_body ~uuid:(Uuidm.to_string test.uuid) ~csrf_token in
  let req =
    make_post_request ~path:"/api/admin/user/activate/toggle" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Updated user successfully message" true
    (String.includes ~affix:"Updated user successfully" resp);
  let updated_test_user = Option.get (Storage.find_by_uuid store test.uuid) in
  Alcotest.(check bool)
    "Target user active becomes false" false updated_test_user.active

let check_guard_last_active_user () =
  let store = init_mock_store () in
  let admin, session_cookie, csrf_token = setup_admin_only store in
  let body = make_admin_body ~uuid:(Uuidm.to_string admin.uuid) ~csrf_token in
  let req =
    make_post_request ~path:"/api/admin/user/activate/toggle" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 403 Forbidden" true
    (String.starts_with ~prefix:"HTTP/1.1 403 Forbidden" resp);
  Alcotest.(check bool)
    "Cannot deactivate last active user message" true
    (String.includes ~affix:"Cannot deactivate last active user" resp);
  let updated_admin = Option.get (Storage.find_by_uuid store admin.uuid) in
  Alcotest.(check bool) "Admin user remains active" true updated_admin.active

let check_toggle_admin_superuser () =
  let store = init_mock_store () in
  let _admin, session_cookie, csrf_token, test = setup_admin_and_user store in
  let body = make_admin_body ~uuid:(Uuidm.to_string test.uuid) ~csrf_token in
  let req =
    make_post_request ~path:"/api/admin/user/admin/toggle" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Updated user successfully message" true
    (String.includes ~affix:"Updated user successfully" resp);
  let updated_test_user = Option.get (Storage.find_by_uuid store test.uuid) in
  Alcotest.(check bool)
    "Target user becomes super_user" true updated_test_user.super_user

let check_guard_last_administrator () =
  let store = init_mock_store () in
  let admin, session_cookie, csrf_token, _test = setup_admin_and_user store in
  let body = make_admin_body ~uuid:(Uuidm.to_string admin.uuid) ~csrf_token in
  let req =
    make_post_request ~path:"/api/admin/user/admin/toggle" ~body ~session_cookie
      ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 403 Forbidden" true
    (String.starts_with ~prefix:"HTTP/1.1 403 Forbidden" resp);
  Alcotest.(check bool)
    "Cannot remove last administrator message" true
    (String.includes ~affix:"Cannot remove last administrator" resp);
  let updated_admin = Option.get (Storage.find_by_uuid store admin.uuid) in
  Alcotest.(check bool) "Admin remains super_user" true updated_admin.super_user

let check_delete_account_success () =
  let store = init_mock_store () in
  let _admin, session_cookie, csrf_token, test = setup_admin_and_user store in
  let body = make_admin_body ~uuid:(Uuidm.to_string test.uuid) ~csrf_token in
  let req =
    make_post_request ~path:"/api/admin/user/account/delete" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 200 OK" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
  Alcotest.(check bool)
    "Deleted user successfully message" true
    (String.includes ~affix:"Deleted user successfully" resp);
  let test_in_store = Storage.find_by_uuid store test.uuid in
  Alcotest.(check bool)
    "Target user removed from store" true
    (Option.is_none test_in_store)

let check_delete_account_not_found () =
  let store = init_mock_store () in
  let _admin, session_cookie, csrf_token = setup_admin_only store in
  let uuid = "00000000-0000-0000-0000-000000000000" in
  let body = make_admin_body ~uuid ~csrf_token in
  let req =
    make_post_request ~path:"/api/admin/user/account/delete" ~body
      ~session_cookie ~csrf_token ()
  in
  let resp = query_endpoint req in
  Alcotest.(check bool)
    "Response has HTTP 404 Not Found" true
    (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp);
  Alcotest.(check bool)
    "Account not found error message" true
    (String.includes ~affix:"Account not found" resp)

let tests =
  [
    ("Toggle account active", `Quick, check_toggle_account_active);
    ("Guard last active user", `Quick, check_guard_last_active_user);
    ("Toggle admin superuser", `Quick, check_toggle_admin_superuser);
    ("Guard last administrator", `Quick, check_guard_last_administrator);
    ("Delete account success", `Quick, check_delete_account_success);
    ("Delete account not found", `Quick, check_delete_account_not_found);
  ]
