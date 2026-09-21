open Test_utils
open Mock_devices
open Lwt.Infix

let now = Mirage_ptime.now ()

let setup_user_and_cookies ?(tokens = []) store =
  let base_user = make_mock_user ~tokens () in
  let session_cookie =
    List.find
      (fun (c : User_model.cookie) ->
        String.equal c.name User_model.session_cookie)
      base_user.cookies
  in
  let csrf_cookie =
    User_model.generate_cookie ~name:User_model.csrf_cookie ~uuid:base_user.uuid
      ~created_at:now ~user_agent:(Some "Alcotest-client") ()
  in
  let user = { base_user with cookies = [ session_cookie; csrf_cookie ] } in
  store.Storage.users <- [ user ];
  (user, session_cookie.value, csrf_cookie.value)

let make_test_token ?(name = "test-token") ?(expiry = 3600) () =
  User_model.generate_token ~name ~expiry ~current_time:now

let check_create_token_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let _user, session_cookie, csrf_token = setup_user_and_cookies store in
      let body =
        Fmt.str
          {|{ "token_name": "ci-token", "token_expiry": 86400, "molly_csrf": "%s" }|}
          csrf_token
      in
      let req =
        make_post_request ~path:"/api/tokens/create" ~body ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Response body contains token name" true
        (String.includes ~affix:"ci-token" resp);
      Alcotest.(check bool)
        "Response body contains expires_in 86400" true
        (String.includes ~affix:"\"expires_in\":86400" resp);

      let updated_user = List.hd store.Storage.users in
      Alcotest.(check int)
        "User has 1 token in store" 1
        (List.length updated_user.tokens);
      let created_token = List.hd updated_user.tokens in
      Alcotest.(check string)
        "Token name matches in store" "ci-token" created_token.name;
      Alcotest.(check int)
        "Token expiry matches in store" 86400 created_token.expires_in;
      Lwt.return_unit )

let check_create_token_missing_fields () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let _user, session_cookie, csrf_token = setup_user_and_cookies store in
      let body =
        Fmt.str {|{ "token_name": "ci-token", "molly_csrf": "%s" }|} csrf_token
      in
      let req =
        make_post_request ~path:"/api/tokens/create" ~body ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error message indicates unexpected fields" true
        (String.includes ~affix:"Create token: Unexpected fields" resp);
      Lwt.return_unit )

let check_create_token_invalid_csrf () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let _user, session_cookie, _csrf_token = setup_user_and_cookies store in
      let body =
        {|{ "token_name": "ci-token", "token_expiry": 86400, "molly_csrf": "invalid-csrf-token" }|}
      in
      let req =
        make_post_request ~path:"/api/tokens/create" ~body ~session_cookie
          ~csrf_token:"invalid-csrf-token" ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error message indicates CSRF error" true
        (String.includes ~affix:"Missing CSRF token" resp);
      Lwt.return_unit )

let check_create_token_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let _user, _session_cookie, csrf_token = setup_user_and_cookies store in
      let body =
        Fmt.str
          {|{ "token_name": "ci-token", "token_expiry": 86400, "molly_csrf": "%s" }|}
          csrf_token
      in
      let req =
        make_post_request ~path:"/api/tokens/create" ~body ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error message indicates missing session" true
        (String.includes ~affix:"No molly-session in cookie header" resp);
      Lwt.return_unit )

let check_update_token_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let initial_token =
        make_test_token ~name:"initial-token" ~expiry:3600 ()
      in
      let _user, session_cookie, csrf_token =
        setup_user_and_cookies ~tokens:[ initial_token ] store
      in
      let body =
        Fmt.str
          {|{ "token_name": "updated-token", "token_expiry": 7200, "token_value": "%s", "molly_csrf": "%s" }|}
          initial_token.value csrf_token
      in
      let req =
        make_post_request ~path:"/api/tokens/update" ~body ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Response body contains updated token name" true
        (String.includes ~affix:"updated-token" resp);
      Alcotest.(check bool)
        "Response body contains updated expiry" true
        (String.includes ~affix:"\"expires_in\":7200" resp);

      let updated_user = List.hd store.Storage.users in
      let updated_token = List.hd updated_user.tokens in
      Alcotest.(check string)
        "Store token name updated" "updated-token" updated_token.name;
      Alcotest.(check int)
        "Store token expiry updated" 7200 updated_token.expires_in;
      Alcotest.(check string)
        "Store token value retained" initial_token.value updated_token.value;
      Lwt.return_unit )

let check_update_token_not_found () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let _user, session_cookie, csrf_token = setup_user_and_cookies store in
      let body =
        Fmt.str
          {|{ "token_name": "updated-token", "token_expiry": 7200, "token_value": "missing-uuid", "molly_csrf": "%s" }|}
          csrf_token
      in
      let req =
        make_post_request ~path:"/api/tokens/update" ~body ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 404 Not Found" true
        (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp);
      Alcotest.(check bool)
        "Error message indicates token not found" true
        (String.includes ~affix:"Token not found" resp);
      Lwt.return_unit )

let check_update_token_missing_fields () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let _user, session_cookie, csrf_token = setup_user_and_cookies store in
      let body =
        Fmt.str
          {|{ "token_name": "updated-token", "token_expiry": 7200, "molly_csrf": "%s" }|}
          csrf_token
      in
      let req =
        make_post_request ~path:"/api/tokens/update" ~body ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error message indicates unexpected fields" true
        (String.includes ~affix:"Update token: Unexpected fields" resp);
      Lwt.return_unit )

let check_delete_token_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let token = make_test_token ~name:"token-to-delete" () in
      let _user, session_cookie, csrf_token =
        setup_user_and_cookies ~tokens:[ token ] store
      in
      let body =
        Fmt.str {|{ "token_value": "%s", "molly_csrf": "%s" }|} token.value
          csrf_token
      in
      let req =
        make_post_request ~path:"/api/tokens/delete" ~body ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Response indicates token deleted successfully" true
        (String.includes ~affix:"Token deleted successfully" resp);

      let updated_user = List.hd store.Storage.users in
      Alcotest.(check int)
        "Tokens list in store is now empty" 0
        (List.length updated_user.tokens);
      Lwt.return_unit )

let check_delete_token_missing_fields () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let _user, session_cookie, csrf_token = setup_user_and_cookies store in
      let body = Fmt.str {|{ "molly_csrf": "%s" }|} csrf_token in
      let req =
        make_post_request ~path:"/api/tokens/delete" ~body ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error message indicates unexpected fields" true
        (String.includes ~affix:"Delete token: Unexpected fields" resp);
      Lwt.return_unit )

(* TODO: the test below fails and should be fixed. Tokens which don't exist should not return 200 success reponses on delete. *)
let check_delete_token_not_found () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let _user, session_cookie, csrf_token = setup_user_and_cookies store in
      let body =
        Fmt.str
          {|{ "token_name": "not-found-token", "token_expiry": 7200, "token_value": "this-token-is-not-found", "molly_csrf": "%s" }|}
          csrf_token
      in
      let req =
        make_post_request ~path:"/api/tokens/delete" ~body ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 404 Not Found" true
        (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp);
      Alcotest.(check bool)
        "Error message indicates token not found" true
        (String.includes ~affix:"Token not found" resp);
      Lwt.return_unit )

let tests =
  [
    ("Create token success", `Quick, check_create_token_success);
    ("Create token missing fields", `Quick, check_create_token_missing_fields);
    ("Create token invalid CSRF", `Quick, check_create_token_invalid_csrf);
    ("Create token unauthenticated", `Quick, check_create_token_unauthenticated);
    ("Update token success", `Quick, check_update_token_success);
    ("Update token not found", `Quick, check_update_token_not_found);
    ("Update token missing fields", `Quick, check_update_token_missing_fields);
    ("Delete token success", `Quick, check_delete_token_success);
    ("Delete token missing fields", `Quick, check_delete_token_missing_fields);
    ("Delete token not found", `Quick, check_delete_token_not_found);
  ]
