open Test_utils

let check_add_and_find_session () =
  Middleware.reset_state ();
  let user = make_mock_user ~name:"user1" ~email:"user1@robur.coop" () in
  let now = Mirage_ptime.now () in
  let cookie =
    User_model.generate_cookie ~expires_in:3600 now (Some "Test-Agent") ()
  in
  Middleware.add_session user.uuid cookie;
  (match Middleware.find_session cookie.value with
  | Some (uuid, c) ->
      Alcotest.(check bool)
        "User UUID matches" true
        (Uuidm.equal uuid user.uuid);
      Alcotest.(check string) "Cookie value matches" cookie.value c.value;
      Alcotest.(check int) "Expires at the correct time" 3600 c.expires_in
  | None -> Alcotest.fail "Expected session to be found");
  Alcotest.(check bool)
    "Nonexistent session returns None" true
    (Option.is_none (Middleware.find_session "unknown-session"))

let check_remove_session () =
  Middleware.reset_state ();
  let user = make_mock_user ~name:"user1" ~email:"user1@robur.coop" () in
  let now = Mirage_ptime.now () in
  let cookie = User_model.generate_cookie ~expires_in:3600 now None () in
  Middleware.add_session user.uuid cookie;
  Alcotest.(check bool)
    "Session exists initially" true
    (Option.is_some (Middleware.find_session cookie.value));
  Middleware.remove_session cookie.value;
  Alcotest.(check bool)
    "Session removed" true
    (Option.is_none (Middleware.find_session cookie.value))

let check_user_sessions () =
  Middleware.reset_state ();
  let user1 = make_mock_user ~name:"user1" ~email:"user1@robur.coop" () in
  let user2 = make_mock_user ~name:"user2" ~email:"user2@robur.coop" () in
  let now = Mirage_ptime.now () in
  let c1 = User_model.generate_cookie ~expires_in:3600 now None () in
  let c2 = User_model.generate_cookie ~expires_in:3600 now None () in
  let c3 = User_model.generate_cookie ~expires_in:3600 now None () in
  Middleware.add_session user1.uuid c1;
  Middleware.add_session user1.uuid c2;
  Middleware.add_session user2.uuid c3;
  let u1_sessions = Middleware.user_sessions user1.uuid in
  let u2_sessions = Middleware.user_sessions user2.uuid in
  Alcotest.(check int) "User 1 has 2 sessions" 2 (Utils.SM.cardinal u1_sessions);
  Alcotest.(check bool) "User 1 has c1" true (Utils.SM.mem c1.value u1_sessions);
  Alcotest.(check bool) "User 1 has c2" true (Utils.SM.mem c2.value u1_sessions);
  Alcotest.(check int) "User 2 has 1 session" 1 (Utils.SM.cardinal u2_sessions);
  Alcotest.(check bool) "User 2 has c3" true (Utils.SM.mem c3.value u2_sessions)

let tests =
  [
    ("Add and find session", `Quick, check_add_and_find_session);
    ("Remove session", `Quick, check_remove_session);
    ("User sessions isolation", `Quick, check_user_sessions);
  ]
