open Test_utils

let check_add_and_find_session () =
  let store = Storage.create () in
  let user = make_mock_user ~name:"user1" ~email:"user1@robur.coop" () in
  Storage.add_user store user;
  let now = Ptime_clock.now () in
  let cookie =
    Models.User.generate_cookie ~expires_in:3600 now (Some "Test-Agent") ()
  in
  Storage.add_session store user.uuid cookie;
  (match Storage.find_by_cookie store cookie.value with
  | Some (u, c) ->
      Alcotest.(check bool)
        "User UUID matches" true
        (Uuidm.equal u.uuid user.uuid);
      Alcotest.(check string) "Cookie value matches" cookie.value c.value;
      Alcotest.(check int) "Expires at the correct time" 3600 c.expires_in
  | None -> Alcotest.fail "Expected session to be found");
  Alcotest.(check bool)
    "Nonexistent session returns None" true
    (Option.is_none (Storage.find_by_cookie store "unknown-session"))

let check_remove_session () =
  let store = Storage.create () in
  let user = make_mock_user ~name:"user1" ~email:"user1@robur.coop" () in
  Storage.add_user store user;
  let now = Ptime_clock.now () in
  let cookie = Models.User.generate_cookie ~expires_in:3600 now None () in
  Storage.add_session store user.uuid cookie;
  Alcotest.(check bool)
    "Session exists initially" true
    (Option.is_some (Storage.find_by_cookie store cookie.value));
  Storage.remove_session store cookie.value;
  Alcotest.(check bool)
    "Session removed" true
    (Option.is_none (Storage.find_by_cookie store cookie.value))

let check_user_sessions () =
  let store = Storage.create () in
  let user1 =
    Models.User.update_user
      (make_mock_user ~name:"user1" ~email:"user1@robur.coop" ())
      ~cookies:Utils.SM.empty ()
  in
  let user2 =
    Models.User.update_user
      (make_mock_user ~name:"user2" ~email:"user2@robur.coop" ())
      ~cookies:Utils.SM.empty ()
  in
  Storage.add_user store user1;
  Storage.add_user store user2;
  let now = Ptime_clock.now () in
  let c1 = Models.User.generate_cookie ~expires_in:3600 now None () in
  let c2 = Models.User.generate_cookie ~expires_in:3600 now None () in
  let c3 = Models.User.generate_cookie ~expires_in:3600 now None () in
  Storage.add_session store user1.uuid c1;
  Storage.add_session store user1.uuid c2;
  Storage.add_session store user2.uuid c3;
  let u1_sessions = Storage.user_sessions store user1.uuid in
  let u2_sessions = Storage.user_sessions store user2.uuid in
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
