open Test_utils
open Mock_devices
open Lwt.Infix

let sample_block_data = "sample raw block device data"

let block_json ~name ~size ~compressed =
  Fmt.str {|{"block_name": "%s", "block_size": %d, "block_compressed": %b}|}
    name size compressed

let check_block_create_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts =
        [
          ("albatross_instance", "default");
          ("molly_csrf", csrf_token);
          ("json_data", block_json ~name:"data-vol" ~size:1024 ~compressed:false);
        ]
      in
      let file_part =
        ( "block_data",
          "data-vol.img",
          "application/octet-stream",
          sample_block_data )
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          ~session_cookie ~csrf_token "/api/block/create"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Lwt.return_unit )

let check_block_create_with_token () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user_with_token store >>= fun (_user, token) ->
      let parts =
        [
          ("albatross_instance", "default");
          ("molly_csrf", "dummy_csrf");
          ("json_data", block_json ~name:"data-vol" ~size:1024 ~compressed:false);
        ]
      in
      let file_part =
        ( "block_data",
          "data-vol.img",
          "application/octet-stream",
          sample_block_data )
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          ~token "/api/block/create"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Lwt.return_unit )

let check_block_create_albatross_failure () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts =
        [
          ("albatross_instance", "failing");
          ("molly_csrf", csrf_token);
          ("json_data", block_json ~name:"data-vol" ~size:1024 ~compressed:false);
        ]
      in
      let file_part =
        ( "block_data",
          "data-vol.img",
          "application/octet-stream",
          sample_block_data )
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          ~session_cookie ~csrf_token "/api/block/create"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error message indicates albatross failure" true
        (String.includes ~affix:"albatross failure" resp);
      Lwt.return_unit )

let check_block_create_missing_fields () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts =
        [ ("albatross_instance", "default"); ("molly_csrf", csrf_token) ]
      in
      let file_part =
        ( "block_data",
          "data-vol.img",
          "application/octet-stream",
          sample_block_data )
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          ~session_cookie ~csrf_token "/api/block/create"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error indicates missing fields" true
        (String.includes ~affix:"One or more required fields are missing" resp);
      Lwt.return_unit )

let check_block_create_unknown_instance () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts =
        [
          ("albatross_instance", "nonexistent-instance");
          ("molly_csrf", csrf_token);
          ("json_data", block_json ~name:"data-vol" ~size:1024 ~compressed:false);
        ]
      in
      let file_part =
        ( "block_data",
          "data-vol.img",
          "application/octet-stream",
          sample_block_data )
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          ~session_cookie ~csrf_token "/api/block/create"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 404 Not Found" true
        (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp);
      Alcotest.(check bool)
        "Error indicates instance not found" true
        (String.includes ~affix:"Couldn't find albatross instance" resp);
      Lwt.return_unit )

let check_block_create_invalid_csrf () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, _csrf_token) ->
      let parts =
        [
          ("albatross_instance", "default");
          ("molly_csrf", "wrong_csrf_token");
          ("json_data", block_json ~name:"data-vol" ~size:1024 ~compressed:false);
        ]
      in
      let file_part =
        ( "block_data",
          "data-vol.img",
          "application/octet-stream",
          sample_block_data )
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          ~session_cookie ~csrf_token:"different_csrf_token" "/api/block/create"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error indicates CSRF problem" true
        (String.includes ~affix:"CSRF token" resp);
      Lwt.return_unit )

let check_block_create_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let parts =
        [
          ("albatross_instance", "default");
          ("molly_csrf", "any_csrf");
          ("json_data", block_json ~name:"data-vol" ~size:1024 ~compressed:false);
        ]
      in
      let file_part =
        ( "block_data",
          "data-vol.img",
          "application/octet-stream",
          sample_block_data )
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          "/api/block/create"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error indicates user not found" true
        (String.includes ~affix:"User not found" resp);
      Lwt.return_unit )

let check_block_upload_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts =
        [
          ("albatross_instance", "default");
          ("molly_csrf", csrf_token);
          ("json_data", block_json ~name:"data-vol" ~size:1024 ~compressed:false);
        ]
      in
      let file_part =
        ( "block_data",
          "data-vol.img",
          "application/octet-stream",
          sample_block_data )
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          ~session_cookie ~csrf_token "/api/block/upload"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Lwt.return_unit )

let check_block_upload_with_token () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user_with_token store >>= fun (_user, token) ->
      let parts =
        [
          ("albatross_instance", "default");
          ("molly_csrf", "dummy_csrf");
          ("json_data", block_json ~name:"data-vol" ~size:1024 ~compressed:false);
        ]
      in
      let file_part =
        ( "block_data",
          "data-vol.img",
          "application/octet-stream",
          sample_block_data )
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          ~token "/api/block/upload"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Lwt.return_unit )

let check_block_upload_albatross_failure () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let parts =
        [
          ("albatross_instance", "failing");
          ("molly_csrf", csrf_token);
          ("json_data", block_json ~name:"data-vol" ~size:1024 ~compressed:false);
        ]
      in
      let file_part =
        ( "block_data",
          "data-vol.img",
          "application/octet-stream",
          sample_block_data )
      in
      let req =
        make_multipart_request ~boundary:default_boundary ~parts ~file_part
          ~session_cookie ~csrf_token "/api/block/upload"
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error indicates albatross failure" true
        (String.includes ~affix:"albatross failure" resp);
      Lwt.return_unit )

let check_block_delete_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        Fmt.str
          {|{"block_name": "data-vol", "albatross_instance": "default", "molly_csrf": "%s"}|}
          csrf_token
      in
      let req =
        make_post_request ~path:"/api/block/delete" ~body ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Lwt.return_unit )

let check_block_delete_with_token () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user_with_token store >>= fun (_user, token) ->
      let body =
        {|{"block_name": "data-vol", "albatross_instance": "default", "molly_csrf": "dummy"}|}
      in
      let req = make_post_request ~path:"/api/block/delete" ~body ~token () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Lwt.return_unit )

let check_block_delete_albatross_failure () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        Fmt.str
          {|{"block_name": "data-vol", "albatross_instance": "failing", "molly_csrf": "%s"}|}
          csrf_token
      in
      let req =
        make_post_request ~path:"/api/block/delete" ~body ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 500 Internal Server Error" true
        (String.starts_with ~prefix:"HTTP/1.1 500 Internal Server Error" resp);
      Alcotest.(check bool)
        "Error indicates albatross failure" true
        (String.includes ~affix:"albatross failure" resp);
      Lwt.return_unit )

let check_block_delete_missing_fields () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        Fmt.str {|{"albatross_instance": "default", "molly_csrf": "%s"}|}
          csrf_token
      in
      let req =
        make_post_request ~path:"/api/block/delete" ~body ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 400 Bad Request" true
        (String.starts_with ~prefix:"HTTP/1.1 400 Bad Request" resp);
      Alcotest.(check bool)
        "Error indicates missing block name" true
        (String.includes ~affix:"Couldn't find block name in json" resp);
      Lwt.return_unit )

let check_block_delete_unknown_instance () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        Fmt.str
          {|{"block_name": "data-vol", "albatross_instance": "nonexistent", "molly_csrf": "%s"}|}
          csrf_token
      in
      let req =
        make_post_request ~path:"/api/block/delete" ~body ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 404 Not Found" true
        (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp);
      Alcotest.(check bool)
        "Error indicates instance not found" true
        (String.includes ~affix:"Couldn't find albatross instance" resp);
      Lwt.return_unit )

let check_block_download_success () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        Fmt.str
          {|{"block_name": "data-vol", "albatross_instance": "default", "compression_level": 0, "molly_csrf": "%s"}|}
          csrf_token
      in
      let req =
        make_post_request ~path:"/api/block/download" ~body ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Disposition header is attachment" true
        (String.includes
           ~affix:"content-disposition: attachment; filename=\"data-vol_dump\""
           (String.lowercase_ascii resp));
      Alcotest.(check bool)
        "Response contains streamed mock data" true
        (String.includes ~affix:"mock block dump content" resp);
      Lwt.return_unit )

let check_block_download_with_token () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user_with_token store >>= fun (_user, token) ->
      let body =
        {|{"block_name": "data-vol", "albatross_instance": "default", "compression_level": 0, "molly_csrf": "dummy"}|}
      in
      let req = make_post_request ~path:"/api/block/download" ~body ~token () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Response contains streamed mock data" true
        (String.includes ~affix:"mock block dump content" resp);
      Lwt.return_unit )

let check_block_download_unknown_instance () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let body =
        Fmt.str
          {|{"block_name": "data-vol", "albatross_instance": "nonexistent", "compression_level": 0, "molly_csrf": "%s"}|}
          csrf_token
      in
      let req =
        make_post_request ~path:"/api/block/download" ~body ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 404 Not Found" true
        (String.starts_with ~prefix:"HTTP/1.1 404 Not Found" resp);
      Lwt.return_unit )

let check_blocks_page_authenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      setup_user store >>= fun (_user, session_cookie, csrf_token) ->
      let req =
        make_get_request ~path:"/blocks?instance=default" ~session_cookie
          ~csrf_token ()
      in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Response has HTTP 200 OK" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK" resp);
      Alcotest.(check bool)
        "Content-Type is text/html" true
        (String.includes ~affix:"text/html" resp);
      Lwt.return_unit )

let check_blocks_page_unauthenticated () =
  Lwt_main.run
    ( init_mock_store () >>= fun store ->
      let req = make_get_request ~path:"/blocks?instance=default" () in
      query_endpoint (make_app_request_handler store) req >>= fun resp ->
      Alcotest.(check bool)
        "Redirects to sign-in" true
        (String.starts_with ~prefix:"HTTP/1.1 303 See Other" resp
        || String.includes ~affix:"/sign-in" resp);
      Lwt.return_unit )

let tests =
  [
    ( "Successful block create (multipart form-data)",
      `Quick,
      check_block_create_success );
    ( "Successful block create with Bearer token",
      `Quick,
      check_block_create_with_token );
    ( "Handle Albatross block create error",
      `Quick,
      check_block_create_albatross_failure );
    ( "Reject block create when file or name missing",
      `Quick,
      check_block_create_missing_fields );
    ( "Reject block create when instance is unknown",
      `Quick,
      check_block_create_unknown_instance );
    ( "Reject block create when CSRF token is invalid",
      `Quick,
      check_block_create_invalid_csrf );
    ( "Reject block create when unauthenticated",
      `Quick,
      check_block_create_unauthenticated );
    ("Successful block upload", `Quick, check_block_upload_success);
    ( "Successful block upload with Bearer token",
      `Quick,
      check_block_upload_with_token );
    ( "Handle Albatross block upload error",
      `Quick,
      check_block_upload_albatross_failure );
    ("Successful block delete", `Quick, check_block_delete_success);
    ( "Successful block delete with Bearer token",
      `Quick,
      check_block_delete_with_token );
    ( "Handle Albatross block delete error",
      `Quick,
      check_block_delete_albatross_failure );
    ( "Reject block delete when fields are missing",
      `Quick,
      check_block_delete_missing_fields );
    ( "Reject block delete when instance is unknown",
      `Quick,
      check_block_delete_unknown_instance );
    ( "Successful block download (session auth)",
      `Quick,
      check_block_download_success );
    ( "Successful block download with Bearer token",
      `Quick,
      check_block_download_with_token );
    ( "Reject block download when instance is unknown",
      `Quick,
      check_block_download_unknown_instance );
    ("Blocks HTML page authenticated", `Quick, check_blocks_page_authenticated);
    ( "Blocks HTML page unauthenticated redirects",
      `Quick,
      check_blocks_page_unauthenticated );
  ]
