let current_store = ref (Storage.create ())

let init_mock_store () =
  let s = Storage.create () in
  current_store := s;
  s

let peer (_ : Mhttp_server.flow) = "127.0.0.1"
let now () = Int32.of_float (Unix.gettimeofday ())
let dummy_flow : Mhttp_server.flow = Obj.magic ()

let mock_env : Controller.env =
  {
    tcp = Obj.magic ();
    udp = Obj.magic ();
    m_tcp = Obj.magic ();
    m_udp = Obj.magic ();
    block = Obj.magic ();
    csrf_key = Test_utils.csrf_key;
  }

let secret_key = Mirage_crypto.AES.GCM.of_secret (String.make 32 'k')

let add_device dev value map =
  let (Vif_core.Device.Device (_, _, key)) =
    (Obj.magic dev : (_, _) Vif_core.Device.device)
  in
  Vif_core.Device.Hmap.add key value map

let server_for_store ?policies store : Vif_core.Server.t =
  let albatross_instances =
    ref (Mock_albatross.default_instances ?policies ())
  in
  let dev_map =
    Vif_core.Device.Hmap.empty
    |> add_device Controller.storage store
    |> add_device Controller.happy_eyeballs (Obj.magic ())
    |> add_device Controller.management_happy_eyeballs (Obj.magic ())
    |> add_device Controller.albatross albatross_instances
  in
  Vif_core.Server.
    {
      devices = dev_map;
      cookie_key = secret_key;
      metrics = Vif_core.Metrics.empty ();
    }

let middlewares : (Mhttp_server.flow, Controller.env) Vif_core.Middleware.m =
  Obj.magic Vifu.Middlewares.[ Controller.Session.session ]

let default_handler req _target _server _env =
  Obj.magic
    (Controller.error_page_response ~title:"Page not found"
       ~data:"This page cannot be found. Bad HTTP request method" `Bad_request
       (Obj.magic req))

let dispatch ~meth ~request ?host target =
  let routes : (Mhttp_server.flow, _) Vif_core.Route.t list =
    Obj.magic (App.routes ())
  in
  Vif_core.Route.dispatch ~default:default_handler routes ~meth ~request ?host
    target

let dispatch_reqd ?policies conn reqd =
  let req0 = Vif_core.Request0.of_reqd ~peer dummy_flow (`H1 conn) (`V1 reqd) in
  let server = server_for_store ?policies !current_store in
  let ctx =
    Vif_core.Middleware.
      {
        server;
        req0;
        target = Vif_core.Request0.target req0;
        user's_value = mock_env;
      }
  in
  let env =
    Vif_core.Middleware.run middlewares ctx Vif_core.Middleware.Hmap.empty
  in
  let request = Vif_core.recognize_request ~env req0 in
  let target = Vif_core.Request0.target req0 in
  let meth = Vif_core.Request0.meth req0 in
  let host = Vif_core.Headers.get (Vif_core.Request0.headers req0) "host" in
  let fn = dispatch ~meth ~request ?host target in
  let Vif_core.Response.Sent, () =
    Vif_core.Response.(run server ~now req0 Empty)
      (Obj.magic (fn (Obj.magic server) mock_env))
  in
  Vif_core.Request0.close req0

let make_default_policies = Mock_albatross.make_default_policies

let query_endpoint ?policies req =
  Miou_unix.run @@ fun () ->
  let output_buffer = Buffer.create 1024 in
  let conn_ref = ref None in
  let handler reqd =
    let conn = Option.get !conn_ref in
    dispatch_reqd ?policies conn reqd
  in
  let conn = H1.Server_connection.create handler in
  conn_ref := Some conn;
  let bs = Bigstringaf.of_string ~off:0 ~len:(String.length req) req in
  let _ =
    H1.Server_connection.read_eof conn bs ~off:0 ~len:(Bigstringaf.length bs)
  in
  let rec drain () =
    match H1.Server_connection.next_write_operation conn with
    | `Write iovecs ->
        List.iter
          (fun { H1.IOVec.buffer; off; len } ->
            Buffer.add_string output_buffer
              (Bigstringaf.substring ~off ~len buffer);
            H1.Server_connection.report_write_result conn (`Ok len))
          iovecs;
        drain ()
    | `Yield -> H1.Server_connection.yield_writer conn (fun () -> drain ())
    | `Close _ | `Upgrade -> ()
  in
  drain ();
  Buffer.contents output_buffer

let setup_user ?name ?email ?(password = "Password123!") store =
  let name = Option.value ~default:"test" name in
  let email = Option.value ~default:"test@robur.coop" email in
  let active, super_user =
    if Storage.count_users store = 0 then (true, true) else (false, false)
  in
  let name_lbl = Test_utils.label_of_string_exn name in
  let email_box = Test_utils.email_of_string_exn email in
  let user, cookie =
    Models.User.create_user ~name:name_lbl ~email:email_box ~password
      ~created_at:Ptime.epoch ~active ~super_user
      ~user_agent:(Some "Alcotest-client")
  in
  Storage.add_user store user;
  Controller.Session.add_session user.uuid cookie;
  let session_cookie = cookie.value in
  (user, session_cookie, Test_utils.make_csrf_token ~session_cookie ())

let setup_non_admin_user ?name ?email ?password store =
  let _admin, _admin_session, _admin_csrf_token =
    setup_user ~name:"admin" ~email:"admin@robur.coop" store
  in
  let user, session_cookie, csrf_token =
    setup_user ?name ?email ?password store
  in
  let user = Models.User.update_user user ~active:true () in
  Storage.update_user store user;
  (user, session_cookie, csrf_token)
