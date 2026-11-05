type env = {
  tcp : Mnet.TCP.state;
  udp : Mnet.UDP.state;
  m_tcp : Mnet.TCP.state;
  m_udp : Mnet.UDP.state;
  m_dns : Dns.proto * Mnet_dns_cli.nameserver list;
  m_domain : [ `host ] Domain_name.t;
  block : Mkernel.Block.t;
  csrf_key : string;
}

(* in default context used for test we should use ptime.epoch *)

let now () = Utils.now ()

let storage =
  let finally = Fun.const () in
  Vifu.Device.v ~name:"store" ~finally [] @@ fun { block; _ } ->
  Store.connect block

let make_happy_eyeballs ~name get_stack =
  let daemon_ref = ref None in
  let dns_daemon_ref = ref None in
  let finally _ =
    Option.iter Mnet_dns.Transport.kill !dns_daemon_ref;
    Option.iter Mnet_happy_eyeballs.kill !daemon_ref
  in
  Vifu.Device.v ~name ~finally [] @@ fun env ->
  let tcp, udp, nameservers = get_stack env in
  let daemon, he = Mnet_happy_eyeballs.create tcp in
  let stack = Mnet_dns.Transport.stack udp he in
  let dns = Mnet_dns.create ?nameservers stack in
  let dns_daemon = Mnet_dns.transport dns in
  let getaddrinfo record domain_name =
    let ( let* ) = Result.bind in
    match record with
    | `A ->
        let* _ttl, set = Mnet_dns.getaddrinfo dns Dns.Rr_map.A domain_name in
        let fn ipv4 = Ipaddr.Set.add (Ipaddr.V4 ipv4) in
        Ok (Ipaddr.V4.Set.fold fn set Ipaddr.Set.empty)
    | `AAAA ->
        let* _ttl, set = Mnet_dns.getaddrinfo dns Dns.Rr_map.Aaaa domain_name in
        let fn ipv6 = Ipaddr.Set.add (Ipaddr.V6 ipv6) in
        Ok (Ipaddr.V6.Set.fold fn set Ipaddr.Set.empty)
  in
  Mnet_happy_eyeballs.inject he getaddrinfo;
  daemon_ref := Some daemon;
  dns_daemon_ref := Some dns_daemon;
  he

let happy_eyeballs =
  make_happy_eyeballs ~name:"happy-eyeballs" (fun env ->
      (env.tcp, env.udp, None))

let management_happy_eyeballs =
  make_happy_eyeballs ~name:"management-happy-eyeballs" (fun env ->
      (env.m_tcp, env.m_udp, Some env.m_dns))

let albatross = Albatross_client.make_device storage (fun env -> env.tcp)

let rec add_headers =
  let open Vifu.Response.Syntax in
  function
  | [] -> Vifu.Response.return ()
  | (field, value) :: rest ->
      let* () = Vifu.Response.add ~field value in
      add_headers rest

let page_response ?(headers = []) ?session_cookie ty req
    (_server : Vifu.Server.t) _env =
  let open Vifu.Response.Syntax in
  let* () = add_headers headers in
  let* () =
    match session_cookie with
    | Some c ->
        Vifu.Response.add ~field:"Set-Cookie"
          ("molly_session=" ^ c ^ ";Path=/;HttpOnly=true")
    | None -> Vifu.Response.return ()
  in
  let* () = Vifu.Response.with_tyxml req ty in
  Vifu.Response.respond `OK

let api_response ?(headers = []) ~title ~data http_status req =
  let open Vifu.Response.Syntax in
  let* () = add_headers headers in
  let resp = Models.Response.make title data in
  let* () = Vifu.Response.with_json req Models.Response.to_json resp in
  Vifu.Response.respond http_status

let error_page_response ?(title = "Error") ~data http_status req =
  let open Vifu.Response.Syntax in
  let status = Models.Response.make title data in
  let ty =
    Guest_layout.guest_layout ~page_title:title (Error_page.error_layout status)
  in
  let* () = Vifu.Response.with_tyxml req ty in
  Vifu.Response.respond http_status

let static_asset_response ~etag =
  let etag = Fmt.str {|"%s"|} (Ohex.encode etag) in
  fun ~mime ~content req (_server : Vifu.Server.t) _env ->
    let open Vifu.Response.Syntax in
    let hdrs = Vifu.Request.headers req in
    match Vifu.Headers.get hdrs "if-none-match" with
    | Some etag' when String.equal etag etag' ->
        let* () = Vifu.Response.with_string req "" in
        Vifu.Response.respond `Not_modified
    | _ ->
        let* () = Vifu.Response.add ~field:"Content-Type" mime in
        let* () = Vifu.Response.add ~field:"ETag" etag in
        let* () = Vifu.Response.with_string req content in
        Vifu.Response.respond `OK

let pct_encode str = Uri.pct_encode ~component:`Authority str

let query_param req name =
  match Vifu.Queries.get req name with
  | v :: _ -> Ok v
  | [] -> Error (Fmt.str "Couldn't find %s in query params" name)

module Session = struct
  let session_cookie = "molly_session"

  let session : (env, string) Vifu.Middleware.t =
    Vifu.Middlewares.v ~name:session_cookie @@ fun req _target server _env ->
    match Vifu.Cookie.get ~encrypted:false ~name:session_cookie server req with
    | Ok cookie when not (String.equal cookie "") -> Some cookie
    | _ -> (
        match
          Vifu.Headers.get (Vifu.Request.headers_of_request req) "Cookie"
        with
        | None -> None
        (* workaround for getting csrf tokens *)
        | Some cookies ->
            String.split_on_char ';' cookies
            |> List.find_map (fun c ->
                match String.split_on_char '=' c with
                | [ k; v ] when String.equal (String.trim k) session_cookie ->
                    let v = String.trim v in
                    if String.equal v "" then None else Some v
                | _ -> None))

  let validate_user_session ?(require_admin = false) req storage server =
    let store = Vifu.Server.device storage server in
    match
      Option.bind (Vifu.Request.get session req) (Storage.find_by_cookie store)
    with
    | None -> Error "No molly-session in cookie header. User not found"
    | Some (user, cookie) ->
        if not (Models.User.is_valid_cookie cookie (now ())) then (
          Storage.remove_session store cookie.value;
          Error "Cookie expired.")
        else
          let user, cookie =
            Storage.update_cookie_usage store cookie
              (Vifu.Headers.get (Vifu.Request.headers req) "User-Agent")
              (now ()) user
          in
          if not user.active then Error "User account is deactivated"
          else if require_admin && not user.super_user then
            Error
              "You don't have the necessary permissions to access this service."
          else Ok (user, cookie)
end

let redirect_to_page ~path ?(clear_session = false) ?(with_error = false)
    ?(msg = "") req () =
  let open Vifu.Response.Syntax in
  let msg_cookie =
    if with_error then "flash_msg=error: " ^ pct_encode msg ^ ";Path=/;"
    else "flash_msg=" ^ pct_encode msg ^ ";Path=/;"
  in
  let* () =
    if clear_session then
      Vifu.Response.add ~field:"Set-Cookie"
        (Session.session_cookie ^ "=; Path=/; HttpOnly=true;")
    else Vifu.Response.return ()
  in
  let* () = Vifu.Response.add ~field:"Set-Cookie" msg_cookie in
  let* () = Vifu.Response.add ~field:"location" path in
  let* () = Vifu.Response.with_string req "" in
  Vifu.Response.respond `Found

let construct_instance_redirect_url callback instance_name =
  let separator = if String.contains callback '?' then "&" else "?" in
  Fmt.str "%s%sinstance=%s" callback separator
    (Models.Configuration.name_to_str instance_name)

let redirect_to_instance_selector callback_link ?(msg = "") req () =
  redirect_to_page
    ~path:("/select/instance?callback=" ^ pct_encode callback_link)
    ~msg req ()

let redirect_unauthenticated req msg =
  let target = Vifu.Request.target req in
  let path =
    match target with
    | "" | "/" | "/dashboard" | "/sign-in" | "/sign-up" -> "/sign-in"
    | _ -> "/sign-in?redirect=" ^ pct_encode target
  in
  redirect_to_page ~path ~clear_session:true ~with_error:true ~msg req ()

let with_user_cookie_page req server fn =
  match Session.validate_user_session req storage server with
  | Ok (user, cookie) -> fn user cookie
  | Error msg -> redirect_unauthenticated req msg

let with_admin_cookie_page req server fn =
  match
    Session.validate_user_session ~require_admin:true req storage server
  with
  | Ok (user, cookie) -> fn user cookie
  | Error msg -> redirect_unauthenticated req msg

let with_user_cookie_api ?(error_title = "Bad Request")
    ?(error_status = `Bad_request) req server fn =
  match Session.validate_user_session req storage server with
  | Ok (user, cookie) -> fn user cookie
  | Error msg -> api_response ~title:error_title ~data:msg error_status req

let with_admin_cookie_api req server fn =
  match
    Session.validate_user_session ~require_admin:true req storage server
  with
  | Ok (user, cookie) -> fn user cookie
  | Error msg -> api_response ~title:"Bad Request" ~data:msg `Bad_request req

let verify_csrf env req csrf =
  match Vifu.Request.get Session.session req with
  | None ->
      Logs.debug (fun m ->
          m "csrf verification: no molly-session cookie in header.");
      false
  | Some session_cookie ->
      Csrf.verify ~key:env.csrf_key ~session_cookie ~token:csrf ()

let generate_csrf_token env req =
  let session_cookie =
    Option.value ~default:"" (Vifu.Request.get Session.session req)
  in
  Csrf.generate_token ~key:env.csrf_key ~session_cookie ()

let with_json_csrf req env ~csrf_of f =
  match Vifu.Request.of_json req with
  | Error (`Msg err) ->
      api_response ~title:"Bad Request"
        ~data:(Fmt.str "Failed to parse JSON: %s" err)
        `Bad_request req
  | Ok form ->
      if verify_csrf env req (csrf_of form) then f form
      else
        api_response ~title:"CSRF Token Mismatch"
          ~data:"CSRF token mismatch error. Please refresh and try again."
          `Bad_request req

let make_string_stream str =
  let sent = ref false in
  fun () ->
    if !sent then None
    else (
      sent := true;
      Some str)

let with_instance ?endpoint req server fn =
  let albatross_instances = !(Vifu.Server.device albatross server) in
  let endpoint = Option.value ~default:(Vifu.Request.target req) endpoint in
  match query_param req "instance" with
  | Ok instance -> (
      match Models.Configuration.name_of_str instance with
      | Ok instance_name -> (
          match
            Albatross_client.find_instance_by_name albatross_instances
              instance_name
          with
          | Ok albatross_inst -> fn albatross_inst
          | Error _ ->
              Logs.err (fun m ->
                  m "Couldn't find albatross instance with name %s"
                    (Models.Configuration.name_to_str instance_name));
              error_page_response ~title:"Not Found"
                ~data:
                  ("Couldn't find albatross instance with name: "
                  ^ Models.Configuration.name_to_str instance_name)
                `Not_found req)
      | Error (`Msg err) ->
          error_page_response ~title:"Albatross instance error"
            ~data:("Error with albatross instance name: " ^ err)
            `Bad_request req)
  | Error _ -> redirect_to_instance_selector endpoint req ()

let with_unikernel req fn =
  match query_param req "unikernel" with
  | Ok unikernel -> (
      match Models.Configuration.name_of_str unikernel with
      | Ok unikernel_name -> fn unikernel_name
      | Error (`Msg err) ->
          error_page_response ~title:"Unikernel name error"
            ~data:("Error with unikernel name: " ^ err)
            `Bad_request req)
  | Error err ->
      error_page_response ~title:"Bad Request" ~data:err `Bad_request req

let with_unikernel_and_instance ?endpoint req server fn =
  with_instance ?endpoint req server (fun albatross_inst ->
      with_unikernel req (fun unikernel_name ->
          fn albatross_inst unikernel_name))

let ( let*? ) r f = match r with Ok x -> f x | Error resp -> resp

let with_api_instance ~instance_str req server fn =
  let albatross_instances = !(Vifu.Server.device albatross server) in
  match Models.Configuration.name_of_str instance_str with
  | Error (`Msg err) ->
      api_response ~title:"Bad Request"
        ~data:("Couldn't convert name to albatross instance: " ^ err)
        `Bad_request req
  | Ok instance_name -> (
      match
        Albatross_client.find_instance_by_name albatross_instances instance_name
      with
      | Ok albatross_inst -> fn albatross_inst
      | Error _ ->
          let name_str = Models.Configuration.name_to_str instance_name in
          Logs.err (fun m ->
              m "Couldn't find albatross instance with name %s" name_str);
          api_response ~title:"Not Found"
            ~data:("Couldn't find albatross instance with name: " ^ name_str)
            `Not_found req)

let with_api_instance_and_name ~instance_str ~name_str req server fn =
  with_api_instance ~instance_str req server (fun albatross_inst ->
      match Models.Configuration.name_of_str name_str with
      | Error (`Msg err) ->
          api_response ~title:"Bad Request"
            ~data:("Couldn't convert name: " ^ err)
            `Bad_request req
      | Ok name -> fn albatross_inst name)

let query_albatross_api ?(parse_error_status = `Internal_server_error) ?push tcp
    albatross_inst ~domain ~name cmd req =
  match Albatross_client.query tcp albatross_inst ~domain ~name ?push cmd with
  | Error err ->
      api_response ~title:"Internal Server Error"
        ~data:("Error querying albatross: " ^ err)
        `Internal_server_error req
  | Ok (_hdr, res) -> (
      match Albatross_json.res_to_string res with
      | Ok res -> api_response ~title:"Success" ~data:res `OK req
      | Error (`String err) ->
          api_response ~title:"Error" ~data:err parse_error_status req)

let dashboard_page ?page_title user ~content req server env =
  let csrf = generate_csrf_token env req in
  page_response
    ~headers:[ ("X-MOLLY-CSRF", csrf) ]
    (Dashboard.dashboard_layout ~csrf user ?page_title ~content ())
    req server env

let write_data ?headers ?(error_title = "Failure") ?(on_ok = ignore)
    ?(on_error = Fun.id) ?(success_msg = "Updated password successfully")
    storage env req =
  match Store.write_data storage env.block with
  | Ok () ->
      on_ok ();
      api_response ?headers ~title:"Success" ~data:success_msg `OK req
  | Error (`Msg err) ->
      let data = on_error err in
      api_response ~title:error_title ~data `Internal_server_error req

module Auth = struct
  (* new users have no sessions yet *)
  let sign_up_page req server env =
    let session_cookie = Uuidm.to_string (Models.User.generate_uuid ()) in
    let csrf = Csrf.generate_token ~key:env.csrf_key ~session_cookie () in
    page_response ~session_cookie
      (Guest_layout.guest_layout ~page_title:"Sign up"
         (Sign_up.register_page csrf))
      req server env

  let sign_in_page req server env =
    let session_cookie = Uuidm.to_string (Models.User.generate_uuid ()) in
    let csrf = Csrf.generate_token ~key:env.csrf_key ~session_cookie () in
    page_response ~session_cookie
      (Guest_layout.guest_layout ~page_title:"Sign in" (Sign_in.login_page csrf))
      req server env

  let set_cookie_header (cookie : Models.User.cookie) =
    let cookie_value =
      Fmt.str "%s=%s;Path=/;Max-Age=%d;HttpOnly=true" Session.session_cookie
        cookie.value cookie.expires_in
    in
    [ ("Set-Cookie", cookie_value); ("location", "/dashboard") ]

  let register_user req server env =
    let storage = Vifu.Server.device storage server in
    let user_agent = Vifu.Headers.get (Vifu.Request.headers req) "User-Agent" in
    with_json_csrf req env
      ~csrf_of:(fun (f : Models.Registration.t) -> f.molly_csrf)
      (fun ({ name; email; password; _ } : Models.Registration.t) ->
        match
          Models.Registration.validate_user_input ~name ~email ~password
        with
        | Error err -> api_response ~title:"Failure" ~data:err `Bad_request req
        | Ok (name, email) -> (
            match
              ( Storage.find_by_name storage name,
                Storage.find_by_email storage email )
            with
            | Some _, Some _ ->
                api_response ~title:"Failure"
                  ~data:"A user with this name or email already exist."
                  `Bad_request req
            | Some _, None ->
                api_response ~title:"Failure"
                  ~data:"A user with this name already exist." `Bad_request req
            | None, Some _ ->
                api_response ~title:"Failure"
                  ~data:"A user with this email already exist." `Bad_request req
            | None, None ->
                let active, super_user =
                  if Storage.count_users storage = 0 then (true, true)
                  else (false, false)
                in
                let user, cookie =
                  Models.User.create_user ~name ~email ~password
                    ~created_at:(now ()) ~active ~super_user ~user_agent
                in
                Storage.add_user storage user;
                write_data ~headers:(set_cookie_header cookie)
                  ~on_error:(fun err ->
                    Storage.delete_user storage user;
                    err)
                  ~success_msg:"Registration successful" storage env req))

  let login_user req server env =
    let storage = Vifu.Server.device storage server in
    let user_agent = Vifu.Headers.get (Vifu.Request.headers req) "User-Agent" in
    with_json_csrf req env
      ~csrf_of:(fun (f : Models.Login.t) -> f.molly_csrf)
      (fun ({ email; password; _ } : Models.Login.t) ->
        if String.equal email "" || String.equal password "" then
          api_response ~title:"Failure" ~data:"All fields must be filled."
            `Bad_request req
        else
          match Models.Email.validate_email email with
          | Error (`Msg _err) ->
              api_response ~title:"Failure" ~data:"Invalid email address."
                `Bad_request req
          | Ok email_box -> (
              match
                Models.User.login_user ~email:email_box ~password ~user_agent
                  (Storage.find_by_email storage email_box)
                  (now ())
              with
              | Error (`Msg err) ->
                  api_response ~title:"Failure" ~data:err `Bad_request req
              | Ok (user, cookie) ->
                  Storage.update_user storage user;
                  write_data ~headers:(set_cookie_header cookie)
                    ~on_error:(fun err ->
                      Storage.remove_session storage cookie.value;
                      err)
                    ~success_msg:"Login successful" storage env req))

  let logout_user req server env =
    with_user_cookie_api ~error_title:"Failure" ~error_status:`Unauthorized req
      server (fun _user session_cookie ->
        let storage = Vifu.Server.device storage server in
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Close_sessions.t) -> f.molly_csrf)
          (fun (_ : Models.Close_sessions.t) ->
            Storage.remove_session storage session_cookie.value;
            write_data ~success_msg:"Logout successful" storage env req))

  let update_password req server env =
    with_user_cookie_api ~error_title:"Failure" ~error_status:`Unauthorized req
      server (fun user _session_cookie ->
        let storage = Vifu.Server.device storage server in
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Password.t) -> f.molly_csrf)
          (fun ({ current_password; new_password; confirm_password; _ } :
                 Models.Password.t)
             ->
            let current_hash =
              Models.User.hash_password ~password:current_password
                ~uuid:user.uuid
            in
            if not (String.equal user.password current_hash) then
              api_response ~title:"Failure"
                ~data:"The current password entered is wrong." `Bad_request req
            else if not (String.equal new_password confirm_password) then
              api_response ~title:"Failure"
                ~data:"New password and confirm password do not match"
                `Bad_request req
            else if not (Models.User.password_validation new_password) then
              api_response ~title:"Failure"
                ~data:"New password must be atleast 8 characters." `Bad_request
                req
            else
              let new_hash =
                Models.User.hash_password ~password:new_password ~uuid:user.uuid
              in
              let updated_user =
                Models.User.update_user user ~password:new_hash
                  ~updated_at:(now ()) ()
              in
              Storage.update_user storage updated_user;
              write_data storage env req))

  let close_all_sessions req server env =
    with_user_cookie_api ~error_title:"Failure" ~error_status:`Unauthorized req
      server (fun user session_cookie ->
        let storage = Vifu.Server.device storage server in
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Close_sessions.t) -> f.molly_csrf)
          (fun (_ : Models.Close_sessions.t) ->
            Storage.close_other_sessions storage user.uuid session_cookie.value;
            write_data ~success_msg:"Closed all sessions successfully" storage
              env req))

  let close_session req server env =
    with_user_cookie_api ~error_title:"Failure" ~error_status:`Unauthorized req
      server (fun _user _session_cookie ->
        let storage = Vifu.Server.device storage server in
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Close_session.t) -> f.molly_csrf)
          (fun ({ session_value; _ } : Models.Close_session.t) ->
            Storage.remove_session storage session_value;
            write_data ~success_msg:"Session closed successfully" storage env
              req))

  let account_page req server env =
    with_user_cookie_page req server (fun user cookie ->
        dashboard_page ~page_title:"Account" user
          ~content:
            (User_account.user_account_layout user user.cookies
               ~active_cookie_value:cookie.value (now ()))
          req server env)

  let account_usage req server env =
    with_user_cookie_page req server (fun user _cookie ->
        with_instance ~endpoint:"/usage" req server (fun albatross_inst ->
            let blocks =
              Albatross_client.user_blocks_by_instance env.tcp albatross_inst
                user.name
            in
            let unikernels =
              Albatross_client.user_unikernels_by_instance env.tcp
                albatross_inst user.name
            in
            let policy =
              Option.value ~default:Albatross_client.empty_policy
                (Albatross_client.policy albatross_inst ~domain:user.name)
            in
            dashboard_page ~page_title:"Usage" user
              ~content:
                (Account_usage.account_usage_layout
                   albatross_inst.configuration.name policy unikernels blocks)
              req server env))
end

module Email = struct
  let with_valid_email_config (form : Models.Email.form_t) req f =
    match Models.Email.validate form with
    | Ok email_settings -> f email_settings
    | Error err -> api_response ~title:"Bad Request" ~data:err `Bad_request req

  let email_settings req server env =
    with_admin_cookie_page req server (fun user _cookie ->
        let store = Vifu.Server.device storage server in
        dashboard_page ~page_title:"Email Settings" user
          ~content:
            (Settings_page.settings_layout ~active_tab:Email
               (Email_config.email_config_layout (Storage.email store)))
          req server env)

  let update_email_configuration req server env =
    with_admin_cookie_api req server (fun _user _cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Email.form_t) -> f.molly_csrf)
          (fun form ->
            let store = Vifu.Server.device storage server in
            with_valid_email_config form req (fun email_settings ->
                Storage.store_email store (Some email_settings);
                write_data ~error_title:"Internal Server Error"
                  ~on_error:(Fmt.str "Save failed: %s")
                  ~success_msg:"Email settings verified and saved successfully"
                  store env req)))

  let test_email_configuration req server env =
    with_admin_cookie_api req server (fun _user _cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Email.form_t) -> f.molly_csrf)
          (fun form ->
            let he = Vifu.Server.device happy_eyeballs server in
            with_valid_email_config form req (fun email_settings ->
                let recipient =
                  Option.value ~default:email_settings.from_email
                    email_settings.to_email
                in
                match
                  Email_client.test_email_configuration he email_settings
                    recipient
                with
                | Ok () ->
                    api_response ~title:"Success"
                      ~data:"Test email sent successfully" `OK req
                | Error err ->
                    api_response ~title:"Bad Request"
                      ~data:(Fmt.str "Test failed: %s" err)
                      `Bad_request req)))

  let verify_email req server env =
    with_user_cookie_page req server (fun user _cookie ->
        let store = Vifu.Server.device storage server in
        let csrf = generate_csrf_token env req in
        let email_verification_uuid = Models.User.generate_uuid () in
        let updated_user =
          Models.User.update_user user ~updated_at:(now ())
            ~email_verification_uuid:(Some email_verification_uuid) ()
        in
        Storage.update_user store updated_user;
        match Store.write_data store env.block with
        | Ok () ->
            let verification_link =
              Models.Email.generate_verification_link email_verification_uuid
            in
            Logs.info (fun m -> m "Verification link is: %s" verification_link);
            page_response
              ~headers:[ ("X-MOLLY-CSRF", csrf) ]
              (Verify_email.verify_page csrf updated_user)
              req server env
        | Error (`Msg err) ->
            error_page_response ~title:"Internal Server Error" ~data:err
              `Internal_server_error req)

  let verify_email_token req server env =
    match query_param req "token" with
    | Error err ->
        error_page_response ~title:"Bad Request" ~data:err `Bad_request req
    | Ok verification_token ->
        with_user_cookie_page req server (fun user _cookie ->
            let store = Vifu.Server.device storage server in
            match
              let ( let* ) = Result.bind in
              let* uuid =
                Option.to_result ~none:(`Msg "invalid UUID")
                  (Uuidm.of_string verification_token)
              in
              let u = Storage.find_email_verification_token store uuid in
              Models.User.verify_email_token u verification_token (now ())
            with
            | Ok user' ->
                if Uuidm.equal user.uuid user'.uuid then (
                  Storage.update_user store user';
                  match Store.write_data store env.block with
                  | Ok () -> redirect_to_page ~path:"/dashboard" req ()
                  | Error (`Msg msg) ->
                      api_response ~title:"Internal Server Error" ~data:msg
                        `Internal_server_error req)
                else
                  api_response ~title:"Bad Request"
                    ~data:"Logged in user is not the to-be-verified one"
                    `Bad_request req
            | Error (`Msg s) ->
                redirect_to_page ~path:"/sign-in" ~clear_session:true
                  ~with_error:true ~msg:s req ())
end

module Block = struct
  let blocks req server env =
    with_user_cookie_page req server (fun user _cookie ->
        with_instance ~endpoint:"/blocks" req server (fun albatross_inst ->
            let blocks =
              Albatross_client.user_blocks_by_instance env.tcp albatross_inst
                user.name
            in
            let policy =
              Albatross_client.policy ~domain:user.name albatross_inst
            in
            dashboard_page
              ~page_title:
                (String.capitalize_ascii
                   (Models.Configuration.name_to_str user.name))
              user
              ~content:
                (Block_index.block_index_layout
                   albatross_inst.configuration.name blocks policy)
              req server env))

  let delete_block req server env =
    with_user_cookie_api req server (fun user _cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Block.delete_t) -> f.molly_csrf)
          (fun ({ block_name; albatross_instance; _ } : Models.Block.delete_t)
             ->
            with_api_instance_and_name ~instance_str:albatross_instance
              ~name_str:block_name req server (fun albatross_inst block_name ->
                query_albatross_api env.tcp albatross_inst ~domain:user.name
                  ~name:block_name (`Block_cmd `Block_remove) req)))

  let create_or_upload_block c_or_u req server env =
    with_user_cookie_api req server (fun user _cookie ->
        let cmd_name =
          match c_or_u with `Create -> "create" | `Upload -> "upload"
        in
        let err_resp msg code =
          Logs.warn (fun m -> m "Block %s error: %s" cmd_name msg);
          Error (api_response ~title:"Error" ~data:msg code req)
        in
        let*? {
                albatross_instance;
                molly_csrf = csrf;
                json_data = json;
                block_data;
              } =
          match Vifu.Request.of_multipart_form req with
          | Ok form -> Ok (form : Models.Block.upload_t)
          | Error (`Not_found _) ->
              Logs.info (fun m -> m "Missing Fields");
              err_resp "One or more required fields are missing." `Bad_request
          | Error `Invalid_multipart_form ->
              err_resp "Couldn't decode multipart data" `Bad_request
        in
        with_api_instance ~instance_str:albatross_instance req server
          (fun albatross_inst ->
            let*? { block_name; block_size; block_compressed } =
              match
                Jsont_bytesrw.decode_string Models.Block.block_json_form json
              with
              | Ok v -> Ok v
              | Error err ->
                  err_resp (Fmt.str "unexpected field. got %s" err) `Bad_request
            in
            let*? block_name =
              match Models.Configuration.name_of_str block_name with
              | Ok name -> Ok name
              | Error (`Msg err) ->
                  err_resp (Fmt.str "Invalid block name: %s" err) `Bad_request
            in
            let*? () =
              if verify_csrf env req csrf then Ok ()
              else
                Error
                  (api_response ~title:"CSRF Token Mismatch"
                     ~data:
                       "CSRF token mismatch error. Please refresh and try \
                        again."
                     `Bad_request req)
            in
            let*? () =
              match (c_or_u, block_size) with
              | `Upload, _ -> Ok ()
              | `Create, None ->
                  err_resp
                    (Fmt.str "unexpected field. got %s" json)
                    `Bad_request
              | `Create, Some block_size -> (
                  match
                    Albatross_client.query env.tcp albatross_inst
                      ~domain:user.name ~name:block_name
                      (`Block_cmd (`Block_add block_size))
                  with
                  | Error err ->
                      err_resp
                        (Fmt.str "an error with albatross. got %s" err)
                        `Internal_server_error
                  | Ok (_hdr, res) -> (
                      match Albatross_json.res res with
                      | Error (`String err) ->
                          err_resp
                            (Fmt.str "unexpected field. got %s" err)
                            `Bad_request
                      | Ok _ -> Ok ()))
            in
            let push = make_string_stream block_data in
            query_albatross_api ~parse_error_status:`Bad_request ~push env.tcp
              albatross_inst ~domain:user.name ~name:block_name
              (`Block_cmd (`Block_set block_compressed))
              req))

  let create_block req server env =
    create_or_upload_block `Create req server env

  let upload_block req server env =
    create_or_upload_block `Upload req server env

  let download_block req server env =
    with_user_cookie_api req server (fun user _cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Block.download_t) -> f.molly_csrf)
          (fun ({ albatross_instance; block_name; compression_level; _ } :
                 Models.Block.download_t)
             ->
            with_api_instance_and_name ~instance_str:albatross_instance
              ~name_str:block_name req server
              (fun albatross_inst block_name_obj ->
                let filename = block_name ^ "_dump" in
                let disposition = "attachment; filename=\"" ^ filename ^ "\"" in
                let src =
                  Flux.Source.with_task ~size:0x7ff (fun q ->
                      Fun.protect
                        ~finally:(fun () -> Flux.Bqueue.close q)
                        (fun () ->
                          let cb = function
                            | None -> Ok ()
                            | Some data ->
                                Flux.Bqueue.put q data;
                                Ok ()
                          in
                          let _ =
                            Albatross_client.query_block_dump env.tcp
                              albatross_inst ~domain:user.name
                              ~name:block_name_obj compression_level cb
                          in
                          ()))
                in
                let open Vifu.Response.Syntax in
                let* () =
                  Vifu.Response.add ~field:"Content-type"
                    "application/octet-stream"
                in
                let* () =
                  Vifu.Response.add ~field:"Content-Disposition" disposition
                in
                let* _ =
                  Vifu.Response.add_unless_exists ~field:"connection" "close"
                in
                let* () = Vifu.Response.with_source req src in
                Vifu.Response.respond `OK)))
end

module Unikernel = struct
  let dashboard req server env =
    with_user_cookie_page req server (fun user _cookie ->
        let albatross_instances = !(Vifu.Server.device albatross server) in
        let unikernels_by_albatross_instance =
          Albatross_client.user_unikernels env.tcp albatross_instances user.name
        in
        let deceased_unikernels_by_albatross_instance =
          Albatross_client.user_deceased_by_instance env.tcp albatross_instances
            user.name
        in
        dashboard_page user
          ~content:
            (Unikernel_index.unikernel_index_layout
               unikernels_by_albatross_instance
               deceased_unikernels_by_albatross_instance (now ()))
          req server env)

  let deploy_form req server env =
    with_user_cookie_page req server (fun user _cookie ->
        with_instance ~endpoint:"/unikernel/deploy" req server
          (fun albatross_inst ->
            let he = Vifu.Server.device happy_eyeballs server in
            let builder_jobs = Builder_web.fetch_unikernel_jobs he in
            let unikernels_by_albatross_instance =
              Albatross_client.user_unikernels_by_instance env.tcp
                albatross_inst user.name
            in
            let blocks_by_albatross_instance =
              Albatross_client.user_blocks_by_instance env.tcp albatross_inst
                user.name
            in
            match Albatross_client.policy albatross_inst ~domain:user.name with
            | Some user_policy ->
                dashboard_page ~page_title:"Deploy a Unikernel" user
                  ~content:
                    (Unikernel_create.unikernel_create_layout ~user_policy
                       unikernels_by_albatross_instance
                       blocks_by_albatross_instance
                       albatross_inst.configuration.name builder_jobs)
                  req server env
            | None ->
                error_page_response ~title:"Resource Policy error"
                  ~data:"No user policy" `Bad_request req))

  let unikernel_info req server env =
    with_user_cookie_api req server (fun user _cookie ->
        let albatross_instances = !(Vifu.Server.device albatross server) in
        let successes, failures =
          Utils.LM.fold
            (fun name (instance : Albatross_client.t) (successes, failures) ->
              match
                Albatross_client.query env.tcp instance ~domain:user.name
                  (`Unikernel_cmd `Unikernel_info)
              with
              | Error msg ->
                  let failure : Models.Unikernel.instance_error =
                    {
                      instance = Models.Configuration.name_to_str name;
                      error = msg;
                    }
                  in
                  (successes, failure :: failures)
              | Ok (_hdr, res) -> (
                  match Albatross_json.res res with
                  | Error (`String err) ->
                      Logs.err (fun m ->
                          m "Error parsing response from albatross '%s': %s"
                            (Models.Configuration.name_to_str name)
                            err);
                      let failure : Models.Unikernel.instance_error =
                        {
                          instance = Models.Configuration.name_to_str name;
                          error = err;
                        }
                      in
                      (successes, failure :: failures)
                  | Ok parsed -> (parsed :: successes, failures)))
            albatross_instances ([], [])
        in
        let response_data : Models.Unikernel.info_response =
          { data = successes; errors = failures }
        in
        let data =
          Result.get_ok
            (Jsont_bytesrw.encode_string Models.Unikernel.info_response_json
               response_data)
        in
        api_response ~title:"Unikernel Information" ~data `OK req)

  let unikernel_info_one req server env =
    with_user_cookie_page req server (fun user _cookie ->
        with_unikernel_and_instance req server
          (fun albatross_inst unikernel_name ->
            match
              Albatross_client.user_unikernel env.tcp albatross_inst
                ~user_name:user.name ~unikernel_name
            with
            | Error err ->
                let data =
                  "Couldn't find unikernel "
                  ^ Models.Configuration.name_to_str unikernel_name
                  ^ " on albatross instance "
                  ^ Models.Configuration.name_to_str
                      albatross_inst.configuration.name
                  ^ ": " ^ err
                in
                error_page_response ~title:"Unikernel Not Found" ~data
                  `Not_found req
            | Ok unikernel ->
                let last_update_time =
                  Option.map
                    (fun (u : Models.User.unikernel_update) -> u.timestamp)
                    (Utils.LM.find_opt unikernel_name user.unikernel_updates)
                in
                let scaling_policy =
                  Models.User.Scaling_policy_map.find_opt
                    (unikernel_name, albatross_inst.configuration.name)
                    user.scaling_policies
                in
                let max_allowed =
                  Albatross_client.user_max_allowed_unikernel_instances
                    albatross_inst user.name
                in
                dashboard_page user
                  ~content:
                    (Unikernel_single.unikernel_single_layout ~unikernel_name
                       ~instance_name:albatross_inst.configuration.name
                       ~max_allowed unikernel ~last_update_time
                       ~current_time:(now ()) scaling_policy)
                  req server env))

  let unikernel_console req server env =
    with_user_cookie_api req server (fun user _cookie ->
        with_unikernel_and_instance req server
          (fun albatross_inst unikernel_name ->
            let src =
              Flux.Source.with_task ~size:0x7ff (fun q ->
                  Fun.protect
                    ~finally:(fun () -> Flux.Bqueue.close q)
                    (fun () ->
                      let cb (ts, data) =
                        let json_str =
                          Albatross_json.console_data_to_string (ts, data)
                        in
                        Flux.Bqueue.put q ("data:" ^ json_str ^ "\n\n");
                        Ok ()
                      in
                      let _ =
                        Albatross_client.query_console env.tcp albatross_inst
                          ~domain:user.name ~name:unikernel_name cb
                      in
                      ()))
            in
            let open Vifu.Response.Syntax in
            let* () =
              Vifu.Response.add ~field:"Content-Type" "text/event-stream"
            in
            let* _ =
              Vifu.Response.add_unless_exists ~field:"connection" "close"
            in
            let* () = Vifu.Response.with_source req src in
            Vifu.Response.respond `OK))

  let unikernel_console_viewer req server env =
    with_user_cookie_page req server (fun _user _cookie ->
        with_unikernel_and_instance req server
          (fun _albatross_inst unikernel_name ->
            page_response
              (Console_viewer.console_viewer_layout
                 (Models.Configuration.name_to_str unikernel_name))
              req server env))

  let unikernel_destroy req server env =
    with_user_cookie_api req server (fun user _cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Unikernel.destroy_t) -> f.molly_csrf)
          (fun (form : Models.Unikernel.destroy_t) ->
            with_api_instance_and_name ~instance_str:form.albatross_instance
              ~name_str:form.name req server
              (fun albatross_inst unikernel_name ->
                query_albatross_api env.tcp albatross_inst ~domain:user.name
                  ~name:unikernel_name (`Unikernel_cmd `Unikernel_destroy) req)))

  let unikernel_restart req server env =
    with_user_cookie_api req server (fun user _cookie ->
        with_unikernel_and_instance req server
          (fun albatross_inst unikernel_name ->
            with_json_csrf req env
              ~csrf_of:(fun (f : Models.Unikernel.restart_t) -> f.molly_csrf)
              (fun (form : Models.Unikernel.restart_t) ->
                let restart ?arguments () =
                  query_albatross_api env.tcp albatross_inst ~domain:user.name
                    ~name:unikernel_name
                    (`Unikernel_cmd (`Unikernel_restart arguments))
                    req
                in
                match form.arguments with
                | Some arguments -> (
                    match Albatross_json.arguments_of_json arguments with
                    | Error (`Msg err) ->
                        api_response ~title:"Bad Request"
                          ~data:("Error parsing arguments: " ^ err)
                          `Bad_request req
                    | Ok arguments -> restart ~arguments ())
                | None -> restart ())))

  let fetch_binary_stream he ~job ~version =
    let buf = Buffer.create 65536 in
    let push_chunks = function
      | Some chunk -> Buffer.add_string buf chunk
      | None -> ()
    in
    Builder_web.fetch_unikernel_binary_image he ~job ~version push_chunks
    |> Result.map (fun () -> make_string_stream (Buffer.contents buf))

  let unikernel_create req server env =
    with_user_cookie_api req server (fun user _cookie ->
        let err_resp msg code =
          Logs.warn (fun m -> m "Unikernel_create error: %s" msg);
          Error (api_response ~title:"Error" ~data:msg code req)
        in
        let*? stream =
          match Vifu.Request.of_multipart_form req with
          | Ok stream -> Ok stream
          | Error (`Msg msg) -> err_resp msg `Bad_request
          | Error (`Not_found field) ->
              err_resp ("Missing field: " ^ field) `Bad_request
          | Error `Invalid_multipart_form ->
              err_resp "Invalid multipart form" `Bad_request
        in
        let he = Vifu.Server.device happy_eyeballs server in
        let*? form =
          match
            Models.Unikernel.create_of_parts
              (Models.Multipart.parse_stream stream)
          with
          | Ok form -> Ok form
          | Error msg ->
              Logs.info (fun m -> m "Missing Fields");
              err_resp msg `Bad_request
        in
        with_api_instance_and_name ~instance_str:form.albatross_instance
          ~name_str:form.unikernel_name req server
          (fun albatross_inst unikernel_name ->
            let*? () =
              if verify_csrf env req form.molly_csrf then Ok ()
              else
                Error
                  (api_response ~title:"CSRF Token Mismatch"
                     ~data:
                       "CSRF token mismatch error. Please refresh and try \
                        again."
                     `Bad_request req)
            in
            let*? cfg =
              match Albatross_json.config_of_json form.unikernel_config with
              | Ok cfg -> Ok cfg
              | Error (`Msg err) ->
                  err_resp ("Invalid unikernel arguments: " ^ err) `Bad_request
            in
            let*? push =
              match (form.deploy_mode, form.builder_job, form.binary) with
              | "builder", Some job, _ -> (
                  match fetch_binary_stream he ~job ~version:"latest" with
                  | Ok push -> Ok push
                  | Error (`Msg err) ->
                      err_resp ("Builder Error: " ^ err) `Internal_server_error)
              | "builder", None, _ ->
                  err_resp "Missing builder job or uuid" `Bad_request
              | "manual", _, Some binary_data ->
                  Ok (make_string_stream binary_data)
              | "manual", _, None ->
                  err_resp "One or more required fields are missing."
                    `Bad_request
              | _ -> err_resp "Invalid deploy mode" `Bad_request
            in
            let albatross_cmd =
              if bool_of_string_opt form.unikernel_force_create = Some true then
                `Unikernel_cmd (`Unikernel_force_create cfg)
              else `Unikernel_cmd (`Unikernel_create cfg)
            in
            query_albatross_api ~push env.tcp albatross_inst ~domain:user.name
              ~name:unikernel_name albatross_cmd req))

  let unikernel_prepare_update req server env =
    with_user_cookie_page req server (fun user _cookie ->
        with_unikernel_and_instance req server
          (fun albatross_inst unikernel_name ->
            let he = Vifu.Server.device happy_eyeballs server in
            let open Update_flow in
            let pipeline =
              let ( let* ) = Result.bind in
              let* name, unikernel =
                Albatross_client.user_unikernel env.tcp albatross_inst
                  ~user_name:user.name ~unikernel_name
                |> Result.map_error (fun e -> Albatross_err e)
              in
              check_for_update name unikernel he
            in
            match pipeline with
            | Ok (Update_available (name, unikernel, build_comparison)) ->
                dashboard_page
                  ~page_title:(Vmm_core.Name.to_string name ^ " Update")
                  user
                  ~content:
                    (Unikernel_update.unikernel_update_layout
                       ~instance_name:albatross_inst.configuration.name
                       ~unikernel_name:
                         (Models.Configuration.name_to_str unikernel_name)
                       (name, unikernel) (now ()) build_comparison)
                  req server env
            | Ok (No_update_needed (name, uuid)) ->
                Logs.info (fun m ->
                    m "No update found for %s (uuid %s)"
                      (Models.Configuration.name_to_str unikernel_name)
                      uuid);
                let instance =
                  Models.Configuration.name_to_str
                    albatross_inst.configuration.name
                in
                let label =
                  Option.value ~default:""
                    (Option.map Vmm_core.Name.Label.to_string
                       (Vmm_core.Name.name name))
                in
                redirect_to_page
                  ~path:
                    (Printf.sprintf "/unikernel/info?unikernel=%s&instance=%s"
                       label instance)
                  ~msg:
                    ("There is no update of "
                    ^ Models.Configuration.name_to_str unikernel_name
                    ^ " found on builds.robur.coop")
                  req ()
            | Error (Builder_not_found _ as err) ->
                let log_msg, web_msg =
                  error_response_params
                    (Models.Configuration.name_to_str unikernel_name)
                    err
                in
                Logs.warn (fun m -> m "%s" log_msg);
                let instance =
                  Models.Configuration.name_to_str
                    albatross_inst.configuration.name
                in
                let label = Models.Configuration.name_to_str unikernel_name in
                redirect_to_page
                  ~path:
                    (Printf.sprintf "/unikernel/info?unikernel=%s&instance=%s"
                       label instance)
                  ~msg:web_msg req ()
            | Error err ->
                let log_msg, web_msg =
                  error_response_params
                    (Models.Configuration.name_to_str unikernel_name)
                    err
                in
                Logs.err (fun m -> m "%s" log_msg);
                error_page_response
                  ~title:
                    (Models.Configuration.name_to_str unikernel_name
                    ^ " update Error")
                  ~data:web_msg `Internal_server_error req))

  let force_create_unikernel tcp albatross_inst ~unikernel_name ~push
      (unikernel_cfg : Vmm_core.Unikernel.config) (user : Models.User.user) =
    match
      Albatross_client.query tcp albatross_inst ~domain:user.name
        ~name:unikernel_name ~push
        (`Unikernel_cmd (`Unikernel_force_create unikernel_cfg))
    with
    | Error err ->
        Error
          ( `Msg ("Force create: Error querying albatross: " ^ err),
            `Internal_server_error )
    | Ok (_hdr, res) -> (
        let unikernel_name_str =
          Models.Configuration.name_to_str unikernel_name
        in
        match Albatross_json.res_to_string res with
        | Error (`String err) ->
            Logs.warn (fun m ->
                m
                  "albatross-force-create: albatross_json.res error parsing \
                   albatross response for creating %s: %s"
                  unikernel_name_str err);
            Error (`Msg err, `Internal_server_error)
        | Ok res ->
            Logs.info (fun m ->
                m
                  "albatross-force-create: successfully created %s with result \
                   %s"
                  unikernel_name_str res);
            Ok ())

  let process_change env ~unikernel_name ~job ~to_be_updated_unikernel
      ~currently_running_unikernel (unikernel_cfg : Vmm_core.Unikernel.config)
      (user : Models.User.user) store he (change_kind : [ `Rollback | `Update ])
      albatross_inst =
    let change_string = function
      | `Rollback -> "rollback"
      | `Update -> "update"
    in
    let unikernel_name_str = Models.Configuration.name_to_str unikernel_name in
    let unikernel_update : Models.User.unikernel_update =
      {
        name = unikernel_name;
        job;
        uuid = currently_running_unikernel;
        config = unikernel_cfg;
        timestamp = now ();
      }
    in
    Storage.update_user store
      (Storage.update_user_unikernel_updates unikernel_update user);
    match Store.write_data store env.block with
    | Error (`Msg err) ->
        let data =
          Result.value ~default:""
            (Jsont_bytesrw.encode_string Models.User.unikernel_update_json
               unikernel_update)
        in
        Logs.warn (fun m ->
            m
              "%s: storing update information for %s failed with: %s for data \
               %s"
              (change_string change_kind)
              unikernel_name_str err data);
        Error
          ( `Msg
              (change_string change_kind ^ " storing update information for "
             ^ unikernel_name_str ^ " failed with: " ^ err ^ " for data: "
             ^ data),
            `Internal_server_error )
    | Ok () -> (
        match
          fetch_binary_stream he ~job
            ~version:(Uuidm.to_string to_be_updated_unikernel)
        with
        | Error (`Msg err) ->
            Logs.err (fun m ->
                m
                  "%s: builds.robur.coop: Error while fetching the binary of \
                   %s with error: %s"
                  (change_string change_kind)
                  unikernel_name_str err);
            Error
              ( `Msg
                  (change_string change_kind
                 ^ " :an error occured while fetching the binary from \
                    builds.robur.coop with error: " ^ err),
                `Internal_server_error )
        | Ok push ->
            force_create_unikernel env.tcp albatross_inst ~unikernel_name ~push
              unikernel_cfg user)

  let process_rollback env albatross_inst ~unikernel_name current_time store he
      req (user : Models.User.user) =
    let unikernel_name_str = Models.Configuration.name_to_str unikernel_name in
    match Utils.LM.find_opt unikernel_name user.unikernel_updates with
    | Some old_unikernel ->
        if
          Utils.TimeHelper.diff_in_seconds ~current_time
            ~check_time:old_unikernel.timestamp
          < Utils.rollback_seconds_limit
        then
          match
            process_change env ~unikernel_name ~job:old_unikernel.job
              ~to_be_updated_unikernel:old_unikernel.uuid
              ~currently_running_unikernel:old_unikernel.uuid
              old_unikernel.config user store he `Rollback albatross_inst
          with
          | Ok () ->
              let updated_unikernel_updates =
                Utils.LM.remove unikernel_name user.unikernel_updates
              in
              let user =
                Models.User.update_user user
                  ~unikernel_updates:updated_unikernel_updates ()
              in
              Storage.update_user store user;
              let _ = Store.write_data store env.block in
              api_response ~title:"Rollback Successful"
                ~data:
                  ("Rollback successful. " ^ unikernel_name_str
                 ^ " is now running on build "
                  ^ Uuidm.to_string old_unikernel.uuid)
                `OK req
          | Error (`Msg err, http_status) ->
              api_response ~title:"Rollback Error"
                ~data:
                  ("Rollback failed. " ^ unikernel_name_str
                 ^ " failed to revert to build "
                  ^ Uuidm.to_string old_unikernel.uuid
                  ^ " with error " ^ err)
                http_status req
        else
          api_response ~title:"Rollback Failed"
            ~data:
              (unikernel_name_str
             ^ " rollback failed. We can't do a rollback after 10 minutes of \
                an update.")
            `Bad_request req
    | None ->
        api_response ~title:"Rollback Error"
          ~data:
            (unikernel_name_str
           ^ " rollback failed. Could not find the build information.")
          `Not_found req

  let process_unikernel_update ~unikernel_name ~job ~to_be_updated_unikernel
      ~currently_running_unikernel ~http_liveliness_address ~dns_liveliness env
      cfg user store he albatross_inst req =
    let to_be_updated_unikernel_str = Uuidm.to_string to_be_updated_unikernel in
    match
      process_change env ~unikernel_name ~job ~to_be_updated_unikernel
        ~currently_running_unikernel cfg user store he `Update albatross_inst
    with
    | Ok () -> (
        let unikernel_name_str =
          Models.Configuration.name_to_str unikernel_name
        in
        match
          Liveliness_checks.interval_liveliness_checks ~unikernel_name
            ~http_liveliness_address ~dns_liveliness ~udp:env.udp
            ~happy_eyeballs:he
        with
        | Error (`Msg err) ->
            Logs.err (fun m ->
                m
                  "liveliness-checks for %s and build %s failed with error(s) \
                   %s. now performing a rollback"
                  unikernel_name_str to_be_updated_unikernel_str err);
            process_rollback env albatross_inst ~unikernel_name (now ()) store
              he req user
        | Ok () ->
            api_response ~title:"Update Successful"
              ~data:
                ("Update successful. " ^ unikernel_name_str
               ^ " is now running on build " ^ to_be_updated_unikernel_str)
              `OK req)
    | Error (`Msg err, http_status) ->
        api_response ~title:"Update Error"
          ~data:
            ("Update failed. "
            ^ Models.Configuration.name_to_str unikernel_name
            ^ " failed to update to build " ^ to_be_updated_unikernel_str
            ^ " with error " ^ err)
          http_status req

  let unikernel_update req server env =
    with_user_cookie_api req server (fun user _cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Unikernel.update_t) -> f.molly_csrf)
          (fun (form : Models.Unikernel.update_t) ->
            let store = Vifu.Server.device storage server in
            let he = Vifu.Server.device happy_eyeballs server in
            let http_liveliness_address = form.http_liveliness_address in
            let dns_liveliness = form.dns_liveliness in
            let*? () =
              Liveliness_checks.liveliness_checks ~http_liveliness_address
                ~dns_liveliness ~udp:env.udp ~happy_eyeballs:he
              |> Result.map_error (fun (`Msg err) ->
                  Logs.info (fun m ->
                      m
                        "Liveliness check of currentyly running unikernel \
                         failed with: %s"
                        err);
                  api_response ~title:"Error: Liveliness check failed"
                    ~data:
                      ("Liveliness check of currentyly running unikernel \
                        failed with error: " ^ err)
                    `Internal_server_error req)
            in
            with_api_instance_and_name ~instance_str:form.albatross_instance
              ~name_str:form.unikernel_name req server
              (fun albatross_inst unikernel_name ->
                let*? cfg_opt =
                  Models.Unikernel.config_or_none "unikernel_arguments"
                    form.unikernel_arguments
                  |> Result.map_error (fun (`Msg err) ->
                      api_response ~title:"Error with Unikernel Arguments Json"
                        ~data:
                          ("Could not get the unikernel arguments json: " ^ err)
                        `Bad_request req)
                in
                let*? to_be_updated_unikernel =
                  Option.to_result
                    ~none:
                      (api_response
                         ~title:"Error: Bad to-be-updated unikernel UUID"
                         ~data:
                           "Couldn't convert the to-be-updated unikernel build \
                            UUID"
                         `Bad_request req)
                    (Uuidm.of_string form.to_be_updated_unikernel)
                in
                let*? currently_running_unikernel =
                  Option.to_result
                    ~none:
                      (api_response
                         ~title:"Error: Bad currently running unikernel UUID"
                         ~data:
                           "Couldn't convert the currently running unikernel \
                            build UUID"
                         `Bad_request req)
                    (Uuidm.of_string form.currently_running_unikernel)
                in
                let*? cfg =
                  match cfg_opt with
                  | Some cfg -> Ok cfg
                  | None ->
                      Albatross_client.user_unikernel env.tcp albatross_inst
                        ~user_name:user.name ~unikernel_name
                      |> Result.map (fun (_n, info) ->
                          Models.Unikernel.config_of_info info)
                      |> Result.map_error (fun err ->
                          api_response ~title:"Bad Request"
                            ~data:
                              ("Couldn't find albatross instance, "
                              ^ Models.Configuration.name_to_str
                                  albatross_inst.configuration.name
                              ^ " with error: " ^ err)
                            `Bad_request req)
                in
                process_unikernel_update ~unikernel_name ~job:form.job
                  ~to_be_updated_unikernel ~currently_running_unikernel
                  ~http_liveliness_address ~dns_liveliness env cfg user store he
                  albatross_inst req)))

  let unikernel_rollback req server env =
    with_user_cookie_api req server (fun user _cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Unikernel.rollback_t) -> f.molly_csrf)
          (fun (form : Models.Unikernel.rollback_t) ->
            let store = Vifu.Server.device storage server in
            let he = Vifu.Server.device happy_eyeballs server in
            with_api_instance_and_name ~instance_str:form.albatross_instance
              ~name_str:form.unikernel_name req server
              (fun albatross_inst unikernel_name ->
                process_rollback env albatross_inst ~unikernel_name (now ())
                  store he req user)))

  let unikernel_scaling_policy_update req server env =
    with_user_cookie_api req server (fun user _cookie ->
        with_unikernel_and_instance req server
          (fun albatross_inst unikernel_name ->
            with_json_csrf req env
              ~csrf_of:(fun (f : Models.Unikernel.scaling_t) -> f.molly_csrf)
              (fun (form : Models.Unikernel.scaling_t) ->
                let store = Vifu.Server.device storage server in
                let policy_key =
                  (unikernel_name, albatross_inst.configuration.name)
                in
                let current_scaling_policy =
                  Models.User.Scaling_policy_map.find_opt policy_key
                    user.scaling_policies
                in
                let update_unikernel_scaling scaling_policies =
                  let user =
                    Models.User.update_user user ~scaling_policies ()
                  in
                  Storage.update_user store user;
                  write_data ~error_title:"Internal Server Error"
                    ~on_error:(Fmt.str "Failed to save scaling policy: %s")
                    ~success_msg:
                      "Unikernel scaling policy updated successfully."
                    store env req
                in
                match (form.should_scale, form.max_instances) with
                | true, Some max_instances ->
                    if max_instances >= 1 then
                      if max_instances = 1 then
                        let scaling_policies =
                          Models.User.Scaling_policy_map.remove policy_key
                            user.scaling_policies
                        in
                        update_unikernel_scaling scaling_policies
                      else
                        let max_allowed =
                          Albatross_client.user_max_allowed_unikernel_instances
                            albatross_inst user.name
                        in
                        if max_instances > max_allowed then
                          api_response ~title:"Bad Request"
                            ~data:
                              (Fmt.str
                                 "Error: you can spawn a maximum of %d clones."
                                 max_allowed)
                            `Bad_request req
                        else
                          let new_policy =
                            match current_scaling_policy with
                            | Some p -> { p with max_instances }
                            | None ->
                                {
                                  Models.User.name = unikernel_name;
                                  primary_albatross_instance =
                                    albatross_inst.configuration.name;
                                  max_instances;
                                }
                          in
                          let scaling_policies =
                            Models.User.Scaling_policy_map.add policy_key
                              new_policy user.scaling_policies
                          in
                          update_unikernel_scaling scaling_policies
                    else
                      api_response ~title:"Bad Request"
                        ~data:"Max instances must be greater than 1."
                        `Bad_request req
                | false, _ -> (
                    match current_scaling_policy with
                    | None ->
                        api_response ~title:"Success"
                          ~data:
                            "No scaling policy exist for this unikernel. \
                             Nothing to do here."
                          `OK req
                    | Some _ ->
                        let scaling_policies =
                          Models.User.Scaling_policy_map.remove policy_key
                            user.scaling_policies
                        in
                        update_unikernel_scaling scaling_policies)
                | true, None ->
                    api_response ~title:"Bad Request"
                      ~data:"Missing max number of clones to spawn."
                      `Bad_request req)))

  let unikernel_monitoring_status req server env =
    with_user_cookie_api req server (fun _user _cookie ->
        with_unikernel req (fun unikernel_name ->
            let he = Vifu.Server.device management_happy_eyeballs server in
            let target = Models.Configuration.name_to_str unikernel_name in
            let logs =
              Monitoring.send_tcp_command he ~command:"l*" ~target env.m_domain
            in
            let metrics =
              Monitoring.send_tcp_command he ~command:"m*" ~target env.m_domain
            in
            let status =
              match (logs, metrics) with
              | Ok _, Ok _ -> `OK
              | Error _, _ | _, Error _ -> `Bad_request
            in
            let html =
              Monitoring.monitoring_status_html ~name:target ~logs ~metrics
            in
            let body = Format.asprintf "%a" (Tyxml_html.pp_elt ()) html in
            let open Vifu.Response.Syntax in
            let* () = Vifu.Response.with_text req body in
            Vifu.Response.respond status))

  let unikernel_monitoring_update req server env =
    with_user_cookie_api req server (fun _user _cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Unikernel.monitoring_t) -> f.molly_csrf)
          (fun (form : Models.Unikernel.monitoring_t) ->
            let he = Vifu.Server.device management_happy_eyeballs server in
            match (form.command, form.unikernel_name) with
            | Some command_str, Some unikernel_name -> (
                match Monitoring.check_command command_str with
                | Ok command -> (
                    match
                      Monitoring.send_tcp_command he ~command
                        ~target:unikernel_name env.m_domain
                    with
                    | Error err ->
                        api_response ~title:"Bad Request"
                          ~data:("An error occured: " ^ err)
                          `Bad_request req
                    | Ok response ->
                        api_response ~title:"Success"
                          ~data:("Updated succesfully: " ^ response)
                          `OK req)
                | Error err ->
                    let msg =
                      Fmt.str
                        "Command is malformated. Command: %s, Received error: \
                         %s"
                        command_str err
                    in
                    Logs.info (fun m -> m "%s" msg);
                    api_response ~title:"Bad Request" ~data:msg `Bad_request req
                )
            | _ ->
                api_response ~title:"Bad Request"
                  ~data:"Missing unikernel name or command in request."
                  `Bad_request req))
end

module Albatross_admin = struct
  let instances_redirect req _server _env =
    redirect_to_instance_selector "/dashboard" req ()

  let choose_instance req server env =
    with_user_cookie_page req server (fun user _cookie ->
        let albatross_instances = !(Vifu.Server.device albatross server) in
        let callback =
          match query_param req "callback" with
          | Ok link -> link
          | Error _ -> "/dashboard"
        in
        if Utils.LM.cardinal albatross_instances = 1 then
          let instance_name, _ = Utils.LM.min_binding albatross_instances in
          redirect_to_page
            ~path:(construct_instance_redirect_url callback instance_name)
            req ()
        else
          dashboard_page ~page_title:"Choose instance" user
            ~content:
              (Albatross_instances.select_instance user albatross_instances
                 callback)
            req server env)

  let albatross_settings req server env =
    with_admin_cookie_page req server (fun user _cookie ->
        let albatross_instances = !(Vifu.Server.device albatross server) in
        dashboard_page ~page_title:"Albatross Settings" user
          ~content:
            (Settings_page.settings_layout ~active_tab:Albatross
               (Albatross_config.albatross_config_layout albatross_instances))
          req server env)

  let view_albatross_error_logs req server env =
    with_admin_cookie_page req server (fun user _cookie ->
        with_instance req server (fun albatross_inst ->
            dashboard_page ~page_title:"Albatross Errors" user
              ~content:(Albatross_status.albatross_status_layout albatross_inst)
              req server env))

  let retry_initializing_instance req server env =
    with_admin_cookie_api req server (fun _user _cookie ->
        with_instance req server (fun albatross_inst ->
            let albatross_instances = Vifu.Server.device albatross server in
            match
              Albatross_client.init env.tcp albatross_inst.configuration
            with
            | Ok new_albatross_instance ->
                albatross_instances :=
                  Utils.LM.update albatross_inst.configuration.name
                    (fun _prev -> Some new_albatross_instance)
                    !albatross_instances;
                api_response ~title:"Success"
                  ~data:"Re-initialization successful, instance is back online"
                  `OK req
            | Error (_, err) ->
                api_response ~title:"Internal Server Error"
                  ~data:("Re-initialization failed. See error logs: " ^ err)
                  `Internal_server_error req))

  let update_or_create_albatross_configuration
      (update_or_create : [ `Update | `Create ]) req server env =
    with_admin_cookie_api req server (fun _user _cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Configuration.form_t) -> f.molly_csrf)
          (fun (form : Models.Configuration.form_t) ->
            let store = Vifu.Server.device storage server in
            let albatross_instances = Vifu.Server.device albatross server in
            let*? configuration_settings =
              Models.Configuration.to_configuration form (now ())
              |> Result.map_error (fun (`Msg err) ->
                  api_response ~title:"Bad Request" ~data:err `Bad_request req)
            in
            let*? new_albatross_instance =
              Albatross_client.init env.tcp configuration_settings
              |> Result.map_error (fun (_, err) ->
                  api_response ~title:"Internal Server Error" ~data:err
                    `Internal_server_error req)
            in
            let*? () =
              Storage.upsert_configuration store configuration_settings
                update_or_create
              |> Result.map_error (fun err ->
                  api_response ~title:"Bad Request" ~data:err `Bad_request req)
            in
            write_data ~error_title:"Internal Server Error"
              ~on_ok:(fun () ->
                albatross_instances :=
                  Utils.LM.update configuration_settings.name
                    (fun _prev -> Some new_albatross_instance)
                    !albatross_instances)
              ~on_error:(fun err ->
                Storage.delete_configuration store configuration_settings.name;
                err)
              ~success_msg:"Configuration updated successfully" store env req))

  let create_albatross_configuration req server env =
    update_or_create_albatross_configuration `Create req server env

  let update_albatross_configuration req server env =
    update_or_create_albatross_configuration `Update req server env

  let delete_albatross_config req server env =
    with_admin_cookie_api req server (fun _user _cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Configuration.delete_t) -> f.molly_csrf)
          (fun (form : Models.Configuration.delete_t) ->
            let store = Vifu.Server.device storage server in
            let albatross_instances = Vifu.Server.device albatross server in
            match Models.Configuration.name_of_str form.name with
            | Ok name ->
                Storage.delete_configuration store name;
                write_data ~error_title:"Internal Server Error"
                  ~on_ok:(fun () ->
                    albatross_instances :=
                      Utils.LM.remove name !albatross_instances)
                  ~success_msg:"Configuration delete successfully" store env req
            | Error (`Msg err) ->
                api_response ~title:"Bad Request" ~data:err `Bad_request req))
end

module Admin = struct
  let with_uuid_admin_page req server fn =
    match query_param req "uuid" with
    | Ok uuid_str -> (
        match Uuidm.of_string uuid_str with
        | Some uuid ->
            with_admin_cookie_page req server (fun user _cookie -> fn uuid user)
        | None ->
            error_page_response ~title:"Bad Request" ~data:"Invalid UUID"
              `Bad_request req)
    | Error err ->
        error_page_response ~title:"Bad Request" ~data:err `Bad_request req

  let users req server env =
    with_admin_cookie_page req server (fun user _cookie ->
        let store = Vifu.Server.device storage server in
        dashboard_page ~page_title:"Users" user
          ~content:
            (Users_index.users_index_layout (Storage.users store) (now ()))
          req server env)

  let view_user (page : [ `Profile | `Unikernels | `Policy ]) req server env =
    with_uuid_admin_page req server (fun uuid user ->
        let albatross_instances = !(Vifu.Server.device albatross server) in
        match Storage.find_by_uuid (Vifu.Server.device storage server) uuid with
        | Some u -> (
            let unikernels =
              Albatross_client.user_unikernels env.tcp albatross_instances
                u.name
            in
            let deceased_unikernels =
              Albatross_client.user_deceased_by_instance env.tcp
                albatross_instances u.name
            in
            let current_time = now () in
            let reply content =
              dashboard_page
                ~page_title:
                  (String.capitalize_ascii
                     (Models.Configuration.name_to_str u.name))
                user ~content req server env
            in
            match page with
            | `Profile ->
                reply
                  (User_single.user_single_layout ~active_tab:Profile
                     (User_single.user_profile u)
                     u.uuid)
            | `Unikernels ->
                reply
                  (User_single.user_single_layout ~active_tab:Unikernels
                     (Unikernel_index.unikernel_index_layout unikernels
                        deceased_unikernels current_time)
                     u.uuid)
            | `Policy ->
                reply
                  (User_single.user_single_layout ~active_tab:Policy
                     (User_single.user_policy u
                        ~empty_policy:Albatross_client.empty_policy
                        (Albatross_client.all_policies ~domain:u.name
                           albatross_instances))
                     u.uuid))
        | None ->
            error_page_response ~title:"Not Found"
              ~data:("Couldn't find account with uuid: " ^ Uuidm.to_string uuid)
              `Not_found req)

  let view_user_profile req server env = view_user `Profile req server env
  let view_user_unikernels req server env = view_user `Unikernels req server env
  let view_user_policy req server env = view_user `Policy req server env

  let edit_policy req server env =
    with_uuid_admin_page req server (fun uuid user ->
        with_instance req server (fun albatross_inst ->
            match
              Storage.find_by_uuid (Vifu.Server.device storage server) uuid
            with
            | Some u -> (
                let user_policy =
                  Option.value ~default:Albatross_client.empty_policy
                    (Albatross_client.policy albatross_inst ~domain:u.name)
                in
                match
                  Albatross_client.policy_resource_avalaible albatross_inst
                with
                | Ok unallocated_resources ->
                    dashboard_page
                      ~page_title:
                        (String.capitalize_ascii
                           (Models.Configuration.name_to_str u.name))
                      user
                      ~content:
                        (Update_policy.update_policy_layout u
                           albatross_inst.configuration.name ~user_policy
                           ~unallocated_resources)
                      req server env
                | Error err ->
                    error_page_response ~title:"Bad Request"
                      ~data:("Couldn't get unallocated resources: " ^ err)
                      `Bad_request req)
            | None ->
                error_page_response ~title:"Not Found"
                  ~data:
                    ("Couldn't find account with uuid: " ^ Uuidm.to_string uuid)
                  `Not_found req))

  let update_policy req server env =
    with_admin_cookie_api req server (fun _user _cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Policy.t) -> f.molly_csrf)
          (fun (form : Models.Policy.t) ->
            let store = Vifu.Server.device storage server in
            let*? u =
              Option.bind
                (Uuidm.of_string form.user_uuid)
                (Storage.find_by_uuid store)
              |> Option.to_result
                   ~none:
                     (api_response ~title:"Not Found" ~data:"User not found"
                        `Not_found req)
            in
            with_api_instance ~instance_str:form.albatross_instance req server
              (fun albatross_inst ->
                let*? policy =
                  Models.Policy.to_policy form
                  |> Result.map_error (fun (`Msg err) ->
                      api_response ~title:"Bad Request" ~data:err `Bad_request
                        req)
                in
                let*? root_policy =
                  Albatross_client.policy albatross_inst
                  |> Option.to_result ~none:()
                  |> Result.map_error (fun () ->
                      Logs.err (fun m -> m "policy: root policy can't be null ");
                      api_response ~title:"Internal Server Error"
                        ~data:"Root policy is null" `Internal_server_error req)
                in
                let*? () =
                  Vmm_core.Policy.is_smaller ~super:root_policy ~sub:policy
                  |> Result.map_error (fun (`Msg err) ->
                      Logs.err (fun m ->
                          m "policy %a is not smaller than root policy %a: %s"
                            Vmm_core.Policy.pp policy Vmm_core.Policy.pp
                            root_policy err);
                      api_response ~title:"Bad Request"
                        ~data:("Policy is not smaller than root policy: " ^ err)
                        `Bad_request req)
                in
                let*? _policy =
                  Albatross_client.set_policy env.tcp albatross_inst
                    ~domain:u.name policy
                  |> Result.map_error (fun err ->
                      Logs.err (fun m ->
                          m "error setting policy %a for %s: %s"
                            Vmm_core.Policy.pp policy
                            (Models.Configuration.name_to_str u.name)
                            err);
                      api_response ~title:"Internal Server Error"
                        ~data:("error setting policy: " ^ err)
                        `Internal_server_error req)
                in
                api_response ~title:"Success"
                  ~data:"Updated policy successfully" `OK req)))

  let toggle_account_attribute (form : Models.Admin_user.t) store env req ~key
      update_fn error_on_last ~error_message =
    match
      Option.bind (Uuidm.of_string form.uuid) (Storage.find_by_uuid store)
    with
    | None ->
        Logs.warn (fun m -> m "%s : Account not found" key);
        api_response ~title:"Not Found" ~data:"Account not found" `Not_found req
    | Some user ->
        if error_on_last user then (
          Logs.warn (fun m -> m "%s : Can't perform action on last user" key);
          api_response ~title:"Forbidden" ~data:error_message `Forbidden req)
        else
          let updated_user = update_fn user in
          Storage.update_user store updated_user;
          write_data ~error_title:"Internal Server Error"
            ~on_error:(fun msg ->
              Logs.warn (fun m -> m "%s : Storage error with %s" key msg);
              msg)
            ~success_msg:"Updated user successfully" store env req

  let toggle_account_activation req server env =
    with_admin_cookie_api req server (fun _user _cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Admin_user.t) -> f.molly_csrf)
          (fun (form : Models.Admin_user.t) ->
            let store = Vifu.Server.device storage server in
            toggle_account_attribute form store env req
              ~key:"toggle-active-account"
              (fun user ->
                Models.User.update_user user ~active:(not user.active)
                  ~updated_at:(now ()) ())
              (fun user -> user.active && Storage.count_active store <= 1)
              ~error_message:"Cannot deactivate last active user"))

  let toggle_admin_activation req server env =
    with_admin_cookie_api req server (fun _user _cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Admin_user.t) -> f.molly_csrf)
          (fun (form : Models.Admin_user.t) ->
            let store = Vifu.Server.device storage server in
            toggle_account_attribute form store env req
              ~key:"toggle-admin-account"
              (fun user ->
                Models.User.update_user user ~super_user:(not user.super_user)
                  ~updated_at:(now ()) ())
              (fun user ->
                user.super_user && Storage.count_superusers store <= 1)
              ~error_message:"Cannot remove last administrator"))

  let delete_account req server env =
    with_admin_cookie_api req server (fun _user _cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Admin_user.t) -> f.molly_csrf)
          (fun (form : Models.Admin_user.t) ->
            let store = Vifu.Server.device storage server in
            match
              Option.bind
                (Uuidm.of_string form.uuid)
                (Storage.find_by_uuid store)
            with
            | None ->
                Logs.warn (fun m -> m "delete-account : Account not found");
                api_response ~title:"Not Found" ~data:"Account not found"
                  `Not_found req
            | Some user ->
                Storage.delete_user store user;
                write_data ~error_title:"Internal Server Error"
                  ~on_error:(fun msg ->
                    Logs.warn (fun m ->
                        m "delete-user : Storage error with %s" msg);
                    msg)
                  ~success_msg:"Deleted user successfully" store env req))
end
