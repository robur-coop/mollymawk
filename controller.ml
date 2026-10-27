type env = {
  tcp : Mnet.TCP.state;
  udp : Mnet.UDP.state;
  m_tcp : Mnet.TCP.state;
  m_udp : Mnet.UDP.state;
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
  let tcp, udp = get_stack env in
  let daemon, he = Mnet_happy_eyeballs.create tcp in
  let stack = Mnet_dns.Transport.stack udp he in
  let dns = Mnet_dns.create stack in
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
  make_happy_eyeballs ~name:"happy-eyeballs" (fun env -> (env.tcp, env.udp))

let management_happy_eyeballs =
  make_happy_eyeballs ~name:"management-happy-eyeballs" (fun env ->
      (env.m_tcp, env.m_udp))

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

let static_asset_response ~mime ~content req (_server : Vifu.Server.t) _env =
  let open Vifu.Response.Syntax in
  (* TODO: if-none-match / etag *)
  let* () = Vifu.Response.add ~field:"Content-Type" mime in
  let* () = Vifu.Response.with_string req content in
  Vifu.Response.respond `OK

let pct_encode str = Uri.pct_encode ~component:`Authority str

let query_param req name =
  match Vifu.Queries.get req name with
  | v :: _ -> Ok v
  | [] -> Error (Fmt.str "Couldn't find %s in query params" name)

module Session = struct
  type t = { mutable sessions : (Uuidm.t * Models.User.cookie) Utils.SM.t }

  let session_cookie = "molly_session"
  let state = { sessions = Utils.SM.empty }

  let add_session user_uuid (cookie : Models.User.cookie) =
    state.sessions <-
      Utils.SM.add cookie.value (user_uuid, cookie) state.sessions

  let remove_session cookie_value =
    state.sessions <- Utils.SM.remove cookie_value state.sessions

  let find_session cookie_value = Utils.SM.find_opt cookie_value state.sessions

  let update_cookie_usage (cookie : Models.User.cookie) user_agent now =
    let cookie = { cookie with user_agent; last_access = now } in
    state.sessions <-
      Utils.SM.update cookie.value
        (function Some (uuid, _) -> Some (uuid, cookie) | None -> None)
        state.sessions

  let user_sessions user_uuid =
    Utils.SM.fold
      (fun _ (uuid, (cookie : Models.User.cookie)) acc ->
        if Uuidm.equal uuid user_uuid then Utils.SM.add cookie.value cookie acc
        else acc)
      state.sessions Utils.SM.empty

  let close_other_sessions user_uuid active_cookie_value =
    state.sessions <-
      Utils.SM.filter
        (fun _ (uuid, (cookie : Models.User.cookie)) ->
          (not (Uuidm.equal uuid user_uuid))
          || String.equal cookie.value active_cookie_value)
        state.sessions

  let prune_expired_sessions now =
    state.sessions <-
      Utils.SM.filter
        (fun _ (_, (cookie : Models.User.cookie)) ->
          Models.User.is_valid_cookie cookie now)
        state.sessions

  let remove_user_sessions user_uuid =
    state.sessions <-
      Utils.SM.filter
        (fun _ (uuid, _) -> not (Uuidm.equal uuid user_uuid))
        state.sessions

  let reset_state () = state.sessions <- Utils.SM.empty

  let session : (env, string) Vifu.Middleware.t =
    Vifu.Middlewares.v ~name:session_cookie @@ fun req _target server _env ->
    match Vifu.Cookie.get ~encrypted:false ~name:session_cookie server req with
    | Error _ -> None
    | Ok cookie -> Some cookie

  let validate_user_session ?(require_admin = false) req storage server =
    let store = Vifu.Server.device storage server in
    match Option.bind (Vifu.Request.get session req) find_session with
    | None -> Error "No molly-session in cookie header. User not found"
    | Some (user_uuid, cookie) -> (
        match Storage.find_by_uuid store user_uuid with
        | None -> Error "User not found"
        | Some user ->
            if not (Models.User.is_valid_cookie cookie (now ())) then
              Error "Cookie expired."
            else (
              update_cookie_usage cookie
                (Vifu.Headers.get (Vifu.Request.headers req) "User-Agent")
                (now ());
              if not user.active then Error "User account is deactivated"
              else if require_admin && not user.super_user then
                Error
                  "You don't have the necessary permissions to access this \
                   service."
              else Ok (user, cookie)))
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
  | None -> false
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

let write_data ?(success_msg = "Updated password successfully") storage env req
    =
  match Store.write_data storage env.block with
  | Ok () -> api_response ~title:"Success" ~data:success_msg `OK req
  | Error (`Msg err) ->
      api_response ~title:"Failure" ~data:err `Internal_server_error req

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

  let set_cookie_header cookie =
    let cookie_value =
      Session.session_cookie ^ "=" ^ cookie ^ ";Path=/;HttpOnly=true"
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
            let existing_name = Storage.find_by_name storage name in
            let existing_email = Storage.find_by_email storage email in
            match (existing_name, existing_email) with
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
            | None, None -> (
                let active, super_user =
                  if Storage.count_users storage = 0 then (true, true)
                  else (false, false)
                in
                let user, cookie =
                  Models.User.create_user ~name ~email ~password
                    ~created_at:(now ()) ~active ~super_user ~user_agent
                in
                Storage.add_user storage user;
                Session.add_session user.uuid cookie;
                match Store.write_data storage env.block with
                | Ok () ->
                    api_response
                      ~headers:(set_cookie_header cookie.value)
                      ~title:"Success" ~data:"Registration successful" `OK req
                | Error (`Msg err) ->
                    Storage.delete_user storage user;
                    Session.remove_session cookie.value;
                    api_response ~title:"Failure" ~data:err
                      `Internal_server_error req)))

  let login_user req server env =
    let storage = Vifu.Server.device storage server in
    let user_agent = Vifu.Headers.get (Vifu.Request.headers req) "User-Agent" in
    with_json_csrf req env
      ~csrf_of:(fun (f : Models.Login.t) -> f.molly_csrf)
      (fun ({ email; password; _ } : Models.Login.t) ->
        if email = "" || password = "" then
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
                  Session.prune_expired_sessions (now ());
                  Session.add_session user.uuid cookie;
                  api_response
                    ~headers:(set_cookie_header cookie.value)
                    ~title:"Success" ~data:"Login successful" `OK req))

  let logout_user req server env =
    with_user_cookie_api ~error_title:"Failure" ~error_status:`Unauthorized req
      server (fun _user session_cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Close_sessions.t) -> f.molly_csrf)
          (fun (_ : Models.Close_sessions.t) ->
            Session.remove_session session_cookie.value;
            api_response ~title:"Success" ~data:"Logout successful" `OK req))

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
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Close_sessions.t) -> f.molly_csrf)
          (fun (_ : Models.Close_sessions.t) ->
            Session.close_other_sessions user.uuid session_cookie.value;
            api_response ~title:"Success"
              ~data:"Closed all sessions successfully" `OK req))

  let close_session req server env =
    with_user_cookie_api ~error_title:"Failure" ~error_status:`Unauthorized req
      server (fun _user _session_cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Close_session.t) -> f.molly_csrf)
          (fun ({ session_value; _ } : Models.Close_session.t) ->
            Session.remove_session session_value;
            api_response ~title:"Success" ~data:"Session closed successfully"
              `OK req))

  let account_page req server env =
    with_user_cookie_page req server (fun user cookie ->
        let csrf = generate_csrf_token env req in
        let sessions = Session.user_sessions user.uuid in
        page_response
          ~headers:[ ("X-MOLLY-CSRF", csrf) ]
          (Dashboard.dashboard_layout ~csrf user ~page_title:"Account"
             ~content:
               (User_account.user_account_layout user sessions
                  ~active_cookie_value:cookie.value (now ()))
             ())
          req server env)

  let account_usage req server env =
    with_user_cookie_page req server (fun user _cookie ->
        with_instance ~endpoint:"/usage" req server (fun albatross_inst ->
            let csrf = generate_csrf_token env req in
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
                (Option.join
                   (Result.to_option
                      (Albatross_client.policy albatross_inst ~domain:user.name)))
            in
            page_response
              ~headers:[ ("X-MOLLY-CSRF", csrf) ]
              (Dashboard.dashboard_layout ~csrf user ~page_title:"Usage"
                 ~content:
                   (Account_usage.account_usage_layout
                      albatross_inst.configuration.name policy unikernels blocks)
                 ())
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
        let csrf = generate_csrf_token env req in
        page_response
          ~headers:[ ("X-MOLLY-CSRF", csrf) ]
          (Dashboard.dashboard_layout ~csrf user ~page_title:"Email Settings"
             ~content:
               (Settings_page.settings_layout ~active_tab:Email
                  (Email_config.email_config_layout (Storage.email store)))
             ())
          req server env)

  let update_email_configuration req server env =
    with_admin_cookie_api req server (fun _user _cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Email.form_t) -> f.molly_csrf)
          (fun form ->
            let store = Vifu.Server.device storage server in
            with_valid_email_config form req (fun email_settings ->
                Storage.store_email store (Some email_settings);
                match Store.write_data store env.block with
                | Ok () ->
                    api_response ~title:"Success"
                      ~data:"Email settings verified and saved successfully" `OK
                      req
                | Error (`Msg err) ->
                    api_response ~title:"Internal Server Error"
                      ~data:(Fmt.str "Save failed: %s" err)
                      `Internal_server_error req)))

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
              Result.fold ~ok:Fun.id
                ~error:(fun _ -> None)
                (Albatross_client.policy ~domain:user.name albatross_inst)
            in
            let csrf = generate_csrf_token env req in
            page_response
              ~headers:[ ("X-MOLLY-CSRF", csrf) ]
              (Dashboard.dashboard_layout ~csrf user
                 ~page_title:
                   (String.capitalize_ascii
                      (Models.Configuration.name_to_str user.name))
                 ~content:
                   (Block_index.block_index_layout
                      albatross_inst.configuration.name blocks policy)
                 ())
              req server env))

  let delete_block req server env =
    with_user_cookie_api req server (fun user _cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Block.delete_t) -> f.molly_csrf)
          (fun ({ block_name; albatross_instance = instance_name; _ } :
                 Models.Block.delete_t)
             ->
            let albatross_instances = !(Vifu.Server.device albatross server) in
            match
              ( Models.Configuration.name_of_str instance_name,
                Models.Configuration.name_of_str block_name )
            with
            | Ok instance_name, Ok block_name -> (
                match
                  Albatross_client.find_instance_by_name albatross_instances
                    instance_name
                with
                | Ok albatross_inst -> (
                    match
                      Albatross_client.query env.tcp albatross_inst
                        ~domain:user.name ~name:block_name
                        (`Block_cmd `Block_remove)
                    with
                    | Error err ->
                        api_response ~title:"Internal Server Error"
                          ~data:("Error querying albatross: " ^ err)
                          `Internal_server_error req
                    | Ok (_hdr, res) -> (
                        Albatross_client.set_online albatross_inst;
                        match Albatross_json.res_to_string res with
                        | Ok res ->
                            api_response ~title:"Success" ~data:res `OK req
                        | Error (`String err) ->
                            api_response ~title:"Internal Server Error"
                              ~data:err `Internal_server_error req))
                | Error _ ->
                    Logs.err (fun m ->
                        m "Couldn't find albatross instance with name %s"
                          (Models.Configuration.name_to_str instance_name));
                    api_response ~title:"Not Found"
                      ~data:
                        ("Couldn't find albatross instance with name: "
                        ^ Models.Configuration.name_to_str instance_name)
                      `Not_found req)
            | Error (`Msg err), _ ->
                api_response ~title:"Bad Request"
                  ~data:("Couldn't convert instance name: " ^ err)
                  `Bad_request req
            | _, Error (`Msg err) ->
                api_response ~title:"Bad Request"
                  ~data:("Couldn't convert block name: " ^ err)
                  `Bad_request req))

  let create_or_upload_block c_or_u req server env =
    with_user_cookie_api req server (fun user _cookie ->
        let cmd_name =
          match c_or_u with `Create -> "create" | `Upload -> "upload"
        in
        let generate_http_error_response msg code =
          Logs.warn (fun m -> m "Block %s error: %s" cmd_name msg);
          api_response ~title:"Error" ~data:msg code req
        in
        match Vifu.Request.of_multipart_form req with
        | Error (`Not_found _) ->
            Logs.info (fun m -> m "Missing Fields");
            generate_http_error_response
              "One or more required fields are missing." `Bad_request
        | Error `Invalid_multipart_form ->
            generate_http_error_response "Couldn't decode multipart data"
              `Bad_request
        | Ok
            ({
               albatross_instance;
               molly_csrf = csrf;
               json_data = json;
               block_data;
             } :
              Models.Block.upload_t) -> (
            let albatross_instances = !(Vifu.Server.device albatross server) in
            let stream_to_albatross albatross_inst block_name block_compressed =
              let push = make_string_stream block_data in
              match
                Albatross_client.query env.tcp albatross_inst ~domain:user.name
                  ~name:block_name ~push
                  (`Block_cmd (`Block_set block_compressed))
              with
              | Error err ->
                  generate_http_error_response
                    (Fmt.str "an error with albatross. got %s" err)
                    `Internal_server_error
              | Ok (_hdr, res) -> (
                  Albatross_client.set_online albatross_inst;
                  match Albatross_json.res_to_string res with
                  | Error (`String err) ->
                      generate_http_error_response
                        (Fmt.str "unexpected field. got %s" err)
                        `Bad_request
                  | Ok res -> api_response ~title:"Success" ~data:res `OK req)
            in
            let add_block albatross_inst block_name block_size block_compressed
                =
              match
                Albatross_client.query env.tcp albatross_inst ~domain:user.name
                  ~name:block_name
                  (`Block_cmd (`Block_add block_size))
              with
              | Error err ->
                  generate_http_error_response
                    (Fmt.str "an error with albatross. got %s" err)
                    `Internal_server_error
              | Ok (_hdr, res) -> (
                  Albatross_client.set_online albatross_inst;
                  match Albatross_json.res res with
                  | Error (`String err) ->
                      generate_http_error_response
                        (Fmt.str "unexpected field. got %s" err)
                        `Bad_request
                  | Ok _ ->
                      stream_to_albatross albatross_inst block_name
                        block_compressed)
            in
            match Models.Configuration.name_of_str albatross_instance with
            | Ok albatross_instance -> (
                match
                  Albatross_client.find_instance_by_name albatross_instances
                    albatross_instance
                with
                | Ok albatross_inst -> (
                    match
                      Jsont_bytesrw.decode_string Models.Block.block_json_form
                        json
                    with
                    | Ok { block_name; block_size; block_compressed } -> (
                        match Models.Configuration.name_of_str block_name with
                        | Error (`Msg err) ->
                            generate_http_error_response
                              (Fmt.str "Invalid block name: %s" err)
                              `Bad_request
                        | Ok block_name ->
                            if verify_csrf env req csrf then
                              match (c_or_u, block_size) with
                              | `Create, Some block_size ->
                                  add_block albatross_inst block_name block_size
                                    block_compressed
                              | `Create, None ->
                                  generate_http_error_response
                                    (Fmt.str "unexpected field. got %s" json)
                                    `Bad_request
                              | `Upload, _ ->
                                  stream_to_albatross albatross_inst block_name
                                    block_compressed
                            else
                              api_response ~title:"CSRF Token Mismatch"
                                ~data:
                                  "CSRF token mismatch error. Please refresh \
                                   and try again."
                                `Bad_request req)
                    | Error err ->
                        generate_http_error_response
                          (Fmt.str "unexpected field. got %s" err)
                          `Bad_request)
                | Error _ ->
                    generate_http_error_response
                      (Fmt.str "Couldn't find albatross instance with name %s"
                         (Models.Configuration.name_to_str albatross_instance))
                      `Not_found)
            | Error (`Msg err) ->
                generate_http_error_response
                  (Fmt.str "Couldn't convert name to albatross instance %s" err)
                  `Bad_request))

  let create_block req server env =
    create_or_upload_block `Create req server env

  let upload_block req server env =
    create_or_upload_block `Upload req server env

  let download_block req server env =
    with_user_cookie_api req server (fun user _cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Block.download_t) -> f.molly_csrf)
          (fun ({
                  albatross_instance = instance_name;
                  block_name;
                  compression_level;
                  _;
                } :
                 Models.Block.download_t)
             ->
            let albatross_instances = !(Vifu.Server.device albatross server) in
            match
              ( Models.Configuration.name_of_str instance_name,
                Models.Configuration.name_of_str block_name )
            with
            | Ok instance_name_obj, Ok block_name_obj -> (
                match
                  Albatross_client.find_instance_by_name albatross_instances
                    instance_name_obj
                with
                | Ok albatross_inst ->
                    let filename = block_name ^ "_dump" in
                    let disposition =
                      "attachment; filename=\"" ^ filename ^ "\""
                    in
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
                              Albatross_client.set_online albatross_inst))
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
                      Vifu.Response.add_unless_exists ~field:"connection"
                        "close"
                    in
                    let* () = Vifu.Response.with_source req src in
                    Vifu.Response.respond `OK
                | Error err ->
                    Logs.err (fun m ->
                        m "Couldn't find albatross instance with name %s: %s"
                          (Models.Configuration.name_to_str instance_name_obj)
                          err);
                    api_response ~title:"Not Found"
                      ~data:
                        ("Couldn't find albatross instance with name: "
                        ^ Models.Configuration.name_to_str instance_name_obj)
                      `Not_found req)
            | Error (`Msg err), _ ->
                api_response ~title:"Bad Request"
                  ~data:("Couldn't convert instance name: " ^ err)
                  `Bad_request req
            | _, Error (`Msg err) ->
                api_response ~title:"Bad Request"
                  ~data:
                    ("Couldn't convert block name: " ^ block_name ^ ": " ^ err)
                  `Bad_request req))
end

module Unikernel = struct
  let dashboard req server env =
    with_user_cookie_page req server (fun user _cookie ->
        let albatross_instances = !(Vifu.Server.device albatross server) in
        let csrf = generate_csrf_token env req in
        let unikernels_by_albatross_instance =
          Albatross_client.user_unikernels env.tcp albatross_instances user.name
        in
        let deceased_unikernels_by_albatross_instance =
          Albatross_client.user_deceased_by_instance env.tcp albatross_instances
            user.name
        in
        page_response
          ~headers:[ ("X-MOLLY-CSRF", csrf) ]
          (Dashboard.dashboard_layout ~csrf user
             ~content:
               (Unikernel_index.unikernel_index_layout
                  unikernels_by_albatross_instance
                  deceased_unikernels_by_albatross_instance (now ()))
             ())
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
            let csrf = generate_csrf_token env req in
            match Albatross_client.policy albatross_inst ~domain:user.name with
            | Ok (Some user_policy) ->
                page_response
                  ~headers:[ ("X-MOLLY-CSRF", csrf) ]
                  (Dashboard.dashboard_layout ~csrf user
                     ~page_title:"Deploy a Unikernel"
                     ~content:
                       (Unikernel_create.unikernel_create_layout ~user_policy
                          unikernels_by_albatross_instance
                          blocks_by_albatross_instance
                          albatross_inst.configuration.name builder_jobs)
                     ())
                  req server env
            | Ok None ->
                error_page_response ~title:"Resource Policy error"
                  ~data:"No user policy" `Bad_request req
            | Error err ->
                error_page_response ~title:"Resource Policy error" ~data:err
                  `Bad_request req))

  let unikernel_info req server env =
    with_user_cookie_api req server (fun user _cookie ->
        let albatross_instances = !(Vifu.Server.device albatross server) in
        let successes, failures =
          List.fold_left
            (fun (successes, failures) (name, (instance : Albatross_client.t))
               ->
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
                  Albatross_client.set_online instance;
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
            ([], [])
            (Utils.LM.bindings albatross_instances)
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
                let csrf = generate_csrf_token env req in
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
                page_response
                  ~headers:[ ("X-MOLLY-CSRF", csrf) ]
                  (Dashboard.dashboard_layout ~csrf user
                     ~content:
                       (Unikernel_single.unikernel_single_layout ~unikernel_name
                          ~instance_name:albatross_inst.configuration.name
                          ~max_allowed unikernel ~last_update_time
                          ~current_time:(now ()) scaling_policy)
                     ())
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
                      Albatross_client.set_online albatross_inst))
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
            let albatross_instances = !(Vifu.Server.device albatross server) in
            match
              ( Models.Configuration.name_of_str form.albatross_instance,
                Models.Configuration.name_of_str form.name )
            with
            | Ok instance_name, Ok unikernel_name -> (
                match
                  Albatross_client.find_instance_by_name albatross_instances
                    instance_name
                with
                | Ok albatross_inst -> (
                    match
                      Albatross_client.query env.tcp albatross_inst
                        ~domain:user.name ~name:unikernel_name
                        (`Unikernel_cmd `Unikernel_destroy)
                    with
                    | Error msg ->
                        api_response ~title:"Internal Server Error"
                          ~data:("Error querying albatross: " ^ msg)
                          `Internal_server_error req
                    | Ok (_hdr, res) -> (
                        Albatross_client.set_online albatross_inst;
                        match Albatross_json.res_to_string res with
                        | Ok res ->
                            api_response ~title:"Success" ~data:res `OK req
                        | Error (`String err) ->
                            api_response ~title:"Internal Server Error"
                              ~data:err `Internal_server_error req))
                | Error _ ->
                    Logs.err (fun m ->
                        m "Error finding albatross instance %s"
                          (Models.Configuration.name_to_str instance_name));
                    api_response ~title:"Not Found"
                      ~data:
                        ("Error finding albatross instance: "
                        ^ Models.Configuration.name_to_str instance_name)
                      `Not_found req)
            | Error (`Msg err), _ ->
                api_response ~title:"Bad Request"
                  ~data:("Error converting instance name: " ^ err)
                  `Bad_request req
            | _, Error (`Msg err) ->
                api_response ~title:"Bad Request"
                  ~data:("Error converting unikernel name: " ^ err)
                  `Bad_request req))

  let unikernel_restart req server env =
    with_user_cookie_api req server (fun user _cookie ->
        with_unikernel_and_instance req server
          (fun albatross_inst unikernel_name ->
            with_json_csrf req env
              ~csrf_of:(fun (f : Models.Unikernel.restart_t) -> f.molly_csrf)
              (fun (form : Models.Unikernel.restart_t) ->
                let restart ?arguments () =
                  match
                    Albatross_client.query env.tcp albatross_inst
                      ~domain:user.name ~name:unikernel_name
                      (`Unikernel_cmd (`Unikernel_restart arguments))
                  with
                  | Error msg ->
                      api_response ~title:"Internal Server Error"
                        ~data:("Error querying albatross: " ^ msg)
                        `Internal_server_error req
                  | Ok (_hdr, res) -> (
                      Albatross_client.set_online albatross_inst;
                      match Albatross_json.res_to_string res with
                      | Ok res ->
                          api_response ~title:"Success" ~data:res `OK req
                      | Error (`String err) ->
                          api_response ~title:"Internal Server Error" ~data:err
                            `Internal_server_error req)
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

  let unikernel_create req server env =
    with_user_cookie_api req server (fun user _cookie ->
        let generate_http_error_response msg code =
          Logs.warn (fun m -> m "Unikernel_create error: %s" msg);
          api_response ~title:"Error" ~data:msg code req
        in
        match Vifu.Request.of_multipart_form req with
        | Error (`Msg msg) -> generate_http_error_response msg `Bad_request
        | Error (`Not_found field) ->
            generate_http_error_response
              ("Missing field: " ^ field)
              `Bad_request
        | Error `Invalid_multipart_form ->
            generate_http_error_response "Invalid multipart form" `Bad_request
        | Ok stream -> (
            let parts = Models.Multipart.parse_stream stream in
            let albatross_instances = !(Vifu.Server.device albatross server) in
            let he = Vifu.Server.device happy_eyeballs server in
            match Models.Unikernel.create_of_parts parts with
            | Ok form -> (
                let process_unikernel_create albatross_inst unikernel_name =
                  match Albatross_json.config_of_json form.unikernel_config with
                  | Error (`Msg err) ->
                      generate_http_error_response
                        ("Invalid unikernel arguments: " ^ err)
                        `Bad_request
                  | Ok cfg -> (
                      let albatross_cmd =
                        if
                          bool_of_string_opt form.unikernel_force_create
                          = Some true
                        then `Unikernel_cmd (`Unikernel_force_create cfg)
                        else `Unikernel_cmd (`Unikernel_create cfg)
                      in
                      let execute_albatross_cmd push =
                        match
                          Albatross_client.query env.tcp albatross_inst
                            ~domain:user.name ~name:unikernel_name ~push
                            albatross_cmd
                        with
                        | Error err ->
                            generate_http_error_response
                              ("Albatross Query Error: " ^ err)
                              `Internal_server_error
                        | Ok (_hdr, res) -> (
                            Albatross_client.set_online albatross_inst;
                            match Albatross_json.res_to_string res with
                            | Ok res_json ->
                                api_response ~title:"Success" ~data:res_json `OK
                                  req
                            | Error (`String err_str) ->
                                generate_http_error_response
                                  ("Albatross Response Error: " ^ err_str)
                                  `Internal_server_error)
                      in
                      match form.deploy_mode with
                      | "builder" -> (
                          match form.builder_job with
                          | Some job -> (
                              let buf = Buffer.create 65536 in
                              let push_chunks = function
                                | Some chunk -> Buffer.add_string buf chunk
                                | None -> ()
                              in
                              match
                                Builder_web.fetch_unikernel_binary_image he ~job
                                  ~version:"latest" push_chunks
                              with
                              | Error (`Msg err) ->
                                  generate_http_error_response
                                    ("Builder Error: " ^ err)
                                    `Internal_server_error
                              | Ok () ->
                                  execute_albatross_cmd
                                    (make_string_stream (Buffer.contents buf)))
                          | None ->
                              generate_http_error_response
                                "Missing builder job or uuid" `Bad_request)
                      | "manual" -> (
                          match form.binary with
                          | Some binary_data ->
                              execute_albatross_cmd
                                (make_string_stream binary_data)
                          | None ->
                              generate_http_error_response
                                "One or more required fields are missing."
                                `Bad_request)
                      | _ ->
                          generate_http_error_response "Invalid deploy mode"
                            `Bad_request)
                in
                match
                  ( Models.Configuration.name_of_str form.albatross_instance,
                    Models.Configuration.name_of_str form.unikernel_name )
                with
                | Ok instance_name, Ok unikernel_name -> (
                    match
                      Albatross_client.find_instance_by_name albatross_instances
                        instance_name
                    with
                    | Ok albatross_inst ->
                        if verify_csrf env req form.molly_csrf then
                          process_unikernel_create albatross_inst unikernel_name
                        else
                          api_response ~title:"CSRF Token Mismatch"
                            ~data:
                              "CSRF token mismatch error. Please refresh and \
                               try again."
                            `Bad_request req
                    | Error _ ->
                        generate_http_error_response
                          ("Error finding albatross instance: "
                          ^ Models.Configuration.name_to_str instance_name)
                          `Not_found)
                | Error (`Msg err), _ ->
                    generate_http_error_response
                      ("Error converting instance name: " ^ err)
                      `Bad_request
                | _, Error (`Msg err) ->
                    generate_http_error_response
                      ("Error converting unikernel name: " ^ err)
                      `Bad_request)
            | Error msg ->
                Logs.info (fun m -> m "Missing Fields");
                generate_http_error_response msg `Bad_request))

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
                let csrf = generate_csrf_token env req in
                page_response
                  ~headers:[ ("X-MOLLY-CSRF", csrf) ]
                  (Dashboard.dashboard_layout ~csrf user
                     ~page_title:(Vmm_core.Name.to_string name ^ " Update")
                     ~content:
                       (Unikernel_update.unikernel_update_layout
                          ~instance_name:albatross_inst.configuration.name
                          ~unikernel_name:
                            (Models.Configuration.name_to_str unikernel_name)
                          (name, unikernel) (now ()) build_comparison)
                     ())
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
        Albatross_client.set_online albatross_inst;
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
        let buf = Buffer.create 65536 in
        let push_chunks = function
          | Some chunk -> Buffer.add_string buf chunk
          | None -> ()
        in
        match
          Builder_web.fetch_unikernel_binary_image he ~job
            ~version:(Uuidm.to_string to_be_updated_unikernel)
            push_chunks
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
        | Ok () ->
            let push = make_string_stream (Buffer.contents buf) in
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
            let albatross_instances = !(Vifu.Server.device albatross server) in
            let he = Vifu.Server.device happy_eyeballs server in
            let config_or_none field = function
              | None | Some (Jsont.Null _) -> Ok None
              | Some json -> (
                  match Albatross_json.config_of_jsont json with
                  | Ok cfg -> Ok (Some cfg)
                  | Error (`Msg err) ->
                      let json_str =
                        Result.value ~default:""
                          (Jsont_bytesrw.encode_string Jsont.json json)
                      in
                      Error
                        (`Msg
                           ("invalid json for " ^ field ^ ": " ^ json_str
                          ^ "failed with: " ^ err)))
            in
            let http_liveliness_address = form.http_liveliness_address in
            let dns_liveliness = form.dns_liveliness in
            match
              Liveliness_checks.liveliness_checks ~http_liveliness_address
                ~dns_liveliness ~udp:env.udp ~happy_eyeballs:he
            with
            | Ok () -> (
                match
                  ( Models.Configuration.name_of_str form.albatross_instance,
                    Models.Configuration.name_of_str form.unikernel_name )
                with
                | Ok instance_name, Ok unikernel_name -> (
                    match
                      Albatross_client.find_instance_by_name albatross_instances
                        instance_name
                    with
                    | Ok albatross_inst -> (
                        match
                          config_or_none "unikernel_arguments"
                            form.unikernel_arguments
                        with
                        | Error (`Msg err) ->
                            api_response
                              ~title:"Error with Unikernel Arguments Json"
                              ~data:
                                ("Could not get the unikernel arguments json: "
                               ^ err)
                              `Bad_request req
                        | Ok cfg_opt -> (
                            match
                              ( Uuidm.of_string form.to_be_updated_unikernel,
                                Uuidm.of_string form.currently_running_unikernel
                              )
                            with
                            | None, _ ->
                                api_response
                                  ~title:
                                    "Error: Bad to-be-updated unikernel UUID"
                                  ~data:
                                    "Couldn't convert the to-be-updated \
                                     unikernel build UUID"
                                  `Bad_request req
                            | _, None ->
                                api_response
                                  ~title:
                                    "Error: Bad currently running unikernel \
                                     UUID"
                                  ~data:
                                    "Couldn't convert the currently running \
                                     unikernel build UUID"
                                  `Bad_request req
                            | ( Some to_be_updated_unikernel,
                                Some currently_running_unikernel ) -> (
                                match cfg_opt with
                                | None -> (
                                    match
                                      Albatross_client.user_unikernel env.tcp
                                        albatross_inst ~user_name:user.name
                                        ~unikernel_name
                                    with
                                    | Error err ->
                                        api_response ~title:"Bad Request"
                                          ~data:
                                            ("Couldn't find albatross \
                                              instance, "
                                            ^ Models.Configuration.name_to_str
                                                instance_name
                                            ^ " with error: " ^ err)
                                          `Bad_request req
                                    | Ok (_n, (info : Vmm_core.Unikernel.info))
                                      ->
                                        let (cfg : Vmm_core.Unikernel.config) =
                                          {
                                            Vmm_core.Unikernel.typ = info.typ;
                                            compressed = false;
                                            image = "";
                                            add_name = true;
                                            startup = info.startup;
                                            fail_behaviour = info.fail_behaviour;
                                            cpuids = info.cpuids;
                                            memory = info.memory;
                                            block_devices =
                                              List.map
                                                (fun {
                                                       Vmm_core.Unikernel
                                                       .unikernel_device;
                                                       host_device;
                                                       sector_size;
                                                       _;
                                                     } ->
                                                  ( unikernel_device,
                                                    Some host_device,
                                                    Some sector_size ))
                                                info.block_devices;
                                            bridges =
                                              List.map
                                                (fun {
                                                       Vmm_core.Unikernel
                                                       .unikernel_device;
                                                       host_device;
                                                       mac;
                                                     } ->
                                                  ( unikernel_device,
                                                    Some host_device,
                                                    Some mac ))
                                                info.bridges;
                                            argv = info.argv;
                                            numcpus = info.numcpus;
                                            linux_boot_partition =
                                              info.linux_boot_partition;
                                          }
                                        in
                                        process_unikernel_update ~unikernel_name
                                          ~job:form.job ~to_be_updated_unikernel
                                          ~currently_running_unikernel
                                          ~http_liveliness_address
                                          ~dns_liveliness env cfg user store he
                                          albatross_inst req)
                                | Some cfg ->
                                    process_unikernel_update ~unikernel_name
                                      ~job:form.job ~to_be_updated_unikernel
                                      ~currently_running_unikernel
                                      ~http_liveliness_address ~dns_liveliness
                                      env cfg user store he albatross_inst req))
                        )
                    | Error _ ->
                        api_response ~title:"Albatross Instance Error"
                          ~data:
                            ("An error occured while finding albatross \
                              instance "
                            ^ Models.Configuration.name_to_str instance_name)
                          `Not_found req)
                | Error (`Msg err), _ ->
                    api_response ~title:"Error: Bad instance name"
                      ~data:("Couldn't convert the instance name, error: " ^ err)
                      `Bad_request req
                | _, Error (`Msg err) ->
                    api_response ~title:"Error: Bad unikernel name"
                      ~data:
                        ("Couldn't convert the unikernel name, error: " ^ err)
                      `Bad_request req)
            | Error (`Msg err) ->
                Logs.info (fun m ->
                    m
                      "Liveliness check of currentyly running unikernel failed \
                       with: %s"
                      err);
                api_response ~title:"Error: Liveliness check failed"
                  ~data:
                    ("Liveliness check of currentyly running unikernel failed \
                      with error: " ^ err)
                  `Internal_server_error req))

  let unikernel_rollback req server env =
    with_user_cookie_api req server (fun user _cookie ->
        with_json_csrf req env
          ~csrf_of:(fun (f : Models.Unikernel.rollback_t) -> f.molly_csrf)
          (fun (form : Models.Unikernel.rollback_t) ->
            let store = Vifu.Server.device storage server in
            let albatross_instances = !(Vifu.Server.device albatross server) in
            let he = Vifu.Server.device happy_eyeballs server in
            match
              ( Models.Configuration.name_of_str form.albatross_instance,
                Models.Configuration.name_of_str form.unikernel_name )
            with
            | Ok instance_name, Ok unikernel_name -> (
                match
                  Albatross_client.find_instance_by_name albatross_instances
                    instance_name
                with
                | Ok albatross_inst ->
                    process_rollback env albatross_inst ~unikernel_name (now ())
                      store he req user
                | Error _ ->
                    api_response ~title:"Not Found"
                      ~data:
                        ("Couldn't find albatross instance, "
                        ^ Models.Configuration.name_to_str instance_name)
                      `Not_found req)
            | Error (`Msg err), _ ->
                api_response ~title:"Bad Request"
                  ~data:("Couldn't convert instance name, " ^ err)
                  `Bad_request req
            | _, Error (`Msg err) ->
                api_response ~title:"Bad Request"
                  ~data:("Couldn't convert unikernel name, " ^ err)
                  `Bad_request req))

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
                  match Store.write_data store env.block with
                  | Ok () ->
                      api_response ~title:"Success"
                        ~data:"Unikernel scaling policy updated successfully."
                        `OK req
                  | Error (`Msg err) ->
                      api_response ~title:"Internal Server Error"
                        ~data:(Fmt.str "Failed to save scaling policy: %s" err)
                        `Internal_server_error req
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
                        match
                          Albatross_client.user_max_allowed_unikernel_instances
                            albatross_inst user.name
                        with
                        | Error err ->
                            api_response ~title:"Bad Request"
                              ~data:(Fmt.str "An error occurred: %s" err)
                              `Bad_request req
                        | Ok max_allowed ->
                            if max_instances > max_allowed then
                              api_response ~title:"Bad Request"
                                ~data:
                                  (Fmt.str
                                     "Error: you can spawn a maximum of %d \
                                      clones."
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

  let send_monitoring_tcp_command happy_eyeballs ~command ~target =
    let port = 2323 in
    try
      if Obj.magic happy_eyeballs == Obj.magic () then
        let msg =
          Fmt.str "[monitoring] Failed to connect to %s:%d, error uninitialized"
            target port
        in
        Error msg
      else
        match
          Mnet_happy_eyeballs.connect ~kind:Mnet.TCP.direct happy_eyeballs
            target [ port ]
        with
        | Error (`Msg err) ->
            let msg =
              Fmt.str "[monitoring] Failed to connect to %s:%d, error %s" target
                port err
            in
            Logs.info (fun m -> m "%s" msg);
            Error msg
        | Ok ((_ip, _port), flow) -> (
            try
              Mnet.TCP.write flow command;
              match Mnet.TCP.read flow with
              | Ok [] | Error `Eof ->
                  Mnet.TCP.close flow;
                  let msg =
                    Fmt.str
                      "[monitoring] Received eof while reading from %s:%d \
                       (command: %s)"
                      target port command
                  in
                  Logs.info (fun m -> m "%s" msg);
                  Error msg
              | Error `Refused ->
                  Mnet.TCP.close flow;
                  let msg =
                    Fmt.str
                      "[monitoring] Connection refused while reading from \
                       %s:%d (command: %s)"
                      target port command
                  in
                  Logs.info (fun m -> m "%s" msg);
                  Error msg
              | Ok chunks ->
                  Mnet.TCP.close flow;
                  Ok (String.concat "" chunks)
            with exn ->
              (try Mnet.TCP.close flow with _ -> ());
              let msg =
                Fmt.str
                  "[monitoring] Failed communicating with %s:%d (command: %s), \
                   error %s"
                  target port command (Printexc.to_string exn)
              in
              Logs.info (fun m -> m "%s" msg);
              Error msg)
    with exn ->
      let msg =
        Fmt.str "[monitoring] Failed to connect to %s:%d, error %s" target port
          (Printexc.to_string exn)
      in
      Logs.info (fun m -> m "%s" msg);
      Error msg

  let unikernel_monitoring_status req server _env =
    with_user_cookie_api req server (fun _user _cookie ->
        with_unikernel req (fun unikernel_name ->
            let he = Vifu.Server.device management_happy_eyeballs server in
            let target = Models.Configuration.name_to_str unikernel_name in
            let logs = send_monitoring_tcp_command he ~command:"l*" ~target in
            let metrics =
              send_monitoring_tcp_command he ~command:"m*" ~target
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
                      send_monitoring_tcp_command he ~command
                        ~target:unikernel_name
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
          let csrf = generate_csrf_token env req in
          page_response
            ~headers:[ ("X-MOLLY-CSRF", csrf) ]
            (Dashboard.dashboard_layout ~csrf user ~page_title:"Choose instance"
               ~content:
                 (Albatross_instances.select_instance user
                    (Utils.LM.bindings albatross_instances)
                    callback)
               ())
            req server env)

  let albatross_settings req server env =
    with_admin_cookie_page req server (fun user _cookie ->
        let albatross_instances = !(Vifu.Server.device albatross server) in
        let csrf = generate_csrf_token env req in
        page_response
          ~headers:[ ("X-MOLLY-CSRF", csrf) ]
          (Dashboard.dashboard_layout ~csrf user
             ~page_title:"Albatross Settings"
             ~content:
               (Settings_page.settings_layout ~active_tab:Albatross
                  (Albatross_config.albatross_config_layout albatross_instances))
             ())
          req server env)

  let view_albatross_error_logs req server env =
    with_admin_cookie_page req server (fun user _cookie ->
        with_instance req server (fun albatross_inst ->
            let csrf = generate_csrf_token env req in
            page_response
              ~headers:[ ("X-MOLLY-CSRF", csrf) ]
              (Dashboard.dashboard_layout ~csrf user
                 ~page_title:"Albatross Errors"
                 ~content:
                   (Albatross_status.albatross_status_layout albatross_inst)
                 ())
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
            match Models.Configuration.to_configuration form (now ()) with
            | Ok configuration_settings -> (
                match Albatross_client.init env.tcp configuration_settings with
                | Ok new_albatross_instance -> (
                    match
                      Storage.upsert_configuration store configuration_settings
                        update_or_create
                    with
                    | Error err ->
                        api_response ~title:"Bad Request" ~data:err `Bad_request
                          req
                    | Ok () -> (
                        match Store.write_data store env.block with
                        | Ok () ->
                            albatross_instances :=
                              Utils.LM.update configuration_settings.name
                                (fun _prev -> Some new_albatross_instance)
                                !albatross_instances;
                            api_response ~title:"Success"
                              ~data:"Configuration updated successfully" `OK req
                        | Error (`Msg err) ->
                            api_response ~title:"Internal Server Error"
                              ~data:err `Internal_server_error req))
                | Error (_, err) ->
                    api_response ~title:"Internal Server Error" ~data:err
                      `Internal_server_error req)
            | Error (`Msg err) ->
                api_response ~title:"Bad Request" ~data:err `Bad_request req))

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
            | Ok name -> (
                Storage.delete_configuration store name;
                match Store.write_data store env.block with
                | Ok () ->
                    albatross_instances :=
                      Utils.LM.remove name !albatross_instances;
                    api_response ~title:"Success"
                      ~data:"Configuration delete successfully" `OK req
                | Error (`Msg err) ->
                    api_response ~title:"Internal Server Error" ~data:err
                      `Internal_server_error req)
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
        let csrf = generate_csrf_token env req in
        page_response
          ~headers:[ ("X-MOLLY-CSRF", csrf) ]
          (Dashboard.dashboard_layout ~csrf user ~page_title:"Users"
             ~content:
               (Users_index.users_index_layout (Storage.users store) (now ()))
             ())
          req server env)

  let view_user (page : [ `Profile | `Unikernels | `Policy ]) req server env =
    with_uuid_admin_page req server (fun uuid user ->
        let store = Vifu.Server.device storage server in
        let albatross_instances = !(Vifu.Server.device albatross server) in
        match Storage.find_by_uuid store uuid with
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
            let csrf = generate_csrf_token env req in
            let reply content =
              page_response
                ~headers:[ ("X-MOLLY-CSRF", csrf) ]
                (Dashboard.dashboard_layout ~csrf user
                   ~page_title:
                     (String.capitalize_ascii
                        (Models.Configuration.name_to_str u.name))
                   ~content ())
                req server env
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
            let store = Vifu.Server.device storage server in
            match Storage.find_by_uuid store uuid with
            | Some u -> (
                let user_policy =
                  Option.value ~default:Albatross_client.empty_policy
                    (Option.join
                       (Result.to_option
                          (Albatross_client.policy albatross_inst ~domain:u.name)))
                in
                match
                  Albatross_client.policy_resource_avalaible albatross_inst
                with
                | Ok unallocated_resources ->
                    let csrf = generate_csrf_token env req in
                    page_response
                      ~headers:[ ("X-MOLLY-CSRF", csrf) ]
                      (Dashboard.dashboard_layout ~csrf user
                         ~page_title:
                           (String.capitalize_ascii
                              (Models.Configuration.name_to_str u.name))
                         ~content:
                           (Update_policy.update_policy_layout u
                              albatross_inst.configuration.name ~user_policy
                              ~unallocated_resources)
                         ())
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
            let albatross_instances = !(Vifu.Server.device albatross server) in
            match
              Option.bind
                (Uuidm.of_string form.user_uuid)
                (Storage.find_by_uuid store)
            with
            | Some u -> (
                match
                  Models.Configuration.name_of_str form.albatross_instance
                with
                | Ok instance_name -> (
                    match
                      Albatross_client.find_instance_by_name albatross_instances
                        instance_name
                    with
                    | Ok albatross_inst -> (
                        match Models.Policy.to_policy form with
                        | Ok policy -> (
                            match Albatross_client.policy albatross_inst with
                            | Ok (Some root_policy) -> (
                                match
                                  Vmm_core.Policy.is_smaller ~super:root_policy
                                    ~sub:policy
                                with
                                | Error (`Msg err) ->
                                    Logs.err (fun m ->
                                        m
                                          "policy %a is not smaller than root \
                                           policy %a: %s"
                                          Vmm_core.Policy.pp policy
                                          Vmm_core.Policy.pp root_policy err);
                                    api_response ~title:"Bad Request"
                                      ~data:
                                        ("Policy is not smaller than root \
                                          policy: " ^ err)
                                      `Bad_request req
                                | Ok () -> (
                                    match
                                      Albatross_client.set_policy env.tcp
                                        albatross_inst ~domain:u.name policy
                                    with
                                    | Error err ->
                                        Logs.err (fun m ->
                                            m
                                              "error setting policy %a for %s: \
                                               %s"
                                              Vmm_core.Policy.pp policy
                                              (Models.Configuration.name_to_str
                                                 u.name)
                                              err);
                                        api_response
                                          ~title:"Internal Server Error"
                                          ~data:("error setting policy: " ^ err)
                                          `Internal_server_error req
                                    | Ok _policy ->
                                        api_response ~title:"Success"
                                          ~data:"Updated policy successfully"
                                          `OK req))
                            | Ok None ->
                                Logs.err (fun m ->
                                    m "policy: root policy can't be null ");
                                api_response ~title:"Internal Server Error"
                                  ~data:"Root policy is null"
                                  `Internal_server_error req
                            | Error err ->
                                Logs.err (fun m ->
                                    m
                                      "policy: an error occured while fetching \
                                       root policy: %s"
                                      err);
                                api_response ~title:"Internal Server Error"
                                  ~data:("error with root policy: " ^ err)
                                  `Internal_server_error req)
                        | Error (`Msg err) ->
                            api_response ~title:"Bad Request" ~data:err
                              `Bad_request req)
                    | Error _ ->
                        Logs.err (fun m ->
                            m "Couldn't find albatross instance with name %s"
                              (Models.Configuration.name_to_str instance_name));
                        api_response ~title:"Not Found"
                          ~data:
                            ("Couldn't find albatross instance with name: "
                            ^ Models.Configuration.name_to_str instance_name)
                          `Not_found req)
                | Error (`Msg err) ->
                    api_response ~title:"Bad Request"
                      ~data:
                        ("Couldn't convert name to albatross instance: " ^ err)
                      `Bad_request req)
            | None ->
                api_response ~title:"Not Found" ~data:"User not found"
                  `Not_found req))

  let toggle_account_attribute (form : Models.Admin_user.t) store env req ~key
      update_fn error_on_last ~error_message =
    match
      Option.bind (Uuidm.of_string form.uuid) (Storage.find_by_uuid store)
    with
    | None ->
        Logs.warn (fun m -> m "%s : Account not found" key);
        api_response ~title:"Not Found" ~data:"Account not found" `Not_found req
    | Some user -> (
        if error_on_last user then (
          Logs.warn (fun m -> m "%s : Can't perform action on last user" key);
          api_response ~title:"Forbidden" ~data:error_message `Forbidden req)
        else
          let updated_user = update_fn user in
          Storage.update_user store updated_user;
          match Store.write_data store env.block with
          | Ok () ->
              api_response ~title:"Success" ~data:"Updated user successfully"
                `OK req
          | Error (`Msg msg) ->
              Logs.warn (fun m -> m "%s : Storage error with %s" key msg);
              api_response ~title:"Internal Server Error" ~data:msg
                `Internal_server_error req)

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
            | Some user -> (
                Storage.delete_user store user;
                Session.remove_user_sessions user.uuid;
                match Store.write_data store env.block with
                | Ok () ->
                    api_response ~title:"Success"
                      ~data:"Deleted user successfully" `OK req
                | Error (`Msg msg) ->
                    Logs.warn (fun m ->
                        m "delete-user : Storage error with %s" msg);
                    api_response ~title:"Internal Server Error" ~data:msg
                      `Internal_server_error req)))
end

module Scheduler = struct
  let scaling_groups :
      Autoscaler.Cluster_manager.group Utils.LM.t Utils.LM.t ref =
    ref Utils.LM.empty

  module Log = (val Logs.src_log Autoscaler.a_logs : Logs.LOG)

  let least_used_cpuid tcp albatross_inst user_name =
    let unikernels =
      Albatross_client.user_unikernels_by_instance tcp albatross_inst user_name
    in
    match Albatross_client.policy ~domain:user_name albatross_inst with
    | Error err -> Error err
    | Ok None -> Error "No policy found for user"
    | Ok (Some p) -> (
        let cpu_usage_count = Utils.cpu_usage_count p unikernels in
        let sorted_cpus =
          cpu_usage_count
          |> List.sort (fun (_, c1) (_, c2) -> Int.compare c1 c2)
        in
        match sorted_cpus with
        | (id, _) :: _ -> Ok id
        | [] -> Error "No cpuids available in user policy")

  let put_group ~(user_name : Vmm_core.Name.Label.t)
      ~(unikernel_name : Vmm_core.Name.Label.t) group =
    let unikernel_map =
      Option.value ~default:Utils.LM.empty
        (Utils.LM.find_opt user_name !scaling_groups)
    in
    scaling_groups :=
      Utils.LM.add user_name
        (Utils.LM.add unikernel_name group unikernel_map)
        !scaling_groups

  let spawn_clone tcp store albatross_inst ~unikernel_name ~clone_name
      ~user_name group scaler =
    match Storage.find_by_name store user_name with
    | None -> Error "User not found"
    | Some user -> (
        match
          Albatross_client.user_unikernel tcp albatross_inst ~user_name
            ~unikernel_name
        with
        | Error err ->
            Error
              (Fmt.str "An error occured trying to fetch %s from albatross: %s"
                 (Models.Configuration.name_to_str unikernel_name)
                 err)
        | Ok (_, unikernel) -> (
            if unikernel.block_devices <> [] then (
              Log.info (fun m ->
                  m
                    "spawn_clone: Aborting clone of %s because it has block \
                     devices."
                    (Models.Configuration.name_to_str unikernel_name));
              Error "Unikernels with block devices cannot be safely cloned")
            else
              let bridges =
                List.map
                  (fun { Vmm_core.Unikernel.unikernel_device; host_device; _ }
                     -> (unikernel_device, Some host_device, None))
                  unikernel.bridges
              in
              match least_used_cpuid tcp albatross_inst user_name with
              | Error err -> Error err
              | Ok cpuid -> (
                  let argv =
                    Option.map
                      (fun args ->
                        List.map
                          (fun arg ->
                            String.split_on_char ' ' arg
                            |> List.filter (fun s ->
                                not (String.starts_with ~prefix:"--name=" s))
                            |> String.concat " ")
                          args
                        |> List.filter (fun s -> s <> ""))
                      unikernel.argv
                  in
                  let config : Vmm_core.Unikernel.config =
                    {
                      typ = unikernel.typ;
                      fail_behaviour = unikernel.fail_behaviour;
                      startup = unikernel.startup;
                      memory = unikernel.memory;
                      block_devices = [];
                      add_name = true;
                      bridges;
                      argv;
                      numcpus = unikernel.numcpus;
                      linux_boot_partition = unikernel.linux_boot_partition;
                      compressed = true;
                      image = "";
                      cpuids = Vmm_core.IS.singleton cpuid;
                    }
                  in
                  let buf = Buffer.create 65536 in
                  match
                    Albatross_client.query_unikernel_binary tcp albatross_inst
                      ~domain:user_name ~name:unikernel_name (fun binary_str ->
                        Buffer.add_string buf binary_str;
                        Ok ())
                  with
                  | Error err ->
                      put_group ~user_name ~unikernel_name
                        (Autoscaler.Cluster_manager.remove_cooldown group);
                      Log.err (fun m ->
                          m "Error getting binary for %s: %s"
                            (Models.Configuration.name_to_str unikernel_name)
                            err);
                      Error err
                  | Ok () -> (
                      let push = make_string_stream (Buffer.contents buf) in
                      match
                        Unikernel.force_create_unikernel tcp albatross_inst
                          ~unikernel_name:clone_name ~push config user
                      with
                      | Error (`Msg err, _status) ->
                          put_group ~user_name ~unikernel_name
                            (Autoscaler.Cluster_manager.remove_cooldown group);
                          Error err
                      | Ok () -> (
                          match
                            Autoscaler.Cluster_manager.register_clone group
                              (clone_name, scaler)
                          with
                          | Error e ->
                              Log.err (fun m ->
                                  m "Error registering clone %s: %s"
                                    (Models.Configuration.name_to_str clone_name)
                                    e);
                              Error
                                (Fmt.str "Error registering clone %s: %s"
                                   (Models.Configuration.name_to_str clone_name)
                                   e)
                          | Ok spawn_group ->
                              Log.debug (fun m ->
                                  m "Successfully cloned %s to %s"
                                    (Models.Configuration.name_to_str
                                       unikernel_name)
                                    (Models.Configuration.name_to_str clone_name));
                              put_group ~user_name ~unikernel_name spawn_group;
                              Ok ())))))

  let prune_clone tcp albatross_inst group ~unikernel_name ~clone_to_kill
      ~user_name =
    match
      Albatross_client.query tcp albatross_inst ~domain:user_name
        ~name:clone_to_kill (`Unikernel_cmd `Unikernel_destroy)
    with
    | Error err ->
        Log.err (fun m ->
            m "Error pruning unikernel %s: %s"
              (Models.Configuration.name_to_str clone_to_kill)
              err);
        Error err
    | Ok (_hdr, res) -> (
        Albatross_client.set_online albatross_inst;
        match Albatross_json.res res with
        | Ok _res -> (
            match
              Autoscaler.Cluster_manager.remove_clone group clone_to_kill
            with
            | Error e ->
                Log.err (fun m ->
                    m "Error removing clone %s: %s"
                      (Models.Configuration.name_to_str clone_to_kill)
                      e);
                Error e
            | Ok post_prune_group ->
                Log.debug (fun m ->
                    m "Succesfully pruned %s"
                      (Models.Configuration.name_to_str clone_to_kill));
                put_group ~user_name ~unikernel_name post_prune_group;
                Ok ())
        | Error (`String err) ->
            Log.err (fun m ->
                m "Error pruning unikernel %s: %s"
                  (Models.Configuration.name_to_str clone_to_kill)
                  err);
            Error err)

  let evaluate_scaling tcp store albatross_inst group current_time user_name
      unikernel_name =
    match Autoscaler.Cluster_manager.evaluate_scaling group current_time with
    | Error err ->
        Error
          (Fmt.str "Error evaluating scaling for cluster %s/%s: %s"
             (Models.Configuration.name_to_str user_name)
             (Models.Configuration.name_to_str unikernel_name)
             err)
    | Ok (status, group) -> (
        put_group ~user_name ~unikernel_name group;
        match status with
        | Autoscaler.Normal _scaler -> Ok ()
        | Autoscaler.Pending (`Spawn, ticks, scaler) ->
            Log.info (fun m ->
                m "%s/%s: high usage (%d%%), [ticks: %d/%d]"
                  (Models.Configuration.name_to_str user_name)
                  (Models.Configuration.name_to_str unikernel_name)
                  scaler.last_cpu_usage ticks Autoscaler.scale_up_trigger_ticks);
            Ok ()
        | Autoscaler.Cooldown scaler ->
            Log.info (fun m ->
                m "%s/%s: cooldown (usage: %d%%)"
                  (Models.Configuration.name_to_str user_name)
                  (Models.Configuration.name_to_str unikernel_name)
                  scaler.last_cpu_usage);
            Ok ()
        | Autoscaler.Pending (`Prune, ticks, scaler) ->
            Log.info (fun m ->
                m "%s/%s: low usage (%d%%), [ticks: %d/%d]"
                  (Models.Configuration.name_to_str user_name)
                  (Models.Configuration.name_to_str unikernel_name)
                  scaler.last_cpu_usage ticks
                  Autoscaler.scale_down_trigger_ticks);
            Ok ()
        | Autoscaler.Overloaded scaler -> (
            match Autoscaler.Cluster_manager.next_clone_name group with
            | Error (`Msg e) -> Error (Fmt.str "Error: %s" e)
            | Ok next_name ->
                Log.info (fun m ->
                    m "%s/%s: overloaded (%d%%), spawning new clone: %s"
                      (Models.Configuration.name_to_str user_name)
                      (Models.Configuration.name_to_str unikernel_name)
                      scaler.last_cpu_usage
                      (Models.Configuration.name_to_str next_name));
                spawn_clone tcp store albatross_inst ~unikernel_name
                  ~clone_name:next_name ~user_name group scaler)
        | Autoscaler.Underloaded (clone_to_kill, scaler) ->
            Log.info (fun m ->
                m "[%s/%s]: underloaded (%d%%), pruning: %s"
                  (Models.Configuration.name_to_str user_name)
                  (Models.Configuration.name_to_str unikernel_name)
                  scaler.last_cpu_usage
                  (Models.Configuration.name_to_str clone_to_kill));
            prune_clone tcp albatross_inst group ~unikernel_name ~clone_to_kill
              ~user_name)

  let handle_stats tcp state ((rusage, _, _) : Vmm_core.Stats.t) name store =
    match Vmm_core.Name.name name with
    | None -> Error "VM name has no unikernel label"
    | Some unikernel_name -> (
        match Vmm_core.Name.Path.to_labels (Vmm_core.Name.path name) with
        | [ user_label ] -> (
            match Storage.find_by_name store user_label with
            | None ->
                Error
                  (Fmt.str "User %s not found."
                     (Models.Configuration.name_to_str user_label))
            | Some user ->
                let primary_name =
                  Option.value ~default:unikernel_name
                    (Option.map fst
                       (Autoscaler.Cluster_manager.extract_name_and_clone_id
                          unikernel_name))
                in
                let is_scaling_enabled =
                  Models.User.Scaling_policy_map.exists
                    (fun (p_name, _) _ ->
                      Vmm_core.Name.Label.equal p_name primary_name)
                    user.scaling_policies
                in
                if is_scaling_enabled then (
                  Log.debug (fun m ->
                      m "[Stats] Received stats for %s as %a"
                        (Models.Configuration.name_to_str unikernel_name)
                        Vmm_core.Stats.pp_rusage rusage);
                  let current_time = now () in
                  let group =
                    let unikernel_map =
                      Option.value ~default:Utils.LM.empty
                        (Utils.LM.find_opt user.name !scaling_groups)
                    in
                    match Utils.LM.find_opt primary_name unikernel_map with
                    | Some g -> g
                    | None ->
                        let primary_vm =
                          (primary_name, Autoscaler.create current_time rusage)
                        in
                        Autoscaler.Cluster_manager.create_group primary_vm
                  in
                  let group =
                    if
                      (not
                         (Vmm_core.Name.Label.equal unikernel_name primary_name))
                      && not (List.mem_assoc unikernel_name group.clones)
                    then (
                      let clone_vm =
                        (unikernel_name, Autoscaler.create current_time rusage)
                      in
                      match
                        Autoscaler.Cluster_manager.register_clone group clone_vm
                      with
                      | Ok updated_g -> updated_g
                      | Error err ->
                          Log.err (fun m ->
                              m "Error registering clone %s: %s"
                                (Models.Configuration.name_to_str unikernel_name)
                                err);
                          group)
                    else group
                  in
                  match
                    Autoscaler.Cluster_manager.update_vm_metrics group
                      unikernel_name current_time rusage
                  with
                  | Error err ->
                      Error
                        (Fmt.str "Error updating metrics for unikernel %s: %s"
                           (Models.Configuration.name_to_str unikernel_name)
                           err)
                  | Ok (_, updated_group) ->
                      put_group ~user_name:user.name
                        ~unikernel_name:primary_name updated_group;
                      if Vmm_core.Name.Label.equal unikernel_name primary_name
                      then
                        evaluate_scaling tcp store state updated_group
                          current_time user.name primary_name
                      else Ok ())
                else Ok ())
        | _ -> Error "VM path must contain exactly one user domain level")

  let unikernels_stats tcp (instance : Albatross_client.t) store =
    let cb name st =
      (match handle_stats tcp instance st name store with
      | Ok () -> ()
      | Error err ->
          Log.err (fun m ->
              m "Error handling stats for %s: %s"
                (Vmm_core.Name.to_string name)
                err));
      Ok ()
    in
    match Albatross_client.query_stats tcp instance cb with
    | Error err ->
        Error
          (Fmt.str "Error fetching stats for albatross instance %s: %s"
             (Models.Configuration.name_to_str instance.configuration.name)
             err)
    | Ok () -> Ok ()

  let seconds_until_next_midnight () =
    let current_time = now () in
    let _, ((hour, min, sec), _) = Ptime.to_date_time current_time in
    let passed = (hour * 3600) + (min * 60) + sec in
    86_400 - passed

  let check_user_unikernel_updates tcp albatross_instances
      (user : Models.User.user) he =
    let unikernels_per_instance =
      Albatross_client.user_unikernels tcp albatross_instances user.name
    in
    let results =
      List.map
        (fun (instance_name, unikernels) ->
          let updates =
            List.filter_map
              (fun (name, unikernel) ->
                match Update_flow.check_for_update name unikernel he with
                | Ok
                    (Update_flow.Update_available (name, unikernel, comparison))
                  ->
                    Some (name, unikernel, comparison)
                | _ -> None)
              unikernels
          in
          (instance_name, updates))
        unikernels_per_instance
    in
    let available_updates =
      List.filter (fun (_, updates) -> updates <> []) results
    in
    { Update_flow.user; available_updates }

  let check_available_unikernel_updates tcp albatross_instances
      (users : Models.User.user Utils.UM.t) he =
    Utils.UM.fold
      (fun _ user acc ->
        let update_report =
          check_user_unikernel_updates tcp albatross_instances user he
        in
        if update_report.available_updates = [] then acc
        else update_report :: acc)
      users []

  let run_background_update_check he users tcp email_config albatross_instances
      =
    match email_config with
    | None ->
        Logs.info (fun m ->
            m "Email not configured, skipping background update check")
    | Some email_config -> (
        try
          Logs.info (fun m ->
              m "Starting background update check for all users' unikernels");
          let reports =
            check_available_unikernel_updates tcp albatross_instances users he
          in
          List.iter
            (fun (report : Update_flow.user_unikernel_available_updates) ->
              let references =
                Fmt.str "updates-%s@robur.coop"
                  (Uuidm.to_string report.user.uuid)
              in
              match
                Email_client.send_email ~references he email_config
                  report.user.email ~subject:"Unikernel updates available"
                  ~body:(Email_templates.updated_unikernels report email_config)
              with
              | Ok () ->
                  Logs.info (fun m ->
                      m "Sent update report email to %s"
                        (Emile.to_string report.user.email))
              | Error err ->
                  Logs.err (fun m ->
                      m "Failed to send update report email to %s: %s"
                        (Emile.to_string report.user.email)
                        err))
            reports
        with exn ->
          Logs.err (fun m ->
              m "Background update check failed: %s" (Printexc.to_string exn)))

  let start_background_scheduler he tcp store albatross_instances_ref =
    let rec loop () =
      let delay = seconds_until_next_midnight () in
      Logs.info (fun m -> m "Next background update in %d seconds" delay);
      Mkernel.sleep (Int64.to_int (Duration.of_sec delay));
      (try
         Logs.info (fun m -> m "Starting background update...");
         run_background_update_check he (Storage.users store) tcp
           (Storage.email store) !albatross_instances_ref
       with exn ->
         Logs.err (fun m ->
             m "Background update failed: %s" (Printexc.to_string exn)));
      loop ()
    in
    loop ()

  let start_background_scaler_scheduler tcp store albatross_instances_ref =
    let active_streams = ref Vmm_core.String_set.empty in
    let orphans = Miou.orphans () in
    let rec clean_orphans () =
      match Miou.care orphans with
      | Some (Some prm) ->
          let _ = Miou.await prm in
          clean_orphans ()
      | _ -> ()
    in
    let spawn_stats_stream key (instance : Albatross_client.t) =
      Fun.protect
        ~finally:(fun () ->
          active_streams := Vmm_core.String_set.remove key !active_streams)
        (fun () ->
          let rec stream_loop () =
            let res =
              try Ok (unikernels_stats tcp instance store)
              with exn -> Error exn
            in
            match res with
            | Ok (Ok ()) ->
                Mkernel.sleep (Int64.to_int (Duration.of_sec 10));
                (stream_loop [@tailcall]) ()
            | Ok (Error err) ->
                Log.err (fun m ->
                    m "Stats stream for %s failed: %s"
                      (Models.Configuration.name_to_str
                         instance.configuration.name)
                      err)
            | Error exn ->
                Log.err (fun m ->
                    m "Exception in stats stream for %s: %s. Retrying in 10s..."
                      (Models.Configuration.name_to_str
                         instance.configuration.name)
                      (Printexc.to_string exn));
                Mkernel.sleep (Int64.to_int (Duration.of_sec 10));
                (stream_loop [@tailcall]) ()
          in
          stream_loop ())
    in
    let rec loop () =
      clean_orphans ();
      let current_instances = !albatross_instances_ref in
      Utils.LM.iter
        (fun instance_name instance ->
          let key = Models.Configuration.name_to_str instance_name in
          if not (Vmm_core.String_set.mem key !active_streams) then (
            Log.debug (fun m ->
                m "Spawning new stats stream for albatross instance %s" key);
            active_streams := Vmm_core.String_set.add key !active_streams;
            ignore
              (Miou.async ~orphans (fun () -> spawn_stats_stream key instance))))
        current_instances;
      Mkernel.sleep (Int64.to_int (Duration.of_sec 30));
      loop ()
    in
    loop ()

  let run_prune_dead_clusters tcp albatross_instances_ref =
    let rec loop () =
      Log.debug (fun m ->
          m "Pruning dead clusters in %a" Ptime.Span.pp Autoscaler.death_timeout);
      let sleep_ns =
        Int64.to_int
          (Duration.of_sec
             (Option.value ~default:600
                (Ptime.Span.to_int_s Autoscaler.death_timeout)))
      in
      (try
         Log.debug (fun m -> m "Starting background pruning...");
         Utils.LM.bindings !scaling_groups
         |> List.iter (fun (user_name, unikernel_map) ->
             let results =
               Albatross_client.user_unikernels tcp !albatross_instances_ref
                 user_name
             in
             let active_vm_names =
               List.fold_left
                 (fun acc (_instance_name, vms) ->
                   let names =
                     List.filter_map
                       (fun (name, _info) -> Vmm_core.Name.name name)
                       vms
                   in
                   acc @ names)
                 [] results
             in
             Utils.LM.iter
               (fun unikernel_name group ->
                 match
                   Autoscaler.Cluster_manager.sync_group group active_vm_names
                 with
                 | Ok updated_group ->
                     put_group ~user_name ~unikernel_name updated_group
                 | Error `Primary_dead ->
                     Log.warn (fun m ->
                         m "Primary dead for %s/%s. Removing cluster."
                           (Models.Configuration.name_to_str user_name)
                           (Models.Configuration.name_to_str unikernel_name));
                     let updated_unikernel_map =
                       Utils.LM.remove unikernel_name unikernel_map
                     in
                     if Utils.LM.is_empty updated_unikernel_map then
                       scaling_groups :=
                         Utils.LM.remove user_name !scaling_groups
                     else
                       scaling_groups :=
                         Utils.LM.add user_name updated_unikernel_map
                           !scaling_groups)
               unikernel_map)
       with exn ->
         Log.err (fun m ->
             m "Background pruning failed: %s" (Printexc.to_string exn)));
      Mkernel.sleep sleep_ns;
      loop ()
    in
    loop ()

  let device : (env, unit Miou.t * unit Miou.t * unit Miou.t) Vifu.Device.device
      =
    let finally (p1, p2, p3) =
      Miou.cancel p1;
      Miou.cancel p2;
      Miou.cancel p3
    in
    let open Vifu.Device in
    v ~name:"scheduler" ~finally
      [ value storage; value happy_eyeballs; value albatross ]
    @@ fun store he albatross_instances_ref env ->
    let p1 =
      Miou.async (fun () : unit ->
          start_background_scheduler he env.tcp store albatross_instances_ref)
    in
    let p2 =
      Miou.async (fun () : unit ->
          start_background_scaler_scheduler env.tcp store
            albatross_instances_ref)
    in
    let p3 =
      Miou.async (fun () : unit ->
          run_prune_dead_clusters env.tcp albatross_instances_ref)
    in
    (p1, p2, p3)
end
