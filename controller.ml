type env = {
  tcp : Mnet.TCP.state;
  udp : Mnet.UDP.state;
  m_tcp : Mnet.TCP.state;
  m_udp : Mnet.UDP.state;
  m_tcp : Mnet.TCP.state;
  m_udp : Mnet.UDP.state;
  block : Mkernel.Block.t;
  csrf_key : string;
}

let storage =
  let finally = Fun.const () in
  Vifu.Device.v ~name:"store" ~finally [] @@ fun { block; _ } ->
  Store.connect block

let page_response ty req (_server : Vifu.Server.t) _env =
  let open Vifu.Response.Syntax in
  let* () = Vifu.Response.with_tyxml req ty in
  Vifu.Response.respond `OK

let api_response ~title ~data http_status req =
  let open Vifu.Response.Syntax in
  let status = Utils.Status.make title data in
  let* () = Vifu.Response.with_json req Utils.Status.to_json status in
  Vifu.Response.respond http_status

let static_asset_response ~mime ~content req (_server : Vifu.Server.t) _env =
  let open Vifu.Response.Syntax in
  (* TODO: if-none-match / etag *)
  let* () = Vifu.Response.add ~field:"Content-Type" mime in
  let* () = Vifu.Response.with_string req content in
  Vifu.Response.respond `OK

module Session = struct
  type t = { mutable sessions : (Uuidm.t * User_model.cookie) Utils.SM.t }
  type auth_user = { user_uuid : Uuidm.t; cookie : User_model.cookie }

  let session_cookie = "molly_session"
  let state = { sessions = Utils.SM.empty }

  let add_session user_uuid (cookie : User_model.cookie) =
    state.sessions <-
      Utils.SM.add cookie.value (user_uuid, cookie) state.sessions

  let remove_session cookie_value =
    state.sessions <- Utils.SM.remove cookie_value state.sessions

  let find_session cookie_value = Utils.SM.find_opt cookie_value state.sessions

  let update_cookie_usage (cookie : User_model.cookie) user_agent now =
    let cookie = { cookie with user_agent; last_access = now } in
    state.sessions <-
      Utils.SM.update cookie.value
        (function Some (uuid, _) -> Some (uuid, cookie) | None -> None)
        state.sessions

  let user_sessions user_uuid =
    Utils.SM.fold
      (fun _ (uuid, (cookie : User_model.cookie)) acc ->
        if Uuidm.equal uuid user_uuid then Utils.SM.add cookie.value cookie acc
        else acc)
      state.sessions Utils.SM.empty

  let close_other_sessions user_uuid active_cookie_value =
    state.sessions <-
      Utils.SM.filter
        (fun _ (uuid, (cookie : User_model.cookie)) ->
          (not (Uuidm.equal uuid user_uuid))
          || String.equal cookie.value active_cookie_value)
        state.sessions

  let prune_expired_sessions now =
    state.sessions <-
      Utils.SM.filter
        (fun _ (_, (cookie : User_model.cookie)) ->
          User_model.is_valid_cookie cookie now)
        state.sessions

  let remove_user_sessions user_uuid =
    state.sessions <-
      Utils.SM.filter
        (fun _ (uuid, _) -> not (Uuidm.equal uuid user_uuid))
        state.sessions

  let reset_state () = state.sessions <- Utils.SM.empty

  let session : (env, auth_user) Vifu.Middleware.t =
    Vifu.Middlewares.v ~name:session_cookie @@ fun req _target server _env ->
    match Vifu.Cookie.get ~name:session_cookie server req with
    | Error _ -> None
    | Ok cookie ->
        find_session cookie
        |> Option.map (fun (user_uuid, cookie) -> { user_uuid; cookie })

  let validate_user_session req storage server fn =
    let store = Vifu.Server.device storage server in
    match Vifu.Request.get session req with
    | None -> Error "No molly-session in cookie header"
    | Some { user_uuid; cookie } -> (
        match Utils.UM.find_opt user_uuid store.Storage.users with
        | None -> Error "User not found"
        | Some user ->
            if User_model.is_valid_cookie cookie (Mkernel.now ()) then (
              update_cookie_usage cookie
                (Vifu.Headers.get (Vifu.Request.headers req) "User-Agent")
                (Mkernel.now ());
              fn user)
            else Error "Cookie expired.")
end

module Auth = struct
  let authenticate_user_with_cookie storage server req =
    let user_is_active (user : User_model.user) =
      if user.active then Ok user else Error "User account is deactivated"
    in
    Session.validate_user_session req storage server user_is_active

  let authenticate_admin_with_cookie storage server req =
    let user_is_admin (user : User_model.user) =
      if user.super_user then Ok user else Error "User is not an administrator"
    in
    Session.validate_user_session req storage server user_is_admin

  (* new users have no sessions yet *)
  let sign_up_page req server env =
    let csrf =
      Csrf.generate_token ~key:env.csrf_key ~session_cookie:env.csrf_key
    in
    page_response
      (Guest_layout.guest_layout ~page_title:"Sign up"
         (Sign_up.register_page csrf))
      req server env

  let sign_in_page req server env =
    let csrf =
      Csrf.generate_token ~key:env.csrf_key ~session_cookie:env.csrf_key
    in
    page_response
      (Guest_layout.guest_layout ~page_title:"Sign in" (Sign_in.login_page csrf))
      req server env

  let register_user req server env =
    let storage = Vifu.Server.device storage server in
    let user_agent = Vifu.Headers.get (Vifu.Request.headers req) "User-Agent" in
    match Vifu.Request.of_multipart_form req with
    | Ok ({ name; email; password } : User_model.registration_form_data) -> (
        match
          (Utils.Email.validate_email email, Configuration.name_of_str name)
        with
        | Error (`Msg err), _ ->
            api_response ~title:"Failure"
              ~data:("Incorrect email: " ^ err)
              `Bad_request req
        | _, Error (`Msg err) ->
            api_response ~title:"Failure" ~data:("Incorrect name: " ^ err)
              `Bad_request req
        | Ok email, Ok name -> (
            match
              ( Storage.find_by_email storage email,
                Storage.find_by_name storage name )
            with
            | Some _email, _ ->
                api_response ~title:"Failure" ~data:"This email has been taken."
                  `Bad_request req
            | _, Some _name ->
                api_response ~title:"Failure" ~data:"This name has been taken"
                  `Bad_request req
            | None, None ->
                if User_model.password_validation password then begin
                  let super_user, active =
                    if Storage.count_users storage > 1 then (false, false)
                    else (true, true)
                  in
                  let user, cookie =
                    User_model.create_user ~name ~email ~password
                      ~created_at:(Mkernel.now ()) ~super_user ~active
                      ~user_agent
                  in
                  match Store.write_data storage env.block with
                  | Ok () ->
                      Storage.add_user storage user;
                      Session.add_session user.uuid cookie;
                      api_response ~title:"Success"
                        ~data:"User account created succesfully." `OK req
                  | Error (`Msg err) ->
                      Storage.delete_user storage user;
                      Session.remove_session cookie.value;
                      api_response ~title:"Failure"
                        ~data:("Failed to create user: " ^ err)
                        `Internal_server_error req
                end
                else
                  api_response ~title:"Failure"
                    ~data:"Password must be at least 8 characters." `Bad_request
                    req))
    | _ ->
        api_response ~title:"Failure" ~data:"Invalid multipart form data"
          `Bad_request req
end
