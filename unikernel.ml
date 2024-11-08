open Lwt.Infix

type images = {
  molly_img : string;
  robur_img : string;
  albatross_img : string;
  mirage_img : string;
  dashboard_img : string;
}

module Main
    (R : Mirage_crypto_rng_mirage.S)
    (P : Mirage_clock.PCLOCK)
    (M : Mirage_clock.MCLOCK)
    (T : Mirage_time.S)
    (S : Tcpip.Stack.V4V6)
    (KV_ASSETS : Mirage_kv.RO)
    (BLOCK : Mirage_block.S) =
struct
  module Paf = Paf_mirage.Make (S.TCP)

  let js_contents assets =
    KV_ASSETS.get assets (Mirage_kv.Key.v "main.js") >|= function
    | Error _e -> invalid_arg "JS file could not be loaded"
    | Ok js -> js

  let css_contents assets =
    KV_ASSETS.get assets (Mirage_kv.Key.v "style.css") >|= function
    | Error _e -> invalid_arg "CSS file could not be loaded"
    | Ok css -> css

  let read_image assets key =
    KV_ASSETS.get assets (Mirage_kv.Key.v key) >|= function
    | Error _e -> invalid_arg "Image could not be loaded"
    | Ok img -> img

  let images assets =
    let molly_img = read_image assets "molly_bird.jpeg" in
    let robur_img = read_image assets "robur.png" in
    let albatross_img = read_image assets "albatross_1.png" in
    let mirage_img = read_image assets "mirage_os_1.png" in
    let dashboard_img = read_image assets "dashboard_1.png" in
    Lwt.all [ molly_img; robur_img; albatross_img; mirage_img; dashboard_img ]
    >|= function
    | [ molly_img; robur_img; albatross_img; mirage_img; dashboard_img ] ->
        { molly_img; robur_img; albatross_img; mirage_img; dashboard_img }
    | _ -> failwith "Unexpected number of images"

  module Store = Storage.Make (BLOCK)
  module Map = Map.Make (String)

  let generate_csrf_token store user now reqd =
    let csrf = Middleware.generate_csrf_cookie now reqd in
    let updated_user =
      User_model.update_user user ~updated_at:now
        ~cookies:(csrf :: user.cookies) ()
    in
    Store.update_user store updated_user >>= function
    | Ok () -> Lwt.return (Ok csrf.value)
    | Error (`Msg err) ->
        let error =
          {
            Utils.Status.code = 500;
            title = "CSRF Token Error";
            success = false;
            data = err;
          }
        in
        Lwt.return (Error error)

  let decode_request_body reqd =
    let request_body = Httpaf.Reqd.request_body reqd in
    let finished, notify_finished = Lwt.wait () in
    let wakeup v = Lwt.wakeup_later notify_finished v in
    let on_eof data () = wakeup data in
    let f acc s = acc ^ s in
    let rec on_read on_eof acc bs ~off ~len =
      let str = Bigstringaf.substring ~off ~len bs in
      let acc = acc >>= fun acc -> Lwt.return (f acc str) in
      Httpaf.Body.schedule_read request_body ~on_read:(on_read on_eof acc)
        ~on_eof:(on_eof acc)
    in
    let f_init = Lwt.return "" in
    Httpaf.Body.schedule_read request_body ~on_read:(on_read on_eof f_init)
      ~on_eof:(on_eof f_init);
    finished >>= fun data -> data

  let extract_csrf_token reqd =
    decode_request_body reqd >>= fun data ->
    match
      try Ok (Yojson.Basic.from_string data)
      with Yojson.Json_error s -> Error (`Msg s)
    with
    | Error (`Msg err) ->
        Logs.warn (fun m -> m "Failed to parse JSON: %s" err);
        Lwt.return (Error (`Msg err))
    | Ok (`Assoc json_dict) -> (
        match Utils.Json.get User_model.csrf_cookie json_dict with
        | Some (`String token) -> Lwt.return (Ok (token, json_dict))
        | _ -> Lwt.return (Error (`Msg "Couldn't find CSRF token")))
    | Ok _ ->
        Logs.warn (fun m -> m "JSON is not a dictionary: %s" data);
        Lwt.return (Error (`Msg "not a dictionary"))

  module Albatross = Albatross.Make (T) (P) (S)

  let to_map ~assoc m =
    let open Multipart_form in
    let rec go (map, rest) = function
      | Leaf { header; body } -> (
          let filename =
            Option.bind
              (Header.content_disposition header)
              Content_disposition.filename
          in
          match
            Option.bind
              (Header.content_disposition header)
              Content_disposition.name
          with
          | Some name ->
              (Map.add name (filename, List.assoc body assoc) map, rest)
          | None -> (map, (body, (filename, List.assoc body assoc)) :: rest))
      | Multipart { body; _ } ->
          let fold acc = function Some elt -> go acc elt | None -> acc in
          List.fold_left fold (map, rest) body
    in
    go (Map.empty, []) m

  let authenticate ?(email_verified = true) ?(check_admin = false)
      ?(api_meth = false) ?form_csrf store reqd f =
    let now = Ptime.v (P.now_d_ps ()) in
    match Middleware.session_cookie_value reqd with
    | Error (`Msg err) ->
        Logs.err (fun m ->
            m "auth-middleware: No molly-session in cookie header. %s" err);
        Middleware.redirect_to_page ~path:"/sign-in" ~clear_session:true
          ~with_error:true ~msg:"No session cookie found in request." reqd ()
    | Ok cookie_value -> (
        match Store.find_by_cookie store cookie_value with
        | None ->
            Logs.err (fun m ->
                m "auth-middleware: Failed to find user with key %s"
                  cookie_value);
            Middleware.redirect_to_page ~path:"/sign-in" ~clear_session:true
              ~with_error:true ~msg:"No user account found." reqd ()
        | Some (user, cookie) ->
            if not (User_model.is_valid_cookie cookie now) then (
              Logs.err (fun m ->
                  m
                    "auth-middleware: Session value doesn't match user session \
                     %s"
                    cookie_value);
              Middleware.redirect_to_page ~path:"/sign-in" ~clear_session:true
                ~with_error:true ~msg:"Session cookie is no longer valid." reqd
                ())
            else
              let middlewares =
                (if check_admin then
                   [ Middleware.is_user_admin_middleware api_meth user ]
                 else [])
                @ (if email_verified && false (* TODO *) then
                     [ Middleware.email_verified_middleware user ]
                   else [])
                @ Option.fold ~none:[]
                    ~some:(fun csrf ->
                      [ Middleware.csrf_verification user now csrf ])
                    form_csrf
                @ [ Middleware.auth_middleware user ]
              in
              Middleware.apply_middleware middlewares
                (fun reqd ->
                  let cookie =
                    { cookie with user_agent = Middleware.user_agent reqd }
                  in
                  let cookies =
                    List.map
                      (fun (cookie' : User_model.cookie) ->
                        if String.equal cookie_value cookie'.value then cookie
                        else cookie')
                      user.cookies
                  in
                  let updated_user = User_model.update_user user ~cookies () in
                  Store.update_user store updated_user >>= function
                  | Ok () -> f user
                  | Error (`Msg err) ->
                      Logs.err (fun m -> m "Error with storage: %s" err);
                      Middleware.http_response reqd ~title:"Error" ~data:err
                        `Not_found)
                reqd)

  let reply reqd ?(content_type = "text/plain") ?(header_list = []) data status
      =
    let h =
      Httpaf.Headers.of_list
        [
          ("content-length", string_of_int (String.length data));
          ("content-type", content_type);
        ]
    in
    let headers = Httpaf.Headers.add_list h header_list in
    let resp = Httpaf.Response.create ~headers status in
    Httpaf.Reqd.respond_with_string reqd resp data

  let read_multipart_data reqd =
    let response_body = Httpaf.Reqd.request_body reqd in
    let finished, notify_finished = Lwt.wait () in
    let wakeup v = Lwt.wakeup_later notify_finished v in
    let on_eof data () = wakeup data in
    let f acc s = acc ^ s in
    let rec on_read on_eof acc bs ~off ~len =
      let str = Bigstringaf.substring ~off ~len bs in
      let acc = acc >>= fun acc -> Lwt.return (f acc str) in
      Httpaf.Body.schedule_read response_body ~on_read:(on_read on_eof acc)
        ~on_eof:(on_eof acc)
    in
    let f_init = Lwt.return "" in
    Httpaf.Body.schedule_read response_body ~on_read:(on_read on_eof f_init)
      ~on_eof:(on_eof f_init);
    finished >>= fun data ->
    data >>= fun data ->
    let content_type =
      Httpaf.(
        Headers.get_exn (Reqd.request reqd).Request.headers "content-type")
    in
    let ct = Multipart_form.Content_type.of_string (content_type ^ "\r\n") in
    match ct with
    | Error (`Msg msg) ->
        Logs.warn (fun m -> m "couldn't content-type: %S" msg);
        Error (`Msg ("couldn't content-type:" ^ msg)) |> Lwt.return
    | Ok ct -> (
        match Multipart_form.of_string_to_list data ct with
        | Error (`Msg msg) ->
            Logs.warn (fun m -> m "couldn't multipart: %s" msg);
            Error (`Msg ("Couldn't multipart: " ^ msg)) |> Lwt.return
        | Ok (m, assoc) -> Ok (m, assoc) |> Lwt.return)

  let sign_up reqd =
    let now = Ptime.v (P.now_d_ps ()) in
    let csrf = Middleware.generate_csrf_cookie now reqd in
    let csrf_cookie = csrf.name ^ "=" ^ csrf.value ^ ";Path=/;HttpOnly=true" in
    match Middleware.session_cookie_value reqd with
    | Ok x when x <> "" -> Middleware.redirect_to_dashboard reqd ()
    | Ok _ | Error (`Msg _) ->
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Sign_up.register_page ~csrf:csrf.value ~icon:"/images/robur.png")
             ~header_list:
               [ ("Set-Cookie", csrf_cookie); ("X-MOLLY-CSRF", csrf.value) ]
             `OK)

  let sign_in reqd =
    match Middleware.session_cookie_value reqd with
    | Ok x when x <> "" -> Middleware.redirect_to_dashboard reqd ()
    | Ok _ | Error (`Msg _) ->
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Sign_in.login_page ~icon:"/images/robur.png" ())
             `OK)

  let register store reqd =
    decode_request_body reqd >>= fun data ->
    let json =
      try Ok (Yojson.Basic.from_string data)
      with Yojson.Json_error s -> Error (`Msg s)
    in
    match json with
    | Error (`Msg err) ->
        Logs.warn (fun m -> m "Failed to parse JSON: %s" err);
        Middleware.http_response reqd ~title:"Error" ~data:(String.escaped err)
          `Bad_request
    | Ok (`Assoc json_dict) -> (
        let validate_user_input ~name ~email ~password ~form_csrf =
          if name = "" || email = "" || password = "" then
            Error "All fields must be filled."
          else if String.length name < 4 then
            Error "Name must be at least 3 characters long."
          else if not (Utils.Email.validate_email email) then
            Error "Invalid email address."
          else if not (User_model.password_validation password) then
            Error "Password must be at least 8 characters long."
          else if form_csrf = "" then
            Error "CSRF token mismatch error. Please referesh and try again."
          else Ok "Validation passed."
        in
        match
          Utils.Json.
            ( get "email" json_dict,
              get "password" json_dict,
              get "name" json_dict,
              get "form_csrf" json_dict )
        with
        | ( Some (`String email),
            Some (`String password),
            Some (`String name),
            Some (`String form_csrf) ) -> (
            match validate_user_input ~name ~email ~password ~form_csrf with
            | Error err ->
                Middleware.http_response reqd ~title:"Error"
                  ~data:(String.escaped err) `Bad_request
            | Ok _ ->
                if Middleware.csrf_cookie_verification form_csrf reqd then
                  let existing_email = Store.find_by_email store email in
                  let existing_name = Store.find_by_name store name in
                  match (existing_name, existing_email) with
                  | Some _, None ->
                      Middleware.http_response reqd ~title:"Error"
                        ~data:"A user with this name already exist."
                        `Bad_request
                  | None, Some _ ->
                      Middleware.http_response reqd ~title:"Error"
                        ~data:"A user with this email already exist."
                        `Bad_request
                  | None, None -> (
                      let created_at = Ptime.v (P.now_d_ps ()) in
                      let user, cookie =
                        let active, super_user =
                          if Store.count_users store = 0 then (true, true)
                          else (false, false)
                        in
                        User_model.create_user ~name ~email ~password
                          ~created_at ~active ~super_user
                          ~user_agent:(Middleware.user_agent reqd)
                      in
                      Store.add_user store user >>= function
                      | Ok () ->
                          let cookie_value =
                            cookie.name ^ "=" ^ cookie.value
                            ^ ";Path=/;HttpOnly=true"
                          in
                          let header_list =
                            [
                              ("Set-Cookie", cookie_value);
                              ("location", "/dashboard");
                            ]
                          in
                          Middleware.http_response reqd ~header_list
                            ~title:"Success"
                            ~data:
                              (Yojson.Basic.to_string
                                 (User_model.user_to_json user))
                            `OK
                      | Error (`Msg err) ->
                          Middleware.http_response reqd ~title:"Error"
                            ~data:(String.escaped err) `Bad_request)
                  | _ ->
                      Middleware.http_response reqd ~title:"Error"
                        ~data:"A user with this name or email already exist."
                        `Bad_request
                else
                  Middleware.http_response reqd ~title:"Error"
                    ~data:
                      "CSRF token mismatch error. Please referesh and try \
                       again."
                    `Bad_request)
        | _ ->
            Middleware.http_response reqd ~title:"Error"
              ~data:
                (Fmt.str "Register: Unexpected fields. Got %s"
                   (Yojson.Basic.to_string (`Assoc json_dict)))
              `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:"Register account: expected a dictionary" `Bad_request

  let login store reqd =
    decode_request_body reqd >>= fun data ->
    let json =
      try Ok (Yojson.Basic.from_string data)
      with Yojson.Json_error s -> Error (`Msg s)
    in
    match json with
    | Error (`Msg err) ->
        Logs.warn (fun m -> m "Failed to parse JSON: %s" err);
        Middleware.http_response reqd ~title:"Error" ~data:(String.escaped err)
          `Bad_request
    | Ok (`Assoc json_dict) -> (
        let validate_user_input ~email ~password =
          if email = "" || password = "" then Error "All fields must be filled."
          else if not (Utils.Email.validate_email email) then
            Error "Invalid email address."
          else if String.length password < 8 then
            Error "Password must be at least 8 characters long."
          else Ok "Validation passed."
        in
        match Utils.Json.(get "email" json_dict, get "password" json_dict) with
        | Some (`String email), Some (`String password) -> (
            match validate_user_input ~email ~password with
            | Error err ->
                Middleware.http_response reqd ~title:"Error"
                  ~data:(String.escaped err) `Bad_request
            | Ok _ -> (
                let now = Ptime.v (P.now_d_ps ()) in
                let user = Store.find_by_email store email in
                match
                  User_model.login_user ~email ~password
                    ~user_agent:(Middleware.user_agent reqd)
                    user now
                with
                | Error (`Msg err) ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(String.escaped err) `Bad_request
                | Ok (user, cookie) -> (
                    Store.update_user store user >>= function
                    | Ok () ->
                        let cookie_value =
                          cookie.name ^ "=" ^ cookie.value
                          ^ ";Path=/;HttpOnly=true"
                        in
                        let header_list =
                          [
                            ("Set-Cookie", cookie_value);
                            ("location", "/dashboard");
                          ]
                        in
                        Middleware.http_response reqd ~header_list
                          ~title:"Success"
                          ~data:
                            (Yojson.Basic.to_string
                               (User_model.user_to_json user))
                          `OK
                    | Error (`Msg err) ->
                        Middleware.http_response reqd ~title:"Error"
                          ~data:(String.escaped err) `Internal_server_error)))
        | _ ->
            Middleware.http_response reqd ~title:"Error"
              ~data:
                (Fmt.str "Update password: Unexpected fields. Got %s"
                   (Yojson.Basic.to_string (`Assoc json_dict)))
              `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:"Update password: expected a dictionary" `Bad_request

  let verify_email store reqd (user : User_model.user) =
    let now = Ptime.v (P.now_d_ps ()) in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf -> (
        let email_verification_uuid = User_model.generate_uuid () in
        let updated_user =
          User_model.update_user user
            ~updated_at:(Ptime.v (P.now_d_ps ()))
            ~email_verification_uuid:(Some email_verification_uuid) ()
        in
        Store.update_user store updated_user >>= function
        | Ok () ->
            let verification_link =
              Utils.Email.generate_verification_link email_verification_uuid
            in
            Logs.info (fun m -> m "Verification link is: %s" verification_link);
            Lwt.return
              (reply reqd ~content_type:"text/html"
                 (Verify_email.verify_page user ~csrf ~icon:"/images/robur.png")
                 ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
                 `OK)
        | Error (`Msg err) ->
            Middleware.http_response reqd ~title:"Error"
              ~data:(String.escaped err) `Internal_server_error)
    | Error err ->
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
                ~content:(Error_page.error_layout err)
                ~icon:"/images/robur.png" ())
             `Internal_server_error)

  let verify_email_token store reqd verification_token (user : User_model.user)
      =
    match
      let ( let* ) = Result.bind in
      let* uuid =
        Option.to_result ~none:(`Msg "invalid UUID")
          (Uuidm.of_string verification_token)
      in
      let u = Store.find_email_verification_token store uuid in
      User_model.verify_email_token u verification_token
        (Ptime.v (P.now_d_ps ()))
    with
    | Ok user' ->
        if String.equal user.uuid user'.uuid then
          Store.update_user store user >>= function
          | Ok () -> Middleware.redirect_to_dashboard reqd ()
          | Error (`Msg msg) ->
              Middleware.http_response reqd ~title:"Error"
                ~data:(String.escaped msg) `Internal_server_error
        else
          Middleware.http_response reqd ~title:"Error"
            ~data:"Logged in user is not the to-be-verified one" `Bad_request
    | Error (`Msg s) ->
        Middleware.redirect_to_page ~path:"/sign-in" ~clear_session:true
          ~with_error:true reqd ~msg:s ()

  let toggle_account_attribute json_dict store reqd ~key update_fn error_on_last
      ~error_message =
    match Utils.Json.get "uuid" json_dict with
    | Some (`String uuid) -> (
        match Store.find_by_uuid store uuid with
        | None ->
            Logs.warn (fun m -> m "%s : Account not found" key);
            Middleware.http_response reqd ~title:"Error"
              ~data:"Account not found" `Not_found
        | Some user -> (
            if error_on_last user then (
              Logs.warn (fun m ->
                  m "%s : Can't perform action on last user" key);
              Middleware.http_response reqd ~title:"Error" ~data:error_message
                `Forbidden)
            else
              let updated_user = update_fn user in
              Store.update_user store updated_user >>= function
              | Ok () ->
                  Middleware.http_response reqd ~title:"OK"
                    ~data:"Updated user successfully" `OK
              | Error (`Msg msg) ->
                  Logs.warn (fun m -> m "%s : Storage error with %s" key msg);
                  Middleware.http_response reqd ~title:"Error" ~data:msg
                    `Internal_server_error))
    | _ ->
        Logs.warn (fun m -> m "%s: Failed to parse JSON - no UUID found" key);
        Middleware.http_response reqd ~title:"Error"
          ~data:"Couldn't find a UUID in the JSON." `Not_found

  let toggle_account_activation json_dict store reqd _user =
    toggle_account_attribute json_dict store reqd ~key:"toggle-active-account"
      (fun user ->
        User_model.update_user user ~active:(not user.active)
          ~updated_at:(Ptime.v (P.now_d_ps ()))
          ())
      (fun user -> user.active && Store.count_active store <= 1)
      ~error_message:"Cannot deactivate last active user"

  let toggle_admin_activation json_dict store reqd _user =
    toggle_account_attribute json_dict store reqd ~key:"toggle-admin-account"
      (fun user ->
        User_model.update_user user ~super_user:(not user.super_user)
          ~updated_at:(Ptime.v (P.now_d_ps ()))
          ())
      (fun user -> user.super_user && Store.count_superusers store <= 1)
      ~error_message:"Cannot remove last administrator"

  let dashboard store albatross reqd (user : User_model.user) =
    let now = Ptime.v (P.now_d_ps ()) in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        (* TODO use uuid in the future *)
        (Albatross.query albatross ~domain:user.name
           (`Unikernel_cmd `Unikernel_info)
         >|= function
         | Error msg ->
             Logs.err (fun m ->
                 m "error while communicating with albatross: %s" msg);
             []
         | Ok (_hdr, `Success (`Unikernel_info unikernels)) -> unikernels
         | Ok reply ->
             Logs.err (fun m ->
                 m "expected a unikernel info reply, received %a"
                   (Vmm_commands.pp_wire ~verbose:false)
                   reply);
             [])
        >>= fun unikernels ->
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Dashboard.dashboard_layout ~csrf user
                ~content:
                  (Unikernel_index.unikernel_index_layout unikernels
                     (Ptime.v (P.now_d_ps ())))
                ~icon:"/images/robur.png" ())
             `OK)
    | Error err ->
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
                ~content:(Error_page.error_layout err)
                ~icon:"/images/robur.png" ())
             `Internal_server_error)

  let account_page store reqd (user : User_model.user) =
    match Middleware.session_cookie_value reqd with
    | Ok active_cookie_value -> (
        let now = Ptime.v (P.now_d_ps ()) in
        generate_csrf_token store user now reqd >>= function
        | Ok csrf ->
            Lwt.return
              (reply reqd ~content_type:"text/html"
                 (Dashboard.dashboard_layout ~csrf user
                    ~page_title:"Account | Mollymawk"
                    ~content:
                      (User_account.user_account_layout ~csrf user
                         ~active_cookie_value now)
                    ~icon:"/images/robur.png" ())
                 ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
                 `OK)
        | Error err ->
            Lwt.return
              (reply reqd ~content_type:"text/html"
                 (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
                    ~content:(Error_page.error_layout err)
                    ~icon:"/images/robur.png" ())
                 `Internal_server_error))
    | Error (`Msg err) ->
        let error =
          {
            Utils.Status.code = 401;
            title = "Unauthenticated";
            success = false;
            data = err;
          }
        in
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Guest_layout.guest_layout ~page_title:"401 | Mollymawk"
                ~content:(Error_page.error_layout error)
                ~icon:"/images/robur.png" ())
             `Unauthorized)

  let update_password json_dict store reqd (user : User_model.user) =
    match
      Utils.Json.
        ( get "current_password" json_dict,
          get "new_password" json_dict,
          get "confirm_password" json_dict )
    with
    | ( Some (`String current_password),
        Some (`String new_password),
        Some (`String confirm_password) ) -> (
        let now = Ptime.v (P.now_d_ps ()) in
        let new_password_hash =
          User_model.hash_password ~password:new_password ~uuid:user.uuid
        in
        if
          not
            (String.equal user.password
               (User_model.hash_password ~password:current_password
                  ~uuid:user.uuid))
        then
          Middleware.http_response reqd ~title:"Error"
            ~data:"The current password entered is wrong." `Bad_request
        else if not (String.equal new_password confirm_password) then
          Middleware.http_response reqd ~title:"Error"
            ~data:"New password and confirm password do not match" `Bad_request
        else if not (User_model.password_validation new_password) then
          Middleware.http_response reqd ~title:"Error"
            ~data:"New password must be atleast 8 characters."
            `Internal_server_error
        else
          let updated_user =
            User_model.update_user user ~password:new_password_hash
              ~updated_at:now ()
          in
          Store.update_user store updated_user >>= function
          | Ok () ->
              Middleware.http_response reqd ~title:"OK"
                ~data:"Updated password successfully" `OK
          | Error (`Msg err) ->
              Logs.warn (fun m -> m "Storage error with %s" err);
              Middleware.http_response reqd ~title:"Error" ~data:err
                `Internal_server_error)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:
            (Fmt.str "Update password: Unexpected fields. Got %s"
               (Yojson.Basic.to_string (`Assoc json_dict)))
          `Bad_request

  let new_user_cookies ~user ~filter ~redirect store reqd =
    let now = Ptime.v (P.now_d_ps ()) in
    let cookies = List.filter filter user.User_model.cookies in
    let updated_user =
      User_model.update_user user ~cookies ~updated_at:now ()
    in
    Store.update_user store updated_user >>= function
    | Ok () -> redirect
    | Error (`Msg err) ->
        Logs.warn (fun m -> m "Storage error with %s" err);
        Middleware.http_response reqd ~title:"Error" ~data:err
          `Internal_server_error

  let close_sessions ?to_logout_cookie ?(logout = false) store reqd
      (user : User_model.user) =
    match Middleware.session_cookie_value reqd with
    | Ok cookie_value -> (
        match User_model.user_session_cookie user cookie_value with
        | Some cookie ->
            let filter, redirect =
              match (to_logout_cookie, logout) with
              | None, false ->
                  ( (fun (c : User_model.cookie) ->
                      not
                        (String.equal c.name User_model.session_cookie
                        && c.value <> cookie.value)),
                    Middleware.http_response reqd ~title:"OK"
                      ~data:"Closed all sessions succesfully" `OK )
              | _, true ->
                  ( (fun (c : User_model.cookie) ->
                      not (String.equal c.value cookie.value)),
                    Middleware.http_response reqd ~title:"OK"
                      ~data:"Logout succesful" `OK )
              | Some to_logout_cookie_value, false ->
                  ( (fun (c : User_model.cookie) ->
                      not (String.equal to_logout_cookie_value c.value)),
                    Middleware.redirect_to_page ~path:"/account"
                      ~msg:"Closed session succesfully" reqd () )
            in
            new_user_cookies ~user ~filter ~redirect store reqd
        | None ->
            let error =
              {
                Utils.Status.code = 401;
                title = "Unauthenticated";
                success = false;
                data = "Auth cookie not found";
              }
            in
            Lwt.return
              (reply reqd ~content_type:"text/html"
                 (Guest_layout.guest_layout ~page_title:"401 | Mollymawk"
                    ~content:(Error_page.error_layout error)
                    ~icon:"/images/robur.png" ())
                 `Unauthorized))
    | Error (`Msg err) ->
        let error =
          {
            Utils.Status.code = 401;
            title = "Unauthenticated";
            success = false;
            data = err;
          }
        in
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Guest_layout.guest_layout ~page_title:"401 | Mollymawk"
                ~content:(Error_page.error_layout error)
                ~icon:"/images/robur.png" ())
             `Unauthorized)

  let users store reqd (user : User_model.user) =
    let now = Ptime.v (P.now_d_ps ()) in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Dashboard.dashboard_layout ~csrf user
                ~page_title:"Users | Mollymawk"
                ~content:
                  (Users_index.users_index_layout (Store.users store)
                     (Ptime.v (P.now_d_ps ())))
                ~icon:"/images/robur.png" ())
             `OK)
    | Error err ->
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
                ~content:(Error_page.error_layout err)
                ~icon:"/images/robur.png" ())
             `Internal_server_error)

  let settings store reqd (user : User_model.user) =
    let now = Ptime.v (P.now_d_ps ()) in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Dashboard.dashboard_layout ~csrf user
                ~page_title:"Settings | Mollymawk"
                ~content:
                  (Settings_page.settings_layout (Store.configuration store))
                ~icon:"/images/robur.png" ())
             ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
             `OK)
    | Error err ->
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
                ~content:(Error_page.error_layout err)
                ~icon:"/images/robur.png" ())
             `Internal_server_error)

  let update_settings json_dict stack store albatross reqd _user =
    match
      Configuration.of_json_from_http json_dict (Ptime.v (P.now_d_ps ()))
    with
    | Ok configuration_settings -> (
        Store.update_configuration store configuration_settings >>= function
        | Ok () ->
            Albatross.init stack configuration_settings.server_ip
              ~port:configuration_settings.server_port
              configuration_settings.certificate
              configuration_settings.private_key
            >>= fun new_albatross ->
            albatross := new_albatross;
            Middleware.http_response reqd ~title:"Success"
              ~data:"Configuration updated successfully" `OK
        | Error (`Msg err) ->
            Middleware.http_response reqd ~title:"Error"
              ~data:(String.escaped err) `Internal_server_error)
    | Error (`Msg err) ->
        Middleware.http_response ~title:"Error" ~data:(String.escaped err) reqd
          `Bad_request

  let deploy_form store reqd (user : User_model.user) =
    let now = Ptime.v (P.now_d_ps ()) in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Dashboard.dashboard_layout ~csrf user
                ~page_title:"Deploy a Unikernel | Mollymawk"
                ~content:Unikernel_create.unikernel_create_layout
                ~icon:"/images/robur.png" ())
             ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
             `OK)
    | Error err ->
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
                ~content:(Error_page.error_layout err)
                ~icon:"/images/robur.png" ())
             `Internal_server_error)

  let unikernel_info albatross reqd (user : User_model.user) =
    (* TODO use uuid in the future *)
    Albatross.query albatross ~domain:user.name (`Unikernel_cmd `Unikernel_info)
    >>= function
    | Error msg ->
        Middleware.http_response reqd ~title:"Error"
          ~data:
            (Yojson.Basic.to_string
               (`String ("Error while querying albatross: " ^ msg)))
          `Internal_server_error
    | Ok (_hdr, res) -> (
        match Albatross_json.res res with
        | Ok res ->
            Middleware.http_response reqd ~title:"Success"
              ~data:(Yojson.Basic.to_string res)
              `OK
        | Error (`String res) ->
            Middleware.http_response reqd ~title:"Error"
              ~data:(Yojson.Basic.to_string (`String res))
              `Internal_server_error)

  let unikernel_info_one albatross store name reqd (user : User_model.user) =
    (* TODO use uuid in the future *)
    (Albatross.query albatross ~domain:user.name ~name
       (`Unikernel_cmd `Unikernel_info)
     >|= function
     | Error msg ->
         Logs.err (fun m ->
             m "error while communicating with albatross: %s" msg);
         []
     | Ok (_hdr, `Success (`Unikernel_info unikernel)) -> unikernel
     | Ok reply ->
         Logs.err (fun m ->
             m "expected a unikernel info reply, received %a"
               (Vmm_commands.pp_wire ~verbose:false)
               reply);
         [])
    >>= fun unikernels ->
    if List.length unikernels > 0 then
      (Albatross.query_console ~domain:user.name albatross ~name >|= function
       | Error err ->
           Logs.warn (fun m -> m "error querying console of albatross: %s" err);
           []
       | Ok (_, console_output) -> console_output)
      >>= fun console_output ->
      let now = Ptime.v (P.now_d_ps ()) in
      generate_csrf_token store user now reqd >>= function
      | Ok csrf ->
          Lwt.return
            (reply reqd ~content_type:"text/html"
               (Dashboard.dashboard_layout ~csrf user
                  ~content:
                    (Unikernel_single.unikernel_single_layout
                       (List.hd unikernels) now console_output)
                  ~icon:"/images/robur.png" ())
               ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
               `OK)
      | Error err ->
          Lwt.return
            (reply reqd ~content_type:"text/html"
               (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
                  ~content:(Error_page.error_layout err)
                  ~icon:"/images/robur.png" ())
               `Internal_server_error)
    else
      let error =
        {
          Utils.Status.code = 500;
          title = "An error occured";
          success = false;
          data = "Error while fetching unikernel.";
        }
      in
      Lwt.return
        (reply reqd ~content_type:"text/html"
           (Guest_layout.guest_layout ~page_title:"An Error Occured | Mollymawk"
              ~content:(Error_page.error_layout error)
              ~icon:"/images/robur.png" ())
           `Internal_server_error)

  let unikernel_destroy json_dict albatross reqd (user : User_model.user) =
    (* TODO use uuid in the future *)
    match Utils.Json.get "name" json_dict with
    | Some (`String unikernel_name) -> (
        Albatross.query albatross ~domain:user.name ~name:unikernel_name
          (`Unikernel_cmd `Unikernel_destroy)
        >>= function
        | Error msg ->
            Logs.err (fun m -> m "Error querying albatross: %s" msg);
            Middleware.http_response reqd ~title:"Error"
              ~data:("Error querying albatross: " ^ msg)
              `Internal_server_error
        | Ok (_hdr, res) -> (
            match Albatross_json.res res with
            | Ok res ->
                Middleware.http_response reqd ~title:"Success"
                  ~data:(Yojson.Basic.to_string res)
                  `OK
            | Error (`String res) ->
                Middleware.http_response reqd ~title:"Error"
                  ~data:(Yojson.Basic.to_string (`String res))
                  `Internal_server_error))
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:"Couldn't find unikernel name in json" `Bad_request

  let unikernel_restart json_dict albatross reqd (user : User_model.user) =
    (* TODO use uuid in the future *)
    match Utils.Json.get "name" json_dict with
    | Some (`String unikernel_name) -> (
        Albatross.query albatross ~domain:user.name ~name:unikernel_name
          (`Unikernel_cmd `Unikernel_restart)
        >>= function
        | Error msg ->
            Logs.err (fun m -> m "Error querying albatross: %s" msg);
            Middleware.http_response reqd ~title:"Error"
              ~data:("Error querying albatross: " ^ msg)
              `Internal_server_error
        | Ok (_hdr, res) -> (
            match Albatross_json.res res with
            | Ok res ->
                Middleware.http_response reqd ~title:"Success"
                  ~data:(Yojson.Basic.to_string res)
                  `OK
            | Error (`String res) ->
                Middleware.http_response reqd ~title:"Error"
                  ~data:(Yojson.Basic.to_string (`String res))
                  `Internal_server_error))
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:"Couldn't find unikernel name in json" `Bad_request

  let unikernel_create albatross reqd (user : User_model.user) =
    read_multipart_data reqd >>= fun result ->
    match result with
    | Error (`Msg msg) ->
        Middleware.http_response reqd ~title:"Error"
          ~data:("Couldn't multipart: " ^ msg)
          `Bad_request
    | Ok (m, assoc) -> (
        let m, _r = to_map ~assoc m in
        match
          ( Map.find_opt "arguments" m,
            Map.find_opt "name" m,
            Map.find_opt "binary" m,
            Map.find_opt "molly_csrf" m )
        with
        | ( Some (_, args),
            Some (_, name),
            Some (_, binary),
            Some (_, form_csrf_token) ) ->
            let now = Ptime.v (P.now_d_ps ()) in
            Middleware.csrf_verification user now form_csrf_token
              (fun reqd ->
                match Albatross_json.config_of_json args with
                | Ok cfg -> (
                    let config = { cfg with image = binary } in
                    (* TODO use uuid in the future *)
                    Albatross.query albatross ~domain:user.name ~name
                      (`Unikernel_cmd (`Unikernel_create config))
                    >>= function
                    | Error err ->
                        Logs.warn (fun m ->
                            m "Error querying albatross: %s" err);
                        Middleware.http_response reqd ~title:"Error"
                          ~data:("Error while querying Albatross: " ^ err)
                          `Internal_server_error
                    | Ok (_hdr, res) -> (
                        match Albatross_json.res res with
                        | Ok res ->
                            Middleware.http_response reqd ~title:"Success"
                              ~data:(Yojson.Basic.to_string res)
                              `OK
                        | Error (`String res) ->
                            Middleware.http_response reqd ~title:"Error"
                              ~data:(Yojson.Basic.to_string (`String res))
                              `Internal_server_error))
                | Error (`Msg err) ->
                    Logs.warn (fun m -> m "couldn't decode data %s" err);
                    Middleware.http_response reqd ~title:"Error" ~data:err
                      `Internal_server_error)
              reqd
        | _ ->
            Logs.warn (fun m -> m "couldn't find fields");
            Middleware.http_response reqd ~title:"Error"
              ~data:"Couldn't find fields" `Bad_request)

  let unikernel_console albatross name reqd (user : User_model.user) =
    (* TODO use uuid in the future *)
    Albatross.query_console ~domain:user.name albatross ~name >>= function
    | Error err ->
        Logs.warn (fun m -> m "error querying albatross: %s" err);
        Middleware.http_response reqd ~title:"Error"
          ~data:("Error while querying Albatross: " ^ err)
          `Internal_server_error
    | Ok (_, console_output) ->
        let console_output =
          List.map Albatross_json.console_data_to_json console_output
        in

        Lwt.return
          (reply reqd ~content_type:"application/json"
             (Yojson.Basic.to_string (`List console_output))
             `OK)

  let view_user albatross store uuid reqd (user : User_model.user) =
    match Store.find_by_uuid store uuid with
    | Some u -> (
        (Albatross.query albatross ~domain:u.name
           (`Unikernel_cmd `Unikernel_info)
         >|= function
         | Error msg ->
             Logs.err (fun m ->
                 m "error while communicating with albatross: %s" msg);
             []
         | Ok (_hdr, `Success (`Unikernel_info unikernels)) -> unikernels
         | Ok reply ->
             Logs.err (fun m ->
                 m "expected a unikernel info reply, received %a"
                   (Vmm_commands.pp_wire ~verbose:false)
                   reply);
             [])
        >>= fun unikernels ->
        let policy =
          match Albatross.policy ~domain:u.name albatross with
          | Ok p -> p
          | Error _ -> None
        in
        let now = Ptime.v (P.now_d_ps ()) in
        generate_csrf_token store user now reqd >>= function
        | Ok csrf ->
            Lwt.return
              (reply reqd ~content_type:"text/html"
                 (Dashboard.dashboard_layout ~csrf user
                    ~page_title:(String.capitalize_ascii u.name ^ " | Mollymawk")
                    ~content:
                      (User_single.user_single_layout u unikernels policy now)
                    ~icon:"/images/robur.png" ())
                 ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
                 `OK)
        | Error err ->
            Lwt.return
              (reply reqd ~content_type:"text/html"
                 (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
                    ~content:(Error_page.error_layout err)
                    ~icon:"/images/robur.png" ())
                 `Internal_server_error))
    | None ->
        let status =
          {
            Utils.Status.code = 404;
            title = "Error";
            data = "Couldn't find account with uuid: " ^ uuid;
            success = false;
          }
        in
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Guest_layout.guest_layout ~page_title:"404 | Mollymawk"
                ~content:(Error_page.error_layout status)
                ~icon:"/images/robur.png" ())
             `Not_found)

  let edit_policy albatross store uuid reqd (user : User_model.user) =
    match Store.find_by_uuid store uuid with
    | Some u -> (
        let user_policy =
          match Albatross.policy albatross ~domain:u.name with
          | Ok p -> (
              match p with Some p -> p | None -> Albatross.empty_policy)
          | Error _ -> Albatross.empty_policy
        in
        match Albatross.policy_resource_avalaible albatross with
        | Ok unallocated_resources -> (
            let now = Ptime.v (P.now_d_ps ()) in
            generate_csrf_token store user now reqd >>= function
            | Ok csrf ->
                Lwt.return
                  (reply reqd ~content_type:"text/html"
                     (Dashboard.dashboard_layout ~csrf user
                        ~page_title:
                          (String.capitalize_ascii u.name ^ " | Mollymawk")
                        ~content:
                          (Update_policy.update_policy_layout u ~user_policy
                             ~unallocated_resources)
                        ~icon:"/images/robur.png" ())
                     ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
                     `OK)
            | Error err ->
                Lwt.return
                  (reply reqd ~content_type:"text/html"
                     (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
                        ~content:(Error_page.error_layout err)
                        ~icon:"/images/robur.png" ())
                     `Internal_server_error))
        | Error err ->
            let status =
              {
                Utils.Status.code = 500;
                title = "Error";
                data = "Policy error: " ^ err;
                success = false;
              }
            in
            Lwt.return
              (reply reqd ~content_type:"text/html"
                 (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
                    ~content:(Error_page.error_layout status)
                    ~icon:"/images/robur.png" ())
                 `Not_found))
    | None ->
        let status =
          {
            Utils.Status.code = 404;
            title = "Error";
            data = "Couldn't find account with uuid: " ^ uuid;
            success = false;
          }
        in
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Guest_layout.guest_layout ~page_title:"404 | Mollymawk"
                ~content:(Error_page.error_layout status)
                ~icon:"/images/robur.png" ())
             `Not_found)

  let update_policy json_dict store albatross reqd _user =
    match Utils.Json.get "user_uuid" json_dict with
    | Some (`String user_uuid) -> (
        match Store.find_by_uuid store user_uuid with
        | Some u -> (
            match Albatross_json.policy_of_json json_dict with
            | Ok policy -> (
                match Albatross.policy albatross with
                | Ok (Some root_policy) -> (
                    match
                      Vmm_core.Policy.is_smaller ~super:root_policy ~sub:policy
                    with
                    | Error (`Msg err) ->
                        Logs.err (fun m ->
                            m "policy %a is not smaller than root policy %a: %s"
                              Vmm_core.Policy.pp policy Vmm_core.Policy.pp
                              root_policy err);
                        Middleware.http_response reqd ~title:"Error"
                          ~data:
                            ("policy is not smaller than root policy: " ^ err)
                          `Internal_server_error
                    | Ok () -> (
                        Albatross.set_policy albatross ~domain:u.name policy
                        >>= function
                        | Error err ->
                            Logs.err (fun m ->
                                m "error setting policy %a for %s: %s"
                                  Vmm_core.Policy.pp policy u.name err);
                            Middleware.http_response reqd ~title:"Error"
                              ~data:("error setting policy: " ^ err)
                              `Internal_server_error
                        | Ok policy ->
                            Middleware.http_response reqd ~title:"Success"
                              ~data:
                                (Yojson.Basic.to_string
                                   (Albatross_json.policy_info policy))
                              `OK))
                | Ok None ->
                    Logs.err (fun m -> m "policy: root policy can't be null ");
                    Middleware.http_response reqd ~title:"Error"
                      ~data:"root policy is null" `Internal_server_error
                | Error err ->
                    Logs.err (fun m ->
                        m
                          "policy: an error occured while fetching root \
                           policy: %s"
                          err);
                    Middleware.http_response reqd ~title:"Error"
                      ~data:("error with root policy: " ^ err)
                      `Internal_server_error)
            | Error (`Msg err) ->
                Middleware.http_response reqd ~title:"Error" ~data:err
                  `Bad_request)
        | None ->
            Middleware.http_response reqd ~title:"Error" ~data:"User not found"
              `Not_found)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:
            (Fmt.str "Update policy: Unexpected fields. Got %s"
               (Yojson.Basic.to_string (`Assoc json_dict)))
          `Bad_request

  let volumes store albatross reqd (user : User_model.user) =
    (Albatross.query albatross ~domain:user.name (`Block_cmd `Block_info)
     >|= function
     | Error msg ->
         Logs.err (fun m ->
             m "error while communicating with albatross: %s" msg);
         []
     | Ok (_hdr, `Success (`Block_devices blocks)) -> blocks
     | Ok reply ->
         Logs.err (fun m ->
             m "expected a block info reply, received %a"
               (Vmm_commands.pp_wire ~verbose:false)
               reply);
         [])
    >>= fun blocks ->
    let policy =
      match Albatross.policy ~domain:user.name albatross with
      | Ok p -> p
      | Error _ -> None
    in
    let now = Ptime.v (P.now_d_ps ()) in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Dashboard.dashboard_layout ~csrf user
                ~page_title:(String.capitalize_ascii user.name ^ " | Mollymawk")
                ~content:(Volume_index.volume_index_layout blocks policy)
                ~icon:"/images/robur.png" ())
             ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
             `OK)
    | Error err ->
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
                ~content:(Error_page.error_layout err)
                ~icon:"/images/robur.png" ())
             `Internal_server_error)

  let delete_volume json_dict albatross reqd (user : User_model.user) =
    match Utils.Json.get "block_name" json_dict with
    | Some (`String block_name) -> (
        Albatross.query albatross ~domain:user.name ~name:block_name
          (`Block_cmd `Block_remove)
        >>= function
        | Error msg ->
            Logs.err (fun m -> m "Error querying albatross: %s" msg);
            Middleware.http_response reqd ~title:"Error"
              ~data:("Error querying albatross: " ^ msg)
              `Internal_server_error
        | Ok (_hdr, res) -> (
            match Albatross_json.res res with
            | Ok res ->
                Middleware.http_response reqd ~title:"Success"
                  ~data:(Yojson.Basic.to_string res)
                  `OK
            | Error (`String res) ->
                Middleware.http_response reqd ~title:"Error"
                  ~data:(Yojson.Basic.to_string (`String res))
                  `Internal_server_error))
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:"Couldn't find block name in json" `Bad_request

  let create_volume albatross reqd (user : User_model.user) =
    read_multipart_data reqd >>= fun result ->
    match result with
    | Error (`Msg msg) ->
        Middleware.http_response reqd ~title:"Error"
          ~data:("Couldn't multipart: " ^ msg)
          `Bad_request
    | Ok (m, assoc) -> (
        let m, _r = to_map ~assoc m in
        match
          ( Map.find_opt "block_name" m,
            Map.find_opt "block_size" m,
            Map.find_opt "block_data" m,
            Map.find_opt "block_compressed" m,
            Map.find_opt "molly_csrf" m )
        with
        | ( Some (_, block_name),
            Some (_, block_size),
            Some (_, block_data),
            Some (_, block_compressed),
            Some (_, form_csrf_token) ) ->
            (let now = Ptime.v (P.now_d_ps ()) in
             Middleware.csrf_verification user now form_csrf_token (fun reqd ->
                 Albatross.query albatross ~domain:user.name ~name:block_name
                   (`Block_cmd
                     (`Block_add
                       ( int_of_string block_size,
                         bool_of_string block_compressed,
                         Some block_data )))
                 >>= function
                 | Error msg ->
                     Logs.err (fun m -> m "Error querying albatross: %s" msg);
                     Middleware.http_response reqd ~title:"Error"
                       ~data:("Error querying albatross: " ^ msg)
                       `Internal_server_error
                 | Ok (_hdr, res) -> (
                     match Albatross_json.res res with
                     | Ok res ->
                         Middleware.http_response reqd ~title:"Success"
                           ~data:(Yojson.Basic.to_string res)
                           `OK
                     | Error (`String res) ->
                         Middleware.http_response reqd ~title:"Error"
                           ~data:(Yojson.Basic.to_string (`String res))
                           `Internal_server_error)))
              reqd
        | _ ->
            Logs.warn (fun m -> m "couldn't find fields");
            Middleware.http_response reqd ~title:"Error"
              ~data:"Couldn't find fields" `Bad_request)

  let request_handler stack albatross js_file css_file imgs store
      (_ipaddr, _port) reqd =
    Lwt.async (fun () ->
        let bad_request () =
          Middleware.http_response reqd ~title:"Error"
            ~data:"Bad HTTP request method." `Bad_request
        in
        let req = Httpaf.Reqd.request reqd in
        let path =
          Uri.(pct_decode (path (of_string req.Httpaf.Request.target)))
        in
        let check_meth m f = if m = req.meth then f () else bad_request () in
        match path with
        | "/" ->
            check_meth `GET (fun () ->
                Lwt.return
                  (reply reqd ~content_type:"text/html"
                     (Guest_layout.guest_layout
                        ~page_title:"Deploy unikernels with ease | Mollymawk"
                        ~content:Index_page.index_page ~icon:"/images/robur.png"
                        ())
                     `OK))
        | "/main.js" ->
            check_meth `GET (fun () ->
                Lwt.return
                  (reply reqd ~content_type:"text/javascript" js_file `OK))
        | "/images/molly_bird.jpeg" ->
            check_meth `GET (fun () ->
                Lwt.return
                  (reply reqd ~content_type:"image/jpeg" imgs.molly_img `OK))
        | "/images/albatross_1.png" ->
            check_meth `GET (fun () ->
                Lwt.return
                  (reply reqd ~content_type:"image/png" imgs.albatross_img `OK))
        | "/images/dashboard_1.png" ->
            check_meth `GET (fun () ->
                Lwt.return
                  (reply reqd ~content_type:"image/png" imgs.dashboard_img `OK))
        | "/images/mirage_os_1.png" ->
            check_meth `GET (fun () ->
                Lwt.return
                  (reply reqd ~content_type:"image/png" imgs.mirage_img `OK))
        | "/images/robur.png" ->
            check_meth `GET (fun () ->
                Lwt.return
                  (reply reqd ~content_type:"image/png" imgs.robur_img `OK))
        | "/style.css" ->
            check_meth `GET (fun () ->
                Lwt.return (reply reqd ~content_type:"text/css" css_file `OK))
        | "/sign-up" -> check_meth `GET (fun () -> sign_up reqd)
        | "/sign-in" -> check_meth `GET (fun () -> sign_in reqd)
        | "/api/register" -> check_meth `POST (fun () -> register store reqd)
        | "/api/login" -> check_meth `POST (fun () -> login store reqd)
        | "/verify-email" ->
            check_meth `GET (fun () ->
                authenticate ~email_verified:false store reqd
                  (verify_email store reqd))
        | path when String.starts_with ~prefix:"/auth/verify/token=" path ->
            check_meth `GET (fun () ->
                let token = String.sub path 19 (String.length path - 19) in
                authenticate ~email_verified:false store reqd
                  (verify_email_token store reqd token))
        | "/dashboard" ->
            check_meth `GET (fun () ->
                authenticate store reqd (dashboard store !albatross reqd))
        | "/account" ->
            check_meth `GET (fun () ->
                authenticate store reqd (account_page store reqd))
        | "/account/password/update" ->
            check_meth `POST (fun () ->
                extract_csrf_token reqd >>= function
                | Ok (form_csrf, json_dict) ->
                    authenticate ~form_csrf store reqd
                      (update_password json_dict store reqd)
                | Error (`Msg msg) ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(String.escaped msg) `Bad_request)
        | "/account/sessions/close" ->
            check_meth `POST (fun () ->
                extract_csrf_token reqd >>= function
                | Ok (form_csrf, _) ->
                    authenticate ~form_csrf store reqd
                      (close_sessions store reqd)
                | Error (`Msg msg) ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(String.escaped msg) `Bad_request)
        | "/logout" ->
            check_meth `POST (fun () ->
                extract_csrf_token reqd >>= function
                | Ok (form_csrf, _) ->
                    authenticate ~form_csrf store reqd
                      (close_sessions ~logout:true store reqd)
                | Error (`Msg msg) ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(String.escaped msg) `Bad_request)
        | path when String.starts_with ~prefix:"/account/session/close/" path ->
            check_meth `GET (fun () ->
                match
                  String.split_on_char '/'
                    (String.sub path 23 (String.length path - 23))
                with
                | [ to_logout_cookie; form_csrf ] ->
                    authenticate ~form_csrf store reqd
                      (close_sessions ~to_logout_cookie store reqd)
                | _ ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:"An error occured. Please refresh and try again"
                      `Bad_request)
        | "/volumes" ->
            check_meth `GET (fun () ->
                authenticate store reqd (volumes store !albatross reqd))
        | "/api/volume/delete" ->
            check_meth `POST (fun () ->
                extract_csrf_token reqd >>= function
                | Ok (form_csrf, json_dict) ->
                    authenticate ~form_csrf ~api_meth:true store reqd
                      (delete_volume json_dict !albatross reqd)
                | Error (`Msg msg) ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(String.escaped msg) `Bad_request)
        | "/api/volume/create" ->
            check_meth `POST (fun () ->
                authenticate store reqd (create_volume !albatross reqd))
        | "/admin/users" ->
            check_meth `GET (fun () ->
                authenticate ~check_admin:true store reqd (users store reqd))
        | path when String.starts_with ~prefix:"/admin/user/" path ->
            check_meth `GET (fun () ->
                let uuid = String.sub path 12 (String.length path - 12) in
                authenticate ~check_admin:true store reqd
                  (view_user !albatross store uuid reqd))
        | path when String.starts_with ~prefix:"/admin/u/policy/edit/" path ->
            check_meth `GET (fun () ->
                let uuid = String.sub path 21 (String.length path - 21) in
                authenticate ~check_admin:true store reqd
                  (edit_policy !albatross store uuid reqd))
        | "/admin/settings" ->
            check_meth `GET (fun () ->
                authenticate ~check_admin:true store reqd (settings store reqd))
        | "/api/admin/settings/update" ->
            check_meth `POST (fun () ->
                extract_csrf_token reqd >>= function
                | Ok (form_csrf, json_dict) ->
                    authenticate ~check_admin:true ~form_csrf ~api_meth:true
                      store reqd
                      (update_settings json_dict stack store albatross reqd)
                | Error (`Msg msg) ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(String.escaped msg) `Bad_request)
        | "/api/admin/u/policy/update" ->
            check_meth `POST (fun () ->
                extract_csrf_token reqd >>= function
                | Ok (form_csrf, json_dict) ->
                    authenticate ~check_admin:true ~form_csrf ~api_meth:true
                      store reqd
                      (update_policy json_dict store !albatross reqd)
                | Error (`Msg msg) ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(String.escaped msg) `Bad_request)
        | "/api/admin/user/activate/toggle" ->
            check_meth `POST (fun () ->
                extract_csrf_token reqd >>= function
                | Ok (form_csrf, json_dict) ->
                    authenticate ~check_admin:true ~form_csrf ~api_meth:true
                      store reqd
                      (toggle_account_activation json_dict store reqd)
                | Error (`Msg msg) ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(String.escaped msg) `Bad_request)
        | "/api/admin/user/admin/toggle" ->
            check_meth `POST (fun () ->
                extract_csrf_token reqd >>= function
                | Ok (form_csrf, json_dict) ->
                    authenticate ~check_admin:true ~form_csrf ~api_meth:true
                      store reqd
                      (toggle_admin_activation json_dict store reqd)
                | Error (`Msg msg) ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(String.escaped msg) `Bad_request)
        | "/api/unikernels" ->
            check_meth `GET (fun () ->
                authenticate ~api_meth:true store reqd
                  (unikernel_info !albatross reqd))
        | path when String.starts_with ~prefix:"/unikernel/info/" path ->
            check_meth `GET (fun () ->
                let unikernel_name =
                  String.sub path 16 (String.length path - 16)
                in
                authenticate store reqd
                  (unikernel_info_one !albatross store unikernel_name reqd))
        | "/unikernel/deploy" ->
            check_meth `GET (fun () ->
                authenticate store reqd (deploy_form store reqd))
        | "/unikernel/destroy" ->
            check_meth `POST (fun () ->
                extract_csrf_token reqd >>= function
                | Ok (form_csrf, json_dict) ->
                    authenticate store reqd ~form_csrf
                      (unikernel_destroy json_dict !albatross reqd)
                | Error (`Msg msg) ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(String.escaped msg) `Bad_request)
        | "/unikernel/restart" ->
            check_meth `POST (fun () ->
                extract_csrf_token reqd >>= function
                | Ok (form_csrf, json_dict) ->
                    authenticate store reqd ~form_csrf
                      (unikernel_restart json_dict !albatross reqd)
                | Error (`Msg msg) ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(String.escaped msg) `Bad_request)
        | path when String.starts_with ~prefix:"/unikernel/console/" path ->
            check_meth `GET (fun () ->
                let unikernel_name =
                  String.sub path 19 (String.length path - 19)
                in
                authenticate store reqd
                  (unikernel_console !albatross unikernel_name reqd))
        | "/unikernel/create" ->
            check_meth `POST (fun () ->
                authenticate store reqd (unikernel_create !albatross reqd))
        | _ ->
            let error =
              {
                Utils.Status.code = 404;
                title = "Page not found";
                success = false;
                data = "This page cannot be found.";
              }
            in
            Lwt.return
              (reply reqd ~content_type:"text/html"
                 (Guest_layout.guest_layout ~page_title:"404 | Mollymawk"
                    ~content:(Error_page.error_layout error)
                    ~icon:"/images/robur.png" ())
                 `Not_found))

  let pp_error ppf = function
    | #Httpaf.Status.t as code -> Httpaf.Status.pp_hum ppf code
    | `Exn exn -> Fmt.pf ppf "exception %s" (Printexc.to_string exn)

  let error_handler _dst ?request err _ =
    Logs.err (fun m ->
        m "error %a while processing request %a" pp_error err
          Fmt.(option ~none:(any "unknown") Httpaf.Request.pp_hum)
          request)

  let start _ _ _ _ stack assets storage =
    js_contents assets >>= fun js_file ->
    css_contents assets >>= fun css_file ->
    images assets >>= fun imgs ->
    Store.connect storage >>= function
    | Error (`Msg msg) -> failwith msg
    | Ok store ->
        let c = Store.configuration store in
        Albatross.init stack c.Configuration.server_ip ~port:c.server_port
          c.certificate c.private_key
        >>= fun albatross ->
        let albatross = ref albatross in
        let port = 8080 in
        Logs.info (fun m ->
            m "Initialise an HTTP server (no HTTPS) on http://127.0.0.1:%u/"
              port);
        let request_handler _flow =
          request_handler stack albatross js_file css_file imgs store
        in
        Paf.init ~port:8080 (S.tcp stack) >>= fun service ->
        let http = Paf.http_service ~error_handler request_handler in
        let (`Initialized th) = Paf.serve http service in
        th
end
