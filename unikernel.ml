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
    (BLOCK : Mirage_block.S)
    (Http_client : Http_mirage_client.S) =
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
        Logs.warn (fun m ->
            m "couldn't parse content-type %s: %S" content_type msg);
        Error
          (`Msg ("couldn't parse content-type " ^ content_type ^ ": " ^ msg))
        |> Lwt.return
    | Ok ct -> (
        match Multipart_form.of_string_to_list data ct with
        | Error (`Msg msg) ->
            Logs.warn (fun m -> m "couldn't decode multipart data: %s" msg);
            Error (`Msg ("Couldn't decode multipart data: " ^ msg))
            |> Lwt.return
        | Ok (m, assoc) -> Ok (m, assoc) |> Lwt.return)

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
            data =
              `String
                ("An error occured while generating a CSRF token. Error: " ^ err);
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
    match Middleware.header "Content-Type" reqd with
    | Some header when String.starts_with ~prefix:"multipart/form-data" header
      -> (
        read_multipart_data reqd >>= fun result ->
        match result with
        | Error (`Msg err) ->
            Logs.warn (fun m -> m "Failed to read multipart data: %s" err);
            Lwt.return
              (Error (`Msg ("Couldn't process multipart request: " ^ err)))
        | Ok (m, assoc) -> (
            let multipart_body, _r = to_map ~assoc m in
            match Map.find_opt "molly_csrf" multipart_body with
            | None ->
                Logs.warn (fun m -> m "No csrf token in multipart request");
                Lwt.return (Error (`Msg "Couldn't find CSRF token"))
            | Some (_, token) ->
                Lwt.return (Ok (token, `Multipart multipart_body))))
    | None | _ -> (
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
            | Some (`String token) -> Lwt.return (Ok (token, `JSON json_dict))
            | _ ->
                Logs.warn (fun m ->
                    m "No csrf token in session request with Json body");
                Lwt.return (Error (`Msg "Couldn't find CSRF token")))
        | Ok _ ->
            Logs.warn (fun m -> m "JSON is not a dictionary: %s" data);
            Lwt.return (Error (`Msg "not a dictionary")))

  let extract_json_body reqd =
    decode_request_body reqd >>= fun data ->
    match
      try Ok (Yojson.Basic.from_string data)
      with Yojson.Json_error s -> Error (`Msg s)
    with
    | Error (`Msg err) ->
        Logs.warn (fun m -> m "Failed to parse JSON: %s" err);
        Lwt.return (Error (`Msg err))
    | Ok (`Assoc json_dict) -> Lwt.return (Ok json_dict)
    | Ok _ ->
        Logs.warn (fun m -> m "JSON is not a dictionary: %s" data);
        Lwt.return (Error (`Msg "not a dictionary"))

  module Albatross = Albatross.Make (T) (P) (S)

  let process_session_request ~request_body ?form_csrf ~email_verified f reqd
      user =
    let current_time = Ptime.v (P.now_d_ps ()) in
    let middlewares ~form_csrf user =
      (if email_verified && false (* TODO *) then
         [ Middleware.email_verified_middleware user ]
       else [])
      @ Option.fold ~none:[]
          ~some:(fun csrf ->
            [ Middleware.csrf_verification user current_time csrf ])
          form_csrf
    in
    Middleware.apply_middleware
      (middlewares ~form_csrf user)
      (fun _reqd -> f ~request_body user)
      reqd

  let authenticate_user ~check_admin ~check_token store reqd =
    let ( let* ) = Result.bind in
    let current_time = Ptime.v (P.now_d_ps ()) in
    let user_is_active user =
      if user.User_model.active then Ok ()
      else Error "User account is deactivated"
    in
    let user_is_admin user =
      if (check_admin && user.User_model.super_user) || not check_admin then
        Ok ()
      else
        Error "You don't have the necessary permissions to access this service."
    in
    let check_cookie reqd =
      match Middleware.session_cookie_value reqd with
      | Error (`Msg err) ->
          Error (`Cookie, "No molly-session in cookie header. %s" ^ err)
      | Ok cookie_value -> (
          match Store.find_by_cookie store cookie_value with
          | None ->
              Error (`Cookie, "Failed to find user with cookie " ^ cookie_value)
          | Some (user, cookie) ->
              if User_model.is_valid_cookie cookie current_time then
                match
                  let* () = user_is_active user in
                  user_is_admin user
                with
                | Error msg -> Error (`Cookie, msg)
                | Ok () -> Ok (`Cookie (user, cookie))
              else
                Error
                  ( `Cookie,
                    "Session value doesn't match user session " ^ cookie_value
                  ))
    in
    let valid_token token_value =
      match Store.find_by_api_token store token_value with
      | Some (user, token) ->
          if User_model.is_valid_token token current_time then Ok (user, token)
          else Error (`Token, "Token value is not valid " ^ token_value)
      | None -> Error (`Token, "Failed to find user with token " ^ token_value)
    in
    if check_token then
      match Middleware.api_authentication reqd with
      | Some token_value -> (
          let* user, token = valid_token token_value in
          match
            let* () = user_is_active user in
            user_is_admin user
          with
          | Ok () -> Ok (`Token (user, token))
          | Error msg -> Error (`Token, msg))
      | None -> check_cookie reqd
    else check_cookie reqd

  let authenticate ?(email_verified = true) ?(check_admin = false)
      ?(api_meth = false) ?(check_csrf = false) ?(check_token = false) store
      reqd f =
    match authenticate_user ~check_admin ~check_token store reqd with
    | Error (`Cookie, msg) ->
        Logs.err (fun m -> m "authenticate: %s" msg);
        if api_meth then
          Middleware.http_response reqd ~title:"Error" ~data:(`String msg)
            `Bad_request
        else
          Middleware.redirect_to_page ~path:"/sign-in" ~clear_session:true
            ~with_error:true ~msg reqd ()
    | Error (`Token, msg) ->
        Logs.err (fun m -> m "authenticate: %s" msg);
        Middleware.http_response reqd ~title:"Error" ~data:(`String msg)
          `Bad_request
    | Ok (`Token (user, token)) -> (
        Store.increment_token_usage store token user >>= function
        | Error (`Msg err) ->
            Middleware.http_response reqd ~title:"Error" ~data:(`String err)
              `Internal_server_error
        | Ok () -> (
            extract_json_body reqd >>= function
            | Ok request_body -> f ~request_body:(`JSON request_body) user
            | Error (`Msg msg) ->
                Middleware.http_response reqd ~title:"Error"
                  ~data:(`String (String.escaped msg))
                  `Bad_request))
    | Ok (`Cookie (user, cookie)) -> (
        Store.update_cookie_usage store cookie user reqd >>= function
        | Error (`Msg err) ->
            Logs.err (fun m -> m "Error with storage: %s" err);
            Middleware.http_response reqd ~title:"Error"
              ~data:(`String (String.escaped err))
              `Internal_server_error
        | Ok () ->
            if check_csrf then
              extract_csrf_token reqd >>= function
              | Ok (form_csrf, request_body) ->
                  process_session_request ~email_verified ~request_body
                    ~form_csrf f reqd user
              | Error (`Msg msg) ->
                  Middleware.http_response reqd ~title:"Error"
                    ~data:(`String (String.escaped msg))
                    `Bad_request
            else
              process_session_request ~email_verified ~request_body:`Nothing f
                reqd user)

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

  let user_volumes albatross user_name =
    Albatross.query albatross ~domain:user_name (`Block_cmd `Block_info)
    >|= function
    | Error msg ->
        Logs.err (fun m -> m "error while communicating with albatross: %s" msg);
        []
    | Ok (_hdr, `Success (`Block_devices blocks)) -> blocks
    | Ok reply ->
        Logs.err (fun m ->
            m "expected a block info reply, received %a"
              (Vmm_commands.pp_wire ~verbose:false)
              reply);
        []

  let user_unikernels albatross user_name =
    Albatross.query albatross ~domain:user_name (`Unikernel_cmd `Unikernel_info)
    >|= function
    | Error msg ->
        Logs.err (fun m -> m "error while communicating with albatross: %s" msg);
        []
    | Ok (_hdr, `Success (`Old_unikernel_info3 unikernels)) -> unikernels
    | Ok reply ->
        Logs.err (fun m ->
            m "expected a unikernel info reply, received %a"
              (Vmm_commands.pp_wire ~verbose:false)
              reply);
        []

  let user_unikernel albatross ~user_name ~unikernel_name =
    Albatross.query albatross ~domain:user_name ~name:unikernel_name
      (`Unikernel_cmd `Unikernel_info)
    >|= function
    | Error err ->
        Logs.err (fun m ->
            m
              "Error while communicating with albatross. Trying to fetch %s \
               resulted in : %s"
              unikernel_name err);
        Error err
    | Ok (_hdr, `Success (`Old_unikernel_info3 [ unikernel ])) -> Ok unikernel
    | Ok (_hdr, `Success (`Unikernel_info unikernels)) ->
        let message =
          Printf.sprintf
            "Expected a single unikernel information from albatross, received \
             %u"
            (List.length unikernels)
        in
        Logs.err (fun m -> m "%s" message);
        Error message
    | Ok reply ->
        let message =
          Printf.sprintf
            "Trying to fetch %s: expected a unikernel info reply, received %s"
            unikernel_name
            (Format.asprintf "%a" (Vmm_commands.pp_wire ~verbose:false) reply)
        in
        Logs.err (fun m -> m "%s" message);
        Error message

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
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String (String.escaped err))
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
                  ~data:(`String (String.escaped err))
                  `Bad_request
            | Ok _ ->
                if Middleware.csrf_cookie_verification form_csrf reqd then
                  let existing_email = Store.find_by_email store email in
                  let existing_name = Store.find_by_name store name in
                  match (existing_name, existing_email) with
                  | Some _, None ->
                      Middleware.http_response reqd ~title:"Error"
                        ~data:(`String "A user with this name already exist.")
                        `Bad_request
                  | None, Some _ ->
                      Middleware.http_response reqd ~title:"Error"
                        ~data:(`String "A user with this email already exist.")
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
                            ~data:(User_model.user_to_json user)
                            `OK
                      | Error (`Msg err) ->
                          Middleware.http_response reqd ~title:"Error"
                            ~data:(`String (String.escaped err))
                            `Bad_request)
                  | _ ->
                      Middleware.http_response reqd ~title:"Error"
                        ~data:
                          (`String
                             "A user with this name or email already exist.")
                        `Bad_request
                else
                  Middleware.http_response reqd ~title:"Error"
                    ~data:
                      (`String
                         "CSRF token mismatch error. Please referesh and try \
                          again.") `Bad_request)
        | _ ->
            Middleware.http_response reqd ~title:"Error"
              ~data:
                (`String
                   (Fmt.str "Register: Unexpected fields. Got %s"
                      (Yojson.Basic.to_string (`Assoc json_dict))))
              `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Register account: expected a dictionary") `Bad_request

  let login store reqd =
    decode_request_body reqd >>= fun data ->
    let json =
      try Ok (Yojson.Basic.from_string data)
      with Yojson.Json_error s -> Error (`Msg s)
    in
    match json with
    | Error (`Msg err) ->
        Logs.warn (fun m -> m "Failed to parse JSON: %s" err);
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String (String.escaped err))
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
                  ~data:(`String (String.escaped err))
                  `Bad_request
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
                      ~data:(`String (String.escaped err))
                      `Bad_request
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
                          ~data:(User_model.user_to_json user)
                          `OK
                    | Error (`Msg err) ->
                        Middleware.http_response reqd ~title:"Error"
                          ~data:(`String (String.escaped err))
                          `Internal_server_error)))
        | _ ->
            Middleware.http_response reqd ~title:"Error"
              ~data:
                (`String
                   (Fmt.str "Update password: Unexpected fields. Got %s"
                      (Yojson.Basic.to_string (`Assoc json_dict))))
              `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Update password: expected a dictionary") `Bad_request

  let verify_email store reqd ~request_body:_ (user : User_model.user) =
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
              ~data:(`String (String.escaped err))
              `Internal_server_error)
    | Error err ->
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
                ~content:(Error_page.error_layout err)
                ~icon:"/images/robur.png" ())
             `Internal_server_error)

  let verify_email_token store reqd verification_token ~request_body:_
      (user : User_model.user) =
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
                ~data:(`String (String.escaped msg))
                `Internal_server_error
        else
          Middleware.http_response reqd ~title:"Error"
            ~data:(`String "Logged in user is not the to-be-verified one")
            `Bad_request
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
              ~data:(`String "Account not found") `Not_found
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
                    ~data:(`String "Updated user successfully") `OK
              | Error (`Msg msg) ->
                  Logs.warn (fun m -> m "%s : Storage error with %s" key msg);
                  Middleware.http_response reqd ~title:"Error"
                    ~data:(`String (String.escaped msg))
                    `Internal_server_error))
    | _ ->
        Logs.warn (fun m -> m "%s: Failed to parse JSON - no UUID found" key);
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Couldn't find a UUID in the JSON.") `Not_found

  let toggle_account_activation ~request_body store reqd _user =
    match request_body with
    | `JSON json_dict ->
        toggle_account_attribute json_dict store reqd
          ~key:"toggle-active-account"
          (fun user ->
            User_model.update_user user ~active:(not user.active)
              ~updated_at:(Ptime.v (P.now_d_ps ()))
              ())
          (fun user -> user.active && Store.count_active store <= 1)
          ~error_message:(`String "Cannot deactivate last active user")
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Expected a JSON body ") `Bad_request

  let toggle_admin_activation ~request_body store reqd _user =
    match request_body with
    | `JSON json_dict ->
        toggle_account_attribute json_dict store reqd
          ~key:"toggle-admin-account"
          (fun user ->
            User_model.update_user user ~super_user:(not user.super_user)
              ~updated_at:(Ptime.v (P.now_d_ps ()))
              ())
          (fun user -> user.super_user && Store.count_superusers store <= 1)
          ~error_message:(`String "Cannot remove last administrator")
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Expected a JSON body") `Bad_request

  let dashboard store albatross reqd ~request_body:_ (user : User_model.user) =
    let now = Ptime.v (P.now_d_ps ()) in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        (* TODO use uuid in the future *)
        user_unikernels albatross user.name >>= fun unikernels ->
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

  let account_page store reqd ~request_body:_ (user : User_model.user) =
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
                      (User_account.user_account_layout user
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
            data = `String err;
          }
        in
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Guest_layout.guest_layout ~page_title:"401 | Mollymawk"
                ~content:(Error_page.error_layout error)
                ~icon:"/images/robur.png" ())
             `Unauthorized)

  let update_password store reqd ~request_body (user : User_model.user) =
    match request_body with
    | `JSON json_dict -> (
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
                ~data:(`String "The current password entered is wrong.")
                `Bad_request
            else if not (String.equal new_password confirm_password) then
              Middleware.http_response reqd ~title:"Error"
                ~data:(`String "New password and confirm password do not match")
                `Bad_request
            else if not (User_model.password_validation new_password) then
              Middleware.http_response reqd ~title:"Error"
                ~data:(`String "New password must be atleast 8 characters.")
                `Internal_server_error
            else
              let updated_user =
                User_model.update_user user ~password:new_password_hash
                  ~updated_at:now ()
              in
              Store.update_user store updated_user >>= function
              | Ok () ->
                  Middleware.http_response reqd ~title:"OK"
                    ~data:(`String "Updated password successfully") `OK
              | Error (`Msg err) ->
                  Logs.warn (fun m -> m "Storage error with %s" err);
                  Middleware.http_response reqd ~title:"Error"
                    ~data:(`String (String.escaped err))
                    `Internal_server_error)
        | _ ->
            Middleware.http_response reqd ~title:"Error"
              ~data:
                (`String
                   (Fmt.str "Update password: Unexpected fields. Got %s"
                      (Yojson.Basic.to_string (`Assoc json_dict))))
              `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Expected a JSON body") `Bad_request

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
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String (String.escaped err))
          `Internal_server_error

  let close_sessions ?to_logout_cookie ?(logout = false) store reqd
      ~request_body:_ (user : User_model.user) =
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
                      ~data:(`String "Closed all sessions succesfully") `OK )
              | _, true ->
                  ( (fun (c : User_model.cookie) ->
                      not (String.equal c.value cookie.value)),
                    Middleware.http_response reqd ~title:"OK"
                      ~data:(`String "Logout succesful") `OK )
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
                data = `String "Auth cookie not found";
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
            data = `String err;
          }
        in
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Guest_layout.guest_layout ~page_title:"401 | Mollymawk"
                ~content:(Error_page.error_layout error)
                ~icon:"/images/robur.png" ())
             `Unauthorized)

  let close_session store reqd ~request_body (user : User_model.user) =
    match request_body with
    | `JSON json_dict -> (
        match Utils.Json.(get "session_value" json_dict) with
        | Some (`String session_value) -> (
            let now = Ptime.v (P.now_d_ps ()) in
            let cookies =
              List.filter
                (fun (cookie : User_model.cookie) ->
                  not (String.equal cookie.value session_value))
                user.cookies
            in
            let updated_user =
              User_model.update_user user ~cookies ~updated_at:now ()
            in
            Store.update_user store updated_user >>= function
            | Ok () ->
                Middleware.http_response reqd ~title:"Success"
                  ~data:(`String "Session closed succesfully") `OK
            | Error (`Msg err) ->
                Logs.warn (fun m -> m "Storage error with %s" err);
                Middleware.http_response reqd ~title:"Error"
                  ~data:(`String (String.escaped err))
                  `Internal_server_error)
        | _ ->
            Middleware.http_response reqd ~title:"Error"
              ~data:
                (`String
                   (Fmt.str "Close session: Unexpected fields. Got %s"
                      (Yojson.Basic.to_string (`Assoc json_dict))))
              `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Expected a JSON body ") `Bad_request

  let users store reqd ~request_body:_ (user : User_model.user) =
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

  let settings store reqd ~request_body:_ (user : User_model.user) =
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

  let update_settings stack store albatross reqd ~request_body _user =
    match request_body with
    | `JSON json_dict -> (
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
                  ~data:(`String "Configuration updated successfully") `OK
            | Error (`Msg err) ->
                Middleware.http_response reqd ~title:"Error"
                  ~data:(`String (String.escaped err))
                  `Internal_server_error)
        | Error (`Msg err) ->
            Middleware.http_response ~title:"Error"
              ~data:(`String (String.escaped err))
              reqd `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Expected a JSON body ") `Bad_request

  let deploy_form store reqd ~request_body:_ (user : User_model.user) =
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

  let unikernel_info albatross reqd ~request_body:_ (user : User_model.user) =
    (* TODO use uuid in the future *)
    Albatross.query albatross ~domain:user.name (`Unikernel_cmd `Unikernel_info)
    >>= function
    | Error msg ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String ("Error while querying albatross: " ^ msg))
          `Internal_server_error
    | Ok (_hdr, res) -> (
        match Albatross_json.res res with
        | Ok res -> Middleware.http_response reqd ~title:"Success" ~data:res `OK
        | Error (`String err) ->
            Middleware.http_response reqd ~title:"Error"
              ~data:(`String (String.escaped err))
              `Internal_server_error)

  let unikernel_info_one albatross store name reqd ~request_body:_
      (user : User_model.user) =
    (* TODO use uuid in the future *)
    user_unikernel albatross ~user_name:user.name ~unikernel_name:name
    >>= fun unikernel_info ->
    match unikernel_info with
    | Error err ->
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
                ~content:
                  (Error_page.error_layout
                     {
                       code = 500;
                       success = false;
                       title = "Albatross Error";
                       data =
                         `String
                           ("An error occured trying to fetch " ^ name
                          ^ "from albatross: " ^ err);
                     })
                ~icon:"/images/robur.png" ())
             `Internal_server_error)
    | Ok unikernel -> (
        ( Albatross.query_console ~domain:user.name albatross ~name >|= function
          | Error err ->
              Logs.warn (fun m ->
                  m "error querying console of albatross: %s" err);
              []
          | Ok (_, console_output) -> console_output )
        >>= fun console_output ->
        let now = Ptime.v (P.now_d_ps ()) in
        generate_csrf_token store user now reqd >>= function
        | Ok csrf ->
            Lwt.return
              (reply reqd ~content_type:"text/html"
                 (Dashboard.dashboard_layout ~csrf user
                    ~content:
                      (Unikernel_single.unikernel_single_layout
                         ~unikernel_name:name unikernel now console_output)
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

  let unikernel_prepare_update albatross store name reqd http_client
      ~request_body:_ (user : User_model.user) =
    (* TODO use uuid in the future *)
    user_unikernel albatross ~user_name:user.name ~unikernel_name:name
    >>= fun unikernel_info ->
    match unikernel_info with
    | Error err ->
        Middleware.redirect_to_error
          ~data:
            (`String
               ("An error occured while fetching " ^ name
              ^ " from albatross with error " ^ err))
          ~title:"Albatross Error" ~api_meth:false `Internal_server_error reqd
          ()
    | Ok (unikernel_name, unikernel) -> (
        Builder_web.send_request http_client
          ("/hash?sha256=" ^ Ohex.encode unikernel.digest)
        >>= function
        | Error (`Msg err) ->
            Logs.err (fun m ->
                m
                  "builds.robur.coop: Error while fetching the current build \
                   info of %s with error: %s"
                  name err);
            Middleware.redirect_to_error
              ~data:
                (`String
                   ("An error occured while fetching the current build \
                     information from builds.robur.coop. The error is: " ^ err))
              ~title:(name ^ " update Error") ~api_meth:false
              `Internal_server_error reqd ()
        | Ok response_body -> (
            match
              Builder_web.build_of_json (Yojson.Basic.from_string response_body)
            with
            | Error (`Msg err) ->
                Logs.err (fun m ->
                    m
                      "JSON parsing of the current build of %s from \
                       builds.robur.coop failed with error: %s"
                      name err);
                Middleware.redirect_to_error
                  ~data:
                    (`String
                       ("An error occured while parsing the json of the \
                         current build from builds.robur.coop. The error is: "
                      ^ err))
                  ~title:(name ^ " update Error") ~api_meth:false
                  `Internal_server_error reqd ()
            | Ok current_job_data -> (
                Builder_web.send_request http_client
                  ("/job/" ^ current_job_data.job ^ "/build/latest")
                >>= function
                | Error (`Msg err) ->
                    Logs.err (fun m ->
                        m
                          "builds.robur.coop: Error while fetching the latest \
                           build info of %s with error: %s"
                          name err);
                    Middleware.redirect_to_error
                      ~data:
                        (`String
                           ("An error occured while fetching the latest build \
                             information from builds.robur.coop. The error \
                             is: " ^ err))
                      ~title:(name ^ " update Error") ~api_meth:false
                      `Internal_server_error reqd ()
                | Ok response_body -> (
                    match
                      Builder_web.build_of_json
                        (Yojson.Basic.from_string response_body)
                    with
                    | Error (`Msg err) ->
                        Logs.err (fun m ->
                            m
                              "JSON parsing of the latest build of %s from \
                               builds.robur.coop failed with error: %s"
                              name err);
                        Middleware.redirect_to_error
                          ~data:
                            (`String
                               ("An error occured while parsing the json of \
                                 the latest build from builds.robur.coop. The \
                                 error is: " ^ err))
                          ~title:(name ^ "update Error") ~api_meth:false
                          `Internal_server_error reqd ()
                    | Ok latest_job_data -> (
                        if
                          String.equal latest_job_data.uuid
                            current_job_data.uuid
                        then (
                          Logs.info (fun m ->
                              m
                                "There is no new update of %s found with uuid  \
                                 %s"
                                name latest_job_data.uuid);
                          Middleware.redirect_to_page
                            ~path:
                              ("/unikernel/info/"
                              ^ Option.value ~default:""
                                  (Vmm_core.Name.name unikernel_name))
                            reqd
                            ~msg:
                              ("There is no update of " ^ name
                             ^ " found on builds.robur.coop")
                            ())
                        else
                          Builder_web.send_request http_client
                            ("/compare/" ^ current_job_data.uuid ^ "/"
                           ^ latest_job_data.uuid ^ "")
                          >>= function
                          | Error (`Msg err) ->
                              Logs.err (fun m ->
                                  m
                                    "builds.robur.coop: Error while fetching \
                                     the diff between the current and latest \
                                     build info of %s with error: %s"
                                    name err);
                              Middleware.redirect_to_error
                                ~data:
                                  (`String
                                     ("An error occured while fetching the \
                                       diff between the latest and the current \
                                       build information from \
                                       builds.robur.coop. The error is: " ^ err
                                     ))
                                ~title:(name ^ " update Error") ~api_meth:false
                                `Internal_server_error reqd ()
                          | Ok response_body -> (
                              match
                                Builder_web.compare_of_json
                                  (Yojson.Basic.from_string response_body)
                              with
                              | Ok build_comparison -> (
                                  let now = Ptime.v (P.now_d_ps ()) in
                                  generate_csrf_token store user now reqd
                                  >>= function
                                  | Ok csrf ->
                                      Lwt.return
                                        (reply reqd ~content_type:"text/html"
                                           (Dashboard.dashboard_layout ~csrf
                                              user
                                              ~page_title:
                                                (Vmm_core.Name.to_string
                                                   unikernel_name
                                                ^ " Update | Mollymawk")
                                              ~content:
                                                (Unikernel_update
                                                 .unikernel_update_layout
                                                   ~unikernel_name:name
                                                   (unikernel_name, unikernel)
                                                   now build_comparison)
                                              ~icon:"/images/robur.png" ())
                                           ~header_list:
                                             [ ("X-MOLLY-CSRF", csrf) ]
                                           `OK)
                                  | Error err ->
                                      Lwt.return
                                        (reply reqd ~content_type:"text/html"
                                           (Guest_layout.guest_layout
                                              ~page_title:
                                                "CSRF Token Error | Mollymawk"
                                              ~content:
                                                (Error_page.error_layout err)
                                              ~icon:"/images/robur.png" ())
                                           `Internal_server_error))
                              | Error (`Msg err) ->
                                  Logs.err (fun m ->
                                      m
                                        "JSON parsing of the diff between the \
                                         latest and current build of %s from \
                                         builds.robur.coop failed with error: \
                                         %s"
                                        name err);
                                  Middleware.redirect_to_error
                                    ~data:
                                      (`String
                                         ("An error occured while parsing the \
                                           json of the diff between the latest \
                                           and curent build from \
                                           builds.robur.coop. The error is: "
                                        ^ err))
                                    ~title:(name ^ " update Error")
                                    ~api_meth:false `Internal_server_error reqd
                                    ()))))))

  let process_unikernel_update ~unikernel_name ~job ~build
      (unikernel_cfg : Vmm_core.Unikernel.config) (user : User_model.user)
      albatross http_client reqd =
    Builder_web.send_request http_client
      ("/job/" ^ job ^ "/build/" ^ build ^ "/main-binary")
    >>= function
    | Error (`Msg err) ->
        Logs.err (fun m ->
            m
              "builds.robur.coop: Error while fetching the binary of %s with \
               error: %s"
              unikernel_name err);
        Middleware.http_response reqd ~title:"Error"
          ~data:
            (`String
               ("An error occured while fetching the binary from \
                 builds.robur.coop with error " ^ err))
          `Internal_server_error
    | Ok image -> (
        match
          Albatross.manifest_devices_match ~bridges:unikernel_cfg.bridges
            ~block_devices:unikernel_cfg.block_devices image
        with
        | Error (`Msg err) ->
            Middleware.http_response reqd ~title:"Error"
              ~data:
                (`String
                   ("An error occured with the unikernel configuration: " ^ err))
              `Bad_request
        | Ok () -> (
            let unikernel_config = { unikernel_cfg with image } in
            Albatross.query albatross ~domain:user.name ~name:unikernel_name
              (`Unikernel_cmd (`Unikernel_force_create unikernel_config))
            >>= function
            | Error msg ->
                Logs.err (fun m ->
                    m "albatross-force-create: error querying albatross: %s" msg);
                Middleware.http_response reqd ~title:"Error"
                  ~data:
                    (`String ("Force create: Error querying albatross: " ^ msg))
                  `Internal_server_error
            | Ok (_hdr, res) -> (
                match Albatross_json.res res with
                | Error (`String err) ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:
                        (`String
                           ("albatross force-create: " ^ String.escaped err))
                      `Internal_server_error
                | Ok res ->
                    Logs.info (fun m ->
                        m "%s has been updated succesfully with result: %s"
                          unikernel_name
                          (Yojson.Basic.to_string res));
                    Middleware.http_response reqd ~title:"Update Successful"
                      ~data:
                        (`String
                           (unikernel_name
                          ^ " has been updated to the latest build with uuid "
                          ^ build))
                      `OK)))

  let unikernel_update albatross reqd http_client ~request_body
      (user : User_model.user) =
    let config_or_none field = function
      | None | Some `Null -> Ok None
      | Some json -> (
          match Albatross_json.config_of_json (Yojson.Basic.to_string json) with
          | Ok cfg -> Ok (Some cfg)
          | Error (`Msg err) ->
              Error
                (`Msg
                   ("invalid json for " ^ field ^ ": "
                   ^ Yojson.Basic.to_string json
                   ^ "failed with: " ^ err)))
    in
    match request_body with
    | `JSON json_dict -> (
        match
          Utils.Json.
            ( get "job" json_dict,
              get "build" json_dict,
              get "unikernel_name" json_dict,
              get "unikernel_arguments" json_dict )
        with
        | ( Some (`String job),
            Some (`String build),
            Some (`String unikernel_name),
            configuration ) -> (
            match config_or_none "unikernel_arguments" configuration with
            | Error (`Msg err) ->
                Middleware.http_response reqd
                  ~title:"Error with Unikernel Arguments Json"
                  ~data:
                    (`String
                       ("Could not get the unikernel arguments json: " ^ err))
                  `OK
            | Ok None -> (
                user_unikernel albatross ~user_name:user.name ~unikernel_name
                >>= fun unikernel_info ->
                match unikernel_info with
                | Error err ->
                    Middleware.redirect_to_error
                      ~data:
                        (`String
                           ("An error occured while fetching " ^ unikernel_name
                          ^ " from albatross with error " ^ err))
                      ~title:"Albatross Error" ~api_meth:false
                      `Internal_server_error reqd ()
                | Ok (n, unikernel) -> (
                    match
                      Albatross_json.(
                        unikernel_info (n, unikernel)
                        |> Yojson.Basic.to_string |> config_of_json)
                    with
                    | Ok cfg ->
                        process_unikernel_update ~unikernel_name ~job ~build cfg
                          user albatross http_client reqd
                    | Error (`Msg err) ->
                        Logs.warn (fun m -> m "Couldn't decode data %s" err);
                        Middleware.http_response reqd ~title:"Error"
                          ~data:(`String (String.escaped err))
                          `Internal_server_error))
            | Ok (Some cfg) ->
                process_unikernel_update ~unikernel_name ~job ~build cfg user
                  albatross http_client reqd)
        | _ ->
            Middleware.http_response reqd ~title:"Error"
              ~data:(`String "Couldn't find job or build in json. Received ")
              `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Expected a JSON body ") `Bad_request

  let unikernel_destroy ~request_body albatross reqd (user : User_model.user) =
    (* TODO use uuid in the future *)
    match request_body with
    | `JSON json_dict -> (
        match Utils.Json.get "name" json_dict with
        | Some (`String unikernel_name) -> (
            Albatross.query albatross ~domain:user.name ~name:unikernel_name
              (`Unikernel_cmd `Unikernel_destroy)
            >>= function
            | Error msg ->
                Logs.err (fun m -> m "Error querying albatross: %s" msg);
                Middleware.http_response reqd ~title:"Error"
                  ~data:(`String ("Error querying albatross: " ^ msg))
                  `Internal_server_error
            | Ok (_hdr, res) -> (
                match Albatross_json.res res with
                | Ok res ->
                    Middleware.http_response reqd ~title:"Success" ~data:res `OK
                | Error (`String err) ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(`String (String.escaped err))
                      `Internal_server_error))
        | _ ->
            Middleware.http_response reqd ~title:"Error"
              ~data:(`String "Couldn't find unikernel name in json")
              `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Expected a JSON body ") `Bad_request

  let unikernel_restart ~request_body albatross reqd (user : User_model.user) =
    (* TODO use uuid in the future *)
    match request_body with
    | `JSON json_dict -> (
        match Utils.Json.get "name" json_dict with
        | Some (`String unikernel_name) -> (
            Albatross.query albatross ~domain:user.name ~name:unikernel_name
              (`Unikernel_cmd (`Unikernel_restart None))
            >>= function
            | Error msg ->
                Logs.err (fun m -> m "Error querying albatross: %s" msg);
                Middleware.http_response reqd ~title:"Error"
                  ~data:(`String ("Error querying albatross: " ^ msg))
                  `Internal_server_error
            | Ok (_hdr, res) -> (
                match Albatross_json.res res with
                | Ok res ->
                    Middleware.http_response reqd ~title:"Success" ~data:res `OK
                | Error (`String err) ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(`String (String.escaped err))
                      `Internal_server_error))
        | _ ->
            Middleware.http_response reqd ~title:"Error"
              ~data:(`String "Couldn't find unikernel name in json")
              `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Expected a JSON body ") `Bad_request

  let unikernel_create albatross reqd ~request_body (user : User_model.user) =
    match request_body with
    | `Multipart m -> (
        match
          ( Map.find_opt "arguments" m,
            Map.find_opt "name" m,
            Map.find_opt "binary" m )
        with
        | Some (_, args), Some (_, name), Some (_, binary) -> (
            match Albatross_json.config_of_json args with
            | Ok cfg -> (
                let config = { cfg with image = binary } in
                (* TODO use uuid in the future *)
                Albatross.query albatross ~domain:user.name ~name
                  (`Unikernel_cmd (`Unikernel_create config))
                >>= function
                | Error err ->
                    Logs.warn (fun m -> m "Error querying albatross: %s" err);
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(`String ("Error while querying Albatross: " ^ err))
                      `Internal_server_error
                | Ok (_hdr, res) -> (
                    match Albatross_json.res res with
                    | Ok res ->
                        Middleware.http_response reqd ~title:"Success" ~data:res
                          `OK
                    | Error (`String err) ->
                        Middleware.http_response reqd ~title:"Error"
                          ~data:(`String (String.escaped err))
                          `Internal_server_error))
            | Error (`Msg err) ->
                Logs.warn (fun m -> m "couldn't decode data %s" err);
                Middleware.http_response reqd ~title:"Error"
                  ~data:(`String (String.escaped err))
                  `Internal_server_error)
        | _ ->
            Logs.warn (fun m -> m "couldn't find fields");
            Middleware.http_response reqd ~title:"Error"
              ~data:(`String "Couldn't find fields") `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Expected a multipart/form-data request body ")
          `Bad_request

  let unikernel_console albatross name reqd ~request_body:_
      (user : User_model.user) =
    (* TODO use uuid in the future *)
    Albatross.query_console ~domain:user.name albatross ~name >>= function
    | Error err ->
        Logs.warn (fun m -> m "error querying albatross: %s" err);
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String ("Error while querying Albatross: " ^ err))
          `Internal_server_error
    | Ok (_, console_output) ->
        let console_output =
          List.map Albatross_json.console_data_to_json console_output
        in
        Lwt.return
          (reply reqd ~content_type:"application/json"
             (Yojson.Basic.to_string (`List console_output))
             `OK)

  let view_user albatross store uuid reqd ~request_body:_
      (user : User_model.user) =
    match Store.find_by_uuid store uuid with
    | Some u -> (
        user_unikernels albatross user.name >>= fun unikernels ->
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
            data = `String ("Couldn't find account with uuid: " ^ uuid);
            success = false;
          }
        in
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Guest_layout.guest_layout ~page_title:"404 | Mollymawk"
                ~content:(Error_page.error_layout status)
                ~icon:"/images/robur.png" ())
             `Not_found)

  let edit_policy albatross store uuid reqd ~request_body:_
      (user : User_model.user) =
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
                data = `String ("Policy error: " ^ err);
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
            data = `String ("Couldn't find account with uuid: " ^ uuid);
            success = false;
          }
        in
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Guest_layout.guest_layout ~page_title:"404 | Mollymawk"
                ~content:(Error_page.error_layout status)
                ~icon:"/images/robur.png" ())
             `Not_found)

  let update_policy store albatross reqd ~request_body _user =
    match request_body with
    | `JSON json_dict -> (
        match Utils.Json.get "user_uuid" json_dict with
        | Some (`String user_uuid) -> (
            match Store.find_by_uuid store user_uuid with
            | Some u -> (
                match Albatross_json.policy_of_json json_dict with
                | Ok policy -> (
                    match Albatross.policy albatross with
                    | Ok (Some root_policy) -> (
                        match
                          Vmm_core.Policy.is_smaller ~super:root_policy
                            ~sub:policy
                        with
                        | Error (`Msg err) ->
                            Logs.err (fun m ->
                                m
                                  "policy %a is not smaller than root policy \
                                   %a: %s"
                                  Vmm_core.Policy.pp policy Vmm_core.Policy.pp
                                  root_policy err);
                            Middleware.http_response reqd ~title:"Error"
                              ~data:
                                (`String
                                   ("Policy is not smaller than root policy: "
                                  ^ err))
                              `Internal_server_error
                        | Ok () -> (
                            Albatross.set_policy albatross ~domain:u.name policy
                            >>= function
                            | Error err ->
                                Logs.err (fun m ->
                                    m "error setting policy %a for %s: %s"
                                      Vmm_core.Policy.pp policy u.name err);
                                Middleware.http_response reqd ~title:"Error"
                                  ~data:
                                    (`String ("error setting policy: " ^ err))
                                  `Internal_server_error
                            | Ok policy ->
                                Middleware.http_response reqd ~title:"Success"
                                  ~data:(Albatross_json.policy_info policy)
                                  `OK))
                    | Ok None ->
                        Logs.err (fun m ->
                            m "policy: root policy can't be null ");
                        Middleware.http_response reqd ~title:"Error"
                          ~data:(`String "Root policy is null")
                          `Internal_server_error
                    | Error err ->
                        Logs.err (fun m ->
                            m
                              "policy: an error occured while fetching root \
                               policy: %s"
                              err);
                        Middleware.http_response reqd ~title:"Error"
                          ~data:
                            (`String
                               ("error with root policy: " ^ String.escaped err))
                          `Internal_server_error)
                | Error (`Msg err) ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(`String (String.escaped err))
                      `Bad_request)
            | None ->
                Middleware.http_response reqd ~title:"Error"
                  ~data:(`String "User not found") `Not_found)
        | _ ->
            Middleware.http_response reqd ~title:"Error"
              ~data:
                (`String
                   (Fmt.str "Update policy: Unexpected fields. Got %s"
                      (Yojson.Basic.to_string (`Assoc json_dict))))
              `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Expected a JSON body ") `Bad_request

  let volumes store albatross reqd ~request_body:_ (user : User_model.user) =
    user_volumes albatross user.name >>= fun blocks ->
    let policy =
      Result.fold ~ok:Fun.id
        ~error:(fun _ -> None)
        (Albatross.policy ~domain:user.name albatross)
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

  let delete_volume albatross reqd ~request_body (user : User_model.user) =
    match request_body with
    | `JSON json_dict -> (
        match Utils.Json.get "block_name" json_dict with
        | Some (`String block_name) -> (
            Albatross.query albatross ~domain:user.name ~name:block_name
              (`Block_cmd `Block_remove)
            >>= function
            | Error err ->
                Logs.err (fun m ->
                    m "Error querying albatross: %s" (String.escaped err));
                Middleware.http_response reqd ~title:"Error"
                  ~data:
                    (`String ("Error querying albatross: " ^ String.escaped err))
                  `Internal_server_error
            | Ok (_hdr, res) -> (
                match Albatross_json.res res with
                | Ok res ->
                    Middleware.http_response reqd ~title:"Success" ~data:res `OK
                | Error (`String err) ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(`String (String.escaped err))
                      `Internal_server_error))
        | _ ->
            Middleware.http_response reqd ~title:"Error"
              ~data:(`String "Couldn't find block name in json") `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Expected a JSON body ") `Bad_request

  let create_volume albatross reqd ~request_body (user : User_model.user) =
    match request_body with
    | `Multipart m -> (
        match (Map.find_opt "json_data" m, Map.find_opt "block_data" m) with
        | Some (_, json_data), Some (_, block_data) -> (
            let json =
              try Ok (Yojson.Basic.from_string json_data)
              with Yojson.Json_error s -> Error (`Msg s)
            in
            match json with
            | Ok (`Assoc json_dict) -> (
                match
                  Utils.Json.
                    ( get "block_name" json_dict,
                      get "block_size" json_dict,
                      get "block_compressed" json_dict )
                with
                | ( Some (`String block_name),
                    Some (`Int block_size),
                    Some (`Bool block_compressed) ) -> (
                    Albatross.query albatross ~domain:user.name ~name:block_name
                      (`Block_cmd
                         (`Block_add
                            (block_size, block_compressed, Some block_data)))
                    >>= function
                    | Error err ->
                        Logs.err (fun m ->
                            m "Error querying albatross: %s"
                              (String.escaped err));
                        Middleware.http_response reqd ~title:"Error"
                          ~data:
                            (`String
                               ("Error querying albatross: "
                              ^ String.escaped err))
                          `Internal_server_error
                    | Ok (_hdr, res) -> (
                        match Albatross_json.res res with
                        | Ok res ->
                            Middleware.http_response reqd ~title:"Success"
                              ~data:res `OK
                        | Error (`String err) ->
                            Middleware.http_response reqd ~title:"Error"
                              ~data:(`String (String.escaped err))
                              `Internal_server_error))
                | _ ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:
                        (`String
                           (Fmt.str "Create volume: Unexpected fields. Got %s"
                              (Yojson.Basic.to_string (`Assoc json_dict))))
                      `Bad_request)
            | _ ->
                Middleware.http_response reqd ~title:"Error"
                  ~data:(`String "Create volume: expected a dictionary")
                  `Bad_request)
        | _ ->
            Logs.warn (fun m -> m "couldn't find fields");
            Middleware.http_response reqd ~title:"Error"
              ~data:(`String "Couldn't find fields") `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Expected a multipart/form-data request body ")
          `Bad_request

  let download_volume albatross reqd ~request_body (user : User_model.user) =
    match request_body with
    | `JSON json_dict -> (
        match
          Utils.Json.
            (get "block_name" json_dict, get "compression_level" json_dict)
        with
        | Some (`String block_name), Some (`Int compression_level) -> (
            Albatross.query albatross ~domain:user.name ~name:block_name
              (`Block_cmd (`Block_dump compression_level))
            >>= function
            | Error err ->
                Logs.err (fun m ->
                    m "Error querying albatross: %s" (String.escaped err));
                Middleware.http_response reqd ~title:"Error"
                  ~data:
                    (`String ("Error querying albatross: " ^ String.escaped err))
                  `Internal_server_error
            | Ok (_hdr, res) -> (
                match Albatross_json.res res with
                | Ok res ->
                    let file_content = Yojson.Basic.to_string res in
                    let filename = block_name ^ "_dump" in
                    let disposition =
                      "attachment; filename=\"" ^ filename ^ "\""
                    in
                    reply reqd ~content_type:"application/octet-stream"
                      ~header_list:[ ("Content-Disposition", disposition) ]
                      file_content `OK
                    |> Lwt.return
                | Error (`String err) ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(`String (String.escaped err))
                      `Internal_server_error))
        | _ ->
            Middleware.http_response reqd ~title:"Error"
              ~data:(`String "Couldn't find block name in json") `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Expected a JSON body ") `Bad_request

  let upload_to_volume albatross reqd ~request_body (user : User_model.user) =
    match request_body with
    | `Multipart m -> (
        match (Map.find_opt "json_data" m, Map.find_opt "block_data" m) with
        | Some (_, json_data), Some (_, block_data) -> (
            let json =
              try Ok (Yojson.Basic.from_string json_data)
              with Yojson.Json_error s -> Error (`Msg s)
            in
            match json with
            | Ok (`Assoc json_dict) -> (
                match
                  Utils.Json.
                    ( get "block_name" json_dict,
                      get "block_compressed" json_dict )
                with
                | Some (`String block_name), Some (`Bool block_compressed) -> (
                    Albatross.query albatross ~domain:user.name ~name:block_name
                      (`Block_cmd (`Block_set (block_compressed, block_data)))
                    >>= function
                    | Error err ->
                        Logs.err (fun m ->
                            m "Error querying albatross: %s"
                              (String.escaped err));
                        Middleware.http_response reqd ~title:"Error"
                          ~data:
                            (`String
                               ("Error querying albatross: "
                              ^ String.escaped err))
                          `Internal_server_error
                    | Ok (_hdr, res) -> (
                        match Albatross_json.res res with
                        | Ok res ->
                            Middleware.http_response reqd ~title:"Success"
                              ~data:res `OK
                        | Error (`String err) ->
                            Middleware.http_response reqd ~title:"Error"
                              ~data:(`String (String.escaped err))
                              `Internal_server_error))
                | _ ->
                    Middleware.http_response reqd ~title:"Error"
                      ~data:
                        (`String
                           (Fmt.str
                              "Upload to volume: Unexpected fields. Got %s"
                              (Yojson.Basic.to_string (`Assoc json_dict))))
                      `Bad_request)
            | _ ->
                Middleware.http_response reqd ~title:"Error"
                  ~data:(`String "Upload to volume: expected a dictionary")
                  `Bad_request)
        | _ ->
            Logs.warn (fun m -> m "couldn't find fields");
            Middleware.http_response reqd ~title:"Error"
              ~data:(`String "Couldn't find fields") `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Expected a multipart/form-data request body ")
          `Bad_request

  let account_usage store albatross reqd ~request_body:_
      (user : User_model.user) =
    let now = Ptime.v (P.now_d_ps ()) in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        user_volumes albatross user.name >>= fun blocks ->
        user_unikernels albatross user.name >>= fun unikernels ->
        let policy =
          match Albatross.policy albatross ~domain:user.name with
          | Ok p -> (
              match p with Some p -> p | None -> Albatross.empty_policy)
          | Error _ -> Albatross.empty_policy
        in
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Dashboard.dashboard_layout ~csrf user
                ~page_title:"Usage | Mollymawk"
                ~content:
                  (Account_usage.account_usage_layout policy unikernels blocks)
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

  let api_tokens store reqd ~request_body:_ (user : User_model.user) =
    let now = Ptime.v (P.now_d_ps ()) in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Dashboard.dashboard_layout ~csrf user
                ~page_title:"Tokens | Mollymawk"
                ~content:(Tokens_index.tokens_index_layout user.tokens now)
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

  let create_token store reqd ~request_body (user : User_model.user) =
    match request_body with
    | `JSON json_dict -> (
        match
          Utils.Json.(get "token_name" json_dict, get "token_expiry" json_dict)
        with
        | Some (`String name), Some (`Int expiry) -> (
            let now = Ptime.v (P.now_d_ps ()) in
            let token =
              User_model.generate_token ~name ~expiry ~current_time:now
            in
            let updated_user =
              User_model.update_user user ~tokens:(token :: user.tokens)
                ~updated_at:now ()
            in
            Store.update_user store updated_user >>= function
            | Ok () ->
                Middleware.http_response reqd ~title:"Success"
                  ~data:(User_model.token_to_json token)
                  `OK
            | Error (`Msg err) ->
                Logs.warn (fun m -> m "Storage error with %s" err);
                Middleware.http_response reqd ~title:"Error"
                  ~data:(`String (String.escaped err))
                  `Internal_server_error)
        | _ ->
            Middleware.http_response reqd ~title:"Error"
              ~data:
                (`String
                   (Fmt.str "Create token: Unexpected fields. Got %s"
                      (Yojson.Basic.to_string (`Assoc json_dict))))
              `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Expected a JSON body ") `Bad_request

  let delete_token store reqd ~request_body (user : User_model.user) =
    match request_body with
    | `JSON json_dict -> (
        match Utils.Json.(get "token_value" json_dict) with
        | Some (`String value) -> (
            let now = Ptime.v (P.now_d_ps ()) in
            let tokens =
              List.filter
                (fun (token : User_model.token) ->
                  not (String.equal token.value value))
                user.tokens
            in
            let updated_user =
              User_model.update_user user ~tokens ~updated_at:now ()
            in
            Store.update_user store updated_user >>= function
            | Ok () ->
                Middleware.http_response reqd ~title:"Success"
                  ~data:(`String "Token deleted succesfully") `OK
            | Error (`Msg err) ->
                Logs.warn (fun m -> m "Storage error with %s" err);
                Middleware.http_response reqd ~title:"Error"
                  ~data:(`String (String.escaped err))
                  `Internal_server_error)
        | _ ->
            Middleware.http_response reqd ~title:"Error"
              ~data:
                (`String
                   (Fmt.str "Delete token: Unexpected fields. Got %s"
                      (Yojson.Basic.to_string (`Assoc json_dict))))
              `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Expected a JSON body ") `Bad_request

  let update_token store reqd ~request_body (user : User_model.user) =
    match request_body with
    | `JSON json_dict -> (
        match
          Utils.Json.
            ( get "token_name" json_dict,
              get "token_expiry" json_dict,
              get "token_value" json_dict )
        with
        | Some (`String name), Some (`Int expiry), Some (`String value) -> (
            let now = Ptime.v (P.now_d_ps ()) in
            let token =
              List.find_opt
                (fun (token : User_model.token) ->
                  String.equal token.value value)
                user.tokens
            in
            match token with
            | Some token_ -> (
                let updated_token = { token_ with name; expires_in = expiry } in
                let user_tokens =
                  List.filter
                    (fun (token : User_model.token) ->
                      not (String.equal token.value value))
                    user.tokens
                in
                let updated_user =
                  User_model.update_user user
                    ~tokens:(updated_token :: user_tokens)
                    ~updated_at:now ()
                in
                Store.update_user store updated_user >>= function
                | Ok () ->
                    Middleware.http_response reqd ~title:"Success"
                      ~data:(User_model.token_to_json updated_token)
                      `OK
                | Error (`Msg err) ->
                    Logs.warn (fun m -> m "Storage error with %s" err);
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(`String (String.escaped err))
                      `Internal_server_error)
            | None ->
                Middleware.http_response reqd ~title:"Error"
                  ~data:(`String "Token not found") `Bad_request)
        | _ ->
            Middleware.http_response reqd ~title:"Error"
              ~data:
                (`String
                   (Fmt.str "Update token: Unexpected fields. Got %s"
                      (Yojson.Basic.to_string (`Assoc json_dict))))
              `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Expected a JSON body ") `Bad_request

  let request_handler stack albatross js_file css_file imgs store http_client
      (_ipaddr, _port) reqd =
    Lwt.async (fun () ->
        let bad_request () =
          Middleware.http_response reqd ~title:"Error"
            ~data:(`String "Bad HTTP request method.") `Bad_request
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
                authenticate ~check_csrf:true store reqd
                  (update_password store reqd))
        | "/api/account/sessions/close" ->
            check_meth `POST (fun () ->
                authenticate ~check_csrf:true store reqd
                  (close_sessions store reqd))
        | "/logout" ->
            check_meth `POST (fun () ->
                authenticate ~check_csrf:true store reqd
                  (close_sessions ~logout:true store reqd))
        | "/api/account/session/close" ->
            check_meth `POST (fun () ->
                authenticate ~check_csrf:true store reqd
                  (close_session store reqd))
        | "/volumes" ->
            check_meth `GET (fun () ->
                authenticate store reqd (volumes store !albatross reqd))
        | "/api/volume/delete" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~check_csrf:true ~api_meth:true
                  store reqd
                  (delete_volume !albatross reqd))
        | "/api/volume/create" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~check_csrf:true ~api_meth:true
                  store reqd
                  (create_volume !albatross reqd))
        | "/api/volume/download" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~check_csrf:true ~api_meth:true
                  store reqd
                  (download_volume !albatross reqd))
        | "/api/volume/upload" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~check_csrf:true ~api_meth:true
                  store reqd
                  (upload_to_volume !albatross reqd))
        | "/tokens" ->
            check_meth `GET (fun () ->
                authenticate store reqd (api_tokens store reqd))
        | "/api/tokens/create" ->
            check_meth `POST (fun () ->
                authenticate ~check_csrf:true ~api_meth:true store reqd
                  (create_token store reqd))
        | "/api/tokens/delete" ->
            check_meth `POST (fun () ->
                authenticate ~check_csrf:true ~api_meth:true store reqd
                  (delete_token store reqd))
        | "/api/tokens/update" ->
            check_meth `POST (fun () ->
                authenticate ~check_csrf:true ~api_meth:true store reqd
                  (update_token store reqd))
        | "/admin/users" ->
            check_meth `GET (fun () ->
                authenticate ~check_admin:true store reqd (users store reqd))
        | "/usage" ->
            check_meth `GET (fun () ->
                authenticate store reqd (account_usage store !albatross reqd))
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
                authenticate ~check_csrf:true ~check_admin:true ~api_meth:true
                  store reqd
                  (update_settings stack store albatross reqd))
        | "/api/admin/u/policy/update" ->
            check_meth `POST (fun () ->
                authenticate ~check_csrf:true ~check_admin:true ~api_meth:true
                  store reqd
                  (update_policy store !albatross reqd))
        | "/api/admin/user/activate/toggle" ->
            check_meth `POST (fun () ->
                authenticate ~check_csrf:true ~check_admin:true ~api_meth:true
                  store reqd
                  (toggle_account_activation store reqd))
        | "/api/admin/user/admin/toggle" ->
            check_meth `POST (fun () ->
                authenticate ~check_csrf:true ~check_admin:true ~api_meth:true
                  store reqd
                  (toggle_admin_activation store reqd))
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
        | "/api/unikernel/destroy" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (unikernel_destroy !albatross reqd))
        | "/api/unikernel/restart" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~check_csrf:true ~api_meth:true
                  store reqd
                  (unikernel_restart !albatross reqd))
        | path when String.starts_with ~prefix:"/unikernel/console/" path ->
            check_meth `GET (fun () ->
                let unikernel_name =
                  String.sub path 19 (String.length path - 19)
                in
                authenticate store reqd
                  (unikernel_console !albatross unikernel_name reqd))
        | "/api/unikernel/create" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~check_csrf:true ~api_meth:true
                  store reqd
                  (unikernel_create !albatross reqd))
        | path when String.starts_with ~prefix:"/unikernel/update/" path ->
            check_meth `GET (fun () ->
                let unikernel_name =
                  String.sub path 18 (String.length path - 18)
                in
                authenticate store reqd
                  (unikernel_prepare_update !albatross store unikernel_name reqd
                     http_client))
        | "/api/unikernel/update" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~check_csrf:true ~api_meth:true
                  store reqd
                  (unikernel_update !albatross reqd http_client))
        | _ ->
            let error =
              {
                Utils.Status.code = 404;
                title = "Page not found";
                success = false;
                data = `String "This page cannot be found.";
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

  let start _ _ _ _ stack assets storage http_client =
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
            http_client
        in
        Paf.init ~port:8080 (S.tcp stack) >>= fun service ->
        let http = Paf.http_service ~error_handler request_handler in
        let (`Initialized th) = Paf.serve http service in
        th
end
