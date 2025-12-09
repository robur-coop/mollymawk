open Lwt.Infix

type images = {
  molly_img : string;
  robur_img : string;
  albatross_img : string;
  mirage_img : string;
  dashboard_img : string;
}

module K = struct
  open Cmdliner

  let port =
    let doc = Arg.info ~doc:"HTTP listen port." [ "port" ] in
    Mirage_runtime.register_arg Arg.(value & opt int 8080 doc)
end

module Main
    (S : Tcpip.Stack.V4V6)
    (KV_ASSETS : Mirage_kv.RO)
    (BLOCK : Mirage_block.S)
    (Http_client : Http_mirage_client.S) =
struct
  module Paf = Paf_mirage.Make (S.TCP)
  module Liveliness = Liveliness_checks.Make (S)
  module Albatross_state = Albatross.Make (S)

  let update_albatross_status (t : Albatross.t)
      (err :
        Albatross.Status.category
        * (Vmm_commands.header * Vmm_commands.res)
        * string) =
    let category, reply, kind = err in
    let err_str =
      Fmt.str "Expected a %s reply, got %a" kind
        (Vmm_commands.pp_wire ~verbose:false)
        reply
    in
    Logs.err (fun m ->
        m "albatross %s: %s"
          (Configuration.name_to_str t.configuration.name)
          err_str);
    t.status <-
      Albatross.Status.update t.status (Albatross.Status.make category err_str)

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
    let molly_img = read_image assets "mollymawk_bird_by_katelyn.jpg" in
    let robur_img = read_image assets "robur.png" in
    let albatross_img =
      read_image assets "interconnections_by_guerrillabuzz.jpg"
    in
    let mirage_img = read_image assets "server_room_by_yuriy-vertikov.jpg" in
    let dashboard_img = read_image assets "dashboard_1.png" in
    Lwt.all [ molly_img; robur_img; albatross_img; mirage_img; dashboard_img ]
    >|= function
    | [ molly_img; robur_img; albatross_img; mirage_img; dashboard_img ] ->
        { molly_img; robur_img; albatross_img; mirage_img; dashboard_img }
    | _ -> failwith "Unexpected number of images"

  module Store = Storage.Make (BLOCK)
  module Map = Map.Make (String)

  let csrf_verification f user csrf reqd =
    let now = Mirage_ptime.now () in
    Middleware.csrf_verification user now csrf f reqd

  let read_multipart_data reqd =
    let response_body = H1.Reqd.request_body reqd in
    let finished, notify_finished = Lwt.wait () in
    let wakeup v = Lwt.wakeup_later notify_finished v in
    let on_eof data () = wakeup (Buffer.contents data) in
    let request = H1.Reqd.request reqd in
    let initial_size =
      Option.bind
        (H1.Headers.get request.headers "content-length")
        int_of_string_opt
      |> Option.value ~default:65536
    in
    let rec on_read on_eof acc bs ~off ~len =
      let str = Bigstringaf.substring ~off ~len bs in
      Buffer.add_string acc str;
      H1.Body.Reader.schedule_read response_body ~on_read:(on_read on_eof acc)
        ~on_eof:(on_eof acc)
    in
    let f_init = Buffer.create initial_size in
    H1.Body.Reader.schedule_read response_body ~on_read:(on_read on_eof f_init)
      ~on_eof:(on_eof f_init);
    finished >>= fun data ->
    let content_type =
      H1.(Headers.get_exn request.Request.headers "content-type")
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

  let get_multipart_request_as_stream reqd =
    match Middleware.header "Content-Type" reqd with
    | None -> Lwt.return_error "Missing Content-Type header"
    | Some content_type_str
      when not
             (String.starts_with ~prefix:"multipart/form-data" content_type_str)
      ->
        Lwt.return_error "Expected multipart/form-data"
    | Some content_type_str -> (
        match
          Multipart_form.Content_type.of_string (content_type_str ^ "\r\n")
        with
        | Error (`Msg msg) -> Lwt.return_error ("Invalid Content-Type: " ^ msg)
        | Ok ct ->
            let request_body_stream, push_to_parser_input =
              Lwt_stream.create ()
            in
            let body_reader = H1.Reqd.request_body reqd in
            let rec feed_body () =
              H1.Body.Reader.schedule_read body_reader
                ~on_read:(fun bs ~off ~len ->
                  push_to_parser_input
                    (Some (Bigstringaf.substring ~off ~len bs));
                  feed_body ())
                ~on_eof:(fun () ->
                  Logs.debug (fun m -> m "feed_body: EOF");
                  push_to_parser_input None)
            in
            feed_body ();
            let identify hdrs =
              let open Multipart_form in
              let filename =
                Option.bind
                  (Header.content_disposition hdrs)
                  Content_disposition.filename
              in
              let name =
                Option.bind
                  (Header.content_disposition hdrs)
                  Content_disposition.name
              in
              (name, filename)
            in
            Lwt.return_ok
              (Multipart_form_lwt.stream ~identify request_body_stream ct))

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
    let request_body = H1.Reqd.request_body reqd in
    let finished, notify_finished = Lwt.wait () in
    let wakeup v = Lwt.wakeup_later notify_finished v in
    let on_eof data () = wakeup (Buffer.contents data) in
    let request = H1.Reqd.request reqd in
    let initial_size =
      Option.bind
        (H1.Headers.get request.headers "content-length")
        int_of_string_opt
      |> Option.value ~default:65536
    in
    let rec on_read on_eof acc bs ~off ~len =
      let str = Bigstringaf.substring ~off ~len bs in
      Buffer.add_string acc str;
      H1.Body.Reader.schedule_read request_body ~on_read:(on_read on_eof acc)
        ~on_eof:(on_eof acc)
    in
    let f_init = Buffer.create initial_size in
    H1.Body.Reader.schedule_read request_body ~on_read:(on_read on_eof f_init)
      ~on_eof:(on_eof f_init);
    finished

  let extract_json_body reqd =
    decode_request_body reqd >>= fun data ->
    match Utils.Json.from_string data with
    | Error (`Msg err) ->
        Logs.warn (fun m -> m "Failed to parse JSON: %s" err);
        Lwt.return (Error (`Msg err))
    | Ok (`Assoc json_dict) -> Lwt.return (Ok json_dict)
    | Ok _ ->
        Logs.warn (fun m -> m "JSON is not a dictionary: %s" data);
        Lwt.return (Error (`Msg "not a dictionary"))

  let extract_json_csrf_token f token_or_cookie user reqd =
    extract_json_body reqd >>= function
    | Error (`Msg err) ->
        Middleware.http_response reqd ~data:(`String err) `Bad_request
    | Ok json_dict -> (
        match token_or_cookie with
        | `Token -> f user json_dict reqd
        | `Cookie -> (
            match Utils.Json.get User_model.csrf_cookie json_dict with
            | Some (`String token) ->
                csrf_verification (f user json_dict) user token reqd
            | _ ->
                Logs.warn (fun m ->
                    m "No csrf token in session request with Json body");
                Middleware.http_response reqd
                  ~data:(`String "Couldn't find CSRF token") `Bad_request))

  let extract_multipart_csrf_token f token_or_cookie user reqd =
    match Middleware.header "Content-Type" reqd with
    | Some header when String.starts_with ~prefix:"multipart/form-data" header
      -> (
        read_multipart_data reqd >>= function
        | Error (`Msg err) ->
            Logs.warn (fun m -> m "Failed to read multipart data: %s" err);
            Middleware.http_response reqd
              ~data:(`String ("Couldn't process multipart request: " ^ err))
              `Bad_request
        | Ok (m, assoc) -> (
            let multipart_body, _r = to_map ~assoc m in
            match token_or_cookie with
            | `Token -> f user multipart_body reqd
            | `Cookie -> (
                match Map.find_opt "molly_csrf" multipart_body with
                | None ->
                    Logs.warn (fun m -> m "No csrf token in multipart request");
                    Middleware.http_response reqd
                      ~data:(`String "Couldn't find CSRF token") `Bad_request
                | Some (_, token) ->
                    csrf_verification (f user multipart_body) user token reqd)))
    | None | _ ->
        Logs.warn (fun m -> m "Not a multipart request");
        Middleware.http_response reqd
          ~data:(`String "Expected multipart form data") `Bad_request

  let email_verification f _ user reqd =
    if false (* TODO *) then
      Middleware.email_verified_middleware user (f user) reqd
    else f user reqd

  let authenticate_user ~check_admin ~check_token store reqd =
    let ( let* ) = Result.bind in
    let current_time = Mirage_ptime.now () in
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
          Error (`Cookie, "No molly-session in cookie header. " ^ err)
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

  let authenticate ?(check_admin = false) ?(api_meth = false)
      ?(check_token = false) store reqd f =
    match authenticate_user ~check_admin ~check_token store reqd with
    | Error (v, msg) ->
        Logs.err (fun m -> m "authenticate: %s" msg);
        if api_meth || v = `Token then
          Middleware.http_response reqd ~data:(`String msg) `Bad_request
        else
          Middleware.redirect_to_page ~path:"/sign-in" ~clear_session:true
            ~with_error:true ~msg reqd ()
    | Ok (`Token (user, token)) -> (
        Store.increment_token_usage store token user >>= function
        | Error (`Msg err) ->
            Middleware.http_response reqd ~data:(`String err)
              `Internal_server_error
        | Ok () -> f `Token user reqd)
    | Ok (`Cookie (user, cookie)) -> (
        Store.update_cookie_usage store cookie user reqd >>= function
        | Error (`Msg err) ->
            Logs.err (fun m -> m "Error with storage: %s" err);
            Middleware.http_response reqd ~data:(`String err)
              `Internal_server_error
        | Ok () -> f `Cookie user reqd)

  let reply reqd ?(content_type = "text/plain") ?(header_list = []) data status
      =
    let h =
      H1.Headers.of_list
        [
          ("content-length", string_of_int (String.length data));
          ("content-type", content_type);
          ("x-frame-options", "DENY");
          ("content-security-policy", "frame-ancestors 'none'");
        ]
    in
    let headers = H1.Headers.add_list h header_list in
    let resp = H1.Response.create ~headers status in
    H1.Reqd.respond_with_string reqd resp data;
    Lwt.return_unit

  let user_volumes_by_instance stack (state : Albatross.t) user_name =
    Albatross_state.query stack state ~domain:user_name (`Block_cmd `Block_info)
    >|= function
    | Error _msg -> []
    | Ok (_hdr, `Success (`Block_devices blocks)) ->
        Albatross.set_online state;
        blocks
    | Ok reply ->
        update_albatross_status state (`Incompatible, reply, "block info");
        []

  let user_unikernels_by_instance stack (state : Albatross.t) user_name =
    Albatross_state.query stack state ~domain:user_name
      (`Unikernel_cmd `Unikernel_info)
    >|= function
    | Error _msg -> []
    | Ok (_hdr, `Success (`Old_unikernel_info3 unikernels))
    | Ok (_hdr, `Success (`Old_unikernel_info4 unikernels))
    | Ok (_hdr, `Success (`Unikernel_info unikernels)) ->
        Albatross.set_online state;
        unikernels
    | Ok reply ->
        update_albatross_status state (`Incompatible, reply, "unikernel info");
        []

  let user_unikernels stack (albatross_instances : Albatross_state.a_map)
      user_name =
    Lwt_list.map_p
      (fun (_name, (instance : Albatross.t)) ->
        Albatross_state.query stack instance ~domain:user_name
          (`Unikernel_cmd `Unikernel_info)
        >|= function
        | Error _msg -> (instance.configuration.name, [])
        | Ok (_hdr, `Success (`Old_unikernel_info3 unikernels))
        | Ok (_hdr, `Success (`Old_unikernel_info4 unikernels))
        | Ok (_hdr, `Success (`Unikernel_info unikernels)) ->
            Albatross.set_online instance;
            (instance.configuration.name, unikernels)
        | Ok reply ->
            update_albatross_status instance
              (`Incompatible, reply, "unikernel info");
            (instance.configuration.name, []))
      (Albatross.Albatross_map.bindings albatross_instances)

  let user_unikernel stack state ~user_name ~unikernel_name =
    Albatross_state.query stack state ~domain:user_name ~name:unikernel_name
      (`Unikernel_cmd `Unikernel_info)
    >|= function
    | Error err ->
        Error
          (Fmt.str "Error fetching '%s' from '%s': %s"
             (Configuration.name_to_str unikernel_name)
             (Configuration.name_to_str state.configuration.name)
             err)
    | Ok (_hdr, `Success (`Unikernel_info [ unikernel ]))
    | Ok (_hdr, `Success (`Old_unikernel_info3 [ unikernel ]))
    | Ok (_hdr, `Success (`Old_unikernel_info4 [ unikernel ])) ->
        Albatross.set_online state;
        Ok unikernel
    | Ok ((_hdr, `Success (`Unikernel_info unikernels)) as e)
    | Ok ((_hdr, `Success (`Old_unikernel_info3 unikernels)) as e)
    | Ok ((_hdr, `Success (`Old_unikernel_info4 unikernels)) as e) ->
        let message =
          Fmt.str "Expected one unikernel, but got %u" (List.length unikernels)
        in
        update_albatross_status state (`Incompatible, e, "unikernel info");
        Error message
    | Ok reply ->
        let message =
          Fmt.str "Expected unikernel info for '%s', got %a"
            (Configuration.name_to_str unikernel_name)
            (Vmm_commands.pp_wire ~verbose:false)
            reply
        in
        update_albatross_status state (`Incompatible, reply, "unikernel info");
        Error message

  let sign_up reqd =
    let now = Mirage_ptime.now () in
    let csrf = Middleware.generate_csrf_cookie now reqd in
    let csrf_cookie = csrf.name ^ "=" ^ csrf.value ^ ";Path=/;HttpOnly=true" in
    match Middleware.session_cookie_value reqd with
    | Ok x when x <> "" ->
        Middleware.redirect_to_page ~path:"/dashboard" reqd ()
    | Ok _ | Error (`Msg _) ->
        reply reqd ~content_type:"text/html"
          (Sign_up.register_page ~csrf:csrf.value ~icon:"/images/robur.png")
          ~header_list:
            [ ("Set-Cookie", csrf_cookie); ("X-MOLLY-CSRF", csrf.value) ]
          `OK

  let sign_in reqd =
    match Middleware.session_cookie_value reqd with
    | Ok x when x <> "" ->
        Middleware.redirect_to_page ~path:"/dashboard" reqd ()
    | Ok _ | Error (`Msg _) ->
        reply reqd ~content_type:"text/html"
          (Sign_in.login_page ~icon:"/images/robur.png" ())
          `OK

  let register store reqd =
    decode_request_body reqd >>= fun data ->
    match Utils.Json.from_string data with
    | Error (`Msg err) ->
        Logs.warn (fun m -> m "Failed to parse JSON: %s" err);
        Middleware.http_response reqd ~data:(`String err) `Bad_request
    | Ok (`Assoc json_dict) -> (
        let validate_user_input ~name ~email ~password ~form_csrf =
          if name = "" || email = "" || password = "" then
            Error (`Msg "All fields must be filled.")
          else if String.length name < 4 then
            Error (`Msg "Name must be at least 3 characters long.")
          else if not (Utils.Email.validate_email email) then
            Error (`Msg "Invalid email address.")
          else if not (User_model.password_validation password) then
            Error (`Msg "Password must be at least 8 characters long.")
          else if form_csrf = "" then
            Error
              (`Msg "CSRF token mismatch error. Please referesh and try again.")
          else Configuration.name_of_str name
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
            | Error (`Msg err) ->
                Middleware.http_response reqd
                  ~data:(`String (String.escaped err))
                  `Bad_request
            | Ok name ->
                if Middleware.csrf_cookie_verification form_csrf reqd then
                  let existing_email = Store.find_by_email store email in
                  let existing_name = Store.find_by_name store name in
                  match (existing_name, existing_email) with
                  | Some _, None ->
                      Middleware.http_response reqd
                        ~data:(`String "A user with this name already exist.")
                        `Bad_request
                  | None, Some _ ->
                      Middleware.http_response reqd
                        ~data:(`String "A user with this email already exist.")
                        `Bad_request
                  | None, None -> (
                      let created_at = Mirage_ptime.now () in
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
                            ~data:(User_model.user_to_json user)
                            `OK
                      | Error (`Msg err) ->
                          Middleware.http_response reqd ~data:(`String err)
                            `Internal_server_error)
                  | _ ->
                      Middleware.http_response reqd
                        ~data:
                          (`String
                             "A user with this name or email already exist.")
                        `Bad_request
                else
                  Middleware.http_response reqd
                    ~data:
                      (`String
                         "CSRF token mismatch error. Please referesh and try \
                          again.") `Bad_request)
        | _ ->
            Middleware.http_response reqd
              ~data:
                (`String
                   (Fmt.str "Register: Unexpected fields. Got %s"
                      (Utils.Json.to_string (`Assoc json_dict))))
              `Bad_request)
    | _ ->
        Middleware.http_response reqd
          ~data:(`String "Register account: expected a dictionary") `Bad_request

  let login store reqd =
    decode_request_body reqd >>= fun data ->
    match Utils.Json.from_string data with
    | Error (`Msg err) ->
        Logs.warn (fun m -> m "Failed to parse JSON: %s" err);
        Middleware.http_response reqd ~data:(`String err) `Bad_request
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
                Middleware.http_response reqd ~data:(`String err) `Bad_request
            | Ok _ -> (
                let now = Mirage_ptime.now () in
                let user = Store.find_by_email store email in
                match
                  User_model.login_user ~email ~password
                    ~user_agent:(Middleware.user_agent reqd)
                    user now
                with
                | Error (`Msg err) ->
                    Middleware.http_response reqd ~data:(`String err)
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
                          ~data:(User_model.user_to_json user)
                          `OK
                    | Error (`Msg err) ->
                        Middleware.http_response reqd ~data:(`String err)
                          `Internal_server_error)))
        | _ ->
            Middleware.http_response reqd
              ~data:
                (`String
                   (Fmt.str "Update password: Unexpected fields. Got %s"
                      (Utils.Json.to_string (`Assoc json_dict))))
              `Bad_request)
    | _ ->
        Middleware.http_response reqd
          ~data:(`String "Update password: expected a dictionary") `Bad_request

  let verify_email store (user : User_model.user) reqd =
    let now = Mirage_ptime.now () in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf -> (
        let email_verification_uuid = User_model.generate_uuid () in
        let updated_user =
          User_model.update_user user ~updated_at:now
            ~email_verification_uuid:(Some email_verification_uuid) ()
        in
        Store.update_user store updated_user >>= function
        | Ok () ->
            let verification_link =
              Utils.Email.generate_verification_link email_verification_uuid
            in
            Logs.info (fun m -> m "Verification link is: %s" verification_link);
            reply reqd ~content_type:"text/html"
              (Verify_email.verify_page user ~csrf ~icon:"/images/robur.png")
              ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
              `OK
        | Error (`Msg err) ->
            Middleware.http_response ~api_meth:false reqd ~data:(`String err)
              `Internal_server_error)
    | Error err ->
        Middleware.http_response ~api_meth:false reqd ~title:err.title
          ~data:err.data `Internal_server_error

  let verify_email_token store verification_token (user : User_model.user) reqd
      =
    match
      let ( let* ) = Result.bind in
      let* uuid =
        Option.to_result ~none:(`Msg "invalid UUID")
          (Uuidm.of_string verification_token)
      in
      let u = Store.find_email_verification_token store uuid in
      User_model.verify_email_token u verification_token (Mirage_ptime.now ())
    with
    | Ok user' ->
        if String.equal user.uuid user'.uuid then
          Store.update_user store user >>= function
          | Ok () -> Middleware.redirect_to_page ~path:"/dashboard" reqd ()
          | Error (`Msg msg) ->
              Middleware.http_response reqd ~data:(`String msg)
                `Internal_server_error
        else
          Middleware.http_response reqd
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
            Middleware.http_response reqd ~data:(`String "Account not found")
              `Not_found
        | Some user -> (
            if error_on_last user then (
              Logs.warn (fun m ->
                  m "%s : Can't perform action on last user" key);
              Middleware.http_response reqd ~data:error_message `Forbidden)
            else
              let updated_user = update_fn user in
              Store.update_user store updated_user >>= function
              | Ok () ->
                  Middleware.http_response reqd
                    ~data:(`String "Updated user successfully") `OK
              | Error (`Msg msg) ->
                  Logs.warn (fun m -> m "%s : Storage error with %s" key msg);
                  Middleware.http_response reqd ~data:(`String msg)
                    `Internal_server_error))
    | _ ->
        Logs.warn (fun m -> m "%s: Failed to parse JSON - no UUID found" key);
        Middleware.http_response reqd
          ~data:(`String "Couldn't find a UUID in the JSON.") `Bad_request

  let toggle_account_activation store _user json_dict reqd =
    toggle_account_attribute json_dict store reqd ~key:"toggle-active-account"
      (fun user ->
        User_model.update_user user ~active:(not user.active)
          ~updated_at:(Mirage_ptime.now ()) ())
      (fun user -> user.active && Store.count_active store <= 1)
      ~error_message:(`String "Cannot deactivate last active user")

  let toggle_admin_activation store _user json_dict reqd =
    toggle_account_attribute json_dict store reqd ~key:"toggle-admin-account"
      (fun user ->
        User_model.update_user user ~super_user:(not user.super_user)
          ~updated_at:(Mirage_ptime.now ()) ())
      (fun user -> user.super_user && Store.count_superusers store <= 1)
      ~error_message:(`String "Cannot remove last administrator")

  let delete_account store _user json_dict reqd =
    match Utils.Json.get "uuid" json_dict with
    | Some (`String uuid) -> (
        match Store.find_by_uuid store uuid with
        | None ->
            Logs.warn (fun m -> m "delete-account : Account not found");
            Middleware.http_response reqd ~data:(`String "Account not found")
              `Not_found
        | Some user -> (
            Store.delete_user store user >>= function
            | Ok () ->
                Middleware.http_response reqd
                  ~data:(`String "Deleted user successfully") `OK
            | Error (`Msg msg) ->
                Logs.warn (fun m -> m "delete-user : Storage error with %s" msg);
                Middleware.http_response reqd
                  ~data:(`String (String.escaped msg))
                  `Internal_server_error))
    | _ ->
        Logs.warn (fun m ->
            m "delete-user: Failed to parse JSON - no UUID found");
        Middleware.http_response reqd
          ~data:(`String "delete-user: Couldn't find a UUID in the JSON.")
          `Bad_request

  let dashboard stack albatross_instances store _ (user : User_model.user) reqd
      =
    let now = Mirage_ptime.now () in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        (* TODO use uuid in the future *)
        user_unikernels stack albatross_instances user.name
        >>= fun unikernels_by_albatross_instance ->
        reply reqd ~content_type:"text/html"
          (Dashboard.dashboard_layout ~csrf user
             ~content:
               (Unikernel_index.unikernel_index_layout
                  unikernels_by_albatross_instance now)
             ~icon:"/images/robur.png" ())
          `OK
    | Error err ->
        Middleware.http_response ~api_meth:false reqd ~title:err.title
          ~data:err.data `Internal_server_error

  let account_page store _ (user : User_model.user) reqd =
    match Middleware.session_cookie_value reqd with
    | Ok active_cookie_value -> (
        let now = Mirage_ptime.now () in
        generate_csrf_token store user now reqd >>= function
        | Ok csrf ->
            reply reqd ~content_type:"text/html"
              (Dashboard.dashboard_layout ~csrf user ~page_title:"Account"
                 ~content:
                   (User_account.user_account_layout user ~active_cookie_value
                      now)
                 ~icon:"/images/robur.png" ())
              ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
              `OK
        | Error err ->
            Middleware.http_response ~api_meth:false reqd ~title:err.title
              ~data:err.data `Internal_server_error)
    | Error (`Msg err) ->
        Middleware.http_response ~api_meth:false reqd ~data:(`String err)
          `Bad_request

  let update_password store (user : User_model.user) json_dict reqd =
    match
      Utils.Json.
        ( get "current_password" json_dict,
          get "new_password" json_dict,
          get "confirm_password" json_dict )
    with
    | ( Some (`String current_password),
        Some (`String new_password),
        Some (`String confirm_password) ) -> (
        let now = Mirage_ptime.now () in
        let new_password_hash =
          User_model.hash_password ~password:new_password ~uuid:user.uuid
        in
        if
          not
            (String.equal user.password
               (User_model.hash_password ~password:current_password
                  ~uuid:user.uuid))
        then
          Middleware.http_response reqd
            ~data:(`String "The current password entered is wrong.")
            `Bad_request
        else if not (String.equal new_password confirm_password) then
          Middleware.http_response reqd
            ~data:(`String "New password and confirm password do not match")
            `Bad_request
        else if not (User_model.password_validation new_password) then
          Middleware.http_response reqd
            ~data:(`String "New password must be atleast 8 characters.")
            `Bad_request
        else
          let updated_user =
            User_model.update_user user ~password:new_password_hash
              ~updated_at:now ()
          in
          Store.update_user store updated_user >>= function
          | Ok () ->
              Middleware.http_response reqd
                ~data:(`String "Updated password successfully") `OK
          | Error (`Msg err) ->
              Logs.warn (fun m -> m "Storage error with %s" err);
              Middleware.http_response reqd ~data:(`String err)
                `Internal_server_error)
    | _ ->
        Middleware.http_response reqd
          ~data:
            (`String
               (Fmt.str "Update password: Unexpected fields. Got %s"
                  (Utils.Json.to_string (`Assoc json_dict))))
          `Bad_request

  let new_user_cookies ~user ~filter ~redirect store reqd =
    let now = Mirage_ptime.now () in
    let cookies = List.filter filter user.User_model.cookies in
    let updated_user =
      User_model.update_user user ~cookies ~updated_at:now ()
    in
    Store.update_user store updated_user >>= function
    | Ok () -> redirect
    | Error (`Msg err) ->
        Logs.warn (fun m -> m "Storage error with %s" err);
        Middleware.http_response reqd ~data:(`String err) `Internal_server_error

  let close_sessions ?to_logout_cookie ?(logout = false) store
      (user : User_model.user) _json_dict reqd =
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
                    Middleware.http_response reqd
                      ~data:(`String "Closed all sessions succesfully") `OK )
              | _, true ->
                  ( (fun (c : User_model.cookie) ->
                      not (String.equal c.value cookie.value)),
                    Middleware.http_response reqd
                      ~data:(`String "Logout succesful") `OK )
              | Some to_logout_cookie_value, false ->
                  ( (fun (c : User_model.cookie) ->
                      not (String.equal to_logout_cookie_value c.value)),
                    Middleware.redirect_to_page ~path:"/account"
                      ~msg:"Closed session succesfully" reqd () )
            in
            new_user_cookies ~user ~filter ~redirect store reqd
        | None ->
            Middleware.http_response ~api_meth:false reqd
              ~data:(`String "Authentication cookie not found.") `Not_found)
    | Error (`Msg err) ->
        Middleware.http_response ~api_meth:false reqd
          ~data:
            (`String
               ("Session cookie error: Couldn't find a session cookie. " ^ err))
          `Bad_request

  let close_session store (user : User_model.user) json_dict reqd =
    match Utils.Json.(get "session_value" json_dict) with
    | Some (`String session_value) -> (
        let now = Mirage_ptime.now () in
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
            Middleware.http_response reqd
              ~data:(`String "Session closed succesfully") `OK
        | Error (`Msg err) ->
            Logs.warn (fun m -> m "Storage error with %s" err);
            Middleware.http_response reqd ~data:(`String err)
              `Internal_server_error)
    | _ ->
        Middleware.http_response reqd
          ~data:
            (`String
               (Fmt.str "Close session: Unexpected fields. Got %s"
                  (Utils.Json.to_string (`Assoc json_dict))))
          `Bad_request

  let users store _ (user : User_model.user) reqd =
    let now = Mirage_ptime.now () in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        reply reqd ~content_type:"text/html"
          (Dashboard.dashboard_layout ~csrf user ~page_title:"Users"
             ~content:(Users_index.users_index_layout (Store.users store) now)
             ~icon:"/images/robur.png" ())
          `OK
    | Error err ->
        Middleware.http_response ~api_meth:false reqd ~title:err.title
          ~data:err.data `Internal_server_error

  let settings store albatross_instances _ (user : User_model.user) reqd =
    let now = Mirage_ptime.now () in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        reply reqd ~content_type:"text/html"
          (Dashboard.dashboard_layout ~csrf user ~page_title:"Settings"
             ~content:(Settings_page.settings_layout albatross_instances)
             ~icon:"/images/robur.png" ())
          ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
          `OK
    | Error err ->
        Middleware.http_response ~api_meth:false reqd ~title:err.title
          ~data:err.data `Internal_server_error

  let retry_initializing_instance stack albatross_instances
      (albatross : Albatross.t) _ _user reqd =
    Albatross_state.init stack albatross.configuration >>= function
    | Ok new_albatross_instance ->
        albatross_instances :=
          Albatross.Albatross_map.update albatross.configuration.name
            (fun _prev -> Some new_albatross_instance)
            !albatross_instances;
        Middleware.http_response reqd
          ~data:
            (`String "Re-initialization successful, instance is back online")
          `OK
    | Error (_, err) ->
        Middleware.http_response reqd
          ~data:(`String ("Re-initialization failed. See error logs: " ^ err))
          `Internal_server_error

  let update_settings stack store albatross_instances
      (update_or_create : [ `Update | `Create ]) _user json_dict reqd =
    match Configuration.of_json_from_http json_dict (Mirage_ptime.now ()) with
    | Ok configuration_settings -> (
        Albatross_state.init stack configuration_settings >>= function
        | Ok new_albatross_instance -> (
            Store.upsert_configuration store configuration_settings
              update_or_create
            >>= function
            | Ok _new_configurations ->
                albatross_instances :=
                  Albatross.Albatross_map.update configuration_settings.name
                    (fun _prev -> Some new_albatross_instance)
                    !albatross_instances;
                Middleware.http_response reqd
                  ~data:(`String "Configuration updated successfully") `OK
            | Error (`Msg err) ->
                Middleware.http_response reqd ~data:(`String err)
                  `Internal_server_error)
        | Error (_, err) ->
            Middleware.http_response reqd ~data:(`String err)
              `Internal_server_error)
    | Error (`Msg err) ->
        Middleware.http_response
          ~data:(`String (String.escaped err))
          reqd `Bad_request

  let delete_albatross_config store albatross_instances _user json_dict reqd =
    match Utils.Json.get "name" json_dict with
    | Some (`String name) -> (
        match Configuration.name_of_str name with
        | Ok name -> (
            Store.delete_configuration store name >>= function
            | Ok _new_configurations ->
                albatross_instances :=
                  Albatross.Albatross_map.remove name !albatross_instances;
                Middleware.http_response reqd
                  ~data:(`String "Configuration delete successfully") `OK
            | Error (`Msg err) ->
                Middleware.http_response reqd ~data:(`String err)
                  `Internal_server_error)
        | Error (`Msg err) ->
            Middleware.http_response reqd ~data:(`String err) `Bad_request)
    | _ ->
        Middleware.http_response reqd
          ~data:
            (`String
               (Fmt.str "Delete albatross config: Unexpected fields. Got %s"
                  (Utils.Json.to_string (`Assoc json_dict))))
          `Bad_request

  let deploy_form stack store albatross _ (user : User_model.user) reqd =
    let now = Mirage_ptime.now () in
    user_unikernels_by_instance stack albatross user.name
    >>= fun unikernels_by_albatross_instance ->
    user_volumes_by_instance stack albatross user.name
    >>= fun blocks_by_albatross_instance ->
    generate_csrf_token store user now reqd >>= function
    | Ok csrf -> (
        match Albatross_state.policy albatross ~domain:user.name with
        | Ok p -> (
            match p with
            | Some user_policy ->
                reply reqd ~content_type:"text/html"
                  (Dashboard.dashboard_layout ~csrf user
                     ~page_title:"Deploy a Unikernel"
                     ~content:
                       (Unikernel_create.unikernel_create_layout ~user_policy
                          unikernels_by_albatross_instance
                          blocks_by_albatross_instance
                          albatross.configuration.name)
                     ~icon:"/images/robur.png" ())
                  ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
                  `OK
            | None ->
                Middleware.http_response ~api_meth:false reqd
                  ~title:"Resource Policy error"
                  ~data:(`String "No user policy") `Bad_request)
        | Error err ->
            Middleware.http_response ~api_meth:false reqd
              ~title:"Resource Policy error" ~data:(`String err) `Bad_request)
    | Error err ->
        Middleware.http_response ~api_meth:false reqd ~title:err.title
          ~data:err.data `Internal_server_error

  let unikernel_info stack albatross_instances _ (user : User_model.user) reqd =
    (* TODO use uuid in the future *)
    Lwt_list.fold_left_s
      (fun (successes, failures) (name, (instance : Albatross.t)) ->
        Albatross_state.query stack instance ~domain:user.name
          (`Unikernel_cmd `Unikernel_info)
        >>= function
        | Error msg ->
            let failure =
              `Assoc
                [
                  ("instance", `String (Configuration.name_to_str name));
                  ("error", `String msg);
                ]
            in
            Lwt.return (successes, failure :: failures)
        | Ok (_hdr, res) -> (
            Albatross.set_online instance;
            match Albatross_json.res res with
            | Error (`String err) ->
                Logs.err (fun m ->
                    m "Error parsing response from albatross '%s': %s"
                      (Configuration.name_to_str name)
                      err);
                let failure =
                  `Assoc
                    [
                      ("instance", `String (Configuration.name_to_str name));
                      ("error", `String err);
                    ]
                in
                Lwt.return (successes, failure :: failures)
            | Ok parsed -> Lwt.return (parsed :: successes, failures)))
      ([], [])
      (Albatross.Albatross_map.bindings albatross_instances)
    >>= fun (successes, failures) ->
    let response_data =
      `Assoc [ ("data", `List successes); ("errors", `List failures) ]
    in
    Middleware.http_response reqd ~title:"Unikernel Information"
      ~data:response_data `OK

  let unikernel_info_one stack store albatross unikernel_name _
      (user : User_model.user) reqd =
    (* TODO use uuid in the future *)
    user_unikernel stack albatross ~user_name:user.name ~unikernel_name
    >>= function
    | Error err ->
        let data =
          "An error occured trying to fetch "
          ^ Configuration.name_to_str unikernel_name
          ^ "from albatross: " ^ err
        in
        Middleware.http_response ~api_meth:false reqd ~data:(`String data)
          `Internal_server_error
    | Ok unikernel -> (
        let now = Mirage_ptime.now () in
        generate_csrf_token store user now reqd >>= function
        | Ok csrf ->
            let last_update_time =
              match
                List.find_opt
                  (fun (u : User_model.unikernel_update) ->
                    Vmm_core.Name.Label.equal u.name unikernel_name)
                  user.unikernel_updates
              with
              | Some unikernel_update -> Some unikernel_update.timestamp
              | None -> None
            in
            reply reqd ~content_type:"text/html"
              (Dashboard.dashboard_layout ~csrf user
                 ~content:
                   (Unikernel_single.unikernel_single_layout ~unikernel_name
                      ~instance_name:albatross.configuration.name unikernel
                      ~last_update_time ~current_time:now)
                 ~icon:"/images/robur.png" ())
              ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
              `OK
        | Error err ->
            Middleware.http_response ~api_meth:false reqd ~title:err.title
              ~data:err.data `Internal_server_error)

  let unikernel_prepare_update stack store http_client albatross unikernel_name
      _ (user : User_model.user) reqd =
    (* TODO use uuid in the future *)
    user_unikernel stack albatross ~user_name:user.name ~unikernel_name
    >>= function
    | Error err ->
        Middleware.http_response ~api_meth:false
          ~data:
            (`String
               ("An error occured while fetching "
               ^ Configuration.name_to_str unikernel_name
               ^ " from albatross with error " ^ err))
          reqd `Internal_server_error
    | Ok (name, unikernel) -> (
        let unikernel_name = Configuration.name_to_str unikernel_name in
        Utils.send_http_request http_client ~base_url:Builder_web.base_url
          ~path:("/hash?sha256=" ^ Ohex.encode unikernel.digest)
        >>= function
        | Error (`Msg err) ->
            Logs.err (fun m ->
                m
                  "builds.robur.coop: Error while fetching the current build \
                   info of %s with error: %s"
                  unikernel_name err);
            Middleware.http_response ~api_meth:false
              ~data:
                (`String
                   ("An error occured while fetching the current build \
                     information from builds.robur.coop. The error is: " ^ err))
              ~title:(unikernel_name ^ " update Error")
              reqd `Internal_server_error
        | Ok response_body -> (
            match Utils.Json.from_string response_body with
            | Error (`Msg err) ->
                Logs.err (fun m ->
                    m
                      "Failed to parse JSON response from builds.robur.coop \
                       for unikernel %s: %s"
                      unikernel_name err);
                Middleware.http_response ~api_meth:false
                  ~data:
                    (`String
                       ("An error occured while parsing the json response from \
                         builds.robur.coop. The error is: " ^ err))
                  ~title:(unikernel_name ^ " update Error")
                  reqd `Internal_server_error
            | Ok json -> (
                match Builder_web.build_of_json json with
                | Error (`Msg err) ->
                    Logs.err (fun m ->
                        m
                          "JSON parsing of the current build of %s from \
                           builds.robur.coop failed with error: %s"
                          unikernel_name err);
                    Middleware.http_response ~api_meth:false
                      ~data:
                        (`String
                           ("An error occured while parsing the json of the \
                             current build from builds.robur.coop. The error \
                             is: " ^ err))
                      ~title:(unikernel_name ^ " update Error")
                      reqd `Internal_server_error
                | Ok current_job_data -> (
                    Utils.send_http_request http_client
                      ~base_url:Builder_web.base_url
                      ~path:("/job/" ^ current_job_data.job ^ "/build/latest")
                    >>= function
                    | Error (`Msg err) ->
                        Logs.err (fun m ->
                            m
                              "builds.robur.coop: Error while fetching the \
                               latest build info of %s with error: %s"
                              unikernel_name err);
                        Middleware.http_response
                          ~data:
                            (`String
                               ("An error occured while fetching the latest \
                                 build information from builds.robur.coop. The \
                                 error is: " ^ err))
                          ~title:(unikernel_name ^ " update Error")
                          ~api_meth:false reqd `Internal_server_error
                    | Ok response_body -> (
                        match Utils.Json.from_string response_body with
                        | Error (`Msg err) ->
                            Logs.err (fun m ->
                                m
                                  "Failed to parse JSON response from \
                                   builds.robur.coop for latest unikernel %s: \
                                   %s"
                                  unikernel_name err);
                            Middleware.http_response ~api_meth:false
                              ~data:
                                (`String
                                   ("An error occured while parsing the json \
                                     response of the latest unikernel from \
                                     builds.robur.coop. The error is: " ^ err))
                              ~title:(unikernel_name ^ " update Error")
                              reqd `Internal_server_error
                        | Ok json -> (
                            match Builder_web.build_of_json json with
                            | Error (`Msg err) ->
                                Logs.err (fun m ->
                                    m
                                      "JSON parsing of the latest build of %s \
                                       from builds.robur.coop failed with \
                                       error: %s"
                                      unikernel_name err);
                                Middleware.http_response
                                  ~data:
                                    (`String
                                       ("An error occured while parsing the \
                                         json of the latest build from \
                                         builds.robur.coop. The error is: "
                                      ^ err))
                                  ~title:(unikernel_name ^ "update Error")
                                  ~api_meth:false reqd `Internal_server_error
                            | Ok latest_job_data -> (
                                if
                                  String.equal latest_job_data.uuid
                                    current_job_data.uuid
                                then (
                                  Logs.info (fun m ->
                                      m
                                        "There is no new update of %s found \
                                         with uuid  %s"
                                        unikernel_name latest_job_data.uuid);
                                  Middleware.redirect_to_page
                                    ~path:
                                      ("/unikernel/info?unikernel="
                                      ^ Option.value ~default:""
                                          (Option.map
                                             Vmm_core.Name.Label.to_string
                                             (Vmm_core.Name.name name))
                                      ^ "&instance="
                                      ^ Configuration.name_to_str
                                          albatross.configuration.name)
                                    reqd
                                    ~msg:
                                      ("There is no update of " ^ unikernel_name
                                     ^ " found on builds.robur.coop")
                                    ())
                                else
                                  Utils.send_http_request http_client
                                    ~base_url:Builder_web.base_url
                                    ~path:
                                      ("/compare/" ^ current_job_data.uuid ^ "/"
                                     ^ latest_job_data.uuid ^ "")
                                  >>= function
                                  | Error (`Msg err) ->
                                      Logs.err (fun m ->
                                          m
                                            "builds.robur.coop: Error while \
                                             fetching the diff between the \
                                             current and latest build info of \
                                             %s with error: %s"
                                            unikernel_name err);
                                      Middleware.http_response
                                        ~data:
                                          (`String
                                             ("An error occured while fetching \
                                               the diff between the latest and \
                                               the current build information \
                                               from builds.robur.coop. The \
                                               error is: " ^ err))
                                        ~title:(unikernel_name ^ " update Error")
                                        ~api_meth:false reqd
                                        `Internal_server_error
                                  | Ok response_body -> (
                                      match
                                        Utils.Json.from_string response_body
                                      with
                                      | Error (`Msg err) ->
                                          Logs.err (fun m ->
                                              m
                                                "Failed to parse JSON response \
                                                 of the diff between the \
                                                 current and latest build from \
                                                 builds.robur.coop for \
                                                 unikernel %s: %s"
                                                unikernel_name err);
                                          Middleware.http_response
                                            ~api_meth:false
                                            ~data:
                                              (`String
                                                 ("An error occured while \
                                                   parsing the json response \
                                                   of the diff between the \
                                                   current and latest build \
                                                   from builds.robur.coop. The \
                                                   error is: " ^ err))
                                            ~title:
                                              (unikernel_name ^ " update Error")
                                            reqd `Internal_server_error
                                      | Ok json -> (
                                          match
                                            Builder_web.compare_of_json json
                                          with
                                          | Ok build_comparison -> (
                                              let now = Mirage_ptime.now () in
                                              generate_csrf_token store user now
                                                reqd
                                              >>= function
                                              | Ok csrf ->
                                                  reply reqd
                                                    ~content_type:"text/html"
                                                    (Dashboard.dashboard_layout
                                                       ~csrf user
                                                       ~page_title:
                                                         (Vmm_core.Name
                                                          .to_string name
                                                         ^ " Update")
                                                       ~content:
                                                         (Unikernel_update
                                                          .unikernel_update_layout
                                                            ~instance_name:
                                                              albatross
                                                                .configuration
                                                                .name
                                                            ~unikernel_name
                                                            (name, unikernel)
                                                            now build_comparison)
                                                       ~icon:"/images/robur.png"
                                                       ())
                                                    ~header_list:
                                                      [ ("X-MOLLY-CSRF", csrf) ]
                                                    `OK
                                              | Error err ->
                                                  Middleware.http_response
                                                    ~api_meth:false reqd
                                                    ~title:err.title
                                                    ~data:err.data
                                                    `Internal_server_error)
                                          | Error (`Msg err) ->
                                              Logs.err (fun m ->
                                                  m
                                                    "JSON parsing of the diff \
                                                     between the latest and \
                                                     current build of %s from \
                                                     builds.robur.coop failed \
                                                     with error: %s"
                                                    unikernel_name err);
                                              Middleware.http_response
                                                ~data:
                                                  (`String
                                                     ("An error occured while \
                                                       parsing the json of the \
                                                       diff between the latest \
                                                       and curent build from \
                                                       builds.robur.coop. The \
                                                       error is: " ^ err))
                                                ~title:
                                                  (unikernel_name
                                                 ^ " update Error")
                                                ~api_meth:false reqd
                                                `Internal_server_error)))))))))

  let force_create_unikernel stack albatross ~unikernel_name ~push
      (unikernel_cfg : Vmm_core.Unikernel.config) (user : User_model.user) =
    Albatross_state.query stack albatross ~domain:user.name ~name:unikernel_name
      ~push
      (`Unikernel_cmd (`Unikernel_force_create unikernel_cfg))
    >>= function
    | Error err ->
        Lwt.return
          (Error
             ( `Msg ("Force create: Error querying albatross: " ^ err),
               `Internal_server_error ))
    | Ok (_hdr, res) -> (
        let unikernel_name = Configuration.name_to_str unikernel_name in
        Albatross.set_online albatross;
        match Albatross_json.res res with
        | Error (`String err) ->
            Logs.warn (fun m ->
                m
                  "albatross-force-create: albatross_json.res error parsing \
                   albatross response for creating %s: %s"
                  unikernel_name err);
            Lwt.return (Error (`Msg err, `Internal_server_error))
        | Ok res ->
            Logs.info (fun m ->
                m
                  "albatross-force-create: succesfully created %s with result \
                   %s"
                  unikernel_name (Utils.Json.to_string res));
            Lwt.return (Ok ()))

  let process_change stack ~unikernel_name ~job ~to_be_updated_unikernel
      ~currently_running_unikernel (unikernel_cfg : Vmm_core.Unikernel.config)
      (user : User_model.user) store http_client
      (change_kind : [ `Rollback | `Update ]) albatross =
    let change_string = function
      | `Rollback -> "rollback"
      | `Update -> "update"
    in
    let unikernel_name_str = Configuration.name_to_str unikernel_name in
    let unikernel_update : User_model.unikernel_update =
      {
        name = unikernel_name;
        job;
        uuid = currently_running_unikernel;
        config = unikernel_cfg;
        timestamp = Mirage_ptime.now ();
      }
    in
    Store.update_user_unikernel_updates store unikernel_update user >>= function
    | Error (`Msg err) ->
        let data =
          Utils.Json.to_string
            (User_model.unikernel_update_to_json unikernel_update)
        in
        Logs.warn (fun m ->
            m
              "%s: storing update information for %s failed with: %s for data \
               %s"
              (change_string change_kind)
              unikernel_name_str err data);
        Lwt.return
          (Error
             ( `Msg
                 (change_string change_kind ^ " storing update information for "
                ^ unikernel_name_str ^ " failed with: " ^ err ^ " for data: "
                ^ data),
               (`Internal_server_error : H1.Status.t) ))
    | Ok () -> (
        let data_stream, push_chunks = Lwt_stream.create () in
        let push () = Lwt_stream.get data_stream in
        let url =
          Builder_web.base_url ^ "/job/"
          ^ Configuration.name_to_str job
          ^ "/build/" ^ to_be_updated_unikernel ^ "/main-binary"
        in
        let f resp _acc chunk =
          if
            Http_mirage_client.Status.is_successful
              resp.Http_mirage_client.status
          then Lwt.return (push_chunks (Some chunk))
          else Lwt.return_unit
        in

        Lwt.both
          ( Http_mirage_client.request http_client ~follow_redirect:true url f ()
          >>= fun e ->
            push_chunks None;
            match e with
            | Error (`Msg err) -> Lwt.return (Error (`Msg err))
            | Error `Cycle -> Lwt.return (Error (`Msg "returned cycle"))
            | Error `Not_found -> Lwt.return (Error (`Msg "returned not found"))
            | Ok (resp, ()) ->
                if
                  Http_mirage_client.Status.is_successful
                    resp.Http_mirage_client.status
                then Lwt.return (Ok ())
                else
                  Lwt.return
                    (Error
                       (`Msg
                          ("accessing " ^ url ^ " resulted in an error: "
                          ^ Http_mirage_client.Status.to_string resp.status
                          ^ " " ^ resp.reason))) )
          (force_create_unikernel stack albatross ~unikernel_name ~push
             unikernel_cfg user)
        >>= function
        | Error (`Msg err), _ ->
            Logs.err (fun m ->
                m
                  "%s: builds.robur.coop: Error while fetching the binary of \
                   %s with error: %s"
                  (change_string change_kind)
                  unikernel_name_str err);
            Lwt.return
              (Error
                 ( `Msg
                     (change_string change_kind
                    ^ " :an error occured while fetching the binary from \
                       builds.robur.coop with error: " ^ err),
                   `Internal_server_error ))
        | Ok (), Error (msg, status) -> Lwt.return (Error (msg, status))
        | Ok (), Ok () -> Lwt.return (Ok ()))
  (* TODO: check manifest in a streaming fashion
            match
              Albatross.manifest_devices_match ~bridges:unikernel_cfg.bridges
                ~block_devices:unikernel_cfg.block_devices unikernel_image
            with
            | Error (`Msg err) ->
                Lwt.return
                  (Error
                     ( `Msg
                         (change_string change_kind
                        ^ " :an error occured with the unikernel \
                           configuration: " ^ err),
                       `Bad_request ))
              | Ok () -> *)

  let process_rollback stack albatross ~unikernel_name current_time store
      http_client reqd (user : User_model.user) =
    match
      List.find_opt
        (fun (u : User_model.unikernel_update) ->
          Vmm_core.Name.Label.equal u.name unikernel_name)
        user.unikernel_updates
    with
    | Some old_unikernel ->
        if
          Utils.TimeHelper.diff_in_seconds ~current_time
            ~check_time:old_unikernel.timestamp
          < Utils.rollback_seconds_limit
        then
          process_change stack ~unikernel_name ~job:old_unikernel.name
            ~to_be_updated_unikernel:old_unikernel.uuid
            ~currently_running_unikernel:old_unikernel.uuid old_unikernel.config
            user store http_client `Rollback albatross
          >>= function
          | Ok _res ->
              Middleware.http_response reqd ~title:"Rollback Successful"
                ~data:
                  (`String
                     ("Rollback succesful. "
                     ^ Configuration.name_to_str unikernel_name
                     ^ " is now running on build " ^ old_unikernel.uuid))
                `OK
          | Error (`Msg err, http_status) ->
              Middleware.http_response reqd ~title:"Rollback Error"
                ~data:
                  (`String
                     ("Rollback failed. "
                     ^ Configuration.name_to_str unikernel_name
                     ^ " failed to revert to build " ^ old_unikernel.uuid
                     ^ " with error " ^ err))
                http_status
        else
          Middleware.http_response reqd ~title:"Rollback Failed"
            ~data:
              (`String
                 (Configuration.name_to_str unikernel_name
                 ^ " rollback failed. We can't do a rollback after 10 minutes \
                    of an update."))
            `Bad_request
    | None ->
        Middleware.http_response reqd ~title:"Rollback Error"
          ~data:
            (`String
               (Configuration.name_to_str unikernel_name
               ^ " rollback failed. Could not find the build information."))
          `Not_found

  let process_unikernel_update ~unikernel_name ~job ~to_be_updated_unikernel
      ~currently_running_unikernel ~http_liveliness_address ~dns_liveliness
      stack cfg user store http_client albatross reqd =
    process_change stack ~unikernel_name ~job ~to_be_updated_unikernel
      ~currently_running_unikernel cfg user store http_client `Update albatross
    >>= function
    | Ok _res -> (
        let unikernel_name_str = Configuration.name_to_str unikernel_name in
        Liveliness.interval_liveliness_checks ~unikernel_name
          ~http_liveliness_address ~dns_liveliness stack http_client
        >>= function
        | Error (`Msg err) ->
            Logs.err (fun m ->
                m
                  "liveliness-checks for %s and build %s failed with error(s) \
                   %s. now performing a rollback"
                  unikernel_name_str to_be_updated_unikernel err);
            process_rollback stack albatross ~unikernel_name
              (Mirage_ptime.now ()) store http_client reqd user
        | Ok () ->
            Middleware.http_response reqd ~title:"Update Successful"
              ~data:
                (`String
                   ("Update succesful. " ^ unikernel_name_str
                  ^ " is now running on build " ^ to_be_updated_unikernel))
              `OK)
    | Error (`Msg err, http_status) ->
        Middleware.http_response reqd ~title:"Update Error"
          ~data:
            (`String
               ("Update failed. "
               ^ Configuration.name_to_str unikernel_name
               ^ " failed to update to build " ^ to_be_updated_unikernel
               ^ " with error " ^ err))
          http_status

  let unikernel_update stack store albatross_instances http_client
      (user : User_model.user) json_dict reqd =
    let config_or_none field = function
      | None | Some `Null -> Ok None
      | Some json -> (
          match Albatross_json.config_of_json (Utils.Json.to_string json) with
          | Ok cfg -> Ok (Some cfg)
          | Error (`Msg err) ->
              Error
                (`Msg
                   ("invalid json for " ^ field ^ ": "
                  ^ Utils.Json.to_string json ^ "failed with: " ^ err)))
    in
    match
      Utils.Json.
        ( get "albatross_instance" json_dict,
          get "job" json_dict,
          get "to_be_updated_unikernel" json_dict,
          get "currently_running_unikernel" json_dict,
          get "unikernel_name" json_dict,
          get "http_liveliness_address" json_dict,
          get "dns_liveliness" json_dict,
          get "unikernel_arguments" json_dict )
    with
    | ( Some (`String instance_name),
        Some (`String job),
        Some (`String to_be_updated_unikernel),
        Some (`String currently_running_unikernel),
        Some (`String unikernel_name),
        http_liveliness_address,
        dns_liveliness,
        configuration ) -> (
        Liveliness.liveliness_checks ~http_liveliness_address ~dns_liveliness
          stack http_client
        >>= function
        | Ok () -> (
            match
              ( Configuration.name_of_str instance_name,
                Configuration.name_of_str unikernel_name,
                Configuration.name_of_str job )
            with
            | Ok instance_name, Ok unikernel_name, Ok job -> (
                match
                  Albatross_state.find_instance_by_name albatross_instances
                    instance_name
                with
                | Ok albatross -> (
                    match
                      config_or_none "unikernel_arguments" configuration
                    with
                    | Error (`Msg err) ->
                        Middleware.http_response reqd
                          ~title:"Error with Unikernel Arguments Json"
                          ~data:
                            (`String
                               ("Could not get the unikernel arguments json: "
                              ^ err))
                          `Bad_request
                    | Ok None -> (
                        user_unikernel stack albatross ~user_name:user.name
                          ~unikernel_name
                        >>= function
                        | Error err ->
                            Middleware.http_response reqd
                              ~data:
                                (`String
                                   ("Couldn't find albatross instance, "
                                   ^ Configuration.name_to_str instance_name
                                   ^ " with error: " ^ err))
                              `Bad_request
                        | Ok (_n, (info : Vmm_core.Unikernel.info)) ->
                            let (cfg : Vmm_core.Unikernel.config) =
                              {
                                Vmm_core.Unikernel.typ = info.typ;
                                compressed = false;
                                image = "";
                                add_name = true;
                                startup = info.startup;
                                fail_behaviour = info.fail_behaviour;
                                cpuid = info.cpuid;
                                memory = info.cpuid;
                                block_devices =
                                  List.map
                                    (fun {
                                           Vmm_core.Unikernel.unikernel_device;
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
                                           Vmm_core.Unikernel.unikernel_device;
                                           host_device;
                                           mac;
                                         } ->
                                      ( unikernel_device,
                                        Some host_device,
                                        Some mac ))
                                    info.bridges;
                                argv = info.argv;
                              }
                            in
                            process_unikernel_update ~unikernel_name ~job
                              ~to_be_updated_unikernel
                              ~currently_running_unikernel
                              ~http_liveliness_address ~dns_liveliness stack cfg
                              user store http_client albatross reqd)
                    | Ok (Some cfg) ->
                        process_unikernel_update ~unikernel_name ~job
                          ~to_be_updated_unikernel ~currently_running_unikernel
                          ~http_liveliness_address ~dns_liveliness stack cfg
                          user store http_client albatross reqd)
                | _ ->
                    Middleware.http_response
                      ~data:
                        (`String
                           ("An error occured while finding albatross instance "
                           ^ Configuration.name_to_str instance_name))
                      ~title:"Albatross Instance Error" reqd `Not_found)
            | Error (`Msg err), _, _ ->
                Middleware.http_response reqd ~title:"Error: Bad instance name"
                  ~data:
                    (`String
                       ("Couldn't convert the instance name, error: " ^ err))
                  `Bad_request
            | _, Error (`Msg err), _ ->
                Middleware.http_response reqd ~title:"Error: Bad unikernel name"
                  ~data:
                    (`String
                       ("Couldn't convert the unikernel name, error: " ^ err))
                  `Bad_request
            | _, _, Error (`Msg err) ->
                Middleware.http_response reqd ~title:"Error: Bad job name"
                  ~data:
                    (`String ("Couldn't convert the job name, error: " ^ err))
                  `Bad_request)
        | Error (`Msg err) ->
            Logs.info (fun m ->
                m
                  "Liveliness check of currentyly running unikernel failed \
                   with: %s"
                  err);
            Middleware.http_response reqd
              ~title:"Error: Liveliness check failed"
              ~data:
                (`String
                   ("Liveliness check of currentyly running unikernel failed \
                     with error: " ^ err))
              `Internal_server_error)
    | _ ->
        Middleware.http_response reqd
          ~data:(`String "Couldn't find job or build in json. Received ")
          `Bad_request

  let unikernel_rollback stack store albatross_instances http_client
      (user : User_model.user) json_dict reqd =
    match
      Utils.Json.
        (get "unikernel_name" json_dict, get "albatross_instance" json_dict)
    with
    | Some (`String unikernel_name), Some (`String instance_name) -> (
        match
          ( Configuration.name_of_str instance_name,
            Configuration.name_of_str unikernel_name )
        with
        | Ok instance_name, Ok unikernel_name -> (
            match
              Albatross_state.find_instance_by_name albatross_instances
                instance_name
            with
            | Ok albatross ->
                process_rollback stack albatross ~unikernel_name
                  (Mirage_ptime.now ()) store http_client reqd user
            | _ ->
                Middleware.http_response reqd
                  ~data:
                    (`String
                       ("Couldn't find albatross instance, "
                       ^ Configuration.name_to_str instance_name))
                  `Not_found)
        | Error (`Msg err), _ ->
            Middleware.http_response reqd
              ~data:(`String ("Couldn't convert instance name, " ^ err))
              `Bad_request
        | _, Error (`Msg err) ->
            Middleware.http_response reqd
              ~data:(`String ("Couldn't convert unikernel name, " ^ err))
              `Bad_request)
    | _ ->
        Middleware.http_response reqd
          ~data:(`String "Couldn't find unikernel name in json") `Bad_request

  let unikernel_destroy stack albatross_instances (user : User_model.user)
      json_dict reqd =
    (* TODO use uuid in the future *)
    match
      Utils.Json.(get "name" json_dict, get "albatross_instance" json_dict)
    with
    | Some (`String unikernel_name), Some (`String instance_name) -> (
        match
          ( Configuration.name_of_str instance_name,
            Configuration.name_of_str unikernel_name )
        with
        | Ok instance_name, Ok unikernel_name -> (
            match
              Albatross_state.find_instance_by_name albatross_instances
                instance_name
            with
            | Ok albatross -> (
                Albatross_state.query stack albatross ~domain:user.name
                  ~name:unikernel_name (`Unikernel_cmd `Unikernel_destroy)
                >>= function
                | Error msg ->
                    Middleware.http_response reqd
                      ~data:(`String ("Error querying albatross: " ^ msg))
                      `Internal_server_error
                | Ok (_hdr, res) -> (
                    Albatross.set_online albatross;
                    match Albatross_json.res res with
                    | Ok res -> Middleware.http_response reqd ~data:res `OK
                    | Error (`String err) ->
                        Middleware.http_response reqd ~data:(`String err)
                          `Internal_server_error))
            | _ ->
                Logs.err (fun m ->
                    m "Error finding albatross instance %s"
                      (Configuration.name_to_str instance_name));
                Middleware.http_response reqd
                  ~data:
                    (`String
                       ("Error finding albatross instance: "
                       ^ Configuration.name_to_str instance_name))
                  `Not_found)
        | Error (`Msg err), _ ->
            Middleware.http_response reqd
              ~data:(`String ("Error converting instance name: " ^ err))
              `Bad_request
        | _, Error (`Msg err) ->
            Middleware.http_response reqd
              ~data:(`String ("Error converting unikernel name: " ^ err))
              `Bad_request)
    | _ ->
        Middleware.http_response reqd
          ~data:(`String "Couldn't find unikernel name in json") `Bad_request

  let unikernel_restart stack albatross_instances (user : User_model.user)
      json_dict reqd =
    (* TODO use uuid in the future *)
    match
      Utils.Json.(get "name" json_dict, get "albatross_instance" json_dict)
    with
    | Some (`String unikernel_name), Some (`String instance_name) -> (
        match
          ( Configuration.name_of_str instance_name,
            Configuration.name_of_str unikernel_name )
        with
        | Ok instance_name, Ok unikernel_name -> (
            match
              Albatross_state.find_instance_by_name albatross_instances
                instance_name
            with
            | Ok albatross -> (
                Albatross_state.query stack albatross ~domain:user.name
                  ~name:unikernel_name
                  (`Unikernel_cmd (`Unikernel_restart None))
                >>= function
                | Error msg ->
                    Middleware.http_response reqd
                      ~data:(`String ("Error querying albatross: " ^ msg))
                      `Internal_server_error
                | Ok (_hdr, res) -> (
                    Albatross.set_online albatross;
                    match Albatross_json.res res with
                    | Ok res -> Middleware.http_response reqd ~data:res `OK
                    | Error (`String err) ->
                        Middleware.http_response reqd ~data:(`String err)
                          `Internal_server_error))
            | _ ->
                Logs.err (fun m ->
                    m "Error finding albatross instance %s"
                      (Configuration.name_to_str instance_name));
                Middleware.http_response reqd
                  ~data:
                    (`String
                       ("Error finding albatross instance: "
                       ^ Configuration.name_to_str instance_name))
                  `Not_found)
        | Error (`Msg err), _ ->
            Middleware.http_response reqd
              ~data:(`String ("Error converting instance name: " ^ err))
              `Bad_request
        | _, Error (`Msg err) ->
            Middleware.http_response reqd
              ~data:(`String ("Error converting unikernel name: " ^ err))
              `Bad_request)
    | _ ->
        Middleware.http_response reqd
          ~data:(`String "Couldn't find unikernel name in json") `Bad_request

  let unikernel_create stack albatross_instances token_or_cookie
      (user : User_model.user) reqd =
    let generate_http_error_response msg code =
      Logs.warn (fun m -> m "Unikernel_create error: %s" msg);
      Middleware.http_response reqd ~data:(`String msg) code
    in
    get_multipart_request_as_stream reqd >>= function
    | Error msg -> generate_http_error_response msg `Bad_request
    | Ok (`Parse th, stream) -> (
        let consume_part_content contents =
          Lwt_stream.to_list contents >|= String.concat ""
        in
        let name_ref = ref None in
        let cfg_ref = ref None in
        let force_ref = ref None in
        let csrf_ref = ref None in
        let albatross_instance_ref = ref None in
        let process_stream () =
          Lwt_stream.iter_s
            (function
              | (Some "albatross_instance", _), _, contents ->
                  consume_part_content contents >>= fun v ->
                  albatross_instance_ref := Some v;
                  Lwt.return_unit
              | (Some "unikernel_name", _), _, contents ->
                  consume_part_content contents >>= fun v ->
                  name_ref := Some v;
                  Lwt.return_unit
              | (Some "unikernel_config", _), _, contents ->
                  consume_part_content contents >>= fun v ->
                  cfg_ref := Some v;
                  Lwt.return_unit
              | (Some "unikernel_force_create", _), _, contents ->
                  consume_part_content contents >>= fun v ->
                  force_ref := Some v;
                  Lwt.return_unit
              | (Some "molly_csrf", _), _, contents ->
                  consume_part_content contents >>= fun v ->
                  csrf_ref := Some v;
                  Lwt.return_unit
              | (Some "binary", _), _, contents -> (
                  match
                    ( !albatross_instance_ref,
                      !name_ref,
                      !cfg_ref,
                      !force_ref,
                      !csrf_ref )
                  with
                  | ( Some instance_name,
                      Some unikernel_name,
                      Some cfg,
                      Some force_create,
                      Some csrf ) -> (
                      let process_unikernel_create albatross unikernel_name
                          _reqd =
                        match Albatross_json.config_of_json cfg with
                        | Error (`Msg err) ->
                            generate_http_error_response
                              ("Invalid unikernel arguments: " ^ err)
                              `Bad_request
                        | Ok cfg -> (
                            let albatross_cmd =
                              if bool_of_string force_create then
                                `Unikernel_cmd (`Unikernel_force_create cfg)
                              else `Unikernel_cmd (`Unikernel_create cfg)
                            in
                            Albatross_state.query stack albatross
                              ~domain:user.name ~name:unikernel_name
                              ~push:(fun () -> Lwt_stream.get contents)
                              albatross_cmd
                            >>= function
                            | Error err ->
                                generate_http_error_response
                                  ("Albatross Query Error: " ^ err)
                                  `Internal_server_error
                            | Ok (_hdr, res) -> (
                                Albatross.set_online albatross;
                                match Albatross_json.res res with
                                | Ok res_json ->
                                    Middleware.http_response reqd ~data:res_json
                                      `OK
                                | Error (`String err_str) ->
                                    generate_http_error_response
                                      ("Albatross Response Error: " ^ err_str)
                                      `Internal_server_error
                                | Error (`Msg err_msg) ->
                                    generate_http_error_response
                                      ("Albatross JSON Error: " ^ err_msg)
                                      `Internal_server_error))
                      in
                      match
                        ( Configuration.name_of_str instance_name,
                          Configuration.name_of_str unikernel_name )
                      with
                      | Ok instance_name, Ok unikernel_name -> (
                          match
                            Albatross_state.find_instance_by_name
                              albatross_instances instance_name
                          with
                          | Ok albatross -> (
                              match token_or_cookie with
                              | `Token ->
                                  process_unikernel_create albatross
                                    unikernel_name reqd
                              | `Cookie ->
                                  csrf_verification
                                    (process_unikernel_create albatross
                                       unikernel_name)
                                    user csrf reqd)
                          | _ ->
                              generate_http_error_response
                                ("Error finding albatross instance: "
                                ^ Configuration.name_to_str instance_name)
                                `Not_found)
                      | Error (`Msg err), _ ->
                          generate_http_error_response
                            ("Error converting instance name: " ^ err)
                            `Bad_request
                      | _, Error (`Msg err) ->
                          generate_http_error_response
                            ("Error converting unikernel name: " ^ err)
                            `Bad_request)
                  | _ ->
                      Logs.info (fun m -> m "Missing Fields");
                      generate_http_error_response
                        "One or more required fields are missing." `Bad_request)
              | (opt_n, _), _, contents ->
                  Logs.debug (fun m ->
                      m "Junking unexpected part: %s"
                        (Option.value ~default:"<unnamed>" opt_n));
                  Lwt_stream.junk_while (Fun.const true) contents)
            stream
        in
        Lwt.both th (process_stream ()) >>= fun (parse_res, ()) ->
        match parse_res with
        | Error (`Msg e) ->
            Logs.info (fun m -> m "Multipart parser thread error: %s" e);
            Lwt.return_unit
        | Ok _ ->
            Logs.info (fun m ->
                m "Multipart streamed correctly and unikernel created.");
            Lwt.return_unit)

  let unikernel_console stack albatross unikernel_name _
      (user : User_model.user) reqd =
    (* TODO use uuid in the future *)
    let response = Middleware.http_event_source_response reqd `OK in
    let f (ts, data) =
      let json = Albatross_json.console_data_to_json (ts, data) in
      response (Yojson.Basic.to_string json)
    in
    Albatross_state.query_console stack albatross ~domain:user.name
      ~name:unikernel_name f
    >>= function
    | Error _err -> Lwt.return_unit
    | Ok () ->
        Albatross.set_online albatross;
        Lwt.return_unit

  let view_user stack albatross_instances store uuid
      (page : [> `Profile | `Unikernels | `Policy ]) _ (user : User_model.user)
      reqd =
    match Store.find_by_uuid store uuid with
    | Some u -> (
        user_unikernels stack albatross_instances u.name >>= fun unikernels ->
        let now = Mirage_ptime.now () in
        generate_csrf_token store user now reqd >>= function
        | Ok csrf -> (
            let reply content =
              reply reqd ~content_type:"text/html"
                (Dashboard.dashboard_layout ~csrf user
                   ~page_title:
                     (String.capitalize_ascii
                        (Configuration.name_to_str u.name))
                   ~content ~icon:"/images/robur.png" ())
                ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
                `OK
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
                     (Unikernel_index.unikernel_index_layout unikernels now)
                     u.uuid)
            | `Policy ->
                reply
                  (User_single.user_single_layout ~active_tab:Policy
                     (User_single.user_policy u
                        ~empty_policy:Albatross_state.empty_policy
                        (Albatross_state.all_policies ~domain:u.name
                           albatross_instances))
                     u.uuid))
        | Error err ->
            Middleware.http_response ~api_meth:false ~title:err.title
              ~data:err.data reqd `Internal_server_error)
    | None ->
        Middleware.http_response ~api_meth:false
          ~data:(`String ("Couldn't find account with uuid: " ^ uuid))
          reqd `Not_found

  let edit_policy store uuid albatross _ (user : User_model.user) reqd =
    match Store.find_by_uuid store uuid with
    | Some u -> (
        let user_policy =
          Option.value ~default:Albatross_state.empty_policy
            (Option.join
               (Result.to_option
                  (Albatross_state.policy albatross ~domain:u.name)))
        in
        match Albatross_state.policy_resource_avalaible albatross with
        | Ok unallocated_resources -> (
            let now = Mirage_ptime.now () in
            generate_csrf_token store user now reqd >>= function
            | Ok csrf ->
                reply reqd ~content_type:"text/html"
                  (Dashboard.dashboard_layout ~csrf user
                     ~page_title:
                       (String.capitalize_ascii
                          (Configuration.name_to_str u.name))
                     ~content:
                       (Update_policy.update_policy_layout u
                          albatross.configuration.name ~user_policy
                          ~unallocated_resources)
                     ~icon:"/images/robur.png" ())
                  ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
                  `OK
            | Error err ->
                Middleware.http_response ~api_meth:false ~title:err.title
                  ~data:err.data reqd `Bad_request)
        | Error err ->
            Middleware.http_response ~api_meth:false
              ~data:(`String ("Couldn't get unallocated resources: " ^ err))
              reqd `Bad_request)
    | None ->
        Middleware.http_response ~api_meth:false
          ~data:(`String ("Couldn't find account with uuid: " ^ uuid))
          reqd `Not_found

  let update_policy stack albatross_instances store _user json_dict reqd =
    match
      Utils.Json.(get "user_uuid" json_dict, get "albatross_instance" json_dict)
    with
    | Some (`String user_uuid), Some (`String instance_name) -> (
        match Store.find_by_uuid store user_uuid with
        | Some u -> (
            match Configuration.name_of_str instance_name with
            | Ok instance_name -> (
                match
                  Albatross_state.find_instance_by_name albatross_instances
                    instance_name
                with
                | Ok albatross -> (
                    match Albatross_json.policy_of_json json_dict with
                    | Ok policy -> (
                        match Albatross_state.policy albatross with
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
                                Middleware.http_response reqd
                                  ~data:
                                    (`String
                                       ("Policy is not smaller than root \
                                         policy: " ^ err))
                                  `Bad_request
                            | Ok () -> (
                                Albatross_state.set_policy stack albatross
                                  ~domain:u.name policy
                                >>= function
                                | Error err ->
                                    Logs.err (fun m ->
                                        m "error setting policy %a for %s: %s"
                                          Vmm_core.Policy.pp policy
                                          (Configuration.name_to_str u.name)
                                          err);
                                    Middleware.http_response reqd
                                      ~data:
                                        (`String ("error setting policy: " ^ err))
                                      `Internal_server_error
                                | Ok policy ->
                                    Middleware.http_response reqd
                                      ~data:(Albatross_json.policy_info policy)
                                      `OK))
                        | Ok None ->
                            Logs.err (fun m ->
                                m "policy: root policy can't be null ");
                            Middleware.http_response reqd
                              ~data:(`String "Root policy is null")
                              `Internal_server_error
                        | Error err ->
                            Logs.err (fun m ->
                                m
                                  "policy: an error occured while fetching \
                                   root policy: %s"
                                  err);
                            Middleware.http_response reqd
                              ~data:(`String ("error with root policy: " ^ err))
                              `Internal_server_error)
                    | Error (`Msg err) ->
                        Middleware.http_response reqd ~data:(`String err)
                          `Bad_request)
                | _ ->
                    Logs.err (fun m ->
                        m "Couldn't find albatross instance with name %s"
                          (Configuration.name_to_str instance_name));
                    Middleware.http_response reqd
                      ~data:
                        (`String
                           ("Couldn't find albatross instance with name: "
                           ^ Configuration.name_to_str instance_name))
                      `Not_found)
            | Error (`Msg err) ->
                Middleware.http_response reqd
                  ~data:
                    (`String
                       ("Couldn't convert name to albatross instance: " ^ err))
                  `Bad_request)
        | None ->
            Middleware.http_response reqd ~data:(`String "User not found")
              `Not_found)
    | _ ->
        Middleware.http_response reqd
          ~data:
            (`String
               (Fmt.str "Update policy: Unexpected fields. Got %s"
                  (Utils.Json.to_string (`Assoc json_dict))))
          `Bad_request

  let volumes stack store albatross _ (user : User_model.user) reqd =
    user_volumes_by_instance stack albatross user.name >>= fun blocks ->
    let now = Mirage_ptime.now () in
    let policy =
      Result.fold ~ok:Fun.id
        ~error:(fun _ -> None)
        (Albatross_state.policy ~domain:user.name albatross)
    in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        reply reqd ~content_type:"text/html"
          (Dashboard.dashboard_layout ~csrf user
             ~page_title:
               (String.capitalize_ascii (Configuration.name_to_str user.name))
             ~content:
               (Volume_index.volume_index_layout albatross.configuration.name
                  blocks policy)
             ~icon:"/images/robur.png" ())
          ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
          `OK
    | Error err ->
        Middleware.http_response ~api_meth:false ~title:err.title ~data:err.data
          reqd `Bad_request

  let delete_volume stack albatross_instances (user : User_model.user) json_dict
      reqd =
    match
      Utils.Json.(get "block_name" json_dict, get "albatross_instance" json_dict)
    with
    | Some (`String block_name), Some (`String instance_name) -> (
        match
          ( Configuration.name_of_str instance_name,
            Configuration.name_of_str block_name )
        with
        | Ok instance_name, Ok block_name -> (
            match
              Albatross_state.find_instance_by_name albatross_instances
                instance_name
            with
            | Ok albatross -> (
                Albatross_state.query stack albatross ~domain:user.name
                  ~name:block_name (`Block_cmd `Block_remove)
                >>= function
                | Error err ->
                    Middleware.http_response reqd
                      ~data:(`String ("Error querying albatross: " ^ err))
                      `Internal_server_error
                | Ok (_hdr, res) -> (
                    Albatross.set_online albatross;
                    match Albatross_json.res res with
                    | Ok res -> Middleware.http_response reqd ~data:res `OK
                    | Error (`String err) ->
                        Middleware.http_response reqd ~data:(`String err)
                          `Internal_server_error))
            | _ ->
                Logs.err (fun m ->
                    m "Couldn't find albatross instance with name %s"
                      (Configuration.name_to_str instance_name));
                Middleware.http_response reqd
                  ~data:
                    (`String
                       ("Couldn't find albatross instance with name: "
                       ^ Configuration.name_to_str instance_name))
                  `Not_found)
        | Error (`Msg err), _ ->
            Middleware.http_response reqd
              ~data:(`String ("Couldn't convert instance name: " ^ err))
              `Bad_request
        | _, Error (`Msg err) ->
            Middleware.http_response reqd
              ~data:(`String ("Couldn't convert block name: " ^ err))
              `Bad_request)
    | _ ->
        Middleware.http_response reqd
          ~data:(`String "Couldn't find block name in json") `Bad_request

  let create_or_upload_volume stack albatross_instances c_or_u token_or_cookie
      (user : User_model.user) reqd =
    let cmd_name =
      match c_or_u with `Create -> "create" | `Upload -> "upload"
    in
    let generate_http_error_response msg code =
      Logs.warn (fun m -> m "Block %s error: %s" cmd_name msg);
      Middleware.http_response reqd ~data:(`String msg) code
    in
    get_multipart_request_as_stream reqd >>= function
    | Error msg -> generate_http_error_response msg `Bad_request
    | Ok (`Parse th, stream) -> (
        let consume_part_content contents =
          Lwt_stream.to_list contents >|= String.concat ""
        in
        let json_data_ref = ref None in
        let csrf_ref = ref None in
        let albatross_ref = ref None in

        let process_stream () =
          Lwt_stream.iter_s
            (function
              | (Some "albatross_instance", _), _, contents ->
                  consume_part_content contents >>= fun v ->
                  albatross_ref := Some v;
                  Lwt.return_unit
              | (Some "molly_csrf", _), _, contents ->
                  consume_part_content contents >>= fun v ->
                  csrf_ref := Some v;
                  Lwt.return_unit
              | (Some "json_data", _), _, contents ->
                  consume_part_content contents >>= fun v ->
                  json_data_ref := Some v;
                  Lwt.return_unit
              | (Some "block_data", _), _, contents -> (
                  match (!albatross_ref, !csrf_ref, !json_data_ref) with
                  | Some albatross_instance, Some csrf, Some json -> (
                      let add_block stack albatross block_name block_size =
                        Albatross_state.query stack albatross ~domain:user.name
                          ~name:block_name
                          (`Block_cmd (`Block_add block_size))
                        >>= function
                        | Error err ->
                            generate_http_error_response
                              (Fmt.str "an error with albatross. got %s" err)
                              `Internal_server_error
                            >|= fun () -> Error ()
                        | Ok (_hdr, res) -> (
                            Albatross.set_online albatross;
                            match Albatross_json.res res with
                            | Error (`String err) ->
                                generate_http_error_response
                                  (Fmt.str "unexpected field. got %s" err)
                                  `Bad_request
                                >|= fun () -> Error ()
                            | Ok _ -> Lwt.return (Ok ()))
                      in
                      let stream_to_albatross stack albatross block_name
                          block_compressed =
                        let push () = Lwt_stream.get contents in
                        Albatross_state.query stack albatross ~domain:user.name
                          ~name:block_name ~push
                          (`Block_cmd (`Block_set block_compressed))
                        >>= function
                        | Error err ->
                            generate_http_error_response
                              (Fmt.str "an error with albatross. got %s" err)
                              `Internal_server_error
                        | Ok (_hdr, res) -> (
                            Albatross.set_online albatross;
                            match Albatross_json.res res with
                            | Error (`String err) ->
                                generate_http_error_response
                                  (Fmt.str "unexpected field. got %s" err)
                                  `Bad_request
                            | Ok res ->
                                Middleware.http_response reqd ~data:res `OK)
                      in
                      let parsed_json = Utils.Json.from_string json in
                      match Configuration.name_of_str albatross_instance with
                      | Ok albatross_instance -> (
                          match
                            Albatross_state.find_instance_by_name
                              albatross_instances albatross_instance
                          with
                          | Ok albatross -> (
                              match parsed_json with
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
                                      match
                                        Configuration.name_of_str block_name
                                      with
                                      | Error (`Msg err) ->
                                          generate_http_error_response
                                            (Fmt.str "Invalid block name: %s"
                                               err)
                                            `Bad_request
                                      | Ok block_name -> (
                                          let add_block stack albatross
                                              block_name block_size =
                                            add_block stack albatross block_name
                                              block_size
                                            >>= function
                                            | Ok () ->
                                                stream_to_albatross stack
                                                  albatross block_name
                                                  block_compressed
                                            | Error () -> Lwt.return_unit
                                          in
                                          match token_or_cookie with
                                          | `Token -> (
                                              match c_or_u with
                                              | `Create ->
                                                  add_block stack albatross
                                                    block_name block_size
                                              | `Upload ->
                                                  stream_to_albatross stack
                                                    albatross block_name
                                                    block_compressed)
                                          | `Cookie -> (
                                              match c_or_u with
                                              | `Create ->
                                                  csrf_verification
                                                    (fun _reqd ->
                                                      add_block stack albatross
                                                        block_name block_size)
                                                    user csrf reqd
                                              | `Upload ->
                                                  csrf_verification
                                                    (fun _reqd ->
                                                      stream_to_albatross stack
                                                        albatross block_name
                                                        block_compressed)
                                                    user csrf reqd)))
                                  | _ ->
                                      generate_http_error_response
                                        (Fmt.str "unexpected field. got %s"
                                           (Utils.Json.to_string
                                              (`Assoc json_dict)))
                                        `Bad_request)
                              | _ ->
                                  generate_http_error_response
                                    "expected a dictionary" `Bad_request)
                          | _ ->
                              generate_http_error_response
                                (Fmt.str
                                   "Couldn't find albatross instance with name \
                                    %s"
                                   (Configuration.name_to_str albatross_instance))
                                `Not_found)
                      | Error (`Msg err) ->
                          generate_http_error_response
                            (Fmt.str
                               "Couldn't convert name to albatross instance %s"
                               err)
                            `Bad_request)
                  | _ ->
                      Logs.info (fun m -> m "Missing Fields");
                      generate_http_error_response
                        "One or more required fields are missing." `Bad_request)
              | (opt_n, _), _, contents ->
                  Logs.debug (fun m ->
                      m "Junking unexpected part: %s"
                        (Option.value ~default:"<unnamed>" opt_n));
                  Lwt_stream.junk_while (Fun.const true) contents)
            stream
        in
        Lwt.both th (process_stream ()) >>= fun (parse_res, ()) ->
        match parse_res with
        | Error (`Msg e) ->
            Logs.info (fun m -> m "Multipart parser thread error: %s" e);
            Lwt.return_unit
        | Ok _ ->
            Logs.info (fun m -> m "Data %s to volume successfully." cmd_name);
            Lwt.return_unit)

  let download_volume stack albatross_instances (user : User_model.user)
      json_dict reqd =
    match
      Utils.Json.
        ( get "albatross_instance" json_dict,
          get "block_name" json_dict,
          get "compression_level" json_dict )
    with
    | ( Some (`String instance_name),
        Some (`String block_name),
        Some (`Int compression_level) ) -> (
        let filename = block_name ^ "_dump" in
        let disposition = "attachment; filename=\"" ^ filename ^ "\"" in
        let headers =
          [
            ("Content-type", "application/octet-stream");
            ("Content-Disposition", disposition);
          ]
        in
        let headers = H1.Headers.(of_list headers) in
        let response = H1.Response.create ~headers `OK in
        let writer = H1.Reqd.respond_with_streaming reqd response in
        let fini = ref false in
        let response data =
          match data with
          | None ->
              H1.Body.Writer.close writer;
              fini := true;
              Ok ()
          | Some data ->
              if H1.Body.Writer.is_closed writer then Error ()
              else (
                H1.Body.Writer.write_string writer data;
                H1.Body.Writer.flush writer Fun.id;
                Ok ())
        in
        match
          ( Configuration.name_of_str instance_name,
            Configuration.name_of_str block_name )
        with
        | Ok instance_name, Ok block_name -> (
            match
              Albatross_state.find_instance_by_name albatross_instances
                instance_name
            with
            | Ok albatross -> (
                Albatross_state.query_block_dump stack albatross
                  ~domain:user.name ~name:block_name compression_level response
                >>= function
                | Error _err -> Lwt.return_unit
                | Ok () ->
                    Albatross.set_online albatross;
                    Lwt.return_unit)
            | Error err ->
                Logs.err (fun m ->
                    m "Couldn't find albatross instance with name %s: %s"
                      (Configuration.name_to_str instance_name)
                      err);
                Middleware.http_response reqd
                  ~data:
                    (`String
                       ("Couldn't find albatross instance with name: "
                       ^ Configuration.name_to_str instance_name))
                  `Not_found)
        | Error (`Msg err), _ ->
            Middleware.http_response reqd
              ~data:(`String ("Couldn't convert instance name: " ^ err))
              `Bad_request
        | _, Error (`Msg err) ->
            Middleware.http_response reqd
              ~data:
                (`String
                   ("Couldn't convert block name: " ^ block_name ^ ": " ^ err))
              `Bad_request)
    | _ ->
        Middleware.http_response reqd
          ~data:(`String "Couldn't find block name in json") `Bad_request

  let account_usage stack store albatross _ (user : User_model.user) reqd =
    let now = Mirage_ptime.now () in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        user_volumes_by_instance stack albatross user.name >>= fun blocks ->
        user_unikernels_by_instance stack albatross user.name
        >>= fun unikernels ->
        let policy =
          Option.value ~default:Albatross_state.empty_policy
            (Option.join
               (Result.to_option
                  (Albatross_state.policy albatross ~domain:user.name)))
        in
        reply reqd ~content_type:"text/html"
          (Dashboard.dashboard_layout ~csrf user ~page_title:"Usage"
             ~content:
               (Account_usage.account_usage_layout albatross.configuration.name
                  policy unikernels blocks)
             ~icon:"/images/robur.png" ())
          ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
          `OK
    | Error err ->
        Middleware.http_response ~api_meth:false ~title:err.title ~data:err.data
          reqd `Internal_server_error

  let choose_instance store (albatross_instances : Albatross_state.a_map)
      callback _ (user : User_model.user) reqd =
    let now = Mirage_ptime.now () in
    if Albatross.Albatross_map.cardinal albatross_instances = 1 then
      let instance_name, _ =
        Albatross.Albatross_map.min_binding albatross_instances
      in
      Middleware.redirect_to_page
        ~path:
          (Middleware.construct_instance_redirect_url callback instance_name)
        reqd ()
    else
      generate_csrf_token store user now reqd >>= function
      | Ok csrf ->
          reply reqd ~content_type:"text/html"
            (Dashboard.dashboard_layout ~csrf user ~page_title:"Choose instance"
               ~content:
                 (Albatross_instances.select_instance user
                    (Albatross.Albatross_map.bindings albatross_instances)
                    callback)
               ~icon:"/images/robur.png" ())
            ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
            `OK
      | Error err ->
          Middleware.http_response ~api_meth:false ~title:err.title
            ~data:err.data reqd `Internal_server_error

  let api_tokens store _ (user : User_model.user) reqd =
    let now = Mirage_ptime.now () in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        reply reqd ~content_type:"text/html"
          (Dashboard.dashboard_layout ~csrf user ~page_title:"Tokens"
             ~content:(Tokens_index.tokens_index_layout user.tokens now)
             ~icon:"/images/robur.png" ())
          ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
          `OK
    | Error err ->
        Middleware.http_response reqd ~title:err.title ~data:err.data
          `Internal_server_error

  let create_token store (user : User_model.user) json_dict reqd =
    match
      Utils.Json.(get "token_name" json_dict, get "token_expiry" json_dict)
    with
    | Some (`String name), Some (`Int expiry) -> (
        let now = Mirage_ptime.now () in
        let token = User_model.generate_token ~name ~expiry ~current_time:now in
        let updated_user =
          User_model.update_user user ~tokens:(token :: user.tokens)
            ~updated_at:now ()
        in
        Store.update_user store updated_user >>= function
        | Ok () ->
            Middleware.http_response reqd
              ~data:(User_model.token_to_json token)
              `OK
        | Error (`Msg err) ->
            Logs.warn (fun m -> m "Storage error with %s" err);
            Middleware.http_response reqd ~data:(`String err)
              `Internal_server_error)
    | _ ->
        Middleware.http_response reqd
          ~data:
            (`String
               (Fmt.str "Create token: Unexpected fields. Got %s"
                  (Utils.Json.to_string (`Assoc json_dict))))
          `Bad_request

  let delete_token store (user : User_model.user) json_dict reqd =
    match Utils.Json.(get "token_value" json_dict) with
    | Some (`String value) -> (
        let now = Mirage_ptime.now () in
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
            Middleware.http_response reqd
              ~data:(`String "Token deleted succesfully") `OK
        | Error (`Msg err) ->
            Logs.warn (fun m -> m "Storage error with %s" err);
            Middleware.http_response reqd ~data:(`String err)
              `Internal_server_error)
    | _ ->
        Middleware.http_response reqd
          ~data:
            (`String
               (Fmt.str "Delete token: Unexpected fields. Got %s"
                  (Utils.Json.to_string (`Assoc json_dict))))
          `Bad_request

  let update_token store (user : User_model.user) json_dict reqd =
    match
      Utils.Json.
        ( get "token_name" json_dict,
          get "token_expiry" json_dict,
          get "token_value" json_dict )
    with
    | Some (`String name), Some (`Int expiry), Some (`String value) -> (
        let now = Mirage_ptime.now () in
        let token =
          List.find_opt
            (fun (token : User_model.token) -> String.equal token.value value)
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
                Middleware.http_response reqd
                  ~data:(User_model.token_to_json updated_token)
                  `OK
            | Error (`Msg err) ->
                Logs.warn (fun m -> m "Storage error with %s" err);
                Middleware.http_response reqd ~data:(`String err)
                  `Internal_server_error)
        | None ->
            Middleware.http_response reqd ~data:(`String "Token not found")
              `Not_found)
    | _ ->
        Middleware.http_response reqd
          ~data:
            (`String
               (Fmt.str "Update token: Unexpected fields. Got %s"
                  (Utils.Json.to_string (`Assoc json_dict))))
          `Bad_request

  let view_albatross_error_logs store albatross _ (user : User_model.user) reqd
      =
    let now = Mirage_ptime.now () in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        reply reqd ~content_type:"text/html"
          (Dashboard.dashboard_layout ~csrf user ~page_title:"Albatross Errors"
             ~content:(Albatross_status.albatross_status_layout albatross)
             ~icon:"/images/robur.png" ())
          ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
          `OK
    | Error err ->
        Middleware.http_response ~api_meth:false ~title:err.title ~data:err.data
          reqd `Internal_server_error

  let request_handler stack albatross_instances js_file css_file imgs store
      http_client flow (_ipaddr, _port) reqd =
    Lwt.async (fun () ->
        let bad_request () =
          Middleware.http_response reqd
            ~data:(`String "Bad HTTP request method.") `Bad_request
        in
        let req = H1.Reqd.request reqd in
        let path = Uri.(pct_decode (path (of_string req.H1.Request.target))) in
        let check_meth m f = if m = req.meth then f () else bad_request () in
        let get_query_parameter name =
          match
            Uri.get_query_param (Uri.of_string req.H1.Request.target) name
          with
          | Some param -> Ok param
          | None -> Error (Fmt.str "Couldn't find %s in query params" name)
        in
        let unikernel fn (token_or_cookie : [> `Cookie | `Token ])
            (user : User_model.user) reqd =
          match get_query_parameter "unikernel" with
          | Ok unikernel -> (
              match Configuration.name_of_str unikernel with
              | Ok unikernel -> fn unikernel token_or_cookie user reqd
              | Error (`Msg err) ->
                  Middleware.http_response ~api_meth:false
                    ~title:"Unikernel name error"
                    ~data:(`String ("Error with unikernel name: " ^ err))
                    reqd `Bad_request)
          | Error err ->
              Middleware.http_response ~api_meth:false ~data:(`String err) reqd
                `Bad_request
        in
        let albatross_instance endpoint fn
            (token_or_cookie : [> `Cookie | `Token ]) (user : User_model.user)
            reqd =
          match get_query_parameter "instance" with
          | Ok instance -> (
              match Configuration.name_of_str instance with
              | Ok instance_name -> (
                  match
                    Albatross_state.find_instance_by_name !albatross_instances
                      instance_name
                  with
                  | Ok albatross -> fn albatross token_or_cookie user reqd
                  | _ ->
                      Logs.err (fun m ->
                          m "Couldn't find albatross instance with name %s"
                            (Configuration.name_to_str instance_name));
                      Middleware.http_response ~api_meth:false
                        ~data:
                          (`String
                             ("Couldn't find albatross instance with name: "
                             ^ Configuration.name_to_str instance_name))
                        reqd `Not_found)
              | Error (`Msg err) ->
                  Middleware.http_response ~api_meth:false
                    ~title:"Albatross instance error"
                    ~data:
                      (`String ("Error with albatross instance name: " ^ err))
                    reqd `Bad_request)
          | Error _ -> Middleware.redirect_to_instance_selector endpoint reqd ()
        in
        match path with
        | "/" ->
            check_meth `GET (fun () ->
                reply reqd ~content_type:"text/html"
                  (Guest_layout.guest_layout
                     ~page_title:"Deploy unikernels with ease"
                     ~content:Index_page.index_page ~icon:"/images/robur.png" ())
                  `OK)
        | "/main.js" ->
            check_meth `GET (fun () ->
                reply reqd ~content_type:"text/javascript" js_file `OK)
        | "/images/molly_bird.jpeg" ->
            check_meth `GET (fun () ->
                reply reqd ~content_type:"image/jpeg" imgs.molly_img `OK)
        | "/images/albatross_1.png" ->
            check_meth `GET (fun () ->
                reply reqd ~content_type:"image/jpeg" imgs.albatross_img `OK)
        | "/images/dashboard_1.png" ->
            check_meth `GET (fun () ->
                reply reqd ~content_type:"image/png" imgs.dashboard_img `OK)
        | "/images/mirage_os_1.png" ->
            check_meth `GET (fun () ->
                reply reqd ~content_type:"image/jpeg" imgs.mirage_img `OK)
        | "/images/robur.png" ->
            check_meth `GET (fun () ->
                reply reqd ~content_type:"image/png" imgs.robur_img `OK)
        | "/style.css" ->
            check_meth `GET (fun () ->
                reply reqd ~content_type:"text/css" css_file `OK)
        | "/sign-up" -> check_meth `GET (fun () -> sign_up reqd)
        | "/sign-in" -> check_meth `GET (fun () -> sign_in reqd)
        | "/api/register" -> check_meth `POST (fun () -> register store reqd)
        | "/api/login" -> check_meth `POST (fun () -> login store reqd)
        | "/verify-email" ->
            check_meth `GET (fun () ->
                authenticate store reqd
                  (email_verification (verify_email store)))
        | "/auth/verify" ->
            check_meth `GET (fun () ->
                match get_query_parameter "token" with
                | Ok token ->
                    authenticate store reqd
                      (email_verification (verify_email_token store token))
                | Error err ->
                    Middleware.http_response ~api_meth:false ~data:(`String err)
                      reqd `Bad_request)
        | "/albatross/instances" ->
            check_meth `GET (fun () ->
                Middleware.redirect_to_instance_selector "/dashboard" reqd ())
        | "/dashboard" ->
            check_meth `GET (fun () ->
                authenticate store reqd
                  (dashboard stack !albatross_instances store))
        | "/account" ->
            check_meth `GET (fun () ->
                authenticate store reqd (account_page store))
        | "/account/password/update" ->
            check_meth `POST (fun () ->
                authenticate store reqd
                  (extract_json_csrf_token (update_password store)))
        | "/api/account/sessions/close" ->
            check_meth `POST (fun () ->
                authenticate store reqd
                  (extract_json_csrf_token (close_sessions store)))
        | "/logout" ->
            check_meth `POST (fun () ->
                authenticate store reqd
                  (extract_json_csrf_token (close_sessions ~logout:true store)))
        | "/api/account/session/close" ->
            check_meth `POST (fun () ->
                authenticate store reqd
                  (extract_json_csrf_token (close_session store)))
        | "/volumes" ->
            check_meth `GET (fun () ->
                authenticate store reqd
                  (albatross_instance "/volumes" (volumes stack store)))
        | "/api/volume/delete" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (extract_json_csrf_token
                     (delete_volume stack !albatross_instances)))
        | "/api/volume/create" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (fun token_or_cookie user reqd ->
                    create_or_upload_volume stack !albatross_instances `Create
                      token_or_cookie user reqd))
        | "/api/volume/download" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (extract_json_csrf_token
                     (download_volume stack !albatross_instances)))
            >>= fun () -> Paf.TCP.close flow
        | "/api/volume/upload" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (fun token_or_cookie user reqd ->
                    create_or_upload_volume stack !albatross_instances `Upload
                      token_or_cookie user reqd))
        | "/tokens" ->
            check_meth `GET (fun () ->
                authenticate store reqd (api_tokens store))
        | "/api/tokens/create" ->
            check_meth `POST (fun () ->
                authenticate ~api_meth:true store reqd
                  (extract_json_csrf_token (create_token store)))
        | "/api/tokens/delete" ->
            check_meth `POST (fun () ->
                authenticate ~api_meth:true store reqd
                  (extract_json_csrf_token (delete_token store)))
        | "/api/tokens/update" ->
            check_meth `POST (fun () ->
                authenticate ~api_meth:true store reqd
                  (extract_json_csrf_token (update_token store)))
        | "/admin/users" ->
            check_meth `GET (fun () ->
                authenticate ~check_admin:true store reqd (users store))
        | "/usage" ->
            check_meth `GET (fun () ->
                authenticate store reqd
                  (albatross_instance "/usage" (account_usage stack store)))
        | "/select/instance" ->
            check_meth `GET (fun () ->
                let callback_link =
                  match
                    Uri.get_query_param
                      (Uri.of_string req.H1.Request.target)
                      "callback"
                  with
                  | Some link -> link
                  | None -> "/dashboard"
                in
                authenticate store reqd
                  (choose_instance store !albatross_instances callback_link))
        | "/admin/user/profile" ->
            check_meth `GET (fun () ->
                match get_query_parameter "uuid" with
                | Ok uuid ->
                    authenticate ~check_admin:true store reqd
                      (view_user stack !albatross_instances store uuid `Profile)
                | Error err ->
                    Middleware.http_response ~api_meth:false ~data:(`String err)
                      reqd `Bad_request)
        | "/admin/user/unikernels" ->
            check_meth `GET (fun () ->
                match get_query_parameter "uuid" with
                | Ok uuid ->
                    authenticate ~check_admin:true store reqd
                      (view_user stack !albatross_instances store uuid
                         `Unikernels)
                | Error err ->
                    Middleware.http_response ~api_meth:false ~data:(`String err)
                      reqd `Bad_request)
        | "/admin/user/policy" ->
            check_meth `GET (fun () ->
                match get_query_parameter "uuid" with
                | Ok uuid ->
                    authenticate ~check_admin:true store reqd
                      (view_user stack !albatross_instances store uuid `Policy)
                | Error err ->
                    Middleware.http_response ~api_meth:false ~data:(`String err)
                      reqd `Bad_request)
        | "/admin/u/policy/edit" ->
            check_meth `GET (fun () ->
                match get_query_parameter "uuid" with
                | Ok uuid ->
                    authenticate ~check_admin:true store reqd
                      (albatross_instance req.H1.Request.target
                         (edit_policy store uuid))
                | Error err ->
                    Middleware.http_response ~api_meth:false ~data:(`String err)
                      reqd `Bad_request)
        | "/admin/settings" ->
            check_meth `GET (fun () ->
                authenticate ~check_admin:true store reqd
                  (settings store !albatross_instances))
        | "/admin/albatross/errors" ->
            check_meth `GET (fun () ->
                authenticate ~check_admin:true store reqd
                  (albatross_instance req.H1.Request.target
                     (view_albatross_error_logs store)))
        | "/api/admin/albatross/retry" ->
            check_meth `GET (fun () ->
                authenticate ~check_admin:true ~api_meth:true store reqd
                  (albatross_instance req.H1.Request.target
                     (retry_initializing_instance stack albatross_instances)))
        | "/api/admin/settings/update" ->
            check_meth `POST (fun () ->
                authenticate ~check_admin:true ~api_meth:true store reqd
                  (extract_json_csrf_token
                     (update_settings stack store albatross_instances `Update)))
        | "/api/admin/settings/create" ->
            check_meth `POST (fun () ->
                authenticate ~check_admin:true ~api_meth:true store reqd
                  (extract_json_csrf_token
                     (update_settings stack store albatross_instances `Create)))
        | "/api/admin/settings/delete" ->
            check_meth `POST (fun () ->
                authenticate ~check_admin:true ~api_meth:true store reqd
                  (extract_json_csrf_token
                     (delete_albatross_config store albatross_instances)))
        | "/api/admin/u/policy/update" ->
            check_meth `POST (fun () ->
                authenticate ~check_admin:true ~api_meth:true store reqd
                  (extract_json_csrf_token
                     (update_policy stack !albatross_instances store)))
        | "/api/admin/user/activate/toggle" ->
            check_meth `POST (fun () ->
                authenticate ~check_admin:true ~api_meth:true store reqd
                  (extract_json_csrf_token (toggle_account_activation store)))
        | "/api/admin/user/admin/toggle" ->
            check_meth `POST (fun () ->
                authenticate ~check_admin:true ~api_meth:true store reqd
                  (extract_json_csrf_token (toggle_admin_activation store)))
        | "/api/admin/user/account/delete" ->
            check_meth `POST (fun () ->
                authenticate ~check_admin:true ~api_meth:true store reqd
                  (extract_json_csrf_token (delete_account store)))
        | "/api/unikernels" ->
            check_meth `GET (fun () ->
                authenticate ~api_meth:true ~check_token:true store reqd
                  (unikernel_info stack !albatross_instances))
        | "/unikernel/info" ->
            check_meth `GET (fun () ->
                authenticate store reqd
                  (albatross_instance req.H1.Request.target (fun albatross ->
                       unikernel (unikernel_info_one stack store albatross))))
        | "/unikernel/deploy" ->
            check_meth `GET (fun () ->
                authenticate store reqd
                  (albatross_instance "/unikernel/deploy"
                     (deploy_form stack store)))
        | "/api/unikernel/destroy" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (extract_json_csrf_token
                     (unikernel_destroy stack !albatross_instances)))
        | "/api/unikernel/restart" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (extract_json_csrf_token
                     (unikernel_restart stack !albatross_instances)))
        | "/api/unikernel/console" ->
            check_meth `GET (fun () ->
                authenticate store reqd ~check_token:true ~api_meth:true
                  (albatross_instance req.H1.Request.target (fun albatross ->
                       unikernel (unikernel_console stack albatross))))
        | "/api/unikernel/create" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (fun token_or_cookie user reqd ->
                    unikernel_create stack !albatross_instances token_or_cookie
                      user reqd))
        | "/unikernel/update" ->
            check_meth `GET (fun () ->
                authenticate store reqd
                  (albatross_instance req.H1.Request.target (fun albatross ->
                       unikernel
                         (unikernel_prepare_update stack store http_client
                            albatross))))
        | "/api/unikernel/update" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (extract_json_csrf_token
                     (unikernel_update stack store !albatross_instances
                        http_client)))
        | "/api/unikernel/rollback" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (extract_json_csrf_token
                     (unikernel_rollback stack store !albatross_instances
                        http_client)))
        | _ ->
            Middleware.http_response ~api_meth:false ~title:"Page not found"
              ~data:(`String "This page cannot be found.") reqd `Bad_request)

  let pp_error ppf = function
    | #H1.Status.t as code -> H1.Status.pp_hum ppf code
    | `Exn exn -> Fmt.pf ppf "exception %s" (Printexc.to_string exn)

  let error_handler _dst ?request err _ =
    Logs.err (fun m ->
        m "error %a while processing request %a" pp_error err
          Fmt.(option ~none:(any "unknown") H1.Request.pp_hum)
          request)

  let start stack assets storage http_client =
    js_contents assets >>= fun js_file ->
    css_contents assets >>= fun css_file ->
    images assets >>= fun imgs ->
    Store.connect storage >>= function
    | Error (`Msg msg) -> failwith msg
    | Ok store ->
        Albatross_state.init_all stack (Store.configurations store)
        >>= fun albatross_instances ->
        let albatross_instances = ref albatross_instances in
        let port = K.port () in
        Logs.info (fun m ->
            m "Initialise an HTTP server (no HTTPS) on port %u" port);
        let request_handler =
          request_handler stack albatross_instances js_file css_file imgs store
            http_client
        in
        Paf.init ~port (S.tcp stack) >>= fun service ->
        let http = Paf.http_service ~error_handler request_handler in
        let (`Initialized th) = Paf.serve http service in
        th
end
