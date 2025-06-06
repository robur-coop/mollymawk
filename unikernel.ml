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
    Mirage_runtime.register_arg Arg.(value & opt int 80 doc)
end

module Main
    (S : Tcpip.Stack.V4V6)
    (KV_ASSETS : Mirage_kv.RO)
    (BLOCK : Mirage_block.S)
    (Http_client : Http_mirage_client.S) =
struct
  module Paf = Paf_mirage.Make (S.TCP)
  module Liveliness = Liveliness_checks.Make (S)
  module Albatross = Albatross.Make (S)

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

  let extract_json_csrf_token f token_or_cookie user reqd =
    extract_json_body reqd >>= function
    | Error (`Msg err) ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String (String.escaped err))
          `Bad_request
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
                Middleware.http_response reqd ~title:"Error"
                  ~data:(`String "Couldn't find CSRF token") `Bad_request))

  let extract_multipart_csrf_token f token_or_cookie user reqd =
    match Middleware.header "Content-Type" reqd with
    | Some header when String.starts_with ~prefix:"multipart/form-data" header
      -> (
        read_multipart_data reqd >>= function
        | Error (`Msg err) ->
            Logs.warn (fun m -> m "Failed to read multipart data: %s" err);
            Middleware.http_response reqd ~title:"Error"
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
                    Middleware.http_response reqd ~title:"Error"
                      ~data:(`String "Couldn't find CSRF token") `Bad_request
                | Some (_, token) ->
                    csrf_verification (f user multipart_body) user token reqd)))
    | None | _ ->
        Logs.warn (fun m -> m "Not a multipart request");
        Middleware.http_response reqd ~title:"Error"
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

  let authenticate ?(check_admin = false) ?(api_meth = false)
      ?(check_token = false) store reqd f =
    match authenticate_user ~check_admin ~check_token store reqd with
    | Error (v, msg) ->
        Logs.err (fun m -> m "authenticate: %s" msg);
        if api_meth || v = `Token then
          Middleware.http_response reqd ~title:"Error" ~data:(`String msg)
            `Bad_request
        else
          Middleware.redirect_to_page ~path:"/sign-in" ~clear_session:true
            ~with_error:true ~msg reqd ()
    | Ok (`Token (user, token)) -> (
        Store.increment_token_usage store token user >>= function
        | Error (`Msg err) ->
            Middleware.http_response reqd ~title:"Error" ~data:(`String err)
              `Internal_server_error
        | Ok () -> f `Token user reqd)
    | Ok (`Cookie (user, cookie)) -> (
        Store.update_cookie_usage store cookie user reqd >>= function
        | Error (`Msg err) ->
            Logs.err (fun m -> m "Error with storage: %s" err);
            Middleware.http_response reqd ~title:"Error"
              ~data:(`String (String.escaped err))
              `Internal_server_error
        | Ok () -> f `Cookie user reqd)

  let reply reqd ?(content_type = "text/plain") ?(header_list = []) data status
      =
    let h =
      H1.Headers.of_list
        [
          ("content-length", string_of_int (String.length data));
          ("content-type", content_type);
        ]
    in
    let headers = H1.Headers.add_list h header_list in
    let resp = H1.Response.create ~headers status in
    H1.Reqd.respond_with_string reqd resp data;
    Lwt.return_unit

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
    let now = Mirage_ptime.now () in
    let csrf = Middleware.generate_csrf_cookie now reqd in
    let csrf_cookie = csrf.name ^ "=" ^ csrf.value ^ ";Path=/;HttpOnly=true" in
    match Middleware.session_cookie_value reqd with
    | Ok x when x <> "" -> Middleware.redirect_to_dashboard reqd ()
    | Ok _ | Error (`Msg _) ->
        reply reqd ~content_type:"text/html"
          (Sign_up.register_page ~csrf:csrf.value ~icon:"/images/robur.png")
          ~header_list:
            [ ("Set-Cookie", csrf_cookie); ("X-MOLLY-CSRF", csrf.value) ]
          `OK

  let sign_in reqd =
    match Middleware.session_cookie_value reqd with
    | Ok x when x <> "" -> Middleware.redirect_to_dashboard reqd ()
    | Ok _ | Error (`Msg _) ->
        reply reqd ~content_type:"text/html"
          (Sign_in.login_page ~icon:"/images/robur.png" ())
          `OK

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
                let now = Mirage_ptime.now () in
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
            Middleware.http_response reqd ~title:"Error"
              ~data:(`String (String.escaped err))
              `Internal_server_error)
    | Error err ->
        reply reqd ~content_type:"text/html"
          (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
             ~content:(Error_page.error_layout err)
             ~icon:"/images/robur.png" ())
          `Internal_server_error

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

  let dashboard store albatross _ (user : User_model.user) reqd =
    let now = Mirage_ptime.now () in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        (* TODO use uuid in the future *)
        user_unikernels albatross user.name >>= fun unikernels ->
        reply reqd ~content_type:"text/html"
          (Dashboard.dashboard_layout ~csrf user
             ~content:(Unikernel_index.unikernel_index_layout unikernels now)
             ~icon:"/images/robur.png" ())
          `OK
    | Error err ->
        reply reqd ~content_type:"text/html"
          (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
             ~content:(Error_page.error_layout err)
             ~icon:"/images/robur.png" ())
          `Internal_server_error

  let account_page store _ (user : User_model.user) reqd =
    match Middleware.session_cookie_value reqd with
    | Ok active_cookie_value -> (
        let now = Mirage_ptime.now () in
        generate_csrf_token store user now reqd >>= function
        | Ok csrf ->
            reply reqd ~content_type:"text/html"
              (Dashboard.dashboard_layout ~csrf user
                 ~page_title:"Account | Mollymawk"
                 ~content:
                   (User_account.user_account_layout user ~active_cookie_value
                      now)
                 ~icon:"/images/robur.png" ())
              ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
              `OK
        | Error err ->
            reply reqd ~content_type:"text/html"
              (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
                 ~content:(Error_page.error_layout err)
                 ~icon:"/images/robur.png" ())
              `Internal_server_error)
    | Error (`Msg err) ->
        let error =
          {
            Utils.Status.code = 401;
            title = "Unauthenticated";
            success = false;
            data = `String err;
          }
        in
        reply reqd ~content_type:"text/html"
          (Guest_layout.guest_layout ~page_title:"401 | Mollymawk"
             ~content:(Error_page.error_layout error)
             ~icon:"/images/robur.png" ())
          `Unauthorized

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
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String (String.escaped err))
          `Internal_server_error

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
            reply reqd ~content_type:"text/html"
              (Guest_layout.guest_layout ~page_title:"401 | Mollymawk"
                 ~content:(Error_page.error_layout error)
                 ~icon:"/images/robur.png" ())
              `Unauthorized)
    | Error (`Msg err) ->
        let error =
          {
            Utils.Status.code = 401;
            title = "Unauthenticated";
            success = false;
            data = `String err;
          }
        in
        reply reqd ~content_type:"text/html"
          (Guest_layout.guest_layout ~page_title:"401 | Mollymawk"
             ~content:(Error_page.error_layout error)
             ~icon:"/images/robur.png" ())
          `Unauthorized

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
          `Bad_request

  let users store _ (user : User_model.user) reqd =
    let now = Mirage_ptime.now () in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        reply reqd ~content_type:"text/html"
          (Dashboard.dashboard_layout ~csrf user ~page_title:"Users | Mollymawk"
             ~content:(Users_index.users_index_layout (Store.users store) now)
             ~icon:"/images/robur.png" ())
          `OK
    | Error err ->
        reply reqd ~content_type:"text/html"
          (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
             ~content:(Error_page.error_layout err)
             ~icon:"/images/robur.png" ())
          `Internal_server_error

  let settings store _ (user : User_model.user) reqd =
    let now = Mirage_ptime.now () in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        reply reqd ~content_type:"text/html"
          (Dashboard.dashboard_layout ~csrf user
             ~page_title:"Settings | Mollymawk"
             ~content:
               (Settings_page.settings_layout (Store.configuration store))
             ~icon:"/images/robur.png" ())
          ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
          `OK
    | Error err ->
        reply reqd ~content_type:"text/html"
          (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
             ~content:(Error_page.error_layout err)
             ~icon:"/images/robur.png" ())
          `Internal_server_error

  let update_settings stack store albatross _user json_dict reqd =
    match Configuration.of_json_from_http json_dict (Mirage_ptime.now ()) with
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
          reqd `Bad_request

  let deploy_form store albatross _ (user : User_model.user) reqd =
    let now = Mirage_ptime.now () in
    user_unikernels albatross user.name >>= fun unikernels ->
    user_volumes albatross user.name >>= fun blocks ->
    generate_csrf_token store user now reqd >>= function
    | Ok csrf -> (
        let missing_policy_error ?(err = None) () =
          {
            Utils.Status.code = 500;
            title = "Resource policy error";
            data = `String (Option.value err ~default:"No policy found");
            success = false;
          }
        in
        match Albatross.policy albatross ~domain:user.name with
        | Ok p -> (
            match p with
            | Some user_policy ->
                reply reqd ~content_type:"text/html"
                  (Dashboard.dashboard_layout ~csrf user
                     ~page_title:"Deploy a Unikernel | Mollymawk"
                     ~content:
                       (Unikernel_create.unikernel_create_layout ~user_policy
                          unikernels blocks)
                     ~icon:"/images/robur.png" ())
                  ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
                  `OK
            | None ->
                reply reqd ~content_type:"text/html"
                  (Guest_layout.guest_layout
                     ~page_title:"Resource policy error | Mollymawk"
                     ~content:
                       (Error_page.error_layout (missing_policy_error ()))
                     ~icon:"/images/robur.png" ())
                  `Internal_server_error)
        | Error err ->
            reply reqd ~content_type:"text/html"
              (Guest_layout.guest_layout
                 ~page_title:"Resource policy error | Mollymawk"
                 ~content:
                   (Error_page.error_layout
                      (missing_policy_error ~err:(Some err) ()))
                 ~icon:"/images/robur.png" ())
              `Internal_server_error)
    | Error err ->
        reply reqd ~content_type:"text/html"
          (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
             ~content:(Error_page.error_layout err)
             ~icon:"/images/robur.png" ())
          `Internal_server_error

  let unikernel_info albatross _ (user : User_model.user) reqd =
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

  let unikernel_info_one albatross store name _ (user : User_model.user) reqd =
    (* TODO use uuid in the future *)
    user_unikernel albatross ~user_name:user.name ~unikernel_name:name
    >>= fun unikernel_info ->
    match unikernel_info with
    | Error err ->
        reply reqd ~content_type:"text/html"
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
          `Internal_server_error
    | Ok unikernel -> (
        let now = Mirage_ptime.now () in
        generate_csrf_token store user now reqd >>= function
        | Ok csrf ->
            let last_update_time =
              match
                List.find_opt
                  (fun (u : User_model.unikernel_update) ->
                    String.equal u.name name)
                  user.unikernel_updates
              with
              | Some unikernel_update -> Some unikernel_update.timestamp
              | None -> None
            in
            reply reqd ~content_type:"text/html"
              (Dashboard.dashboard_layout ~csrf user
                 ~content:
                   (Unikernel_single.unikernel_single_layout
                      ~unikernel_name:name unikernel ~last_update_time
                      ~current_time:now)
                 ~icon:"/images/robur.png" ())
              ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
              `OK
        | Error err ->
            reply reqd ~content_type:"text/html"
              (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
                 ~content:(Error_page.error_layout err)
                 ~icon:"/images/robur.png" ())
              `Internal_server_error)

  let unikernel_prepare_update albatross store name http_client _
      (user : User_model.user) reqd =
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
        Utils.send_http_request http_client ~base_url:Builder_web.base_url
          ~path:("/hash?sha256=" ^ Ohex.encode unikernel.digest)
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
                Utils.send_http_request http_client
                  ~base_url:Builder_web.base_url
                  ~path:("/job/" ^ current_job_data.job ^ "/build/latest")
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
                          Utils.send_http_request http_client
                            ~base_url:Builder_web.base_url
                            ~path:
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
                                  let now = Mirage_ptime.now () in
                                  generate_csrf_token store user now reqd
                                  >>= function
                                  | Ok csrf ->
                                      reply reqd ~content_type:"text/html"
                                        (Dashboard.dashboard_layout ~csrf user
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
                                        ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
                                        `OK
                                  | Error err ->
                                      reply reqd ~content_type:"text/html"
                                        (Guest_layout.guest_layout
                                           ~page_title:
                                             "CSRF Token Error | Mollymawk"
                                           ~content:
                                             (Error_page.error_layout err)
                                           ~icon:"/images/robur.png" ())
                                        `Internal_server_error)
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

  let force_create_unikernel ~unikernel_name ~push
      (unikernel_cfg : Vmm_core.Unikernel.config) (user : User_model.user)
      albatross =
    Albatross.query albatross ~domain:user.name ~name:unikernel_name ~push
      (`Unikernel_cmd (`Unikernel_force_create unikernel_cfg))
    >>= function
    | Error err ->
        Logs.warn (fun m ->
            m
              "albatross-force-create: error querying albatross for creating \
               %s: %s"
              unikernel_name err);
        Lwt.return
          (Error
             ( `Msg ("Force create: Error querying albatross: " ^ err),
               `Internal_server_error ))
    | Ok (_hdr, res) -> (
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
                  unikernel_name
                  (Yojson.Basic.to_string res));
            Lwt.return (Ok ()))

  let process_change ~unikernel_name ~job ~to_be_updated_unikernel
      ~currently_running_unikernel (unikernel_cfg : Vmm_core.Unikernel.config)
      (user : User_model.user) store http_client
      (change_kind : [ `Rollback | `Update ]) albatross =
    let change_string = function
      | `Rollback -> "rollback"
      | `Update -> "update"
    in
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
          Yojson.Basic.to_string
            (User_model.unikernel_update_to_json unikernel_update)
        in
        Logs.warn (fun m ->
            m
              "%s: storing update information for %s failed with: %s for data \
               %s"
              (change_string change_kind)
              unikernel_name err data);
        Lwt.return
          (Error
             ( `Msg
                 (change_string change_kind ^ " storing update information for "
                ^ unikernel_name ^ " failed with: " ^ err ^ " for data: " ^ data
                 ),
               (`Internal_server_error : H1.Status.t) ))
    | Ok () -> (
        let data_stream, push_chunks = Lwt_stream.create () in
        let push () = Lwt_stream.get data_stream in
        let url =
          Builder_web.base_url ^ "/job/" ^ job ^ "/build/"
          ^ to_be_updated_unikernel ^ "/main-binary"
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
          (force_create_unikernel ~unikernel_name ~push unikernel_cfg user
             albatross)
        >>= function
        | Error (`Msg err), _ ->
            Logs.err (fun m ->
                m
                  "%s: builds.robur.coop: Error while fetching the binary of \
                   %s with error: %s"
                  (change_string change_kind)
                  unikernel_name err);
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

  let process_rollback ~unikernel_name current_time albatross store http_client
      reqd (user : User_model.user) =
    match
      List.find_opt
        (fun (u : User_model.unikernel_update) ->
          String.equal u.name unikernel_name)
        user.unikernel_updates
    with
    | Some old_unikernel ->
        if
          Utils.TimeHelper.diff_in_seconds ~current_time
            ~check_time:old_unikernel.timestamp
          < Utils.rollback_seconds_limit
        then
          process_change ~unikernel_name ~job:old_unikernel.name
            ~to_be_updated_unikernel:old_unikernel.uuid
            ~currently_running_unikernel:old_unikernel.uuid old_unikernel.config
            user store http_client `Rollback albatross
          >>= function
          | Ok _res ->
              Middleware.http_response reqd ~title:"Rollback Successful"
                ~data:
                  (`String
                     ("Rollback succesful. " ^ unikernel_name
                    ^ " is now running on build " ^ old_unikernel.uuid))
                `OK
          | Error (`Msg err, http_status) ->
              Middleware.http_response reqd ~title:"Rollback Error"
                ~data:
                  (`String
                     ("Rollback failed. " ^ unikernel_name
                    ^ " failed to revert to build " ^ old_unikernel.uuid
                    ^ " with error " ^ err))
                http_status
        else
          Middleware.http_response reqd ~title:"Rollback Failed"
            ~data:
              (`String
                 (unikernel_name
                ^ " rollback failed. We can't do a rollback after 10 minutes \
                   of an update."))
            `Bad_request
    | None ->
        Middleware.http_response reqd ~title:"Rollback Error"
          ~data:
            (`String
               (unikernel_name
              ^ " rollback failed. Could not find the build information."))
          `Internal_server_error

  let process_unikernel_update ~unikernel_name ~job ~to_be_updated_unikernel
      ~currently_running_unikernel ~http_liveliness_address ~dns_liveliness
      stack cfg user store http_client albatross reqd =
    process_change ~unikernel_name ~job ~to_be_updated_unikernel
      ~currently_running_unikernel cfg user store http_client `Update albatross
    >>= function
    | Ok _res -> (
        Liveliness.interval_liveliness_checks ~unikernel_name
          ~http_liveliness_address ~dns_liveliness stack http_client
        >>= function
        | Error (`Msg err) ->
            Logs.err (fun m ->
                m
                  "liveliness-checks for %s and build %s failed with error(s) \
                   %s. now performing a rollback"
                  unikernel_name to_be_updated_unikernel err);
            process_rollback ~unikernel_name (Mirage_ptime.now ()) albatross
              store http_client reqd user
        | Ok () ->
            Middleware.http_response reqd ~title:"Update Successful"
              ~data:
                (`String
                   ("Update succesful. " ^ unikernel_name
                  ^ " is now running on build " ^ to_be_updated_unikernel))
              `OK)
    | Error (`Msg err, http_status) ->
        Middleware.http_response reqd ~title:"Update Error"
          ~data:
            (`String
               ("Update failed. " ^ unikernel_name
              ^ " failed to update to build " ^ to_be_updated_unikernel
              ^ " with error " ^ err))
          http_status

  let unikernel_update albatross store stack http_client
      (user : User_model.user) json_dict reqd =
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
    match
      Utils.Json.
        ( get "job" json_dict,
          get "to_be_updated_unikernel" json_dict,
          get "currently_running_unikernel" json_dict,
          get "unikernel_name" json_dict,
          get "http_liveliness_address" json_dict,
          get "dns_liveliness" json_dict,
          get "unikernel_arguments" json_dict )
    with
    | ( Some (`String job),
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
            match config_or_none "unikernel_arguments" configuration with
            | Error (`Msg err) ->
                Middleware.http_response reqd
                  ~title:"Error with Unikernel Arguments Json"
                  ~data:
                    (`String
                       ("Could not get the unikernel arguments json: " ^ err))
                  `Bad_request
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
                        process_unikernel_update ~unikernel_name ~job
                          ~to_be_updated_unikernel ~currently_running_unikernel
                          ~http_liveliness_address ~dns_liveliness stack cfg
                          user store http_client albatross reqd
                    | Error (`Msg err) ->
                        Logs.warn (fun m -> m "Couldn't decode data %s" err);
                        Middleware.http_response reqd ~title:"Error"
                          ~data:(`String (String.escaped err))
                          `Internal_server_error))
            | Ok (Some cfg) ->
                process_unikernel_update ~unikernel_name ~job
                  ~to_be_updated_unikernel ~currently_running_unikernel
                  ~http_liveliness_address ~dns_liveliness stack cfg user store
                  http_client albatross reqd)
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
              `Bad_request)
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Couldn't find job or build in json. Received ")
          `Bad_request

  let unikernel_rollback albatross store http_client (user : User_model.user)
      json_dict reqd =
    match Utils.Json.get "unikernel_name" json_dict with
    | Some (`String unikernel_name) ->
        process_rollback ~unikernel_name (Mirage_ptime.now ()) albatross store
          http_client reqd user
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Couldn't find unikernel name in json") `Bad_request

  let unikernel_destroy albatross (user : User_model.user) json_dict reqd =
    (* TODO use uuid in the future *)
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
          ~data:(`String "Couldn't find unikernel name in json") `Bad_request

  let unikernel_restart albatross (user : User_model.user) json_dict reqd =
    (* TODO use uuid in the future *)
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
          ~data:(`String "Couldn't find unikernel name in json") `Bad_request

  let unikernel_create token_or_cookie (user : User_model.user) albatross reqd =
    let generate_http_error_response msg code =
      Logs.warn (fun m -> m "Unikernel_create error: %s" msg);
      Middleware.http_response reqd ~title:"Error" ~data:(`String msg) code
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
        let process_stream () =
          Lwt_stream.iter_s
            (function
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
                  match (!name_ref, !cfg_ref, !force_ref, !csrf_ref) with
                  | Some unikernel_name, Some cfg, Some force_create, Some csrf
                    -> (
                      let process_unikernel_create _reqd =
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
                            Albatross.query albatross ~domain:user.name
                              ~name:unikernel_name
                              ~push:(fun () -> Lwt_stream.get contents)
                              albatross_cmd
                            >>= function
                            | Error err ->
                                generate_http_error_response
                                  ("Albatross Query Error: " ^ err)
                                  `Internal_server_error
                            | Ok (_hdr, res) -> (
                                match Albatross_json.res res with
                                | Ok res_json ->
                                    Middleware.http_response reqd
                                      ~title:"Success" ~data:res_json `OK
                                | Error (`String err_str) ->
                                    generate_http_error_response
                                      ("Albatross Response Error: " ^ err_str)
                                      `Internal_server_error
                                | Error (`Msg err_msg) ->
                                    generate_http_error_response
                                      ("Albatross JSON Error: " ^ err_msg)
                                      `Internal_server_error))
                      in
                      match token_or_cookie with
                      | `Token -> process_unikernel_create reqd
                      | `Cookie ->
                          csrf_verification process_unikernel_create user csrf
                            reqd)
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
        Lwt.both th (process_stream ()) >>= fun (_res, ()) ->
        th >>= function
        | Error (`Msg e) ->
            Logs.info (fun m -> m "Multipart parser thread error: %s" e);
            Lwt.return_unit
        | Ok _ ->
            Logs.info (fun m ->
                m "Multipart streamed correctly and unikernel created.");
            Lwt.return_unit)

  let unikernel_console albatross name _ (user : User_model.user) reqd =
    (* TODO use uuid in the future *)
    let response = Middleware.http_event_source_response reqd `OK in
    let f (ts, data) =
      let json = Albatross_json.console_data_to_json (ts, data) in
      response (Yojson.Basic.to_string json)
    in
    Albatross.query_console ~domain:user.name albatross ~name f >>= function
    | Error err ->
        Logs.warn (fun m -> m "error querying albatross: %s" err);
        Lwt.return_unit
    | Ok () -> Lwt.return_unit

  let view_user albatross store uuid _ (user : User_model.user) reqd =
    match Store.find_by_uuid store uuid with
    | Some u -> (
        user_unikernels albatross u.name >>= fun unikernels ->
        let policy =
          match Albatross.policy ~domain:u.name albatross with
          | Ok p -> p
          | Error _ -> None
        in
        let now = Mirage_ptime.now () in
        generate_csrf_token store user now reqd >>= function
        | Ok csrf ->
            reply reqd ~content_type:"text/html"
              (Dashboard.dashboard_layout ~csrf user
                 ~page_title:(String.capitalize_ascii u.name ^ " | Mollymawk")
                 ~content:
                   (User_single.user_single_layout u unikernels policy now)
                 ~icon:"/images/robur.png" ())
              ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
              `OK
        | Error err ->
            reply reqd ~content_type:"text/html"
              (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
                 ~content:(Error_page.error_layout err)
                 ~icon:"/images/robur.png" ())
              `Internal_server_error)
    | None ->
        let status =
          {
            Utils.Status.code = 404;
            title = "Error";
            data = `String ("Couldn't find account with uuid: " ^ uuid);
            success = false;
          }
        in
        reply reqd ~content_type:"text/html"
          (Guest_layout.guest_layout ~page_title:"404 | Mollymawk"
             ~content:(Error_page.error_layout status)
             ~icon:"/images/robur.png" ())
          `Not_found

  let edit_policy albatross store uuid _ (user : User_model.user) reqd =
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
            let now = Mirage_ptime.now () in
            generate_csrf_token store user now reqd >>= function
            | Ok csrf ->
                reply reqd ~content_type:"text/html"
                  (Dashboard.dashboard_layout ~csrf user
                     ~page_title:
                       (String.capitalize_ascii u.name ^ " | Mollymawk")
                     ~content:
                       (Update_policy.update_policy_layout u ~user_policy
                          ~unallocated_resources)
                     ~icon:"/images/robur.png" ())
                  ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
                  `OK
            | Error err ->
                reply reqd ~content_type:"text/html"
                  (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
                     ~content:(Error_page.error_layout err)
                     ~icon:"/images/robur.png" ())
                  `Internal_server_error)
        | Error err ->
            let status =
              {
                Utils.Status.code = 500;
                title = "Error";
                data = `String ("Policy error: " ^ err);
                success = false;
              }
            in
            reply reqd ~content_type:"text/html"
              (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
                 ~content:(Error_page.error_layout status)
                 ~icon:"/images/robur.png" ())
              `Not_found)
    | None ->
        let status =
          {
            Utils.Status.code = 404;
            title = "Error";
            data = `String ("Couldn't find account with uuid: " ^ uuid);
            success = false;
          }
        in
        reply reqd ~content_type:"text/html"
          (Guest_layout.guest_layout ~page_title:"404 | Mollymawk"
             ~content:(Error_page.error_layout status)
             ~icon:"/images/robur.png" ())
          `Not_found

  let update_policy store albatross _user json_dict reqd =
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
                            (`String
                               ("Policy is not smaller than root policy: " ^ err))
                          `Internal_server_error
                    | Ok () -> (
                        Albatross.set_policy albatross ~domain:u.name policy
                        >>= function
                        | Error err ->
                            Logs.err (fun m ->
                                m "error setting policy %a for %s: %s"
                                  Vmm_core.Policy.pp policy u.name err);
                            Middleware.http_response reqd ~title:"Error"
                              ~data:(`String ("error setting policy: " ^ err))
                              `Internal_server_error
                        | Ok policy ->
                            Middleware.http_response reqd ~title:"Success"
                              ~data:(Albatross_json.policy_info policy)
                              `OK))
                | Ok None ->
                    Logs.err (fun m -> m "policy: root policy can't be null ");
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
          `Bad_request

  let volumes store albatross _ (user : User_model.user) reqd =
    user_volumes albatross user.name >>= fun blocks ->
    let policy =
      Result.fold ~ok:Fun.id
        ~error:(fun _ -> None)
        (Albatross.policy ~domain:user.name albatross)
    in
    let now = Mirage_ptime.now () in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        reply reqd ~content_type:"text/html"
          (Dashboard.dashboard_layout ~csrf user
             ~page_title:(String.capitalize_ascii user.name ^ " | Mollymawk")
             ~content:(Volume_index.volume_index_layout blocks policy)
             ~icon:"/images/robur.png" ())
          ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
          `OK
    | Error err ->
        reply reqd ~content_type:"text/html"
          (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
             ~content:(Error_page.error_layout err)
             ~icon:"/images/robur.png" ())
          `Internal_server_error

  let delete_volume albatross (user : User_model.user) json_dict reqd =
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
          ~data:(`String "Couldn't find block name in json") `Bad_request

  let create_or_upload_volume c_or_u token_or_cookie albatross
      (user : User_model.user) reqd =
    let cmd_name =
      match c_or_u with `Create -> "create" | `Upload -> "upload"
    in
    let generate_http_error_response msg code =
      Logs.warn (fun m -> m "Block %s error: %s" cmd_name msg);
      Middleware.http_response reqd ~title:"Error" ~data:(`String msg) code
    in
    get_multipart_request_as_stream reqd >>= function
    | Error msg -> generate_http_error_response msg `Bad_request
    | Ok (`Parse th, stream) -> (
        let consume_part_content contents =
          Lwt_stream.to_list contents >|= String.concat ""
        in
        let json_data_ref = ref None in
        let csrf_ref = ref None in
        let process_stream () =
          Lwt_stream.iter_s
            (function
              | (Some "molly_csrf", _), _, contents ->
                  consume_part_content contents >>= fun v ->
                  csrf_ref := Some v;
                  Lwt.return_unit
              | (Some "json_data", _), _, contents ->
                  consume_part_content contents >>= fun v ->
                  json_data_ref := Some v;
                  Lwt.return_unit
              | (Some "block_data", _), _, contents -> (
                  match (!csrf_ref, !json_data_ref) with
                  | Some csrf, Some json -> (
                      let cmd block_size block_compressed =
                        match c_or_u with
                        | `Create ->
                            `Block_add (block_size, block_compressed, Some "")
                        | `Upload -> `Block_set (block_compressed, "")
                      in
                      let stream_to_albatross block_name block_size
                          block_compressed _reqd =
                        let push () = Lwt_stream.get contents in
                        Albatross.query albatross ~domain:user.name
                          ~name:block_name ~push
                          (`Block_cmd (cmd block_size block_compressed))
                        >>= function
                        | Error err ->
                            generate_http_error_response
                              (Fmt.str "an error with albatross. got %s" err)
                              `Bad_request
                        | Ok (_hdr, res) -> (
                            match Albatross_json.res res with
                            | Error (`String err) ->
                                generate_http_error_response
                                  (Fmt.str "unexpected field. got %s" err)
                                  `Bad_request
                            | Ok res ->
                                Middleware.http_response reqd ~title:"Success"
                                  ~data:res `OK)
                      in
                      let json =
                        try Ok (Yojson.Basic.from_string json)
                        with Yojson.Json_error s -> Error (`Msg s)
                      in
                      match json with
                      | Ok (`Assoc json_dict) -> (
                          match
                            ( c_or_u,
                              Utils.Json.
                                ( get "block_name" json_dict,
                                  get "block_size" json_dict,
                                  get "block_compressed" json_dict ) )
                          with
                          | ( `Create,
                              ( Some (`String block_name),
                                Some (`Int block_size),
                                Some (`Bool block_compressed) ) ) -> (
                              match token_or_cookie with
                              | `Token ->
                                  stream_to_albatross block_name block_size
                                    block_compressed reqd
                              | `Cookie ->
                                  csrf_verification
                                    (stream_to_albatross block_name block_size
                                       block_compressed)
                                    user csrf reqd)
                          | ( `Upload,
                              ( Some (`String block_name),
                                None,
                                Some (`Bool block_compressed) ) ) -> (
                              match token_or_cookie with
                              | `Token ->
                                  stream_to_albatross block_name 0
                                    block_compressed reqd
                              | `Cookie ->
                                  csrf_verification
                                    (stream_to_albatross block_name 0
                                       block_compressed)
                                    user csrf reqd)
                          | _ ->
                              generate_http_error_response
                                (Fmt.str "unexpected field. got %s"
                                   (Yojson.Basic.to_string (`Assoc json_dict)))
                                `Bad_request)
                      | _ ->
                          generate_http_error_response "expected a dictionary"
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
        Lwt.both th (process_stream ()) >>= fun (_res, ()) ->
        th >>= function
        | Error (`Msg e) ->
            Logs.info (fun m -> m "Multipart parser thread error: %s" e);
            Lwt.return_unit
        | Ok _ ->
            Logs.info (fun m -> m "Data %s to volume succesfully." cmd_name);
            Lwt.return_unit)

  let download_volume albatross (user : User_model.user) json_dict reqd =
    match
      Utils.Json.(get "block_name" json_dict, get "compression_level" json_dict)
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
                let disposition = "attachment; filename=\"" ^ filename ^ "\"" in
                reply reqd ~content_type:"application/octet-stream"
                  ~header_list:[ ("Content-Disposition", disposition) ]
                  file_content `OK
            | Error (`String err) ->
                Middleware.http_response reqd ~title:"Error"
                  ~data:(`String (String.escaped err))
                  `Internal_server_error))
    | _ ->
        Middleware.http_response reqd ~title:"Error"
          ~data:(`String "Couldn't find block name in json") `Bad_request

  let account_usage store albatross _ (user : User_model.user) reqd =
    let now = Mirage_ptime.now () in
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
        reply reqd ~content_type:"text/html"
          (Dashboard.dashboard_layout ~csrf user ~page_title:"Usage | Mollymawk"
             ~content:
               (Account_usage.account_usage_layout policy unikernels blocks)
             ~icon:"/images/robur.png" ())
          ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
          `OK
    | Error err ->
        reply reqd ~content_type:"text/html"
          (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
             ~content:(Error_page.error_layout err)
             ~icon:"/images/robur.png" ())
          `Internal_server_error

  let api_tokens store _ (user : User_model.user) reqd =
    let now = Mirage_ptime.now () in
    generate_csrf_token store user now reqd >>= function
    | Ok csrf ->
        reply reqd ~content_type:"text/html"
          (Dashboard.dashboard_layout ~csrf user
             ~page_title:"Tokens | Mollymawk"
             ~content:(Tokens_index.tokens_index_layout user.tokens now)
             ~icon:"/images/robur.png" ())
          ~header_list:[ ("X-MOLLY-CSRF", csrf) ]
          `OK
    | Error err ->
        reply reqd ~content_type:"text/html"
          (Guest_layout.guest_layout ~page_title:"500 | Mollymawk"
             ~content:(Error_page.error_layout err)
             ~icon:"/images/robur.png" ())
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
          `Bad_request

  let request_handler stack albatross js_file css_file imgs store http_client
      (_ipaddr, _port) reqd =
    Lwt.async (fun () ->
        let bad_request () =
          Middleware.http_response reqd ~title:"Error"
            ~data:(`String "Bad HTTP request method.") `Bad_request
        in
        let req = H1.Reqd.request reqd in
        let path = Uri.(pct_decode (path (of_string req.H1.Request.target))) in
        let check_meth m f = if m = req.meth then f () else bad_request () in
        match path with
        | "/" ->
            check_meth `GET (fun () ->
                reply reqd ~content_type:"text/html"
                  (Guest_layout.guest_layout
                     ~page_title:"Deploy unikernels with ease | Mollymawk"
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
                reply reqd ~content_type:"image/png" imgs.albatross_img `OK)
        | "/images/dashboard_1.png" ->
            check_meth `GET (fun () ->
                reply reqd ~content_type:"image/png" imgs.dashboard_img `OK)
        | "/images/mirage_os_1.png" ->
            check_meth `GET (fun () ->
                reply reqd ~content_type:"image/png" imgs.mirage_img `OK)
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
        | path when String.starts_with ~prefix:"/auth/verify/token=" path ->
            check_meth `GET (fun () ->
                let token = String.sub path 19 (String.length path - 19) in
                authenticate store reqd
                  (email_verification (verify_email_token store token)))
        | "/dashboard" ->
            check_meth `GET (fun () ->
                authenticate store reqd (dashboard store !albatross))
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
                authenticate store reqd (volumes store !albatross))
        | "/api/volume/delete" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (extract_json_csrf_token (delete_volume !albatross)))
        | "/api/volume/create" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (fun token_or_cookie user reqd ->
                    create_or_upload_volume `Create token_or_cookie !albatross
                      user reqd))
        | "/api/volume/download" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (extract_json_csrf_token (download_volume !albatross)))
        | "/api/volume/upload" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (fun token_or_cookie user reqd ->
                    create_or_upload_volume `Upload token_or_cookie !albatross
                      user reqd))
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
                authenticate store reqd (account_usage store !albatross))
        | path when String.starts_with ~prefix:"/admin/user/" path ->
            check_meth `GET (fun () ->
                let uuid = String.sub path 12 (String.length path - 12) in
                authenticate ~check_admin:true store reqd
                  (view_user !albatross store uuid))
        | path when String.starts_with ~prefix:"/admin/u/policy/edit/" path ->
            check_meth `GET (fun () ->
                let uuid = String.sub path 21 (String.length path - 21) in
                authenticate ~check_admin:true store reqd
                  (edit_policy !albatross store uuid))
        | "/admin/settings" ->
            check_meth `GET (fun () ->
                authenticate ~check_admin:true store reqd (settings store))
        | "/api/admin/settings/update" ->
            check_meth `POST (fun () ->
                authenticate ~check_admin:true ~api_meth:true store reqd
                  (extract_json_csrf_token
                     (update_settings stack store albatross)))
        | "/api/admin/u/policy/update" ->
            check_meth `POST (fun () ->
                authenticate ~check_admin:true ~api_meth:true store reqd
                  (extract_json_csrf_token (update_policy store !albatross)))
        | "/api/admin/user/activate/toggle" ->
            check_meth `POST (fun () ->
                authenticate ~check_admin:true ~api_meth:true store reqd
                  (extract_json_csrf_token (toggle_account_activation store)))
        | "/api/admin/user/admin/toggle" ->
            check_meth `POST (fun () ->
                authenticate ~check_admin:true ~api_meth:true store reqd
                  (extract_json_csrf_token (toggle_admin_activation store)))
        | "/api/unikernels" ->
            check_meth `GET (fun () ->
                authenticate ~api_meth:true ~check_token:true store reqd
                  (unikernel_info !albatross))
        | path when String.starts_with ~prefix:"/unikernel/info/" path ->
            check_meth `GET (fun () ->
                let unikernel_name =
                  String.sub path 16 (String.length path - 16)
                in
                authenticate store reqd
                  (unikernel_info_one !albatross store unikernel_name))
        | "/unikernel/deploy" ->
            check_meth `GET (fun () ->
                authenticate store reqd (deploy_form store !albatross))
        | "/api/unikernel/destroy" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (extract_json_csrf_token (unikernel_destroy !albatross)))
        | "/api/unikernel/restart" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (extract_json_csrf_token (unikernel_restart !albatross)))
        | path when String.starts_with ~prefix:"/api/unikernel/console/" path ->
            check_meth `GET (fun () ->
                let unikernel_name =
                  String.sub path 23 (String.length path - 23)
                in
                authenticate store reqd ~check_token:true ~api_meth:true
                  (unikernel_console !albatross unikernel_name))
        | "/api/unikernel/create" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (fun token_or_cookie user reqd ->
                    unikernel_create token_or_cookie user !albatross reqd))
        | path when String.starts_with ~prefix:"/unikernel/update/" path ->
            check_meth `GET (fun () ->
                let unikernel_name =
                  String.sub path 18 (String.length path - 18)
                in
                authenticate store reqd
                  (unikernel_prepare_update !albatross store unikernel_name
                     http_client))
        | "/api/unikernel/update" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (extract_json_csrf_token
                     (unikernel_update !albatross store stack http_client)))
        | "/api/unikernel/rollback" ->
            check_meth `POST (fun () ->
                authenticate ~check_token:true ~api_meth:true store reqd
                  (extract_json_csrf_token
                     (unikernel_rollback !albatross store http_client)))
        | _ ->
            let error =
              {
                Utils.Status.code = 404;
                title = "Page not found";
                success = false;
                data = `String "This page cannot be found.";
              }
            in
            reply reqd ~content_type:"text/html"
              (Guest_layout.guest_layout ~page_title:"404 | Mollymawk"
                 ~content:(Error_page.error_layout error)
                 ~icon:"/images/robur.png" ())
              `Not_found)

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
        let c = Store.configuration store in
        Albatross.init stack c.Configuration.server_ip ~port:c.server_port
          c.certificate c.private_key
        >>= fun albatross ->
        let albatross = ref albatross in
        let port = K.port () in
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
