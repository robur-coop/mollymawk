open Lwt.Infix

type images = { molly_img : string; robur_img : string }

let pp_msg ppf (`Msg msg) = Fmt.pf ppf "%s" msg

let err_to_exit pp = function
  | Ok x -> x
  | Error e ->
      Logs.err (fun m -> m "received error %a" pp e);
      exit Mirage_runtime.argument_error

module Main
    (R : Mirage_random.S)
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
    Lwt.both molly_img robur_img >|= fun (molly, robur) ->
    { molly_img = molly; robur_img = robur }

  let create_html_form assets =
    KV_ASSETS.get assets (Mirage_kv.Key.v "create_unikernel.html") >|= function
    | Error _e -> invalid_arg "Form could not be loaded"
    | Ok html -> html

  module Store = Storage.Make (BLOCK)
  module Map = Map.Make (String)

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

  module Albatross = Albatross.Make (P) (S)

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

  let request_handler stack albatross js_file css_file imgs html store
      (_ipaddr, _port) reqd =
    Lwt.async (fun () ->
        let reply ?(content_type = "text/plain") ?(header_list = []) data =
          let h =
            Httpaf.Headers.of_list
              [
                ("content-length", string_of_int (String.length data));
                ("content-type", content_type);
              ]
          in
          let headers = Httpaf.Headers.add_list h header_list in
          let resp = Httpaf.Response.create ~headers `OK in
          Httpaf.Reqd.respond_with_string reqd resp data
        in
        let reply_json json =
          reply ~content_type:"application/json" (Yojson.Basic.to_string json)
        in
        let path =
          Uri.(
            pct_decode
              (path
                 (of_string (Httpaf.Reqd.request reqd).Httpaf.Request.target)))
        in
        match path with
        | "/" ->
            Lwt.return
              (reply ~content_type:"text/html"
                 (Index.index_page ~icon:"/images/robur.png"))
        | "/unikernel-info" -> (
            (* TODO: middleware, extract domain from middleware *)
            Albatross.query !albatross ~domain:"robur"
              (`Unikernel_cmd `Unikernel_info)
            >|= function
            | Error () -> reply "error while querying albatross"
            | Ok None -> reply "got none"
            | Ok (Some (_hdr, res)) -> reply_json (Albatross_json.res res))
        | path
          when String.(length path >= 16 && sub path 0 16 = "/unikernel/info/")
          -> (
            (* TODO: middleware, extract domain from middleware *)
            let unikernel_name = String.sub path 16 (String.length path - 16) in
            Albatross.query !albatross (`Unikernel_cmd `Unikernel_info)
              ~domain:"robur" ~name:unikernel_name
            >|= function
            | Error () -> reply "error while querying albatross"
            | Ok None -> reply "got none"
            | Ok (Some (_hdr, res)) -> reply_json (Albatross_json.res res))
        | "/main.js" -> Lwt.return (reply ~content_type:"text/plain" js_file)
        | "/images/molly_bird.jpeg" ->
            Lwt.return (reply ~content_type:"image/jpeg" imgs.molly_img)
        | "/images/robur.png" ->
            Lwt.return (reply ~content_type:"image/png" imgs.robur_img)
        | "/style.css" -> Lwt.return (reply ~content_type:"text/css" css_file)
        | "/sign-up" -> (
            match Middleware.has_session_cookie reqd with
            | Some cookie -> (
                match Middleware.cookie_value_from_auth_cookie cookie with
                | Ok "" ->
                    Lwt.return
                      (reply ~content_type:"text/html"
                         (Sign_up.register_page ~icon:"/images/robur.png" ()))
                | _ -> Middleware.redirect_to_dashboard reqd ())
            | None ->
                Lwt.return
                  (reply ~content_type:"text/html"
                     (Sign_up.register_page ~icon:"/images/robur.png" ())))
        | "/sign-in" -> (
            match Middleware.has_session_cookie reqd with
            | Some cookie -> (
                match Middleware.cookie_value_from_auth_cookie cookie with
                | Ok "" ->
                    Lwt.return
                      (reply ~content_type:"text/html"
                         (Sign_in.login_page ~icon:"/images/robur.png" ()))
                | _ -> Middleware.redirect_to_dashboard reqd ())
            | None ->
                Lwt.return
                  (reply ~content_type:"text/html"
                     (Sign_in.login_page ~icon:"/images/robur.png" ())))
        | "/api/register" -> (
            let request = Httpaf.Reqd.request reqd in
            match request.meth with
            | `POST -> (
                decode_request_body reqd >>= fun data ->
                let json =
                  try Ok (Yojson.Basic.from_string data)
                  with Yojson.Json_error s -> Error (`Msg s)
                in
                match json with
                | Error (`Msg s) ->
                    Logs.warn (fun m -> m "Failed to parse JSON: %s" s);
                    let res =
                      "{\"status\": 400, \"message\": \"Bad request body\"}"
                    in
                    Lwt.return (reply ~content_type:"application/json" res)
                | Ok json -> (
                    let validate_user_input ~name ~email ~password =
                      if name = "" || email = "" || password = "" then
                        Error "All fields must be filled."
                      else if String.length name < 4 then
                        Error "Name must be at least 3 characters long."
                      else if not (Utils.Email.validate_email email) then
                        Error "Invalid email address."
                      else if String.length password < 8 then
                        Error "Password must be at least 8 characters long."
                      else Ok "Validation passed."
                    in
                    let name =
                      json
                      |> Yojson.Basic.Util.member "name"
                      |> Yojson.Basic.to_string
                    in
                    let email =
                      json
                      |> Yojson.Basic.Util.member "email"
                      |> Yojson.Basic.to_string
                    in
                    let password =
                      json
                      |> Yojson.Basic.Util.member "password"
                      |> Yojson.Basic.to_string
                    in
                    match validate_user_input ~name ~email ~password with
                    | Error s ->
                        let res =
                          "{\"status\": 400, \"success\": false, \"message\": \
                           \"" ^ s ^ "\"}"
                        in
                        Lwt.return (reply ~content_type:"application/json" res)
                    | Ok _ -> (
                        let _, (s : Storage.t) = !store in
                        let users = s.users in
                        let user =
                          User_model.check_if_user_exists email users
                        in
                        match user with
                        | Some _ ->
                            let res =
                              "{\"status\": 400, \"message\": \"A user with \
                               this email already exists.\"}"
                            in
                            Lwt.return
                              (reply ~content_type:"application/json" res)
                        | None -> (
                            let created_at = Ptime.v (P.now_d_ps ()) in
                            let user =
                              User_model.create_user ~name ~email ~password
                                ~created_at
                            in
                            Store.add_user !store user >>= function
                            | Ok store' ->
                                store := store';
                                let res =
                                  "{\"status\": 200, \"success\": true, \
                                   \"message\": {\"user\": "
                                  ^ Yojson.Basic.to_string
                                      (User_model.user_to_json user)
                                  ^ "}}"
                                in
                                let cookie =
                                  List.find
                                    (fun (c : User_model.cookie) ->
                                      c.name = "molly_session")
                                    user.cookies
                                in
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
                                Lwt.return
                                  (reply ~header_list
                                     ~content_type:"application/json" res)
                            | Error (`Msg _msg) ->
                                let res =
                                  "{\"status\": 400, \"success\": false, \
                                   \"message\": \"Something went wrong. Wait a \
                                   few seconds and try again.\"}"
                                in
                                Lwt.return
                                  (reply ~content_type:"application/json" res)))
                    ))
            | _ ->
                let res =
                  "{\"status\": 400, \"success\": false, \"message\": \"Bad \
                   request method\"}"
                in
                Lwt.return (reply ~content_type:"application/json" res))
        | "/api/login" -> (
            let request = Httpaf.Reqd.request reqd in
            match request.meth with
            | `POST -> (
                decode_request_body reqd >>= fun data ->
                let json =
                  try Ok (Yojson.Basic.from_string data)
                  with Yojson.Json_error s -> Error (`Msg s)
                in
                match json with
                | Error (`Msg s) ->
                    Logs.warn (fun m -> m "Failed to parse JSON: %s" s);
                    let res =
                      "{\"status\": 400, \"message\": \"Bad request body\"}"
                    in
                    Lwt.return (reply ~content_type:"application/json" res)
                | Ok json -> (
                    let validate_user_input ~email ~password =
                      if email = "" || password = "" then
                        Error "All fields must be filled."
                      else if not (Utils.Email.validate_email email) then
                        Error "Invalid email address."
                      else if String.length password < 8 then
                        Error "Password must be at least 8 characters long."
                      else Ok "Validation passed."
                    in
                    let email =
                      json
                      |> Yojson.Basic.Util.member "email"
                      |> Yojson.Basic.to_string
                    in
                    let password =
                      json
                      |> Yojson.Basic.Util.member "password"
                      |> Yojson.Basic.to_string
                    in
                    match validate_user_input ~email ~password with
                    | Error s ->
                        let res =
                          "{\"status\": 400, \"success\": false, \"message\": \
                           \"" ^ s ^ "\"}"
                        in
                        Lwt.return (reply ~content_type:"application/json" res)
                    | Ok _ -> (
                        let now = Ptime.v (P.now_d_ps ()) in
                        let _, (t : Storage.t) = !store in
                        let users = t.users in
                        let login =
                          User_model.login_user ~email ~password users now
                        in
                        match login with
                        | Error (`Msg s) ->
                            let res =
                              "{\"status\": 404, \"message\": \"" ^ s ^ "\"}"
                            in
                            Lwt.return
                              (reply ~content_type:"application/json" res)
                        | Ok user -> (
                            Store.update_user !store user >>= function
                            | Ok store' -> (
                                store := store';
                                let res =
                                  "{\"status\": 200, \"success\": true, \
                                   \"message\": {\"user\": "
                                  ^ Yojson.Basic.to_string
                                      (User_model.user_to_json user)
                                  ^ "}}"
                                in
                                let cookie =
                                  List.find_opt
                                    (fun (c : User_model.cookie) ->
                                      c.name = "molly_session")
                                    user.cookies
                                in
                                match cookie with
                                | Some cookie ->
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
                                    Lwt.return
                                      (reply ~header_list
                                         ~content_type:"application/json" res)
                                | None ->
                                    let res =
                                      "{\"status\": 400, \"success\": false, \
                                       \"message\": \"Something went wrong. \
                                       Wait a few seconds and try again.\"}"
                                    in
                                    Lwt.return
                                      (reply ~content_type:"application/json"
                                         res))
                            | Error (`Msg _msg) ->
                                let res =
                                  "{\"status\": 400, \"success\": false, \
                                   \"message\": \"Something went wrong. Wait a \
                                   few seconds and try again.\"}"
                                in
                                Lwt.return
                                  (reply ~content_type:"application/json" res)))
                    ))
            | _ ->
                let res =
                  "{\"status\": 400, \"success\": false, \"message\": \"Bad \
                   request method\"}"
                in
                Lwt.return (reply ~content_type:"application/json" res))
        | "/verify-email" -> (
            let now = Ptime.v (P.now_d_ps ()) in
            let _, (t : Storage.t) = !store in
            let users = User_model.create_user_session_map t.users in
            let middlewares = [ Middleware.auth_middleware now users ] in
            match Middleware.has_session_cookie reqd with
            | Some cookie -> (
                match Middleware.user_from_auth_cookie cookie users with
                | Ok user -> (
                    let email_verification_uuid = User_model.generate_uuid () in
                    let updated_user =
                      User_model.update_user user ~updated_at:now
                        ~email_verification_uuid:(Some email_verification_uuid)
                        ()
                    in
                    Store.update_user !store updated_user >>= function
                    | Ok store' ->
                        store := store';
                        let verification_link =
                          Utils.Email.generate_verification_link
                            email_verification_uuid
                        in
                        Logs.info (fun m ->
                            m "Verification link is: %s" verification_link);
                        Middleware.apply_middleware middlewares
                          (fun _reqd ->
                            Lwt.return
                              (reply ~content_type:"text/html"
                                 (Verify_email.verify_page ~user
                                    ~icon:"/images/robur.png" ())))
                          reqd
                    | Error (`Msg _msg) ->
                        let res =
                          "{\"status\": 400, \"success\": false, \"message\": \
                           \"Something went wrong. Wait a few seconds and try \
                           again.\"}"
                        in
                        Lwt.return (reply ~content_type:"application/json" res))
                | Error (`Msg s) ->
                    Logs.err (fun m -> m "Error: verify email endpoint %s" s);
                    Middleware.redirect_to_register reqd ())
            | None -> Middleware.redirect_to_login reqd ())
        | path
          when String.(
                 length path >= 19 && sub path 0 19 = "/auth/verify/token=")
          -> (
            let request = Httpaf.Reqd.request reqd in
            match request.meth with
            | `GET -> (
                let _, (t : Storage.t) = !store in
                let users = User_model.create_user_uuid_map t.users in
                let now = Ptime.v (P.now_d_ps ()) in
                let verification_token =
                  String.sub path 19 (String.length path - 19)
                in
                match
                  User_model.verify_email_token users verification_token now
                with
                | Ok user -> (
                    Store.update_user !store user >>= function
                    | Ok store' ->
                        store := store';
                        Middleware.redirect_to_dashboard reqd ()
                    | Error (`Msg _msg) ->
                        let res =
                          "{\"status\": 400, \"success\": false, \"message\": \
                           \"Something went wrong. Wait a few seconds and try \
                           again.\"}"
                        in
                        Lwt.return (reply ~content_type:"application/json" res))
                | Error (`Msg s) -> Middleware.redirect_to_login reqd ~msg:s ())
            | _ ->
                let res =
                  "{\"status\": 400, \"success\": false, \"message\": \"Bad \
                   request method\"}"
                in
                Lwt.return (reply ~content_type:"application/json" res))
        | "/dashboard" ->
            let now = Ptime.v (P.now_d_ps ()) in
            let _, (t : Storage.t) = !store in
            let users = User_model.create_user_session_map t.users in
            let middlewares =
              [
                (* Middleware.email_verified_middleware now u:sers;*)
                Middleware.auth_middleware now users;
              ]
            in
            Middleware.apply_middleware middlewares
              (fun _reqd ->
                match Middleware.user_of_cookie users now reqd with
                | Ok user ->
                    (Albatross.query !albatross
                       ~domain:user.name (* TODO use uuid in the future *)
                       (`Unikernel_cmd `Unikernel_info)
                     >|= function
                     | Error () ->
                         Logs.err (fun m ->
                             m "error while communicating with albatross");
                         []
                     | Ok None -> []
                     | Ok (Some (_hdr, `Success (`Unikernel_info unikernels)))
                       ->
                         unikernels
                     | Ok (Some reply) ->
                         Logs.err (fun m ->
                             m "expected a unikernel info reply, received %a"
                               (Vmm_commands.pp_wire ~verbose:false)
                               reply);
                         [])
                    >>= fun unikernels ->
                    Lwt.return
                      (reply ~content_type:"text/html"
                         (Dashboard.dashboard_layout
                            ~content:
                              (Unikernel_index.unikernel_index_layout unikernels
                                 now)
                            ~icon:"/images/robur.png" ()))
                | Error _ ->
                    Logs.err (fun m -> m "couldn't find user of cookie");
                    assert false)
              reqd
        | "/users" ->
            let _, (t : Storage.t) = !store in
            Lwt.return
              (reply ~content_type:"application/json"
                 (Yojson.Basic.to_string (Storage.t_to_json t)))
        | "/admin/settings" ->
            let now = Ptime.v (P.now_d_ps ()) in
            let _, (t : Storage.t) = !store in
            let users = User_model.create_user_session_map t.users in
            let configuration = t.configuration in
            let middlewares =
              [
                (* Middleware.email_verified_middleware now users;*)
                Middleware.auth_middleware now users;
              ]
              (*TODO: a middleware for admins*)
            in
            Middleware.apply_middleware middlewares
              (fun _reqd ->
                Lwt.return
                  (reply ~content_type:"text/html"
                     (Dashboard.dashboard_layout
                        ~page_title:"Settings | Mollymawk"
                        ~content:(Settings_page.settings_layout ~configuration)
                        ~icon:"/images/robur.png" ())))
              reqd
        | "/api/admin/settings/update" -> (
            let request = Httpaf.Reqd.request reqd in
            let now = Ptime.v (P.now_d_ps ()) in
            match request.meth with
            | `POST -> (
                decode_request_body reqd >>= fun data ->
                let json =
                  try Ok (Yojson.Basic.from_string data)
                  with Yojson.Json_error s -> Error (`Msg s)
                in
                match json with
                | Error (`Msg s) ->
                    Logs.warn (fun m -> m "Failed to parse JSON: %s" s);
                    let res =
                      "{\"status\": 403, \"success\": false, \"message\": \""
                      ^ String.escaped s ^ "\"}"
                    in
                    Lwt.return (reply ~content_type:"application/json" res)
                | Ok json -> (
                    match Configuration.of_json json now with
                    | Ok configuration_settings -> (
                        Store.update_configuration !store configuration_settings
                        >>= function
                        | Ok store' ->
                            store := store';
                            Albatross.init stack
                              configuration_settings.server_ip
                              ~port:configuration_settings.server_port
                              configuration_settings.certificate
                              configuration_settings.private_key
                            >>= fun new_albatross ->
                            albatross := new_albatross;

                            let res =
                              "{\"status\": 200, \"success\": true, \
                               \"message\": \" Configuration updated \
                               successfully\"}"
                            in
                            Lwt.return
                              (reply ~content_type:"application/json" res)
                        | Error (`Msg err) ->
                            let res =
                              "{\"status\": 403, \"success\": false, \
                               \"message\": \"" ^ String.escaped err ^ "\"}"
                            in
                            Lwt.return
                              (reply ~content_type:"application/json" res))
                    | Error (`Msg err) ->
                        let res =
                          "{\"status\": 403, \"success\": false, \"message\": \
                           \"" ^ String.escaped err ^ "\"}"
                        in
                        Lwt.return (reply ~content_type:"application/json" res))
                )
            | _ ->
                let res =
                  "{\"status\": 400, \"success\": false, \"message\": \"Bad \
                   request method\"}"
                in
                Lwt.return (reply ~content_type:"application/json" res))
        | "/unikernel/create" ->
            let now = Ptime.v (P.now_d_ps ()) in
            let _, (t : Storage.t) = !store in
            let users = User_model.create_user_session_map t.users in
            let middlewares = [ Middleware.auth_middleware now users ] in
            Middleware.apply_middleware middlewares
              (fun _reqd -> Lwt.return (reply ~content_type:"text/html" html))
              reqd
        | path
          when String.(
                 length path >= 20 && sub path 0 20 = "/unikernel/shutdown/")
          -> (
            let unikernel_name = String.sub path 20 (String.length path - 20) in
            (* TODO: middleware, extract domain from middleware *)
            Albatross.query !albatross ~domain:"robur"
              (`Unikernel_cmd `Unikernel_destroy) ~name:unikernel_name
            >|= function
            | Error () -> reply "error while querying albatross"
            | Ok None -> reply "got none"
            | Ok (Some (_hdr, res)) -> reply_json (Albatross_json.res res))
        | path
          when String.(
                 length path >= 19 && sub path 0 19 = "/unikernel/console/") ->
            let unikernel_name = String.sub path 19 (String.length path - 19) in
            (* TODO: middleware, extract domain from middleware *)
            ( Albatross.query ~domain:"robur" !albatross
                (`Console_cmd (`Console_subscribe (`Count 10)))
                ~name:unikernel_name
            >|= fun _ -> () )
            >>= fun () ->
            let data =
              Option.value ~default:[]
                (Map.find_opt unikernel_name !albatross.Albatross.console_output)
            in
            reply_json (`List data);
            Lwt.return_unit
        | "/unikernel/deploy" -> (
            let response_body = Httpaf.Reqd.request_body reqd in
            let finished, notify_finished = Lwt.wait () in
            let wakeup v = Lwt.wakeup_later notify_finished v in
            let on_eof data () = wakeup data in
            let f acc s = acc ^ s in
            let rec on_read on_eof acc bs ~off ~len =
              let str = Bigstringaf.substring ~off ~len bs in
              let acc = acc >>= fun acc -> Lwt.return (f acc str) in
              Httpaf.Body.schedule_read response_body
                ~on_read:(on_read on_eof acc) ~on_eof:(on_eof acc)
            in
            let f_init = Lwt.return "" in
            Httpaf.Body.schedule_read response_body
              ~on_read:(on_read on_eof f_init) ~on_eof:(on_eof f_init);
            finished >>= fun data ->
            data >>= fun data ->
            let content_type =
              Httpaf.(
                Headers.get_exn (Reqd.request reqd).Request.headers
                  "content-type")
            in
            let ct =
              Multipart_form.Content_type.of_string (content_type ^ "\r\n")
            in
            match ct with
            | Error (`Msg msg) ->
                Logs.warn (fun m -> m "couldn't content-type: %s" msg);
                Lwt.return (reply "couldn't content-type")
            | Ok ct -> (
                match Multipart_form.of_string_to_list data ct with
                | Error (`Msg msg) ->
                    Logs.warn (fun m -> m "couldn't multipart: %s" msg);
                    Lwt.return (reply ("couldn't multipart: " ^ msg))
                | Ok (m, assoc) -> (
                    let m, _r = to_map ~assoc m in
                    match
                      ( Map.find_opt "arguments" m,
                        Map.find_opt "name" m,
                        Map.find_opt "binary" m )
                    with
                    | Some (_, args), Some (_, name), Some (_, binary) -> (
                        Logs.info (fun m -> m "args %s" args);
                        match Albatross_json.config_of_json args with
                        | Ok cfg -> (
                            let config =
                              { cfg with image = Cstruct.of_string binary }
                            in
                            (* TODO: middleware, extract domain from middleware *)
                            Albatross.query !albatross ~domain:"robur" ~name
                              (`Unikernel_cmd (`Unikernel_create config))
                            >|= function
                            | Error () ->
                                Logs.warn (fun m ->
                                    m "error querying albatross");
                                reply "error while querying albatross"
                            | Ok None ->
                                Logs.warn (fun m -> m "got none");
                                reply "got none"
                            | Ok (Some (_hdr, res)) ->
                                reply_json (Albatross_json.res res))
                        | Error (`Msg msg) ->
                            Logs.warn (fun m -> m "couldn't decode data %s" msg);
                            Lwt.return
                              (reply ("couldn't decode data (of_json): " ^ msg))
                        )
                    | _ ->
                        Logs.warn (fun m -> m "couldn't find fields");
                        Lwt.return (reply "couldn't find fields"))))
        | _ ->
            let res =
              "{\"status\": 404, \"success\": false, \"message\": \"This \
               service does not exist.\"}"
            in
            Lwt.return (reply ~content_type:"application/json" res))

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
    create_html_form assets >>= fun html ->
    Store.Stored_data.connect storage >>= fun stored_data ->
    Store.read_data (Ptime.v (P.now_d_ps ())) stored_data >>= function
    | Error (`Msg msg) -> failwith msg
    | Ok data ->
        let store = ref data in
        let c = (snd data).Storage.configuration in
        Albatross.init stack c.Configuration.server_ip ~port:c.server_port
          c.certificate c.private_key
        >>= fun albatross ->
        let albatross = ref albatross in
        let port = 8080 in
        Logs.info (fun m ->
            m "Initialise an HTTP server (no HTTPS) on http://127.0.0.1:%u/"
              port);
        (* TODO we need a web thingy to edit and upload the albatross configuration:
           ip address, port, certificate, private_key
           and once updated, we need to (a) dump to the disk (b) update the "albatross" value (and call Albatross.init key)
        *)
        let request_handler _flow =
          request_handler stack albatross js_file css_file imgs html store
        in
        Paf.init ~port:8080 (S.tcp stack) >>= fun service ->
        let http = Paf.http_service ~error_handler request_handler in
        let (`Initialized th) = Paf.serve http service in
        th
end
