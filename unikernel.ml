open Lwt.Infix

type images = {
  molly_img : string;
  robur_img : string;
  albatross_img : string;
  mirage_img : string;
  dashboard_img : string;
}

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

  let authenticate ?(email_verified = true) ?(check_admin = false)
      ?(api_meth = false) store reqd f =
    let now = Ptime.v (P.now_d_ps ()) in
    let _, (t : Storage.t) = store in
    let users = User_model.create_user_session_map t.users in
    let middlewares =
      (if check_admin then
         [ Middleware.is_user_admin_middleware api_meth now users ]
       else [])
      @ (if email_verified && false (* TODO *) then
           [ Middleware.email_verified_middleware now users ]
         else [])
      @ [ Middleware.auth_middleware now users ]
    in
    Middleware.apply_middleware middlewares
      (fun _reqd ->
        match Middleware.user_of_cookie users now reqd with
        | Ok user -> f user
        | Error (`Msg msg) ->
            Logs.err (fun m -> m "couldn't find user of cookie: %s" msg);
            assert false)
      reqd

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

  let http_response reqd ?(header_list = []) ~status_code ~title ~data ~success
      http_status =
    let status = { Utils.Status.code = status_code; title; data; success } in
    Lwt.return
      (reply reqd ~content_type:"application/json" ~header_list
         (Utils.Status.to_json status)
         http_status)

  let sign_up reqd =
    match Middleware.has_session_cookie reqd with
    | Some cookie -> (
        match Middleware.cookie_value_from_auth_cookie cookie with
        | Ok "" ->
            Lwt.return
              (reply reqd ~content_type:"text/html"
                 (Sign_up.register_page ~icon:"/images/robur.png" ())
                 `OK)
        | _ -> Middleware.redirect_to_dashboard reqd ())
    | None ->
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Sign_up.register_page ~icon:"/images/robur.png" ())
             `OK)

  let sign_in reqd =
    match Middleware.has_session_cookie reqd with
    | Some cookie -> (
        match Middleware.cookie_value_from_auth_cookie cookie with
        | Ok "" ->
            Lwt.return
              (reply reqd ~content_type:"text/html"
                 (Sign_in.login_page ~icon:"/images/robur.png" ())
                 `OK)
        | _ -> Middleware.redirect_to_dashboard reqd ())
    | None ->
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
        http_response reqd ~status_code:400 ~title:"Error"
          ~data:(String.escaped err) ~success:false `Bad_request
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
          json |> Yojson.Basic.Util.member "name" |> Yojson.Basic.to_string
        in
        let email =
          json |> Yojson.Basic.Util.member "email" |> Yojson.Basic.to_string
        in
        let password =
          json |> Yojson.Basic.Util.member "password" |> Yojson.Basic.to_string
        in
        match validate_user_input ~name ~email ~password with
        | Error err ->
            http_response reqd ~status_code:400 ~title:"Error"
              ~data:(String.escaped err) ~success:false `Bad_request
        | Ok _ -> (
            let _, (s : Storage.t) = !store in
            let users = s.users in
            let user = User_model.check_if_user_exists email users in
            match user with
            | Some _ ->
                http_response reqd ~status_code:400 ~title:"Error"
                  ~data:"A user with this email already exist." ~success:false
                  `Bad_request
            | None -> (
                let created_at = Ptime.v (P.now_d_ps ()) in
                let user =
                  let active, super_user =
                    if List.length users = 0 then (true, true)
                    else (false, false)
                  in
                  User_model.create_user ~name ~email ~password ~created_at
                    ~active ~super_user
                in
                Store.add_user !store user >>= function
                | Ok store' ->
                    store := store';
                    let cookie =
                      List.find
                        (fun (c : User_model.cookie) ->
                          c.name = "molly_session")
                        user.cookies
                    in
                    let cookie_value =
                      cookie.name ^ "=" ^ cookie.value ^ ";Path=/;HttpOnly=true"
                    in
                    let header_list =
                      [
                        ("Set-Cookie", cookie_value); ("location", "/dashboard");
                      ]
                    in
                    http_response reqd ~header_list ~status_code:200
                      ~title:"Success"
                      ~data:
                        (Yojson.Basic.to_string (User_model.user_to_json user))
                      ~success:true `OK
                | Error (`Msg err) ->
                    http_response reqd ~status_code:400 ~title:"Error"
                      ~data:(String.escaped err) ~success:false `Bad_request)))

  let login store reqd =
    decode_request_body reqd >>= fun data ->
    let json =
      try Ok (Yojson.Basic.from_string data)
      with Yojson.Json_error s -> Error (`Msg s)
    in
    match json with
    | Error (`Msg err) ->
        Logs.warn (fun m -> m "Failed to parse JSON: %s" err);
        http_response reqd ~status_code:400 ~title:"Error"
          ~data:(String.escaped err) ~success:false `Bad_request
    | Ok json -> (
        let validate_user_input ~email ~password =
          if email = "" || password = "" then Error "All fields must be filled."
          else if not (Utils.Email.validate_email email) then
            Error "Invalid email address."
          else if String.length password < 8 then
            Error "Password must be at least 8 characters long."
          else Ok "Validation passed."
        in
        let email =
          json |> Yojson.Basic.Util.member "email" |> Yojson.Basic.to_string
        in
        let password =
          json |> Yojson.Basic.Util.member "password" |> Yojson.Basic.to_string
        in
        match validate_user_input ~email ~password with
        | Error err ->
            http_response reqd ~status_code:400 ~title:"Error"
              ~data:(String.escaped err) ~success:false `Bad_request
        | Ok _ -> (
            let now = Ptime.v (P.now_d_ps ()) in
            let _, (t : Storage.t) = !store in
            let users = t.users in
            let login = User_model.login_user ~email ~password users now in
            match login with
            | Error (`Msg err) ->
                http_response reqd ~status_code:400 ~title:"Error"
                  ~data:(String.escaped err) ~success:false `Bad_request
            | Ok user -> (
                Store.update_user !store user >>= function
                | Ok store' -> (
                    store := store';
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
                        http_response reqd ~header_list ~status_code:200
                          ~title:"Success"
                          ~data:
                            (Yojson.Basic.to_string
                               (User_model.user_to_json user))
                          ~success:true `OK
                    | None ->
                        http_response reqd ~status_code:500 ~title:"Error"
                          ~data:
                            "Something went wrong. Wait a few seconds and try \
                             again."
                          ~success:false `Internal_server_error)
                | Error (`Msg err) ->
                    http_response reqd ~status_code:500 ~title:"Error"
                      ~data:(String.escaped err) ~success:false
                      `Internal_server_error)))

  let verify_email store reqd user =
    let email_verification_uuid = User_model.generate_uuid () in
    let updated_user =
      User_model.update_user user
        ~updated_at:(Ptime.v (P.now_d_ps ()))
        ~email_verification_uuid:(Some email_verification_uuid) ()
    in
    Store.update_user !store updated_user >>= function
    | Ok store' ->
        store := store';
        let verification_link =
          Utils.Email.generate_verification_link email_verification_uuid
        in
        Logs.info (fun m -> m "Verification link is: %s" verification_link);
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Verify_email.verify_page ~user ~icon:"/images/robur.png" ())
             `OK)
    | Error (`Msg err) ->
        http_response reqd ~status_code:500 ~title:"Error"
          ~data:(String.escaped err) ~success:false `Internal_server_error

  let verify_email_token store reqd verification_token (user : User_model.user)
      =
    match
      let users =
        User_model.create_user_session_map (snd !store).Storage.users
      in
      User_model.verify_email_token users verification_token
        (Ptime.v (P.now_d_ps ()))
    with
    | Ok user' ->
        if String.equal user.uuid user'.uuid then
          Store.update_user !store user >>= function
          | Ok store' ->
              store := store';
              Middleware.redirect_to_dashboard reqd ()
          | Error (`Msg msg) ->
              http_response reqd ~status_code:500 ~title:"Error"
                ~data:(String.escaped msg) ~success:false `Internal_server_error
        else
          http_response reqd ~status_code:400 ~title:"Error"
            ~data:"Logged in user is not the to-be-verified one" ~success:false
            `Bad_request
    | Error (`Msg s) -> Middleware.redirect_to_login reqd ~msg:s ()

  let toggle_account_attribute store reqd ~key update_fn error_on_last
      ~error_message =
    decode_request_body reqd >>= fun data ->
    let json =
      try Ok (Yojson.Basic.from_string data)
      with Yojson.Json_error s -> Error (`Msg s)
    in
    match json with
    | Error (`Msg err) ->
        Logs.warn (fun m -> m "Failed to parse JSON: %s - %s" key err);
        http_response reqd ~status_code:400 ~title:"Error" ~data:err
          ~success:false `Bad_request
    | Ok (`Assoc json) -> (
        match Utils.Json.get "uuid" json with
        | Some (`String uuid) -> (
            let users =
              User_model.create_user_uuid_map (snd !store).Storage.users
            in
            match List.assoc_opt uuid users with
            | None ->
                Logs.warn (fun m -> m "%s : Account not found" key);
                http_response reqd ~status_code:404 ~title:"Error"
                  ~data:"Account not found" ~success:false `Not_found
            | Some user -> (
                if error_on_last user then (
                  Logs.warn (fun m ->
                      m "%s : Can't perform action on last user" key);
                  http_response reqd ~status_code:403 ~title:"Error"
                    ~data:error_message ~success:false `Forbidden)
                else
                  let updated_user = update_fn user in
                  Store.update_user !store updated_user >>= function
                  | Ok store' ->
                      store := store';
                      http_response reqd ~status_code:200 ~title:"OK"
                        ~data:"Updated user successfully" ~success:true `OK
                  | Error (`Msg msg) ->
                      Logs.warn (fun m ->
                          m "%s : Storage error with %s" key msg);
                      http_response reqd ~status_code:500 ~title:"Error"
                        ~data:msg ~success:false `Internal_server_error))
        | _ ->
            Logs.warn (fun m ->
                m "%s: Failed to parse JSON - no UUID found" key);
            http_response reqd ~status_code:404 ~title:"Error"
              ~data:"Couldn't find a UUID in the JSON." ~success:false
              `Not_found)
    | Ok _ ->
        http_response reqd ~status_code:400 ~title:"Error"
          ~data:"Provided JSON is not a dictionary" ~success:false `Bad_request

  let toggle_account_activation store reqd _user =
    toggle_account_attribute store reqd ~key:"toggle-active-account"
      (fun user ->
        User_model.update_user user ~active:(not user.active)
          ~updated_at:(Ptime.v (P.now_d_ps ()))
          ())
      (fun user ->
        user.active
        && List.length
             (List.filter
                (fun u -> u.User_model.active)
                (snd !store).Storage.users)
           <= 1)
      ~error_message:"Cannot deactivate last active user"

  let toggle_admin_activation store reqd _user =
    toggle_account_attribute store reqd ~key:"toggle-admin-account"
      (fun user ->
        User_model.update_user user ~super_user:(not user.super_user)
          ~updated_at:(Ptime.v (P.now_d_ps ()))
          ())
      (fun user ->
        user.super_user
        && List.length
             (List.filter
                (fun u -> u.User_model.super_user)
                (snd !store).Storage.users)
           <= 1)
      ~error_message:"Cannot remove last administrator"

  let dashboard albatross reqd (user : User_model.user) =
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
         (Dashboard.dashboard_layout user
            ~content:
              (Unikernel_index.unikernel_index_layout unikernels
                 (Ptime.v (P.now_d_ps ())))
            ~icon:"/images/robur.png" ())
         `OK)

  let users store reqd user =
    Lwt.return
      (reply reqd ~content_type:"text/html"
         (Dashboard.dashboard_layout user ~page_title:"Users | Mollymawk"
            ~content:
              (Users_index.users_index_layout (snd store).Storage.users
                 (Ptime.v (P.now_d_ps ())))
            ~icon:"/images/robur.png" ())
         `OK)

  let settings store reqd user =
    Lwt.return
      (reply reqd ~content_type:"text/html"
         (Dashboard.dashboard_layout user ~page_title:"Settings | Mollymawk"
            ~content:
              (Settings_page.settings_layout (snd store).Storage.configuration)
            ~icon:"/images/robur.png" ())
         `OK)

  let update_settings stack store albatross reqd _user =
    decode_request_body reqd >>= fun data ->
    let json =
      try Ok (Yojson.Basic.from_string data)
      with Yojson.Json_error s -> Error (`Msg s)
    in
    match json with
    | Error (`Msg err) ->
        Logs.warn (fun m -> m "Failed to parse JSON: %s" err);
        http_response reqd ~status_code:400 ~title:"Error"
          ~data:(String.escaped err) ~success:false `Bad_request
    | Ok json -> (
        match
          Configuration.of_json_from_http json (Ptime.v (P.now_d_ps ()))
        with
        | Ok configuration_settings -> (
            Store.update_configuration !store configuration_settings
            >>= function
            | Ok store' ->
                store := store';
                Albatross.init stack configuration_settings.server_ip
                  ~port:configuration_settings.server_port
                  configuration_settings.certificate
                  configuration_settings.private_key
                >>= fun new_albatross ->
                albatross := new_albatross;
                http_response reqd ~status_code:200 ~title:"Success"
                  ~data:"Configuration updated successfully" ~success:true `OK
            | Error (`Msg err) ->
                http_response reqd ~status_code:500 ~title:"Error"
                  ~data:(String.escaped err) ~success:false
                  `Internal_server_error)
        | Error (`Msg err) ->
            http_response reqd ~status_code:500 ~title:"Error"
              ~data:(String.escaped err) ~success:false `Bad_request)

  let deploy_form reqd user =
    Lwt.return
      (reply reqd ~content_type:"text/html"
         (Dashboard.dashboard_layout user
            ~page_title:"Deploy a Unikernel | Mollymawk"
            ~content:Unikernel_create.unikernel_create_layout
            ~icon:"/images/robur.png" ())
         `OK)

  let unikernel_info albatross reqd (user : User_model.user) =
    (* TODO use uuid in the future *)
    Albatross.query albatross ~domain:user.name (`Unikernel_cmd `Unikernel_info)
    >>= function
    | Error msg ->
        http_response reqd ~status_code:500 ~title:"Error"
          ~data:
            (Yojson.Safe.to_string
               (`String ("Error while querying albatross: " ^ msg)))
          ~success:false `Internal_server_error
    | Ok (_hdr, res) -> (
        match Albatross_json.res res with
        | Ok res ->
            http_response reqd ~status_code:200 ~title:"Success"
              ~data:(Yojson.Safe.to_string res)
              ~success:true `OK
        | Error (`String res) ->
            http_response reqd ~status_code:500 ~title:"Error"
              ~data:(Yojson.Safe.to_string (`String res))
              ~success:false `Internal_server_error)

  let unikernel_info_one albatross name reqd (user : User_model.user) =
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
      Lwt.return
        (reply reqd ~content_type:"text/html"
           (Dashboard.dashboard_layout user
              ~content:
                (Unikernel_single.unikernel_single_layout (List.hd unikernels)
                   (Ptime.v (P.now_d_ps ())))
              ~icon:"/images/robur.png" ())
           `OK)
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
           (Dashboard.dashboard_layout user
              ~page_title:"An Error Occured | Mollymawk"
              ~content:(Error_page.error_layout error)
              ~icon:"/images/robur.png" ())
           `Internal_server_error)

  let unikernel_destroy albatross name reqd (user : User_model.user) =
    (* TODO use uuid in the future *)
    Albatross.query albatross ~domain:user.name ~name
      (`Unikernel_cmd `Unikernel_destroy)
    >>= function
    | Error msg ->
        Logs.err (fun m -> m "Error querying albatross: %s" msg);
        http_response reqd ~status_code:500 ~title:"Error"
          ~data:("Error querying albatross: " ^ msg)
          ~success:false `Internal_server_error
    | Ok (_hdr, res) -> (
        match Albatross_json.res res with
        | Ok res ->
            http_response reqd ~status_code:200 ~title:"Success"
              ~data:(Yojson.Safe.to_string res)
              ~success:true `OK
        | Error (`String res) ->
            http_response reqd ~status_code:500 ~title:"Error"
              ~data:(Yojson.Safe.to_string (`String res))
              ~success:false `Internal_server_error)

  let unikernel_create albatross reqd (user : User_model.user) =
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
        http_response reqd ~status_code:400 ~title:"Error"
          ~data:("Couldn't content-type: " ^ msg)
          ~success:false `Bad_request
    | Ok ct -> (
        match Multipart_form.of_string_to_list data ct with
        | Error (`Msg msg) ->
            Logs.warn (fun m -> m "couldn't multipart: %s" msg);
            http_response reqd ~status_code:400 ~title:"Error"
              ~data:("Couldn't multipart: " ^ msg)
              ~success:false `Bad_request
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
                    (* TODO use uuid in the future *)
                    Albatross.query albatross ~domain:user.name ~name
                      (`Unikernel_cmd (`Unikernel_create config))
                    >>= function
                    | Error err ->
                        Logs.warn (fun m ->
                            m "Error querying albatross: %s" err);

                        http_response reqd ~status_code:500 ~title:"Error"
                          ~data:("Error while querying Albatross: " ^ err)
                          ~success:false `Internal_server_error
                    | Ok (_hdr, res) -> (
                        match Albatross_json.res res with
                        | Ok res ->
                            http_response reqd ~status_code:200 ~title:"Success"
                              ~data:(Yojson.Safe.to_string res)
                              ~success:true `OK
                        | Error (`String res) ->
                            http_response reqd ~status_code:500 ~title:"Error"
                              ~data:(Yojson.Safe.to_string (`String res))
                              ~success:false `Internal_server_error))
                | Error (`Msg err) ->
                    Logs.warn (fun m -> m "couldn't decode data %s" err);

                    http_response reqd ~status_code:500 ~title:"Error" ~data:err
                      ~success:false `Internal_server_error)
            | _ ->
                Logs.warn (fun m -> m "couldn't find fields");
                http_response reqd ~status_code:400 ~title:"Error"
                  ~data:"Couldn't find fields" ~success:false `Bad_request))

  let unikernel_console albatross name reqd (user : User_model.user) =
    (* TODO use uuid in the future *)
    Albatross.query ~domain:user.name albatross ~name
      (`Console_cmd (`Console_subscribe (`Count 10)))
    >>= function
    | Error err ->
        Logs.warn (fun m -> m "error querying albatross: %s" err);
        http_response reqd ~status_code:500 ~title:"Error"
          ~data:("Error while querying Albatross:  " ^ err)
          ~success:false `Internal_server_error
    | Ok _ -> (
        match
          Result.bind (Vmm_core.Name.path_of_string user.name) (fun domain ->
              Vmm_core.Name.create domain name)
        with
        | Ok name ->
            let data =
              Option.value ~default:[]
                (Albatross.Map.find_opt name albatross.Albatross.console_output)
            in
            Lwt.return
              (reply reqd ~content_type:"application/json"
                 (Yojson.Basic.to_string (`List data))
                 `OK)
        | Error (`Msg err) ->
            http_response reqd ~status_code:500 ~title:"Error"
              ~data:("Error while querying Albatross:  " ^ err)
              ~success:false `Internal_server_error)

  let view_user albatross store uuid reqd (user : User_model.user) =
    let users = User_model.create_user_uuid_map (snd store).Storage.users in
    match User_model.find_user_by_key uuid users with
    | Some u ->
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
        Lwt.return
          (reply reqd ~content_type:"text/html"
             (Dashboard.dashboard_layout user
                ~page_title:(u.name ^ " | Mollymawk")
                ~content:
                  (User_single.user_single_layout u unikernels
                     (Ptime.v (P.now_d_ps ())))
                ~icon:"/images/robur.png" ())
             `OK)
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

  let request_handler stack albatross js_file css_file imgs store
      (_ipaddr, _port) reqd =
    Lwt.async (fun () ->
        let bad_request () =
          http_response reqd ~status_code:400 ~title:"Error"
            ~data:"Bad HTTP request method." ~success:false `Bad_request
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
                authenticate ~email_verified:false !store reqd
                  (verify_email store reqd))
        | path
          when String.(
                 length path >= 19 && sub path 0 19 = "/auth/verify/token=") ->
            check_meth `GET (fun () ->
                let token = String.sub path 19 (String.length path - 19) in
                authenticate ~email_verified:false !store reqd
                  (verify_email_token store reqd token))
        | "/dashboard" ->
            check_meth `GET (fun () ->
                authenticate !store reqd (dashboard !albatross reqd))
        | "/admin/users" ->
            check_meth `GET (fun () ->
                authenticate ~check_admin:true !store reqd (users !store reqd))
        | path when String.(length path >= 12 && sub path 0 12 = "/admin/user/")
          ->
            check_meth `GET (fun () ->
                let uuid = String.sub path 12 (String.length path - 12) in
                authenticate ~check_admin:true !store reqd
                  (view_user !albatross !store uuid reqd))
        | "/admin/settings" ->
            check_meth `GET (fun () ->
                authenticate ~check_admin:true !store reqd
                  (settings !store reqd))
        | "/api/admin/settings/update" ->
            check_meth `POST (fun () ->
                authenticate ~check_admin:true ~api_meth:true !store reqd
                  (update_settings stack store albatross reqd))
        | "/api/admin/user/activate/toggle" ->
            check_meth `POST (fun () ->
                authenticate ~check_admin:true ~api_meth:true !store reqd
                  (toggle_account_activation store reqd))
        | "/api/admin/user/admin/toggle" ->
            check_meth `POST (fun () ->
                authenticate ~check_admin:true ~api_meth:true !store reqd
                  (toggle_admin_activation store reqd))
        | "/api/unikernels" ->
            check_meth `GET (fun () ->
                authenticate ~api_meth:true !store reqd
                  (unikernel_info !albatross reqd))
        | path
          when String.(length path >= 16 && sub path 0 16 = "/unikernel/info/")
          ->
            check_meth `GET (fun () ->
                let unikernel_name =
                  String.sub path 16 (String.length path - 16)
                in
                authenticate !store reqd
                  (unikernel_info_one !albatross unikernel_name reqd))
        | "/unikernel/deploy" ->
            check_meth `GET (fun () ->
                authenticate !store reqd (deploy_form reqd))
        | path
          when String.(
                 length path >= 19 && sub path 0 19 = "/unikernel/destroy/") ->
            check_meth `GET (fun () ->
                let unikernel_name =
                  String.sub path 19 (String.length path - 19)
                in
                authenticate !store reqd
                  (unikernel_destroy !albatross unikernel_name reqd))
        | path
          when String.(
                 length path >= 19 && sub path 0 19 = "/unikernel/console/") ->
            check_meth `GET (fun () ->
                let unikernel_name =
                  String.sub path 19 (String.length path - 19)
                in
                authenticate !store reqd
                  (unikernel_console !albatross unikernel_name reqd))
        | "/unikernel/create" ->
            check_meth `POST (fun () ->
                authenticate !store reqd (unikernel_create !albatross reqd))
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
    Store.Stored_data.connect storage >>= fun stored_data ->
    Store.read_data stored_data >>= function
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
        let request_handler _flow =
          request_handler stack albatross js_file css_file imgs store
        in
        Paf.init ~port:8080 (S.tcp stack) >>= fun service ->
        let http = Paf.http_service ~error_handler request_handler in
        let (`Initialized th) = Paf.serve http service in
        th
end
