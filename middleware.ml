type handler = Httpaf.Reqd.t -> unit Lwt.t
type middleware = handler -> handler

let header header_name reqd =
  let headers = (Httpaf.Reqd.request reqd).headers in
  Httpaf.Headers.get headers header_name

let user_agent reqd = header "User-Agent" reqd

let generate_csrf_cookie now reqd =
  User_model.generate_cookie ~name:User_model.csrf_cookie
    ~user_agent:(user_agent reqd)
    ~uuid:(Uuidm.to_string (User_model.generate_uuid ()))
    ~created_at:now ~expires_in:3600 ()

let cookie cookie_name (reqd : Httpaf.Reqd.t) =
  match header "Cookie" reqd with
  | Some cookies ->
      let cookie_list = String.split_on_char ';' cookies in
      List.find_opt
        (fun cookie ->
          let parts = cookie |> String.split_on_char '=' in
          match parts with
          | [ name; _ ] -> String.equal (String.trim name) cookie_name
          | _ -> false)
        cookie_list
  | _ -> None

let apply_middleware middlewares handler =
  List.fold_right (fun middleware acc -> middleware acc) middlewares handler

let redirect_to_page ~path ?(clear_session = false) ?(with_error = false) reqd
    ?(msg = "") () =
  let msg_cookie =
    if with_error then "flash_msg=error: " ^ Uri.pct_encode msg ^ ";"
    else "flash_msg=" ^ Uri.pct_encode msg ^ ";"
  in
  let header_list =
    let session_header =
      if clear_session then
        [
          ( "Set-Cookie",
            User_model.session_cookie
            ^ "=; Path=/; HttpOnly=true; Expires=2023-10-27T11:00:00.778Z" );
        ]
      else []
    in
    session_header
    @ [
        ("Set-Cookie", msg_cookie);
        ("location", path);
        ("Content-Length", string_of_int (String.length msg));
      ]
  in
  let headers = Httpaf.Headers.of_list header_list in
  let response = Httpaf.Response.create ~headers `Found in
  Httpaf.Reqd.respond_with_string reqd response msg;
  Lwt.return_unit

let redirect_to_error ~title ~data status code api_meth reqd () =
  let error = { Utils.Status.code; title; success = false; data } in
  let data =
    if api_meth then Utils.Status.to_json error
    else
      Guest_layout.guest_layout ~page_title:(title ^ " | Mollymawk")
        ~content:(Error_page.error_layout error)
        ~icon:"/images/robur.png" ()
  in
  Lwt.return
    (let headers =
       Httpaf.Headers.of_list
         [
           ("content-length", string_of_int (String.length data));
           ("content-type", if api_meth then "application/json" else "text/html");
         ]
     in
     let resp = Httpaf.Response.create ~headers status in
     Httpaf.Reqd.respond_with_string reqd resp data)

let redirect_to_verify_email reqd ?(msg = "") () =
  let headers =
    Httpaf.Headers.of_list
      [
        ("location", "/verify-email");
        ("Content-Length", string_of_int (String.length msg));
      ]
  in
  let response = Httpaf.Response.create ~headers `Found in
  Httpaf.Reqd.respond_with_string reqd response msg;
  Lwt.return_unit

let redirect_to_dashboard reqd ?(msg = "") () =
  let headers =
    Httpaf.Headers.of_list
      [
        ("location", "/dashboard");
        ("Content-Length", string_of_int (String.length msg));
      ]
  in
  let response = Httpaf.Response.create ~headers `Found in
  Httpaf.Reqd.respond_with_string reqd response msg;
  Lwt.return_unit

let http_response ~title ?(header_list = []) ?(data = "") reqd http_status =
  let code = Httpaf.Status.to_code http_status
  and success = Httpaf.Status.is_successful http_status in
  let status = { Utils.Status.code; title; data; success } in
  let data = Utils.Status.to_json status in
  let headers =
    Httpaf.Headers.(
      add_list
        (of_list
           [
             ("Content-Type", "application/json");
             ("Content-length", string_of_int (String.length data));
           ])
        header_list)
  in
  let response = Httpaf.Response.create ~headers http_status in
  Httpaf.Reqd.respond_with_string reqd response data;
  Lwt.return_unit

let cookie_value cookie =
  match String.split_on_char '=' cookie with
  | _ :: s :: _ -> Ok s
  | _ -> Error (`Msg "Bad cookie")

let session_cookie_value reqd =
  match cookie User_model.session_cookie reqd with
  | Some auth_cookie -> (
      match cookie_value auth_cookie with
      | Ok cookie_value -> Ok cookie_value
      | Error (`Msg s) ->
          Logs.err (fun m -> m "Error: %s" s);
          Error (`Msg s))
  | None ->
      Logs.err (fun m ->
          m "auth-middleware: No molly-session in cookie header.");
      Error (`Msg "User not found")

let auth_middleware user handler reqd =
  if user.User_model.active then handler reqd
  else
    redirect_to_page ~path:"/sign-in" ~clear_session:true ~with_error:true
      ~msg:"User account is deactivated." reqd ()

let email_verified_middleware user handler reqd =
  if User_model.is_email_verified user then handler reqd
  else redirect_to_verify_email reqd ()

let is_user_admin_middleware api_meth user handler reqd =
  if user.User_model.super_user && user.active then handler reqd
  else
    redirect_to_error ~title:"Unauthorized"
      ~data:"You don't have the necessary permissions to access this service."
      `Unauthorized 401 api_meth reqd ()

let csrf_cookie_verification form_csrf reqd =
  match cookie User_model.csrf_cookie reqd with
  | Some cookie -> (
      match cookie_value cookie with
      | Ok token -> String.equal form_csrf token
      | Error (`Msg err) ->
          Logs.err (fun m -> m "Error retrieving csrf value from cookie %s" err);
          false)
  | None ->
      Logs.err (fun m -> m "Couldn't find csrf cookie.");
      false

let csrf_verification user now form_csrf handler reqd =
  match User_model.user_csrf_token user form_csrf with
  | Some csrf_token ->
      if User_model.is_valid_cookie csrf_token now then handler reqd
      else
        http_response ~title:"CSRF Token Mismatch"
          ~data:"Invalid CSRF token error. Please refresh and try again." reqd
          `Bad_request
  | None ->
      http_response ~data:"Missing CSRF token. Please refresh and try again."
        ~title:"Missing CSRF Token" reqd `Bad_request

let api_authentication reqd =
  match header "Authorization" reqd with
  | Some auth when String.starts_with ~prefix:"Bearer " auth ->
      let token = String.sub auth 7 (String.length auth - 7) in
      Some token
  | _ -> None
