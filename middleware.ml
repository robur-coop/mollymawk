type handler = H1.Reqd.t -> unit Lwt.t
type middleware = handler -> handler

let header header_name reqd =
  let headers = (H1.Reqd.request reqd).headers in
  H1.Headers.get headers header_name

let user_agent reqd = header "User-Agent" reqd

let generate_csrf_cookie now reqd =
  User_model.generate_cookie ~name:User_model.csrf_cookie
    ~user_agent:(user_agent reqd)
    ~uuid:(Uuidm.to_string (User_model.generate_uuid ()))
    ~created_at:now ~expires_in:3600 ()

let cookie cookie_name (reqd : H1.Reqd.t) =
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

let redirect_to_page ~path ?(clear_session = false) ?(with_error = false) reqd
    ?(msg = "") () =
  let msg_cookie =
    if with_error then "flash_msg=error: " ^ Uri.pct_encode msg ^ ";Path=/;"
    else "flash_msg=" ^ Uri.pct_encode msg ^ ";Path=/;"
  in
  let header_list =
    let session_header =
      if clear_session then
        [
          ("Set-Cookie", User_model.session_cookie ^ "=; Path=/; HttpOnly=true;");
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
  let headers = H1.Headers.of_list header_list in
  let response = H1.Response.create ~headers `Found in
  H1.Reqd.respond_with_string reqd response msg;
  Lwt.return_unit

let redirect_to_error ~title ~data ~api_meth status reqd () =
  let code = H1.Status.to_code status
  and success = H1.Status.is_successful status in
  let error = { Utils.Status.code; title; data; success } in
  let data =
    if api_meth then Utils.Status.to_json error
    else
      Guest_layout.guest_layout ~page_title:(title ^ " | Mollymawk")
        ~content:(Error_page.error_layout error)
        ~icon:"/images/robur.png" ()
  in
  Lwt.return
    (let headers =
       H1.Headers.of_list
         [
           ("content-length", string_of_int (String.length data));
           ("content-type", if api_meth then "application/json" else "text/html");
         ]
     in
     let resp = H1.Response.create ~headers status in
     H1.Reqd.respond_with_string reqd resp data)

let redirect_to_verify_email reqd ?(msg = "") () =
  let headers =
    H1.Headers.of_list
      [
        ("location", "/verify-email");
        ("Content-Length", string_of_int (String.length msg));
      ]
  in
  let response = H1.Response.create ~headers `Found in
  H1.Reqd.respond_with_string reqd response msg;
  Lwt.return_unit

let redirect_to_url ~url reqd ?(msg = "") () =
  let headers =
    H1.Headers.of_list
      [
        ("location", url); ("Content-Length", string_of_int (String.length msg));
      ]
  in
  let response = H1.Response.create ~headers `Found in
  H1.Reqd.respond_with_string reqd response msg;
  Lwt.return_unit

let redirect_to_instance_selector callback_link reqd ?(msg = "") () =
  redirect_to_url
    ~url:("/select/instance?callback=" ^ Uri.pct_encode callback_link)
    reqd ~msg ()

let redirect_to_dashboard reqd ?(msg = "") () =
  let headers =
    H1.Headers.of_list
      [
        ("location", "/dashboard");
        ("Content-Length", string_of_int (String.length msg));
      ]
  in
  let response = H1.Response.create ~headers `Found in
  H1.Reqd.respond_with_string reqd response msg;
  Lwt.return_unit

let http_response ~title ?(header_list = []) ?(data = `String "") reqd
    http_status =
  let code = H1.Status.to_code http_status
  and success = H1.Status.is_successful http_status in
  let status = { Utils.Status.code; title; data; success } in
  let data = Utils.Status.to_json status in
  let headers =
    H1.Headers.(
      add_list
        (of_list
           [
             ("Content-Type", "application/json");
             ("Content-length", string_of_int (String.length data));
           ])
        header_list)
  in
  let response = H1.Response.create ~headers http_status in
  H1.Reqd.respond_with_string reqd response data;
  Lwt.return_unit

let http_event_source_response
    ?(header_list = [ ("Content-Type", "text/event-stream") ]) reqd http_status
    =
  let headers = H1.Headers.(of_list header_list) in
  let response = H1.Response.create ~headers http_status in
  let writer = H1.Reqd.respond_with_streaming reqd response in
  let response data =
    if H1.Body.Writer.is_closed writer then Error ()
    else (
      H1.Body.Writer.write_string writer ("data:" ^ data ^ "\n\n");
      H1.Body.Writer.flush writer Fun.id;
      Ok ())
  in
  response

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

let email_verified_middleware user handler reqd =
  if User_model.is_email_verified user then handler reqd
  else redirect_to_verify_email reqd ()

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

let csrf_verification user now form_csrf handler reqd albatross_instances =
  match User_model.user_csrf_token user form_csrf with
  | Some csrf_token ->
      if User_model.is_valid_cookie csrf_token now then
        handler reqd albatross_instances
      else
        http_response ~title:"CSRF Token Mismatch"
          ~data:
            (`String "Invalid CSRF token error. Please refresh and try again.")
          reqd `Bad_request
  | None ->
      http_response
        ~data:(`String "Missing CSRF token. Please refresh and try again.")
        ~title:"Missing CSRF Token" reqd `Bad_request

let api_authentication reqd =
  match header "Authorization" reqd with
  | Some auth when String.starts_with ~prefix:"Bearer " auth ->
      let token = String.sub auth 7 (String.length auth - 7) in
      Some token
  | _ -> None
