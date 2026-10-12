type handler = H1.Reqd.t -> unit Lwt.t
type middleware = handler -> handler

let header header_name reqd =
  let headers = (H1.Reqd.request reqd).headers in
  H1.Headers.get headers header_name

let user_agent reqd = header "User-Agent" reqd
let csrf_expiry = 3600 (* 1 hour *)
let session_cookie = "molly_session"
let csrf_field = "molly_csrf"

let generate_anonymous_csrf_token () =
  Uuidm.to_string (User_model.generate_uuid ())

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

let http_response ?title ~data ?(api_meth = true) ?(header_list = []) reqd
    http_status =
  let title = Option.value title ~default:(H1.Status.to_string http_status) in
  let code = H1.Status.to_code http_status
  and success = H1.Status.is_successful http_status in
  let status = { Utils.Status.code; title; data; success } in
  let data =
    if api_meth then Utils.Status.to_json status
    else if success then Utils.Json.to_string data
    else
      Guest_layout.guest_layout ~page_title:title
        ~content:(Error_page.error_layout status)
        ~icon:"/images/robur.png" ()
  in
  let headers =
    H1.Headers.(
      of_list
        ([
           ("content-length", string_of_int (String.length data));
           ("content-type", if api_meth then "application/json" else "text/html");
           ("x-frame-options", "DENY");
           ("content-security-policy", "frame-ancestors 'none'");
         ]
        @ header_list))
  in
  let resp = H1.Response.create ~headers http_status in
  H1.Reqd.respond_with_string reqd resp data;
  Lwt.return_unit

let redirect_to_page ~path ?(clear_session = false) ?(with_error = false) reqd
    ?(msg = "") () =
  let msg_cookie =
    if with_error then "flash_msg=error: " ^ Uri.pct_encode msg ^ ";Path=/;"
    else "flash_msg=" ^ Uri.pct_encode msg ^ ";Path=/;"
  in
  let header_list =
    let session_header =
      if clear_session then
        [ ("Set-Cookie", session_cookie ^ "=; Path=/; HttpOnly=true;") ]
      else []
    in
    session_header @ [ ("Set-Cookie", msg_cookie); ("location", path) ]
  in
  http_response ~api_meth:false ~title:"Redirecting" ~data:(`String "")
    ~header_list reqd `Found

let construct_instance_redirect_url callback instance_name =
  let separator = if String.contains callback '?' then "&" else "?" in
  Fmt.str "%s%sinstance=%s" callback separator
    (Configuration.name_to_str instance_name)

let redirect_to_instance_selector callback_link reqd ?(msg = "") () =
  redirect_to_page
    ~path:("/select/instance?callback=" ^ Uri.pct_encode callback_link)
    reqd ~msg ()

let http_event_source_response
    ?(header_list =
      [
        ("Content-Type", "text/event-stream");
        ("x-frame-options", "DENY");
        ("content-security-policy", "frame-ancestors 'none'");
      ]) reqd http_status =
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
  match cookie session_cookie reqd with
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
  else redirect_to_page ~path:"/verify/email" reqd ()

let csrf_cookie_verification form_csrf reqd =
  match cookie csrf_field reqd with
  | Some cookie -> (
      match cookie_value cookie with
      | Ok token -> String.equal form_csrf token
      | Error (`Msg err) ->
          Logs.err (fun m -> m "Error retrieving csrf value from cookie %s" err);
          false)
  | None ->
      Logs.err (fun m -> m "Couldn't find csrf cookie.");
      false

let api_authentication reqd =
  match header "Authorization" reqd with
  | Some auth when String.starts_with ~prefix:"Bearer " auth ->
      let token = String.sub auth 7 (String.length auth - 7) in
      Some token
  | _ -> None

type t = {
  mutable sessions : (Uuidm.t * User_model.cookie) Utils.SM.t;
  mutable by_csrf : (Uuidm.t * Ptime.t) Utils.SM.t;
}

let state = { sessions = Utils.SM.empty; by_csrf = Utils.SM.empty }

let add_session user_uuid (cookie : User_model.cookie) =
  state.sessions <- Utils.SM.add cookie.value (user_uuid, cookie) state.sessions

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

let register_csrf token user_uuid created_at =
  state.by_csrf <- Utils.SM.add token (user_uuid, created_at) state.by_csrf

let unregister_csrf token = state.by_csrf <- Utils.SM.remove token state.by_csrf
let find_csrf token = Utils.SM.find_opt token state.by_csrf

let prune_expired_csrf now =
  state.by_csrf <-
    Utils.SM.filter
      (fun _ (_, created_at) ->
        Utils.TimeHelper.diff_in_seconds ~current_time:now
          ~check_time:created_at
        < csrf_expiry)
      state.by_csrf

let generate_csrf_token user_uuid now =
  prune_expired_csrf now;
  let token = Uuidm.to_string (User_model.generate_uuid ()) in
  register_csrf token user_uuid now;
  token

let verify_and_consume_csrf (user : User_model.user) token now =
  match Utils.SM.find_opt token state.by_csrf with
  | Some (uuid, created_at) when Uuidm.equal uuid user.uuid ->
      unregister_csrf token;
      if
        Utils.TimeHelper.diff_in_seconds ~current_time:now
          ~check_time:created_at
        < csrf_expiry
      then Ok ()
      else Error `Expired
  | Some _ -> Error `Mismatch
  | None -> Error `Missing

let csrf_verification user now form_csrf handler reqd =
  match verify_and_consume_csrf user form_csrf now with
  | Ok () -> handler reqd
  | Error `Expired ->
      http_response ~title:"CSRF Token Mismatch"
        ~data:
          (`String "Invalid CSRF token error. Please refresh and try again.")
        reqd `Bad_request
  | Error (`Missing | `Mismatch) ->
      http_response
        ~data:(`String "Missing CSRF token. Please refresh and try again.")
        ~title:"Missing CSRF Token" reqd `Bad_request

let reset_state () =
  state.sessions <- Utils.SM.empty;
  state.by_csrf <- Utils.SM.empty
