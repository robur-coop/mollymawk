type handler = Httpaf.Reqd.t -> unit Lwt.t
type middleware = handler -> handler

let has_session_cookie (reqd : Httpaf.Reqd.t) =
  let headers = (Httpaf.Reqd.request reqd).headers in
  match Httpaf.Headers.get headers "Cookie" with
  | Some cookies ->
      let cookie_list = String.split_on_char ';' cookies in
      List.find_opt
        (fun cookie ->
          let parts = String.trim cookie |> String.split_on_char '=' in
          match parts with
          | [ name; _ ] -> String.equal name "molly_session"
          | _ -> false)
        cookie_list
  | _ -> None

let apply_middleware ~now middlewares handler =
  List.fold_right
    (fun middleware acc -> middleware ~now acc)
    middlewares handler

let get_expired_cookie ~now =
  let current_day = Ptime.to_date_time now in
  let (y, m, d), t = current_day in
  let day_before = Option.get (Ptime.of_date_time ((y, m, d - 1), t)) in
  let day_string = Utils.TimeHelper.string_of_ptime day_before in
  let day_before_name =
    match Ptime.weekday day_before with
    | `Mon -> "Mon"
    | `Tue -> "Tue"
    | `Wed -> "Wed"
    | `Thu -> "Thu"
    | `Fri -> "Fri"
    | `Sat -> "Sat"
    | `Sun -> "Sun"
  in
  "molly_session=" ^ ";Path=/;HttpOnly=true;Expires=" ^ day_before_name ^ ", "
  ^ day_string ^ " UTC"

let redirect_to_login ~now reqd ?(msg = "") () =
  let header_list =
    [ ("Set-Cookie", get_expired_cookie ~now); ("location", "/sign-in") ]
  in
  let headers = Httpaf.Headers.of_list header_list in
  let response = Httpaf.Response.create ~headers `Found in
  Httpaf.Reqd.respond_with_string reqd response msg;
  Lwt.return_unit

let redirect_to_register ~now reqd ?(msg = "") () =
  let header_list =
    [ ("Set-Cookie", get_expired_cookie ~now); ("location", "/sign-up") ]
  in
  let headers = Httpaf.Headers.of_list header_list in
  let response = Httpaf.Response.create ~headers `Found in
  Httpaf.Reqd.respond_with_string reqd response msg;
  Lwt.return_unit

let redirect_to_verify_email reqd ?(msg = "") () =
  let headers = Httpaf.Headers.of_list [ ("location", "/verify-email") ] in
  let response = Httpaf.Response.create ~headers `Found in
  Httpaf.Reqd.respond_with_string reqd response msg;
  Lwt.return_unit

let redirect_to_dashboard reqd ?(msg = "") () =
  let headers = Httpaf.Headers.of_list [ ("location", "/dashboard") ] in
  let response = Httpaf.Response.create ~headers `Found in
  Httpaf.Reqd.respond_with_string reqd response msg;
  Lwt.return_unit

let cookie_value_from_auth_cookie ~cookie =
  let parts = String.trim cookie |> String.split_on_char '=' in
  List.nth parts 1

let user_from_auth_cookie ~cookie ~users =
  let cookie = cookie_value_from_auth_cookie ~cookie in
  User_model.find_user_by_key cookie users

let auth_middleware ~now ~users handler reqd =
  match has_session_cookie reqd with
  | Some auth_cookie -> (
      match user_from_auth_cookie ~cookie:auth_cookie ~users with
      | Some user -> (
          match User_model.user_auth_cookie_from_user ~user with
          | Some cookie -> (
              match
                String.equal cookie.value
                  (cookie_value_from_auth_cookie ~cookie:auth_cookie)
                && User_model.is_valid_cookie ~cookie ~now
              with
              | true -> handler reqd
              | false ->
                  Logs.err (fun m ->
                      m
                        "auth-middleware: Session value doesn't match user \
                         session %s\n"
                        auth_cookie);
                  redirect_to_login ~now reqd ())
          | None ->
              Logs.err (fun m ->
                  m "auth-middleware: User doesn't have a session cookie.\n");
              redirect_to_login ~now reqd ())
      | None ->
          Logs.err (fun m ->
              m "auth-middleware: Failed to find user with key %s\n" auth_cookie);
          redirect_to_register ~now reqd ())
  | None ->
      Logs.err (fun m ->
          m "auth-middleware: No molly-session in cookie header.");
      redirect_to_login ~now reqd ()

let email_verified_middleware ~now ~users handler reqd =
  match has_session_cookie reqd with
  | Some cookie -> (
      match user_from_auth_cookie ~cookie ~users with
      | Some user -> (
          match User_model.user_auth_cookie_from_user ~user with
          | Some cookie -> (
              match
                User_model.(
                  is_valid_cookie ~cookie ~now && is_email_verified ~user)
              with
              | true -> handler reqd
              | false -> redirect_to_verify_email reqd ())
          | None -> redirect_to_login ~now reqd ())
      | None -> redirect_to_register ~now reqd ())
  | None -> redirect_to_login ~now reqd ()
