type handler = Httpaf.Reqd.t -> unit Lwt.t
type middleware = handler -> handler

let has_session_cookie (reqd : Httpaf.Reqd.t) : bool =
  let headers = (Httpaf.Reqd.request reqd).headers in
  match Httpaf.Headers.get headers "Cookie" with
  | Some cookies ->
      let cookie_list = String.split_on_char ';' cookies in
      List.exists
        (fun cookie ->
          let parts = String.trim cookie |> String.split_on_char '=' in
          match parts with
          | [ name; _ ] -> String.equal name "molly_session"
          | _ -> false)
        cookie_list
  | _ -> false

let apply_middleware ~now middlewares handler =
  List.fold_right
    (fun middleware acc -> middleware ~now acc)
    middlewares handler

let redirect_to_login reqd ?(msg = "") () =
  let headers = Httpaf.Headers.of_list [ ("location", "/sign-in") ] in
  let response = Httpaf.Response.create ~headers `Found in
  Httpaf.Reqd.respond_with_string reqd response msg;
  Lwt.return_unit

let redirect_to_dashboard reqd ?(msg = "") () =
  let headers = Httpaf.Headers.of_list [ ("location", "/dashboard") ] in
  let response = Httpaf.Response.create ~headers `Found in
  Httpaf.Reqd.respond_with_string reqd response msg;
  Lwt.return_unit

let auth_middleware ~now ~users handler reqd =
  let headers = (Httpaf.Reqd.request reqd).headers in
  match Httpaf.Headers.get headers "Cookie" with
  | Some cookies -> (
      let cookie_list = String.split_on_char ';' cookies in
      let session_cookie =
        List.find_opt
          (fun cookie ->
            let parts = String.trim cookie |> String.split_on_char '=' in
            match parts with
            | [ name; _ ] -> String.equal name "molly_session"
            | _ -> false)
          cookie_list
      in
      match session_cookie with
      | Some cookie -> (
          let parts = String.trim cookie |> String.split_on_char '=' in
          let value = List.nth parts 1 in
          match User_model.find_user_by_key value users with
          | Some user -> (
              let user_session =
                List.find_opt
                  (fun (cookie : User_model.cookie) ->
                    String.equal cookie.name "molly_session")
                  user.cookies
              in
              match user_session with
              | Some cookie -> (
                  match
                    String.equal cookie.value value
                    && User_model.is_valid_cookie ~cookie ~now
                  with
                  | true -> handler reqd
                  | false ->
                      Logs.err (fun m ->
                          m
                            "auth-middleware: Session value doesn't match user \
                             session %s\n"
                            value);
                      redirect_to_login reqd ())
              | None ->
                  Logs.err (fun m ->
                      m "auth-middleware: User doesn't have a session cookie.\n");
                  redirect_to_login reqd ())
          | None ->
              Logs.err (fun m ->
                  m "auth-middleware: Failed to find user with key %s\n" value);
              redirect_to_login reqd ())
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
