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

let apply_middleware middlewares handler =
  List.fold_right (fun middleware acc -> middleware acc) middlewares handler

let redirect_to_login reqd ?(msg = "") () =
  let header_list =
    [
      ( "Set-Cookie",
        "molly_session=;Path=/;HttpOnly=true;Expires=2023-10-27T11:00:00.778Z"
      );
      ("location", "/sign-in");
    ]
  in
  let headers = Httpaf.Headers.of_list header_list in
  let response = Httpaf.Response.create ~headers `Found in
  Httpaf.Reqd.respond_with_string reqd response msg;
  Lwt.return_unit

let redirect_to_register reqd ?(msg = "") () =
  let header_list =
    [
      ( "Set-Cookie",
        "molly_session=;Path=/;HttpOnly=true;Expires=2023-10-27T11:00:00.778Z"
      );
      ("location", "/sign-up");
    ]
  in
  let headers = Httpaf.Headers.of_list header_list in
  let response = Httpaf.Response.create ~headers `Found in
  Httpaf.Reqd.respond_with_string reqd response msg;
  Lwt.return_unit

let redirect_to_error ~title ~data status user code api_meth reqd () =
  let error = { Utils.Status.code; title; success = false; data } in
  let data =
    if api_meth then Utils.Status.to_json error
    else
      Dashboard.dashboard_layout user ~page_title:(title ^ " | Mollymawk")
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
  let headers = Httpaf.Headers.of_list [ ("location", "/verify-email") ] in
  let response = Httpaf.Response.create ~headers `Found in
  Httpaf.Reqd.respond_with_string reqd response msg;
  Lwt.return_unit

let redirect_to_dashboard reqd ?(msg = "") () =
  let headers = Httpaf.Headers.of_list [ ("location", "/dashboard") ] in
  let response = Httpaf.Response.create ~headers `Found in
  Httpaf.Reqd.respond_with_string reqd response msg;
  Lwt.return_unit

let cookie_value_from_auth_cookie cookie =
  match String.split_on_char '=' (String.trim cookie) with
  | _ :: s :: _ -> Ok (String.trim s)
  | _ -> Error (`Msg "Bad cookie")

let user_from_auth_cookie cookie users =
  match cookie_value_from_auth_cookie cookie with
  | Ok cookie_value -> (
      match User_model.find_user_by_key cookie_value users with
      | Some user -> Ok user
      | None -> Error (`Msg "User not found"))
  | Error (`Msg s) ->
      Logs.err (fun m -> m "Error: %s" s);
      Error (`Msg s)

let user_of_cookie users now reqd =
  match has_session_cookie reqd with
  | Some auth_cookie -> (
      match user_from_auth_cookie auth_cookie users with
      | Ok user -> (
          match User_model.user_auth_cookie_from_user user with
          | Some cookie -> (
              match User_model.is_valid_cookie cookie now with
              | true -> Ok user
              | false ->
                  Logs.err (fun m ->
                      m
                        "auth-middleware: Session value doesn't match user \
                         session %s"
                        auth_cookie);
                  Error (`Msg "User not found"))
          | None ->
              Logs.err (fun m ->
                  m "auth-middleware: User doesn't have a session cookie.");
              Error (`Msg "User not found"))
      | Error (`Msg s) ->
          Logs.err (fun m ->
              m "auth-middleware: Failed to find user with key %s: %s"
                auth_cookie s);
          Error (`Msg "User not found"))
  | None ->
      Logs.err (fun m ->
          m "auth-middleware: No molly-session in cookie header.");
      Error (`Msg "User not found")

let auth_middleware now users handler reqd =
  match user_of_cookie users now reqd with
  | Ok user ->
      if user.User_model.active then handler reqd
      else redirect_to_login ~msg:"User account is deactivated." reqd ()
  | Error (`Msg msg) -> redirect_to_login ~msg reqd ()

let email_verified_middleware now users handler reqd =
  match user_of_cookie users now reqd with
  | Ok user ->
      if User_model.is_email_verified user then handler reqd
      else redirect_to_verify_email reqd ()
  | Error (`Msg msg) -> redirect_to_login ~msg reqd ()

let is_user_admin_middleware api_meth now users handler reqd =
  match user_of_cookie users now reqd with
  | Ok user ->
      if user.User_model.super_user && user.active then handler reqd
      else
        redirect_to_error ~title:"Unauthorized"
          ~data:
            "You don't have the necessary permissions to access this service."
          `Unauthorized user 401 api_meth reqd ()
  | Error (`Msg msg) -> redirect_to_login ~msg reqd ()
