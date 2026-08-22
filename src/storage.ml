open Utils.Json

let current_version = 10
(* version history:
   [1 - 8] deprecated.
   9 email configuration is now stored
   10 we now have scaling policies for unikernels, default is no policy for existing unikernels
*)

type t = User_model.user list * Configuration.t list * Utils.Email.t option

let t_to_json ?(version = current_version) users configurations email =
  `Assoc
    [
      ("version", `Int version);
      ("users", `List (List.map User_model.user_to_json users));
      ("configuration", Configuration.to_json configurations);
      ("email", Utils.Email.to_json email);
    ]

let t_of_json json =
  match json with
  | `Assoc xs -> (
      let ( let* ) = Result.bind in
      match
        ( get "version" xs,
          get "users" xs,
          get "configuration" xs,
          get "email" xs )
      with
      | Some (`Int v), Some (`List users), Some configuration, email ->
          let* () =
            if v = current_version then Ok ()
            else if v = 9 then Ok ()
            else
              Error
                (`Msg
                   (Fmt.str
                      "expected version %u, found version %u. note: version [1 \
                       - 8] is now deprecated."
                      current_version v))
          in
          let* users =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* user =
                  if v = 9 then User_model.(user_v9_of_json cookie_of_json) js
                  else User_model.(user_of_json cookie_of_json) js
                in
                Ok (user :: acc))
              (Ok []) users
          in
          let* configurations = Configuration.of_json configuration in
          let* email =
            match email with
            | None -> Ok None
            | Some e -> (
                match Utils.Email.of_json e with
                | Ok email -> Ok (Some email)
                | Error _msg -> Ok None)
          in
          Ok (users, configurations, email)
      | _ -> Error (`Msg "invalid data: no version and users field"))
  | _ -> Error (`Msg "invalid data: not an assoc")

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let find_by_email users email =
  List.find_opt
    (fun user -> Mrmime.Mailbox.equal user.User_model.email email)
    users

let find_by_name users name =
  List.find_opt
    (fun user -> Vmm_core.Name.Label.compare user.User_model.name name = 0)
    users

let find_by_uuid users uuid =
  List.find_opt (fun user -> String.equal user.User_model.uuid uuid) users

let find_by_cookie users cookie_value =
  List.fold_left
    (fun acc user ->
      match acc with
      | Some _ as s -> s
      | None -> (
          match
            List.find_opt
              (fun (cookie : User_model.cookie) ->
                String.equal User_model.session_cookie cookie.User_model.name
                && String.equal cookie_value cookie.value)
              user.User_model.cookies
          with
          | None -> None
          | Some c -> Some (user, c)))
    None users

let find_by_api_token users token =
  List.find_map
    (fun (user : User_model.user) ->
      match
        List.find_opt
          (fun (token_ : User_model.token) -> String.equal token token_.value)
          user.tokens
      with
      | Some token_ -> Some (user, token_)
      | None -> None)
    users

let increment_token_usage (token : User_model.token) (user : User_model.user) =
  let token = { token with usage_count = token.usage_count + 1 } in
  let tokens =
    List.map
      (fun (token' : User_model.token) ->
        if String.equal token.value token'.value then token else token')
      user.tokens
  in
  User_model.update_user user ~tokens ()
(* update_user store updated_user >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error (`Msg err) ->
      Logs.err (fun m -> m "Error with storage: %s" err);
      Lwt.return (Error (`Msg err)) *)

let update_cookie_usage (cookie : User_model.cookie) user_agent
    (user : User_model.user) =
  let cookie = { cookie with user_agent } in
  let cookies =
    List.map
      (fun (cookie' : User_model.cookie) ->
        if String.equal cookie.value cookie'.value then cookie else cookie')
      user.cookies
  in
  User_model.update_user user ~cookies ()
(* update_user store updated_user >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error (`Msg err) ->
      Logs.err (fun m -> m "Error with storage: %s" err);
      Lwt.return (Error (`Msg err)) *)

let update_user_unikernel_updates (new_update : User_model.unikernel_update)
    (user : User_model.user) =
  let is_unique (u : User_model.unikernel_update) =
    not (Vmm_core.Name.Label.equal u.name new_update.name)
  in
  let updated_list =
    new_update :: List.filter is_unique user.unikernel_updates
  in
  User_model.update_user user ~unikernel_updates:updated_list ()

(* update_user store updated_user >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error (`Msg err) ->
      Logs.err (fun m -> m "Error with storage: %s" err);
      Lwt.return (Error (`Msg err)) *)

let count_users users = List.length users

let find_email_verification_token users uuid =
  List.find_opt
    (fun user ->
      Option.fold ~none:false
        ~some:(fun uu -> Uuidm.equal uu uuid)
        user.User_model.email_verification_uuid)
    users

let count_active users =
  List.length (List.filter (fun u -> u.User_model.active) users)

let count_superusers users =
  List.length (List.filter (fun u -> u.User_model.super_user) users)
