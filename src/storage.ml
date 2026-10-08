open Utils.Json

let current_version = 10
(* version history:
   [1 - 8] deprecated.
   9 email configuration is now stored
   10 we now have scaling policies for unikernels, default is no policy for existing unikernels
*)

type t = {
  (* these fields are persisted to disk *)
  mutable users : User_model.user Utils.SM.t;
  mutable configurations : Configuration.t Utils.LM.t;
  mutable email : Utils.Email.t option;
  (* these fields below are not persisted to disk*)
  mutable by_name : string Utils.LM.t;
  mutable by_email : string Utils.SM.t;
  mutable by_cookie : string Utils.SM.t;
  mutable by_token : string Utils.SM.t;
  mutable by_verification_token : string Utils.SM.t;
}

let configurations { configurations; _ } = configurations
let email { email; _ } = email
let users { users; _ } = users

let email_to_key (email : Mrmime.Mailbox.t) =
  String.lowercase_ascii (Emile.to_string email)

let register_user_indexes t (u : User_model.user) =
  t.by_name <- Utils.LM.add u.name u.uuid t.by_name;
  t.by_email <- Utils.SM.add (email_to_key u.email) u.uuid t.by_email;
  t.by_cookie <-
    Utils.SM.fold
      (fun _ (c : User_model.cookie) acc -> Utils.SM.add c.value u.uuid acc)
      u.cookies t.by_cookie;
  t.by_token <-
    Utils.SM.fold
      (fun _ (tok : User_model.token) acc -> Utils.SM.add tok.value u.uuid acc)
      u.tokens t.by_token;
  match u.email_verification_uuid with
  | Some ev ->
      t.by_verification_token <-
        Utils.SM.add (Uuidm.to_string ev) u.uuid t.by_verification_token
  | None -> ()

let unregister_user_indexes t (u : User_model.user) =
  t.by_name <- Utils.LM.remove u.name t.by_name;
  t.by_email <- Utils.SM.remove (email_to_key u.email) t.by_email;
  t.by_cookie <-
    Utils.SM.fold
      (fun _ (c : User_model.cookie) acc -> Utils.SM.remove c.value acc)
      u.cookies t.by_cookie;
  t.by_token <-
    Utils.SM.fold
      (fun _ (tok : User_model.token) acc -> Utils.SM.remove tok.value acc)
      u.tokens t.by_token;
  match u.email_verification_uuid with
  | Some ev ->
      t.by_verification_token <-
        Utils.SM.remove (Uuidm.to_string ev) t.by_verification_token
  | None -> ()

let create ?(users = Utils.SM.empty) ?(configurations = Utils.LM.empty) ?email
    () =
  let t =
    {
      users;
      configurations;
      email;
      by_name = Utils.LM.empty;
      by_email = Utils.SM.empty;
      by_cookie = Utils.SM.empty;
      by_token = Utils.SM.empty;
      by_verification_token = Utils.SM.empty;
    }
  in
  Utils.SM.iter (fun _ u -> register_user_indexes t u) users;
  t

let t_to_json ?(version = current_version) users configurations email =
  `Assoc
    [
      ("version", `Int version);
      ( "users",
        `List
          (Utils.SM.fold
             (fun _ u acc -> User_model.user_to_json u :: acc)
             users []) );
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
                Ok (Utils.SM.add user.uuid user acc))
              (Ok Utils.SM.empty) users
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

let find_by_email t email =
  match Utils.SM.find_opt (email_to_key email) t.by_email with
  | Some uuid -> Utils.SM.find_opt uuid t.users
  | None -> None

let find_by_name t name =
  match Utils.LM.find_opt name t.by_name with
  | Some uuid -> Utils.SM.find_opt uuid t.users
  | None -> None

let find_by_uuid t uuid = Utils.SM.find_opt uuid t.users

let find_by_cookie t cookie_value =
  match Utils.SM.find_opt cookie_value t.by_cookie with
  | Some uuid -> (
      match Utils.SM.find_opt uuid t.users with
      | Some user -> (
          match User_model.user_session_cookie user cookie_value with
          | Some c -> Some (user, c)
          | None -> None)
      | None -> None)
  | None -> None

let find_by_api_token t token =
  match Utils.SM.find_opt token t.by_token with
  | Some uuid -> (
      match Utils.SM.find_opt uuid t.users with
      | Some user -> (
          match Utils.SM.find_opt token user.tokens with
          | Some token_ -> Some (user, token_)
          | None -> None)
      | None -> None)
  | None -> None

let increment_token_usage (token : User_model.token) (user : User_model.user) =
  let token = { token with usage_count = token.usage_count + 1 } in
  let tokens = Utils.SM.add token.value token user.tokens in
  User_model.update_user user ~tokens ()

let update_cookie_usage (cookie : User_model.cookie) user_agent
    (user : User_model.user) =
  let cookie = { cookie with user_agent } in
  let cookies = Utils.SM.add cookie.value cookie user.cookies in
  User_model.update_user user ~cookies ()

let update_user_unikernel_updates (new_update : User_model.unikernel_update)
    (user : User_model.user) =
  let unikernel_updates =
    Utils.LM.add new_update.name new_update user.unikernel_updates
  in
  User_model.update_user user ~unikernel_updates ()

let count_users t = Utils.SM.cardinal t.users

let find_email_verification_token t uuid =
  match Utils.SM.find_opt (Uuidm.to_string uuid) t.by_verification_token with
  | Some user_uuid -> Utils.SM.find_opt user_uuid t.users
  | None -> None

let count_active t =
  Utils.SM.fold
    (fun _ (u : User_model.user) acc ->
      if u.User_model.active then acc + 1 else acc)
    t.users 0

let count_superusers t =
  Utils.SM.fold
    (fun _ (u : User_model.user) acc ->
      if u.User_model.super_user then acc + 1 else acc)
    t.users 0

let store_email t email = t.email <- email

let insert_configuration t (configuration : Configuration.t) =
  if Utils.LM.mem configuration.name t.configurations then
    Error
      (Fmt.str "configuration %s already exists"
         (Configuration.name_to_str configuration.name))
  else begin
    t.configurations <-
      Utils.LM.add configuration.name configuration t.configurations;
    Ok ()
  end

let update_configuration t (configuration : Configuration.t) =
  if not (Utils.LM.mem configuration.name t.configurations) then
    Error
      (Fmt.str "configuration %s not found"
         (Configuration.name_to_str configuration.name))
  else begin
    t.configurations <-
      Utils.LM.add configuration.name configuration t.configurations;
    Ok ()
  end

let upsert_configuration t (configuration : Configuration.t) mode =
  match mode with
  | `Create -> insert_configuration t configuration
  | `Update -> update_configuration t configuration

let delete_configuration t name =
  t.configurations <- Utils.LM.remove name t.configurations

let add_user t (user : User_model.user) =
  t.users <- Utils.SM.add user.uuid user t.users;
  register_user_indexes t user

let delete_user t (user : User_model.user) =
  t.users <- Utils.SM.remove user.uuid t.users;
  unregister_user_indexes t user

let update_user t (user : User_model.user) =
  (match Utils.SM.find_opt user.uuid t.users with
  | Some old_user -> unregister_user_indexes t old_user
  | None -> ());
  t.users <- Utils.SM.add user.uuid user t.users;
  register_user_indexes t user
