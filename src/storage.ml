let current_version = 10
(* version history:
   [1 - 8] deprecated.
   9 email configuration is now stored
   10 we now have scaling policies for unikernels, default is no policy for existing unikernels
*)

type t = {
  (* these fields are persisted to disk *)
  mutable users : Models.User.user Utils.UM.t;
  mutable configurations : Models.Configuration.t Utils.LM.t;
  mutable email : Models.Email.t option;
  (* these fields below are not persisted to disk*)
  mutable by_name : Uuidm.t Utils.LM.t;
  mutable by_email : Uuidm.t Utils.SM.t;
  mutable by_verification_token : Uuidm.t Utils.SM.t;
}

let configurations { configurations; _ } = configurations
let email { email; _ } = email
let users { users; _ } = users

let email_to_key (email : Mrmime.Mailbox.t) =
  String.lowercase_ascii (Emile.to_string email)

let register_user_indexes t (u : Models.User.user) =
  t.by_name <- Utils.LM.add u.name u.uuid t.by_name;
  t.by_email <- Utils.SM.add (email_to_key u.email) u.uuid t.by_email;
  match u.email_verification_uuid with
  | Some ev ->
      t.by_verification_token <-
        Utils.SM.add (Uuidm.to_string ev) u.uuid t.by_verification_token
  | None -> ()

let unregister_user_indexes t (u : Models.User.user) =
  t.by_name <- Utils.LM.remove u.name t.by_name;
  t.by_email <- Utils.SM.remove (email_to_key u.email) t.by_email;
  match u.email_verification_uuid with
  | Some ev ->
      t.by_verification_token <-
        Utils.SM.remove (Uuidm.to_string ev) t.by_verification_token
  | None -> ()

let create ?(users = Utils.UM.empty) ?(configurations = Utils.LM.empty) ?email
    () =
  let t =
    {
      users;
      configurations;
      email;
      by_name = Utils.LM.empty;
      by_email = Utils.SM.empty;
      by_verification_token = Utils.SM.empty;
    }
  in
  Utils.UM.iter (fun _ u -> register_user_indexes t u) users;
  t

let storage_json ?(version = current_version) () =
  let open Jsont in
  let check_version v =
    if v = current_version || v = 9 then v
    else
      Error.msg Meta.none
        (Fmt.str
           "expected version %u, found version %u. note: version [1 - 8] is \
            now deprecated."
           current_version v)
  in
  let version_codec = map ~dec:check_version ~enc:Fun.id int in
  let users_codec =
    map
      ~dec:(fun us ->
        List.fold_left
          (fun acc (u : Models.User.user) -> Utils.UM.add u.uuid u acc)
          Utils.UM.empty us)
      ~enc:(fun m -> Utils.UM.fold (fun _ u acc -> u :: acc) m [])
      (list Models.User.user_json)
  in
  let email_codec =
    map
      ~dec:(fun j ->
        match Models.Email.of_json j with
        | Ok email -> Some email
        | Error _ -> None)
      ~enc:Models.Email.to_json json
  in
  let make _v users configurations email = (users, configurations, email) in
  Object.map make
  |> Object.mem "version" version_codec ~enc:(fun _ -> version)
  |> Object.mem "users" users_codec ~enc:(fun (u, _, _) -> u)
  |> Object.mem "configuration" Models.Configuration.json ~enc:(fun (_, c, _) ->
      c)
  |> Object.mem "email" email_codec ~dec_absent:None ~enc:(fun (_, _, e) -> e)
  |> Object.finish

let json = storage_json ()

let t_to_json ?(version = current_version) users configurations email =
  Utils.Json.encode (storage_json ~version ()) (users, configurations, email)

let t_of_json j = Utils.Json.decode json j
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let find_by_email t email =
  match Utils.SM.find_opt (email_to_key email) t.by_email with
  | Some uuid -> Utils.UM.find_opt uuid t.users
  | None -> None

let find_by_name t name =
  match Utils.LM.find_opt name t.by_name with
  | Some uuid -> Utils.UM.find_opt uuid t.users
  | None -> None

let find_by_uuid t uuid = Utils.UM.find_opt uuid t.users

let update_user_unikernel_updates (new_update : Models.User.unikernel_update)
    (user : Models.User.user) =
  let unikernel_updates =
    Utils.LM.add new_update.name new_update user.unikernel_updates
  in
  Models.User.update_user user ~unikernel_updates ()

let count_users t = Utils.UM.cardinal t.users

let find_email_verification_token t uuid =
  match Utils.SM.find_opt (Uuidm.to_string uuid) t.by_verification_token with
  | Some user_uuid -> Utils.UM.find_opt user_uuid t.users
  | None -> None

let count_active t =
  Utils.UM.fold
    (fun _ (u : Models.User.user) acc ->
      if u.Models.User.active then acc + 1 else acc)
    t.users 0

let count_superusers t =
  Utils.UM.fold
    (fun _ (u : Models.User.user) acc ->
      if u.Models.User.super_user then acc + 1 else acc)
    t.users 0

let store_email t email = t.email <- email

let insert_configuration t (configuration : Models.Configuration.t) =
  if Utils.LM.mem configuration.name t.configurations then
    Error
      (Fmt.str "configuration %s already exists"
         (Models.Configuration.name_to_str configuration.name))
  else begin
    t.configurations <-
      Utils.LM.add configuration.name configuration t.configurations;
    Ok ()
  end

let update_configuration t (configuration : Models.Configuration.t) =
  if not (Utils.LM.mem configuration.name t.configurations) then
    Error
      (Fmt.str "configuration %s not found"
         (Models.Configuration.name_to_str configuration.name))
  else begin
    t.configurations <-
      Utils.LM.add configuration.name configuration t.configurations;
    Ok ()
  end

let upsert_configuration t (configuration : Models.Configuration.t) mode =
  match mode with
  | `Create -> insert_configuration t configuration
  | `Update -> update_configuration t configuration

let delete_configuration t name =
  t.configurations <- Utils.LM.remove name t.configurations

let add_user t (user : Models.User.user) =
  t.users <- Utils.UM.add user.uuid user t.users;
  register_user_indexes t user

let delete_user t (user : Models.User.user) =
  t.users <- Utils.UM.remove user.uuid t.users;
  unregister_user_indexes t user

let update_user t (user : Models.User.user) =
  (match Utils.UM.find_opt user.uuid t.users with
  | Some old_user -> unregister_user_indexes t old_user
  | None -> ());
  t.users <- Utils.UM.add user.uuid user t.users;
  register_user_indexes t user
