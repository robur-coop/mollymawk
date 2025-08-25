open Utils.Json

(* TODO: increment version number *)
let current_version = 7
(* version history:
   1 was initial (fields until email_verification_uuid)
   2 added active
   3 added super_user (but did not serialise it)
   4 properly serialised super_user
   5 cookie has two new fields last_access and user_agent
   6 tokens has 3 new fields: name, last_access and usage_count
   7 added unikernel_updates to keep track of when unikernels are updated
*)

let t_to_json users configurations =
  `Assoc
    [
      ("version", `Int current_version);
      ("users", `List (List.map User_model.user_to_json users));
      ("configurations", `List (List.map Configuration.to_json configurations));
    ]

let t_of_json json =
  match json with
  | `Assoc xs -> (
      let ( let* ) = Result.bind in
      match (get "version" xs, get "users" xs, get "configurations" xs) with
      | Some (`Int v), Some (`List users), Some (`List configurations) ->
          let* () =
            if v = current_version then Ok ()
            else if v = 6 then Ok ()
            else if v = 5 then Ok ()
            else if v = 4 then Ok ()
            else if v = 3 then Ok ()
            else if v = 2 then Ok ()
            else if v = 1 then Ok ()
            else
              Error
                (`Msg
                   (Fmt.str "expected version %u, found version %u"
                      current_version v))
          in
          let* users =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* user =
                  if v = 1 then User_model.user_v1_of_json js
                  else if v = 2 || v = 3 then User_model.user_v2_of_json js
                  else if v = 4 || v = 5 then
                    User_model.(user_v3_of_json cookie_v1_of_json) js
                  else if v = 6 then
                    User_model.(user_v4_of_json cookie_v1_of_json) js
                  else User_model.(user_of_json cookie_of_json) js
                in
                Ok (user :: acc))
              (Ok []) users
          in
          let* configurations =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* config = Configuration.of_json js in
                Ok (config :: acc))
              (Ok []) configurations
          in
          Ok (users, configurations)
      | _ -> Error (`Msg "invalid data: no version and users field"))
  | _ -> Error (`Msg "invalid data: not an assoc")

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

module Make (BLOCK : Mirage_block.S) = struct
  module Stored_data = OneFFS.Make (BLOCK)
  open Lwt.Infix

  type t = {
    disk : Stored_data.t;
    mutable users : User_model.user list;
    mutable configurations : Configuration.t list;
  }

  let write_data t =
    Stored_data.write t.disk
      (Yojson.Basic.to_string (t_to_json t.users t.configurations))

  let read_data disk =
    Stored_data.read disk >|= function
    | Ok (Some s) ->
        let ( let* ) = Result.bind in
        let* json =
          try Ok (Yojson.Basic.from_string s)
          with Yojson.Json_error msg -> Error (`Msg ("Invalid json: " ^ msg))
        in
        let* t = t_of_json json in
        Ok t
    | Ok None -> Ok ([], [ Configuration.empty () ])
    | Error e ->
        error_msgf "error while reading storage: %a" Stored_data.pp_error e

  let connect block =
    Stored_data.connect block >>= fun disk ->
    read_data disk >|= function
    | Error _ as e -> e
    | Ok (users, configurations) -> Ok { disk; users; configurations }

  let configurations { configurations; _ } = configurations

  let update_configuration t (configuration : Configuration.t) =
    let configurations =
      List.map
        (fun (c : Configuration.t) ->
          if String.equal c.name configuration.name then configuration else c)
        t.configurations
    in
    let t' = { t with configurations } in
    write_data t' >|= function
    | Ok () ->
        t.configurations <- configurations;
        Ok t.configurations
    | Error we ->
        error_msgf "error while writing storage: %a" Stored_data.pp_write_error
          we

  let add_user t user =
    let t' = { t with users = user :: t.users } in
    write_data t' >|= function
    | Ok () ->
        t.users <- user :: t.users;
        Ok ()
    | Error we ->
        error_msgf "error while writing storage: %a" Stored_data.pp_write_error
          we

  let update_user t (user : User_model.user) =
    let users =
      List.map
        (fun (u : User_model.user) ->
          match u.uuid = user.uuid with true -> user | false -> u)
        t.users
    in
    let t' = { t with users } in
    write_data t' >|= function
    | Ok () ->
        t.users <- users;
        Ok ()
    | Error we ->
        error_msgf "error while writing storage: %a" Stored_data.pp_write_error
          we

  let users { users; _ } = users

  let find_by_email store email =
    List.find_opt
      (fun user -> String.equal user.User_model.email email)
      store.users

  let find_by_name store name =
    List.find_opt
      (fun user -> String.equal user.User_model.name name)
      store.users

  let find_by_uuid store uuid =
    List.find_opt
      (fun user -> String.equal user.User_model.uuid uuid)
      store.users

  let find_by_cookie store cookie_value =
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
      None store.users

  let find_by_api_token store token =
    List.find_map
      (fun (user : User_model.user) ->
        match
          List.find_opt
            (fun (token_ : User_model.token) -> String.equal token token_.value)
            user.tokens
        with
        | Some token_ -> Some (user, token_)
        | None -> None)
      store.users

  let increment_token_usage store (token : User_model.token)
      (user : User_model.user) =
    let token = { token with usage_count = token.usage_count + 1 } in
    let tokens =
      List.map
        (fun (token' : User_model.token) ->
          if String.equal token.value token'.value then token else token')
        user.tokens
    in
    let updated_user = User_model.update_user user ~tokens () in
    update_user store updated_user >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error (`Msg err) ->
        Logs.err (fun m -> m "Error with storage: %s" err);
        Lwt.return (Error (`Msg err))

  let update_cookie_usage store (cookie : User_model.cookie)
      (user : User_model.user) reqd =
    let cookie = { cookie with user_agent = Middleware.user_agent reqd } in
    let cookies =
      List.map
        (fun (cookie' : User_model.cookie) ->
          if String.equal cookie.value cookie'.value then cookie else cookie')
        user.cookies
    in
    let updated_user = User_model.update_user user ~cookies () in
    update_user store updated_user >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error (`Msg err) ->
        Logs.err (fun m -> m "Error with storage: %s" err);
        Lwt.return (Error (`Msg err))

  let update_user_unikernel_updates store
      (new_update : User_model.unikernel_update) (user : User_model.user) =
    let is_unique (u : User_model.unikernel_update) =
      not (String.equal u.name new_update.name)
    in
    let updated_list =
      new_update :: List.filter is_unique user.unikernel_updates
    in
    let updated_user =
      User_model.update_user user ~unikernel_updates:updated_list ()
    in
    update_user store updated_user >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error (`Msg err) ->
        Logs.err (fun m -> m "Error with storage: %s" err);
        Lwt.return (Error (`Msg err))

  let count_users store = List.length store.users

  let find_email_verification_token store uuid =
    List.find_opt
      (fun user ->
        Option.fold ~none:false
          ~some:(fun uu -> Uuidm.equal uu uuid)
          user.User_model.email_verification_uuid)
      store.users

  let count_active store =
    List.length (List.filter (fun u -> u.User_model.active) store.users)

  let count_superusers store =
    List.length (List.filter (fun u -> u.User_model.super_user) store.users)
end
