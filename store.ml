include Storage

module Make (BLOCK : Mirage_block.S) = struct
  module Stored_data = OneFFS.Make (BLOCK)
  open Lwt.Infix

  type t = {
    disk : Stored_data.t;
    mutable users : User_model.user list;
    mutable configurations : Configuration.t list;
    mutable email : Utils.Email.t option;
  }

  let write_data t fn res =
    Stored_data.write t.disk
      (Yojson.Basic.to_string
         (Storage.t_to_json t.users t.configurations t.email))
    >|= function
    | Ok () ->
        fn;
        Ok res
    | Error we ->
        Storage.error_msgf "error while writing storage: %a"
          Stored_data.pp_write_error we

  let read_data disk =
    Stored_data.read disk >|= function
    | Ok (Some s) ->
        let ( let* ) = Result.bind in
        let* json = Utils.Json.from_string s in
        let* t = Storage.t_of_json json in
        Ok t
    | Ok None -> Ok ([], [], None)
    | Error e ->
        Storage.error_msgf "error while reading storage: %a"
          Stored_data.pp_error e

  let connect block =
    Stored_data.connect block >>= fun disk ->
    read_data disk >|= function
    | Error _ as e -> e
    | Ok (users, configurations, email) ->
        Ok { disk; users; configurations; email }

  let configurations { configurations; _ } = configurations
  let email { email; _ } = email

  let store_configurations t configurations =
    let t' = { t with configurations } in
    write_data t' (t.configurations <- configurations) t.configurations

  let store_email t email =
    let t' = { t with email } in
    write_data t' (t.email <- email) t.email

  let upsert_configuration t (configuration : Configuration.t)
      (mode : [ `Create | `Update ]) =
    let name_eq (c : Configuration.t) =
      Vmm_core.Name.Label.equal c.name configuration.name
    in
    let exists = List.exists name_eq t.configurations in
    match mode with
    | `Create ->
        if exists then
          Lwt.return
            (Storage.error_msgf "configuration %s already exists"
               (Configuration.name_to_str configuration.name))
        else store_configurations t (t.configurations @ [ configuration ])
    | `Update ->
        if not exists then
          Lwt.return
            (Storage.error_msgf "configuration %s not found"
               (Configuration.name_to_str configuration.name))
        else
          let configurations =
            List.map
              (fun c -> if name_eq c then configuration else c)
              t.configurations
          in
          store_configurations t configurations

  let delete_configuration t name =
    let configurations =
      List.filter
        (fun (c : Configuration.t) ->
          not (Vmm_core.Name.Label.equal c.name name))
        t.configurations
    in
    let t' = { t with configurations } in
    write_data t' (t.configurations <- configurations) t.configurations

  let add_user t user =
    let t' = { t with users = user :: t.users } in
    write_data t' (t.users <- user :: t.users) ()

  let delete_user t (user : User_model.user) =
    let users =
      List.fold_left
        (fun acc u -> if u.User_model.uuid <> user.uuid then u :: acc else acc)
        [] t.users
    in
    let t' = { t with users } in
    write_data t' (t.users <- users) ()

  let update_user t (user : User_model.user) =
    let users =
      List.map
        (fun (u : User_model.user) ->
          match u.uuid = user.uuid with true -> user | false -> u)
        t.users
    in
    let t' = { t with users } in
    write_data t' (t.users <- users) ()

  let users { users; _ } = users
end
