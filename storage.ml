type t = {
  version : int ;
  users : User_model.user list ;
}

let current_version = 1

let get key assoc =
  match List.find_opt (fun (k, _) -> String.equal k key) assoc with
  | None -> None
  | Some (_, f) -> Some f

let t_to_json t =
  `Assoc [ ("version", `Int t.version ) ; ("users", `List (List.map User_model.user_to_json t.users)) ]

let t_of_json = function
  | `Assoc xs ->
    begin
      let ( let* ) = Result.bind in
      match get "version" xs, get "users" xs with
      | Some `Int v, Some `List xs ->
        let* () =
          if v = current_version then Ok () else
            Error (`Msg "can't decode version")
        in
        let* users = List.fold_left (fun acc js ->
            let* acc = acc in
            let* user = User_model.user_of_json js in
            Ok (user :: acc))
            (Ok []) xs
        in
        Ok { version = v ; users }
      | _ -> Error (`Msg "invalid data: no version and users field")
    end
  | _ -> Error (`Msg "invalid data: not an assoc")

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

module Make (BLOCK : Mirage_block.S) = struct
  module Stored_data = OneFFS.Make(BLOCK)

  type store = t * BLOCK.t

  open Lwt.Infix

  let write_data (disk, t) =
    Stored_data.write disk (Yojson.Basic.to_string (t_to_json t))

  let read_data disk =
    Stored_data.read disk >|= function
    | Ok Some s ->
      let ( let* ) = Result.bind in
      let* json =
        try Ok (Yojson.Basic.from_string s) with
          Yojson.Json_error msg -> Error (`Msg ("Invalid json: " ^ msg))
      in
      let* t = t_of_json json in
      Ok (disk, t)
    | Ok None -> Ok (disk, { version = current_version ; users = [] })
    | Error e ->
      error_msgf "error while reading storage: %a" Stored_data.pp_error e

  let add_user (disk, t) user =
    let t = { t with users = user :: t.users } in
    write_data (disk, t) >|= function
    | Ok () -> Ok (disk, t)
    | Error we ->
      error_msgf "erro while writing storage: %a" Stored_data.pp_write_error we
end
