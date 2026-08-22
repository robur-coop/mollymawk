module Make (BLOCK : Mirage_block.S) = struct
  module Stored_data = OneFFS.Make (BLOCK)
  open Lwt.Infix

  let write_data disk data =
    Stored_data.write disk
      (Yojson.Basic.to_string
         (Storage.t_to_json data.Storage.users data.configurations data.email))
    >|= function
    | Ok () -> Ok ()
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
        Ok (disk, { Storage.users; configurations; email })
end
