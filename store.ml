module FS = Filesystem

let superblock = ref (FS.empty_superblock ())

let write_data data disk =
  match
    Filesystem.write_data disk !superblock
      (Yojson.Basic.to_string
         (Storage.t_to_json data.Storage.users data.configurations data.email))
  with
  | Ok sb ->
      superblock := sb;
      Ok ()
  | Error (#FS.decode_err as e) ->
      FS.error_msgf "error while writing storage: %a" FS.pp_decode_err e
  | Error (`Msg msg) ->
      Logs.err (fun m -> m "error writing block device %s" msg);
      FS.error_msgf "writing block device"
  | exception exn -> FS.error_msgf "unknown error: %s" (Printexc.to_string exn)

let read_data disk =
  match FS.read_data disk with
  | Error (`Msg msg) ->
      Logs.err (fun m -> m "error reading block device %s" msg);
      FS.error_msgf "reading block device"
  | Error (#FS.decode_err as e) ->
      Logs.err (fun m -> m "error reading block device %a" FS.pp_decode_err e);
      FS.error_msgf "reading block device"
  | Ok (sb, data) ->
      superblock := sb;
      if String.equal data "" then Ok (Utils.UM.empty, Utils.LM.empty, None)
      else
        let ( let* ) = Result.bind in
        let* json = Utils.Json.from_string data in
        let* t = Storage.t_of_json json in
        Ok t

let init_block disk =
  superblock := FS.init disk;
  Storage.create ()

let connect disk =
  match read_data disk with
  | Error (`Msg e) -> failwith e
  | Ok (users, configurations, email) ->
      Storage.create ~users ~configurations ?email ()
