module FS = Filesystem

let disk = ref None
let superblock = ref (FS.empty_superblock ())

let write_data data =
  match !disk with
  | None -> FS.error_msgf "Disk not connected"
  | Some disk -> (
      match
        Filesystem.write_data disk !superblock
          (Yojson.Basic.to_string
             (Storage.t_to_json data.Storage.users data.configurations
                data.email))
      with
      | Ok sb ->
          superblock := sb;
          Ok ()
      | Error (#FS.decode_err as e) ->
          FS.error_msgf "error while writing storage: %a" FS.pp_decode_err e
      | Error (`Msg msg) ->
          Logs.err (fun m -> m "error writing block device %s" msg);
          FS.error_msgf "writing block device")

let read_data () =
  match !disk with
  | None -> FS.error_msgf "Disk not connected"
  | Some disk -> (
      match FS.read_data disk with
      | Error (`Msg msg) ->
          Logs.err (fun m -> m "error reading block device %s" msg);
          FS.error_msgf "reading block device"
      | Error (#FS.decode_err as e) ->
          Logs.err (fun m ->
              m "error reading block device %a" FS.pp_decode_err e);
          FS.error_msgf "reading block device"
      | Ok (sb, data) ->
          superblock := sb;
          if String.equal data "" then Ok (Utils.UM.empty, Utils.LM.empty, None)
          else
            let ( let* ) = Result.bind in
            let* json = Utils.Json.from_string data in
            let* t = Storage.t_of_json json in
            Ok t)

let init_block blk =
  disk := Some blk;
  superblock := FS.init blk;
  Storage.create ()

let connect blk =
  disk := Some blk;
  match read_data () with
  | Error (`Msg e) -> failwith e
  | Ok (users, configurations, email) ->
      Storage.create ~users ~configurations ?email ()
