module Make (BLOCK : Mirage_block.S) = struct
  module Stored_data = Filesystem.Make (BLOCK)
  open Lwt.Infix

  let disk = ref None
  let superblock = ref (Stored_data.empty_superblock ())

  let write_data data =
    match !disk with
    | None -> Lwt.return (Storage.error_msgf "Disk not connected")
    | Some disk -> (
        Stored_data.write_data disk !superblock
          (Yojson.Basic.to_string
             (Storage.t_to_json data.Storage.users data.configurations
                data.email))
        >|= function
        | Ok sb ->
            superblock := sb;
            Ok ()
        | Error (#Stored_data.decode_err as e) ->
            Storage.error_msgf "error while writing storage: %a"
              Stored_data.pp_decode_err e
        | Error (`Msg msg) ->
            Logs.err (fun m -> m "error writing block device %s" msg);
            Storage.error_msgf "writing block device")

  let read_data () =
    match !disk with
    | None -> Lwt.return (Storage.error_msgf "Disk not connected")
    | Some disk -> (
        Stored_data.read_data disk >|= function
        | Error (`Msg msg) ->
            Logs.err (fun m -> m "error reading block device %s" msg);
            Storage.error_msgf "reading block device"
        | Error (#Stored_data.decode_err as e) ->
            Logs.err (fun m ->
                m "error reading block device %a" Stored_data.pp_decode_err e);
            Storage.error_msgf "reading block device"
        | Ok (sb, data) ->
            superblock := sb;
            Logs.info (fun m -> m "data is %s " data);
            if String.equal data "" then
              Ok (Utils.UM.empty, Utils.LM.empty, None)
            else
              let ( let* ) = Result.bind in
              let* json = Utils.Json.from_string data in
              let* t = Storage.t_of_json json in
              Ok t)

  let init_block blk =
    disk := Some blk;
    Stored_data.init blk >|= function
    | Error (`Msg msg) ->
        Logs.err (fun m -> m "error initializing block device %s" msg);
        Storage.error_msgf "initializing block device"
    | Ok sb ->
        superblock := sb;
        Ok (Storage.create ())

  let connect blk =
    disk := Some blk;
    read_data () >|= function
    | Error _ as e -> e
    | Ok (users, configurations, email) ->
        Ok (Storage.create ~users ~configurations ?email ())
end
