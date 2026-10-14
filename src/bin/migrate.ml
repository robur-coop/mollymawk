open Lwt.Infix
module OFS = OneFFS.Make (Block)
module FS = Filesystem.Make (Block)

let verify_copied_data dst_blk data =
  FS.read_data dst_blk >>= function
  | Error (`Msg msg) -> Fmt.failwith "error reading block device %s" msg
  | Error (#FS.decode_err as e) ->
      Fmt.failwith "error reading block device %a" FS.pp_decode_err e
  | Ok (_sp, migrated_data) ->
      if String.equal data migrated_data then (
        Logs.info (fun m ->
            m "successfully migrated %d bytes" (String.length data));
        Lwt.return_unit)
      else Fmt.failwith "migrated data doesnt match original data"

let migrate src_path dst_path =
  if String.equal src_path dst_path then
    Fmt.failwith
      "source image and destination image cannot be the same %s and %s" src_path
      dst_path
  else
    Block.connect src_path >>= fun src_blk ->
    OFS.connect src_blk >>= fun oneffs ->
    OFS.read oneffs >>= function
    | Error e ->
        Fmt.failwith "failed to read oneffs block %s: %a" src_path OFS.pp_error
          e
    | Ok (Some data) -> (
        Logs.info (fun m -> m "read %d bytes from OneFFS" (String.length data));
        let _ = Yojson.Basic.from_string data in
        Block.connect dst_path >>= fun dst_blk ->
        FS.init dst_blk >>= function
        | Error (`Msg msg) -> Fmt.failwith "failed initializing target: %s" msg
        | Ok sb -> (
            FS.write_data dst_blk sb data >>= function
            | Error (`Msg msg) -> Fmt.failwith "failed writing target: %s" msg
            | Ok _sb -> verify_copied_data dst_blk data))
    | Ok None ->
        Logs.info (fun m ->
            m
              "source block %s is empty, formatting target with empty \
               filesystem"
              src_path);
        Fmt.failwith "nothing to migrate. source block is empty"

let start src dst =
  match Lwt_main.run (migrate src dst) with
  | () -> ()
  | exception Failure msg ->
      Logs.err (fun m -> m "%s" msg);
      exit 1
  | exception exn ->
      Logs.err (fun m -> m "Error: %s" (Printexc.to_string exn));
      exit 1

let src_arg =
  let doc = "path to the source image." in
  Cmdliner.Arg.(
    required & pos 0 (some file) None & info [] ~docv:"OneFFS block image" ~doc)

let dst_arg =
  let doc = "path to the destination image." in
  Cmdliner.Arg.(
    required
    & pos 1 (some file) None
    & info [] ~docv:"Destination block image" ~doc)

let cmd =
  let doc = "migrate from a oneffS image to an fs image." in
  let info = Cmdliner.Cmd.info "migrate" ~version:"1.0.0" ~doc in
  let term = Cmdliner.Term.(const start $ src_arg $ dst_arg) in
  Cmdliner.Cmd.v info term

let () = exit (Cmdliner.Cmd.eval cmd)
