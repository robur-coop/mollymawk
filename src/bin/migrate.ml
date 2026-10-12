open Lwt.Infix
module OFS = OneFFS.Make (Block)
module FS = Filesystem.Make (Block)

let migrate src_path dst_path =
  Block.connect src_path >>= fun src_blk ->
  OFS.connect src_blk >>= fun oneffs ->
  OFS.read oneffs >>= function
  | Error e ->
      Fmt.failwith "failed to read oneffs block %s: %a" src_path OFS.pp_error e
  | Ok None -> (
      Logs.info (fun m ->
          m "source block %s is empty, formatting target with empty filesystem"
            src_path);
      Block.connect dst_path >>= fun dst_blk ->
      FS.init dst_blk >>= function
      | Error (`Msg msg) -> Fmt.failwith "failed initializing target: %s" msg
      | Ok _sb ->
          Logs.info (fun m ->
              m "successfully formatted %s with empty filesystem" dst_path);
          Lwt.return_unit)
  | Ok (Some data) -> (
      Logs.info (fun m -> m "read %d bytes from OneFFS" (String.length data));
      let _ = Yojson.Basic.from_string data in
      Block.connect dst_path >>= fun dst_blk ->
      FS.init dst_blk >>= function
      | Error (`Msg msg) -> Fmt.failwith "failed initializing target: %s" msg
      | Ok sb -> (
          FS.write_data dst_blk sb data >>= function
          | Error (`Msg msg) -> Fmt.failwith "failed writing target: %s" msg
          | Ok _sb ->
              Logs.info (fun m ->
                  m "successfully migrated %d bytes to %s" (String.length data)
                    dst_path);
              Lwt.return_unit))

let start src dst =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
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
    & pos 1 (some string) None
    & info [] ~docv:"Destination block image" ~doc)

let cmd =
  let doc = "migrate from a oneffS image to an fs image." in
  let info = Cmdliner.Cmd.info "migrate" ~version:"1.0.0" ~doc in
  let term = Cmdliner.Term.(const start $ src_arg $ dst_arg) in
  Cmdliner.Cmd.v info term

let () = exit (Cmdliner.Cmd.eval cmd)
