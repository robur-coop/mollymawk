open Cmdliner

let rng =
  let fn () = Mirage_crypto_rng_mkernel.(initialize (module Pfortuna)) in
  Mkernel.map fn Mkernel.[] |> Mkernel.finally Mirage_crypto_rng_mkernel.kill

let http =
  let h1 = H1.Config.
      {
        default with
        read_buffer_size = 0x4000;
        response_buffer_size = 0x4000;
        response_body_buffer_size = 0x4000;
      } in
  `H1 h1

let run stack _name =
  Mkernel.(run [ rng; stack; block "data" ])
  @@ fun _rng (_, tcp, _) blk () ->
  let _data = Store.connect blk in
  let port = 80 in
  let cfg = Vifu.Config.v ~http port in
  Vifu.run ~cfg tcp Routes.routes ()

let name =
  let doc = "The name of the unikernel" in
  let open Arg in
  value & opt string "mollymawk" & info [ "name" ] ~doc ~docv:"NAME"

let term =
  let open Term in
  const run $ Mnet_cli.setup "service" $ name

let cmd =
  let info = Cmd.info "mollymawk" in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
