type env = { tcp : Mnet.TCP.state; block : Mkernel.Block.t }

let rng =
  let fn () =
    Mirage_crypto_rng_mkernel.(initialize (module Mirage_crypto_rng.Fortuna))
  in
  Mkernel.map fn Mkernel.[] |> Mkernel.finally Mirage_crypto_rng_mkernel.kill

let http =
  let h1 =
    H1.Config.
      {
        default with
        read_buffer_size = 0x4000;
        response_buffer_size = 0x4000;
        response_body_buffer_size = 0x4000;
      }
  in
  `H1 h1

let happy_eyeballs =
  let finally (daemon, _) = Mnet_happy_eyeballs.kill daemon in
  Vifu.Device.v ~name:"happy-eyeballs" ~finally [] @@ fun { tcp; _ } ->
  Mnet_happy_eyeballs.create tcp

let make_block_device =
  let finally _ = () in
  Vifu.Device.v ~name:"store" ~finally [] @@ fun { block; _ } ->
  Store.connect block

let run stack _name =
  Mkernel.(run [ rng; stack; block "data" ]) @@ fun _rng (_, tcp, _) block () ->
  let block_device = make_block_device in
  let port = 80 in
  let cfg = Vifu.Config.v ~http port in
  let devices = Vifu.Devices.[ happy_eyeballs; block_device ] in
  let env = { tcp; block } in
  Vifu.run ~devices ~cfg tcp (Routes.routes ()) env

open Cmdliner

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
