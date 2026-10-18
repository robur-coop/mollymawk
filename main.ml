open Cmdliner

let ( let@ ) finally fn = Fun.protect ~finally fn

let rng =
  let fn () = Mirage_crypto_rng_mkernel.(initialize (module Pfortuna)) in
  Mkernel.map fn Mkernel.[]

let run cidr gateway port _name =
  let stack = Mnet.stack ~name:"service" ~gateway cidr in
  Mkernel.(run [ rng; stack; block "data" ])
  @@ fun rng (daemon, tcp, _) blk () ->
  let@ () = fun () -> Mirage_crypto_rng_mkernel.kill rng in
  let@ () = fun () -> Mnet.kill daemon in
  let _data = Store.connect blk in
  let h1 =
    H1.Config.
      {
        default with
        read_buffer_size = 0x4000;
        response_buffer_size = 0x4000;
        response_body_buffer_size = 0x4000;
      }
  in
  let cfg = Vifu.Config.v ~http:(`H1 h1) port in
  Logs.info (fun m ->
      m "Starting mollymawk on  %s:%d" (Ipaddr.V4.Prefix.to_string cidr) port);
  Vifu.run ~cfg tcp Routes.routes ()

let ipv4 =
  let open Arg in
  let doc = "The IP address of mollymawk." in
  let ipaddr = conv (Ipaddr.V4.Prefix.of_string, Ipaddr.V4.Prefix.pp) in
  required & opt (some ipaddr) None & info [ "ipv4" ] ~doc ~docv:"IPv4"

let ipv4_gateway =
  let open Arg in
  let doc = "The IP gateway." in
  let ipaddr = conv (Ipaddr.V4.of_string, Ipaddr.V4.pp) in
  required & opt (some ipaddr) None & info [ "ipv4-gateway" ] ~doc ~docv:"IPv4"

let port =
  let doc = "The port number to listen on" in
  let open Arg in
  value & opt int 8080 & info [ "port" ] ~doc ~docv:"PORT"

let name =
  let doc = "The name of the unikernel" in
  let open Arg in
  value & opt string "mollymawk" & info [ "name" ] ~doc ~docv:"NAME"

let term =
  let open Term in
  const run $ ipv4 $ ipv4_gateway $ port $ name

let cmd =
  let info = Cmd.info "mollymawk" in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
