open Lwt.Infix
open Test_utils

let ca_cert = mock_albatross_config.certificate
let ca_key = mock_albatross_config.private_key

let tls_server_config =
  let authenticator =
    X509.Authenticator.chain_of_trust
      ~time:(fun () -> Some (Mirage_ptime.now ()))
      [ ca_cert ]
  in
  match
    Tls.Config.server
      ~certificates:(`Single ([ ca_cert ], ca_key))
      ~authenticator
      ~acceptable_cas:[ X509.Certificate.subject ca_cert ]
      ()
  with
  | Ok conf -> conf
  | Error (`Msg m) -> failwith m

let drain_image flow =
  let rec loop () =
    Vmm_tls_lwt.read_tls_chunk flow >>= function
    | Ok _ -> loop ()
    | Error `Eof -> Lwt.return_unit
    | Error _ -> Lwt.return_unit
  in
  loop ()

let dummy_info (name : Vmm_core.Name.t) =
  let dummy_cfg =
    Vmm_core.Unikernel.
      {
        typ = `Solo5;
        compressed = false;
        image = "";
        fail_behaviour = `Quit;
        add_name = true;
        startup = None;
        cpuids = Vmm_core.IS.singleton 0;
        memory = 32;
        block_devices = [];
        bridges = [];
        argv = None;
        numcpus = 1;
        linux_boot_partition = None;
      }
  in
  let dummy_u =
    Vmm_core.Unikernel.
      {
        config = dummy_cfg;
        cmd = [||];
        pid = 1234;
        taps = [];
        digest = String.make 32 'a';
        started = Mirage_ptime.now ();
      }
  in
  let info = Vmm_core.Unikernel.info (fun _ -> None) dummy_u in
  (name, info)

let eval_success (name : Vmm_core.Name.t) (cmd : Vmm_commands.t) :
    Vmm_commands.res =
  match cmd with
  | `Unikernel_cmd `Unikernel_info ->
      `Success (`Unikernel_info [ dummy_info name ])
  | `Unikernel_cmd (`Unikernel_create _) ->
      `Success (`String "unikernel created")
  | `Unikernel_cmd (`Unikernel_force_create _) ->
      `Success (`String "unikernel force created")
  | `Unikernel_cmd `Unikernel_destroy ->
      `Success (`String "unikernel destroyed")
  | `Unikernel_cmd (`Unikernel_restart _) ->
      `Success (`String "unikernel restarted")
  | `Block_cmd `Block_info -> `Success (`Block_devices [])
  | `Block_cmd (`Block_add _) -> `Success (`String "block device added")
  | `Block_cmd `Block_remove -> `Success (`String "block device removed")
  | `Policy_cmd `Policy_info -> `Success (`Policies [])
  | _ -> `Success `Empty

let eval_failure (_name : Vmm_core.Name.t) (_cmd : Vmm_commands.t) :
    Vmm_commands.res =
  `Failure "albatross failure"

type mode = Success | Failure

let start_mock_server mode =
  let eval =
    match mode with Success -> eval_success | Failure -> eval_failure
  in
  let handle_connection flow =
    Lwt.catch
      (fun () ->
        let epoch = Tls_lwt.Unix.epoch flow in
        let parsed =
          match epoch with
          | Ok e -> Vmm_tls.handle e.peer_certificate_chain
          | Error () -> Error (`Msg "TLS epoch not available")
        in
        match parsed with
        | Error (`Msg err) ->
            let wire = (Vmm_commands.header Vmm_core.Name.root, `Failure err) in
            Vmm_tls_lwt.write_tls flow wire >>= fun _ -> Vmm_tls_lwt.close flow
        | Ok (name, _policies, _version, cmd) ->
            (match cmd with
              | `Unikernel_cmd (`Unikernel_create _ | `Unikernel_force_create _)
                ->
                  drain_image flow
              | _ -> Lwt.return_unit)
            >>= fun () ->
            let reply = eval name cmd in
            let wire = (Vmm_commands.header name, reply) in
            Vmm_tls_lwt.write_tls flow wire >>= fun _ -> Vmm_tls_lwt.close flow)
      (fun exn ->
        Logs.err (fun m ->
            m "Mock Albatross connection error: %s" (Printexc.to_string exn));
        Lwt.return_unit)
  in
  let rec server_loop file_descr =
    Lwt.catch
      (fun () ->
        Tls_lwt.Unix.accept tls_server_config file_descr
        >>= fun (flow, _addr) ->
        Lwt.async (fun () -> handle_connection flow);
        server_loop file_descr)
      (fun exn ->
        Logs.err (fun m ->
            m "Mock Albatross accept error: %s" (Printexc.to_string exn));
        server_loop file_descr)
  in
  let file_descr = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, 0) in
  Lwt_unix.bind file_descr addr >>= fun () ->
  let port =
    match Lwt_unix.getsockname file_descr with
    | Lwt_unix.ADDR_INET (_, p) -> p
    | _ -> failwith "port not assigned"
  in
  Lwt_unix.listen file_descr 10;
  Lwt.async (fun () -> server_loop file_descr);
  Lwt.return (file_descr, port)

let success_server = start_mock_server Success
let failure_server = start_mock_server Failure

let success_config =
  success_server >>= fun (_file_descr, port) ->
  Lwt.return
    {
      mock_albatross_config with
      server_ip = Ipaddr.of_string_exn "127.0.0.1";
      server_port = port;
    }

let failure_config =
  failure_server >>= fun (_file_descr, port) ->
  Lwt.return
    {
      mock_albatross_config with
      name = label_of_string_exn "failing";
      server_ip = Ipaddr.of_string_exn "127.0.0.1";
      server_port = port;
    }

let albatross_config = success_config
