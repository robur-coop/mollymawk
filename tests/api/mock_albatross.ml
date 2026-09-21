open Lwt.Infix
open Test_utils

let ca_cert = mock_albatross_config.certificate
let ca_key = mock_albatross_config.private_key
let resources = ref (Vmm_resources.empty None)
let error_override = ref None
let last_received_binary = ref None
let last_received_cmd = ref None
let set_error_override (r : Vmm_commands.res) = error_override := Some r
let clear_error_override () = error_override := None
let get_last_binary () = !last_received_binary
let get_last_cmd () = !last_received_cmd
let get_resources () = !resources

let reset () =
  resources := Vmm_resources.empty None;
  error_override := None;
  last_received_binary := None;
  last_received_cmd := None

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

let read_image flow =
  let rec loop acc =
    Vmm_tls_lwt.read_tls_chunk flow >>= function
    | Ok chunk -> loop (chunk :: acc)
    | Error `Eof -> Lwt.return (Ok (String.concat "" (List.rev acc)))
    | Error e -> Lwt.return (Error e)
  in
  loop []

let make_dummy_unikernel config =
  Vmm_core.Unikernel.
    {
      config;
      cmd = [||];
      pid = 1000 + Random.int 50000;
      taps = [];
      digest = "";
      started = Mirage_ptime.now ();
    }

let eval_command name cmd =
  match !error_override with
  | Some err -> err
  | None -> (
      match cmd with
      | `Unikernel_cmd (`Unikernel_create cfg) -> (
          match Vmm_resources.check_unikernel !resources name cfg with
          | Error (`Msg err) -> `Failure err
          | Ok () -> (
              let u = make_dummy_unikernel cfg in
              match Vmm_resources.insert_unikernel !resources name u with
              | Ok r ->
                  resources := r;
                  `Success (`String "unikernel created")
              | Error (`Msg err) -> `Failure err))
      | `Unikernel_cmd (`Unikernel_force_create cfg) -> (
          let r_without =
            match Vmm_resources.remove_unikernel !resources name with
            | Ok r -> r
            | Error _ -> !resources
          in
          let u = make_dummy_unikernel cfg in
          match Vmm_resources.insert_unikernel r_without name u with
          | Ok r ->
              resources := r;
              `Success (`String "unikernel force created")
          | Error (`Msg err) -> `Failure err)
      | `Unikernel_cmd `Unikernel_info ->
          let infos =
            match Vmm_core.Name.name name with
            | None ->
                Vmm_trie.fold (Vmm_core.Name.path name)
                  !resources.Vmm_resources.unikernels
                  (fun id u acc ->
                    (id, Vmm_core.Unikernel.info (fun _ -> None) u) :: acc)
                  []
            | Some _ ->
                Option.fold ~none:[]
                  ~some:(fun u ->
                    [ (name, Vmm_core.Unikernel.info (fun _ -> None) u) ])
                  (Vmm_trie.find name !resources.Vmm_resources.unikernels)
          in
          `Success (`Unikernel_info infos)
      | `Unikernel_cmd `Unikernel_destroy -> (
          match Vmm_resources.remove_unikernel !resources name with
          | Ok r ->
              resources := r;
              `Success `Empty
          | Error (`Msg err) -> `Failure err)
      | `Policy_cmd `Policy_info ->
          let policies =
            Vmm_trie.fold (Vmm_core.Name.path name)
              !resources.Vmm_resources.policies
              (fun name p acc -> (name, p) :: acc)
              []
          in
          `Success (`Policies policies)
      | _ -> `Success `Empty)

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
      | Ok (name, policies, _version, cmd) ->
          last_received_cmd := Some cmd;
          List.iter
            (fun (path, policy) ->
              match Vmm_resources.insert_policy !resources path policy with
              | Ok r -> resources := r
              | Error _ -> ())
            policies;
          (match cmd with
            | `Unikernel_cmd (`Unikernel_create _ | `Unikernel_force_create _)
              -> (
                read_image flow >>= function
                | Ok bin ->
                    last_received_binary := Some bin;
                    Lwt.return_unit
                | Error _ -> Lwt.return_unit)
            | _ -> Lwt.return_unit)
          >>= fun () ->
          let reply = eval_command name cmd in
          let wire = (Vmm_commands.header name, reply) in
          Vmm_tls_lwt.write_tls flow wire >>= fun _ -> Vmm_tls_lwt.close flow)
    (fun exn ->
      Logs.err (fun m ->
          m "Mock Albatross connection error: %s" (Printexc.to_string exn));
      Lwt.return_unit)

let rec server_loop file_descr =
  Lwt.catch
    (fun () ->
      Tls_lwt.Unix.accept tls_server_config file_descr >>= fun (flow, _addr) ->
      Lwt.async (fun () -> handle_connection flow);
      server_loop file_descr)
    (fun exn ->
      Logs.err (fun m ->
          m "Mock Albatross accept error: %s" (Printexc.to_string exn));
      server_loop file_descr)

let mock_server =
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

let albatross_config =
  mock_server >>= fun (_file_descr, port) ->
  Lwt.return
    {
      mock_albatross_config with
      server_ip = Ipaddr.of_string_exn "127.0.0.1";
      server_port = port;
    }
