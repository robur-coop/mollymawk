let ( let* ) = Result.bind

module Status = struct
  type category = [ `Configuration | `Transient | `Incompatible | `Internal ]
  type error = { timestamp : Ptime.t; category : category; details : string }

  type t =
    | Online
    | Degraded of { retries : int; log : error list }
    | Offline of error list

  let to_string = function
    | Online -> "online"
    | Degraded { retries; _ } -> Fmt.str "degraded (%d retries)" retries
    | Offline err -> Fmt.str "offline (%d errors)" (List.length err)

  let make category details =
    { timestamp = Mirage_ptime.now (); category; details }

  let update status new_error =
    match status with
    | Online -> Degraded { retries = 1; log = [ new_error ] }
    | Degraded { retries; log } ->
        Degraded { retries = retries + 1; log = new_error :: log }
    | Offline log -> Offline (new_error :: log)

  let pp_error ppf { timestamp; category; details } =
    let category_str =
      match category with
      | `Configuration -> "configuration"
      | `Transient -> "transient"
      | `Incompatible -> "incompatible"
      | `Internal -> "internal"
    in
    Fmt.pf ppf "[%a] %s: %s" Ptime.pp timestamp category_str details
end

type t = {
  mutable policies : Vmm_core.Policy.t Vmm_trie.t;
  configuration : Configuration.t;
  mutable status : Status.t;
}

let set_online t = t.status <- Online

module String_set = Set.Make (String)

module Albatross_map = Map.Make (struct
  type t = Vmm_core.Name.t

  let compare a b =
    String.compare (Configuration.name_to_str a) (Configuration.name_to_str b)
end)

module Make (S : Tcpip.Stack.V4V6) = struct
  module TLS = Tls_mirage.Make (S.TCP)

  type a_map = t Albatross_map.t

  let empty_policy =
    Vmm_core.Policy.
      {
        unikernels = 0;
        cpuids = Vmm_core.IS.empty;
        memory = 0;
        block = None;
        bridges = Vmm_core.String_set.empty;
      }

  let policy ?domain t =
    let ( let* ) = Result.bind in
    let* path =
      match domain with
      | None -> Ok Vmm_core.Name.root
      | Some domain -> (
          match Vmm_core.Name.path_of_string domain with
          | Error (`Msg msg) ->
              Error
                (Fmt.str "albatross: domain %s is not a path: %s" domain msg)
          | Ok path -> Ok (Vmm_core.Name.create_of_path path))
    in
    Ok (Vmm_trie.find path t.policies)

  let policies ?domain t =
    let ( let* ) = Result.bind in
    let* path =
      match domain with
      | None -> Ok Vmm_core.Name.root_path
      | Some domain -> (
          match Vmm_core.Name.path_of_string domain with
          | Error (`Msg msg) ->
              Error
                (Fmt.str "albatross: domain %s is not a path: %s" domain msg)
          | Ok path -> Ok path)
    in
    Ok (Vmm_trie.fold path t.policies (fun name p acc -> (name, p) :: acc) [])

  let all_policies ?domain t =
    List.map
      (fun (name, albatross_state) -> (name, policy ?domain albatross_state))
      (Albatross_map.bindings t)

  let policy_resource_avalaible t =
    let root_policy =
      match policy t with
      | Ok p -> (
          match p with
          | Some p -> Ok p
          | None ->
              Logs.err (fun m -> m "policy error: empty root policy");
              Error (`Msg "root policy is empty"))
      | Error err ->
          Logs.err (fun m -> m "policy error:  %s" err);
          Error (`Msg "error getting root policy")
    in
    let policies =
      match policies t with
      | Ok p -> p
      | Error err ->
          Logs.err (fun m -> m "policy error:  %s" err);
          []
    in
    let unikernels_used, memory_used, storage_used =
      List.fold_left
        (fun (total_vms, total_memory, total_block) (name_, policy) ->
          if not Vmm_core.Name.(equal name_ root) then
            ( total_vms + policy.Vmm_core.Policy.unikernels,
              total_memory + policy.memory,
              total_block + Option.value ~default:0 policy.block )
          else (total_vms, total_memory, total_block))
        (0, 0, 0) policies
    in
    match root_policy with
    | Ok root_policy ->
        Ok
          Vmm_core.Policy.
            {
              unikernels = root_policy.unikernels - unikernels_used;
              cpuids = root_policy.cpuids;
              memory = root_policy.memory - memory_used;
              block = Option.map (fun b -> b - storage_used) root_policy.block;
              bridges = root_policy.bridges;
            }
    | Error (`Msg err) -> Error err

  let manifest_devices_match ~bridges ~block_devices binary =
    let cachet =
      let map () ~pos len =
        if pos >= String.length binary || len <= 0 then
          (Cachet.Bstr.empty :> Cachet.bigstring)
        else
          let len = min len (max 0 (String.length binary - pos)) in
          let b : Cachet.bigstring =
            Bigarray.Array1.create Bigarray.char Bigarray.c_layout len
          in
          for i = 0 to len - 1 do
            b.{i} <- binary.[pos + i]
          done;
          b
      in
      Cachet.make ~cachesize:8 ~map ()
    in
    let* mft : Solo5_elftool.mft = Solo5_elftool.query_manifest cachet in
    let req_bridges =
      List.map (fun (name, _, _) -> name) bridges |> String_set.of_list
    and req_block_devices =
      List.map (fun (name, _, _) -> name) block_devices |> String_set.of_list
    and mft_bridges =
      List.filter_map
        (function Solo5_elftool.Dev_net_basic name -> Some name | _ -> None)
        mft.Solo5_elftool.entries
      |> String_set.of_list
    and mft_block_devices =
      List.filter_map
        (function Solo5_elftool.Dev_block_basic name -> Some name | _ -> None)
        mft.Solo5_elftool.entries
      |> String_set.of_list
    in
    let req_only_bridges = String_set.(diff req_bridges mft_bridges |> elements)
    and mft_only_bridges = String_set.(diff mft_bridges req_bridges |> elements)
    and req_only_blocks =
      String_set.(diff req_block_devices mft_block_devices |> elements)
    and mft_only_blocks =
      String_set.(diff mft_block_devices req_block_devices |> elements)
    in
    match
      (req_only_bridges, mft_only_bridges, req_only_blocks, mft_only_blocks)
    with
    | [], [], [], [] -> Ok ()
    | req_only_bridges, [], [], [] ->
        Error
          (`Msg
             ("Extra network interfaces specified: "
             ^ String.concat ", " req_only_bridges
             ^ ". Please remove them from the 'network_interfaces' list of \
                your configuration."))
    | [], mft_only_bridges, [], [] ->
        Error
          (`Msg
             ("Missing required network interfaces: "
             ^ String.concat ", " mft_only_bridges
             ^ ". Please add them to the 'network_interfaces' list of your \
                configuration."))
    | [], [], req_only_blocks, [] ->
        Error
          (`Msg
             ("Extra block devices specified: "
             ^ String.concat ", " req_only_blocks
             ^ ". Please remove them from the 'block_devices' list of your \
                configuration."))
    | [], [], [], mft_only_blocks ->
        Error
          (`Msg
             ("Missing required block devices: "
             ^ String.concat ", " mft_only_blocks
             ^ ". Please add them to the 'block_devices' list of your \
                configuration."))
    | req_only_bridges, [], req_only_blocks, [] ->
        Error
          (`Msg
             ("Extra network interfaces: "
             ^ String.concat ", " req_only_bridges
             ^ " and extra block devices: "
             ^ String.concat ", " req_only_blocks
             ^ ". Please remove them from the 'network_interfaces' lists and \
                'block_devices' list of your configuration."))
    | [], mft_only_bridges, [], mft_only_blocks ->
        Error
          (`Msg
             ("Missing network interfaces: "
             ^ String.concat ", " mft_only_bridges
             ^ " and missing block devices: "
             ^ String.concat ", " mft_only_blocks
             ^ ". Please add them to the 'network_interfaces' lists and \
                'block_devices' list of your configuration."))
    | req_only_bridges, [], [], mft_only_blocks ->
        Error
          (`Msg
             ("Extra network interfaces: "
             ^ String.concat ", " req_only_bridges
             ^ " and missing block devices: "
             ^ String.concat ", " mft_only_blocks
             ^ ". Please remove the network interfaces from the \
                'network_interfaces' list and add the block devices to the \
                'block_devices' list of your configuration."))
    | [], mft_only_bridges, req_only_blocks, [] ->
        Error
          (`Msg
             ("Missing network interfaces: "
             ^ String.concat ", " mft_only_bridges
             ^ " and extra block devices: "
             ^ String.concat ", " req_only_blocks
             ^ ". Please add the network interfaces to the \
                'network_interfaces' list and remove the block devices from \
                the 'block_devices' list of your configuration."))
    | req_only_bridges, mft_only_bridges, req_only_blocks, mft_only_blocks ->
        Error
          (`Msg
             ("Missing network interfaces: "
             ^ String.concat ", " req_only_bridges
             ^ " and missing block devices: "
             ^ String.concat ", " req_only_blocks
             ^ " while also having extra network interfaces: "
             ^ String.concat ", " mft_only_bridges
             ^ " and extra block devices: "
             ^ String.concat ", " mft_only_blocks
             ^ ". Please update 'network_interfaces' and 'block_devices' \
                accordingly."))

  let key_ids exts pub issuer =
    let open X509 in
    let auth = (Some (Public_key.id issuer), General_name.empty, None) in
    Extension.(
      add Subject_key_id
        (false, Public_key.id pub)
        (add Authority_key_id (false, auth) exts))

  let timestamps validity =
    let now = Mirage_ptime.now () in
    match
      (* subtracting some seconds here to not require perfectly synchronised
         clocks on client and server *)
      ( Ptime.sub_span now (Ptime.Span.of_int_s (validity / 2)),
        Ptime.add_span now (Ptime.Span.of_int_s validity) )
    with
    | None, _ | _, None -> invalid_arg "span too big - reached end of ptime"
    | Some now, Some exp -> (now, exp)

  let key_cert ?(key_type = `ED25519) ~is_ca ?cmd key name issuer =
    let open X509 in
    let exts =
      let ext =
        Option.map Vmm_asn.to_cert_extension cmd
        |> Option.fold ~none:Extension.empty ~some:(fun p ->
               Extension.singleton (Unsupported Vmm_asn.oid) (false, p))
      in
      let ku, bc =
        if is_ca then
          ( [ `Key_cert_sign; `Digital_signature; `Content_commitment ],
            (true, None) )
        else ([ `Digital_signature; `Key_encipherment ], (false, None))
      in
      Extension.(
        add Basic_constraints (true, bc) (add Key_usage (true, ku) ext))
    in
    let tmp_key = Private_key.generate key_type in
    match
      let name =
        [ Distinguished_name.(Relative_distinguished_name.singleton (CN name)) ]
      in
      let extensions = Signing_request.Ext.(singleton Extensions exts) in
      Signing_request.create name ~extensions tmp_key
    with
    | Error (`Msg msg) ->
        Error
          (Fmt.str "albatross: failed to create %ssigning request: %s"
             (if is_ca then "CA " else "")
             msg)
    | Ok csr -> (
        let valid_from, valid_until = timestamps 300 in
        let extensions =
          let pub = X509.Private_key.public key in
          key_ids exts Signing_request.((info csr).public_key) pub
        in
        match
          Signing_request.sign csr ~valid_from ~valid_until ~extensions key
            issuer
        with
        | Error e ->
            Error
              (Fmt.str "albatross: failed to sign %ssigning request: %a"
                 (if is_ca then "CA " else "")
                 X509.Validation.pp_signature_error e)
        | Ok mycert -> Ok (tmp_key, mycert))

  let gen_cert t ?domain cmd name =
    let ( let* ) = Result.bind in
    let c = t.configuration in
    let* ikey, cert, certs =
      match domain with
      | None -> Ok (c.private_key, c.certificate, [])
      | Some domain ->
          let* policy = policy ~domain t in
          let policy = Option.value ~default:empty_policy policy in
          let cmd = `Policy_cmd (`Policy_add policy) in
          let* key, cert =
            key_cert ~is_ca:true ~cmd c.private_key domain
              (X509.Certificate.subject c.certificate)
          in
          Ok (key, cert, [ cert ])
    in
    let* key, cert =
      key_cert ~is_ca:false ~cmd ikey name (X509.Certificate.subject cert)
    in
    Ok (`Single (cert :: certs, key))

  let decode data =
    match Vmm_asn.wire_of_str data with
    | Error (`Msg msg) ->
        Error (Fmt.str "albatross: failed to decode data %s" msg)
    | Ok (hdr, res) as w ->
        if not Vmm_commands.(is_current hdr.version) then
          Logs.warn (fun m ->
              m "albatross version mismatch, received %a current %a"
                Vmm_commands.pp_version hdr.Vmm_commands.version
                Vmm_commands.pp_version Vmm_commands.current);
        Logs.debug (fun m ->
            m "albatross returned: %a"
              (Vmm_commands.pp_wire ~verbose:true)
              (hdr, res));
        w

  let decode_reply data =
    if String.length data >= 4 then
      let len = Int32.to_int (String.get_int32_be data 0) in
      if String.length data >= 4 + len then (
        if String.length data > 4 + len then
          Logs.warn (fun m ->
              m "received %d trailing bytes" (String.length data - len - 4));
        decode (String.sub data 4 len))
      else
        Error
          (Fmt.str "albatross short reply: received %u bytes, expected %u bytes"
             (String.length data) (len + 4))
    else
      Error
        (Fmt.str "albatross short buffer (%u bytes), need at least 4 bytes"
           (String.length data))

  let split_many data =
    let rec split acc data off =
      if String.length data - off >= 4 then
        let len = Int32.to_int (String.get_int32_be data off) in
        if String.length data - off >= 4 + len then
          split (String.sub data (4 + off) len :: acc) data (off + len + 4)
        else (
          Logs.warn (fun m ->
              m "albatross buffer too small: %u bytes, requires %u bytes"
                (String.length data - 4 - off)
                len);
          acc)
      else if String.length data = off then acc
      else (
        Logs.warn (fun m ->
            m "albatross buffer too small: %u bytes leftover"
              (String.length data - off));
        acc)
    in
    split [] data 0 |> List.rev

  let rec continue_reading name f d flow =
    let open Lwt.Infix in
    let bufs = split_many d in
    let r =
      List.fold_left
        (fun acc d ->
          match acc with Error () -> Error () | Ok () -> f (decode d))
        (Ok ()) bufs
    in
    match r with
    | Error () -> TLS.close flow >>= fun () -> Lwt.return (Ok ())
    | Ok () -> (
        TLS.read flow >>= fun r ->
        match r with
        | Ok (`Data d) -> continue_reading name f (Cstruct.to_string d) flow
        | Ok `Eof ->
            Logs.info (fun m ->
                m "albatross received eof while reading stream %a"
                  Vmm_core.Name.pp name);
            TLS.close flow >>= fun () ->
            Lwt.return (Error "albatross received eof while reading stream")
        | Error e ->
            Logs.err (fun m ->
                m "albatross received error while reading stream %a: %a"
                  Vmm_core.Name.pp name TLS.pp_error e);
            TLS.close flow >>= fun () ->
            Lwt.return (Error "albatross received an error"))

  let reply tls_flow d =
    let open Lwt.Infix in
    TLS.close tls_flow >|= fun () -> decode_reply d

  let console name f tls_flow d =
    let dec = function
      | Error s ->
          Logs.err (fun m ->
              m "albatross stop reading console %a: error %s" Vmm_core.Name.pp
                name s);
          Error ()
      | Ok (_, `Data (`Console_data (ts, data))) -> f (ts, data)
      | Ok (_, `Success (`String _)) ->
          (* ignore the success subscribed *)
          Ok ()
      | Ok w ->
          Logs.warn (fun m ->
              m "albatross unexpected reply, need console output, got %a"
                (Vmm_commands.pp_wire ~verbose:false)
                w);
          Ok ()
    in
    continue_reading name dec d tls_flow

  let block_data name f tls_flow d =
    let dec = function
      | Error s ->
          Logs.err (fun m ->
              m "albatross stop reading block data %a: error %s"
                Vmm_core.Name.pp name s);
          Error ()
      | Ok (_, `Data (`Block_data d)) -> f d
      | Ok (_, `Success (`Block_device_image _)) ->
          (* ignore the success block_device_image *)
          Ok ()
      | Ok w ->
          Logs.warn (fun m ->
              m "albatross unexpected reply, need block data, got %a"
                (Vmm_commands.pp_wire ~verbose:false)
                w);
          Ok ()
    in
    continue_reading name dec d tls_flow

  let raw_query (stack : S.t) t ?(name = Vmm_core.Name.root) certificates cmd
      ?push f =
    let open Lwt.Infix in
    let config = t.configuration in
    S.TCP.create_connection (S.tcp stack) (config.server_ip, config.server_port)
    >>= function
    | Error e ->
        let err =
          Fmt.str
            "albatross %s connection failure %a:%u while quering %a %a: %a"
            (Configuration.name_to_str config.name)
            Ipaddr.pp config.server_ip config.server_port Vmm_core.Name.pp name
            (Vmm_commands.pp ~verbose:false)
            cmd S.TCP.pp_error e
        in
        Logs.err (fun m -> m "%s" err);
        t.status <- Status.update t.status (Status.make `Transient err);
        Lwt.return (Error err)
    | Ok flow -> (
        match
          let authenticator =
            X509.Authenticator.chain_of_trust
              ~time:(fun () -> Some (Mirage_ptime.now ()))
              [ config.certificate ]
          in
          Tls.Config.client ~authenticator ~certificates ()
        with
        | Error (`Msg msg) ->
            let err = Fmt.str "albatross TLS config error: %s" msg in
            Logs.err (fun m -> m "%s" err);
            t.status <- Status.update t.status (Status.make `Configuration err);
            Lwt.return (Error err)
        | Ok tls_config -> (
            TLS.client_of_flow tls_config flow >>= function
            | Error e ->
                S.TCP.close flow >|= fun () ->
                let err =
                  Fmt.str
                    "albatross %s establishing TLS to %a:%u while querying %a \
                     %a: %a"
                    (Configuration.name_to_str config.name)
                    Ipaddr.pp config.server_ip config.server_port
                    Vmm_core.Name.pp name
                    (Vmm_commands.pp ~verbose:false)
                    cmd TLS.pp_write_error e
                in
                Logs.err (fun m -> m "%s" err);
                t.status <- Status.update t.status (Status.make `Transient err);
                Error err
            | Ok tls_flow -> (
                let handle_res () = function
                  | Ok () -> Lwt.return (Ok ())
                  | Error `Closed ->
                      let err = "TLS write error: connection closed" in
                      Logs.err (fun m -> m "Albatross: %s" err);
                      t.status <-
                        Status.update t.status (Status.make `Transient err);
                      Lwt.return (Error err)
                  | Error (`Write err) ->
                      let err =
                        Fmt.str "TLS write error (underlying TCP): %a"
                          S.TCP.pp_write_error err
                      in
                      Logs.err (fun m -> m "Albatross: %s" err);
                      t.status <-
                        Status.update t.status (Status.make `Transient err);
                      Lwt.return (Error err)
                  | Error e ->
                      let err =
                        Fmt.str "TLS write error: %a" TLS.pp_write_error e
                      in
                      Logs.err (fun m -> m "Albatross: %s" err);
                      t.status <-
                        Status.update t.status (Status.make `Transient err);
                      Lwt.return (Error err)
                in
                let write_one data =
                  let buf = Cstruct.create (4 + String.length data) in
                  Cstruct.BE.set_uint32 buf 0
                    (Int32.of_int (String.length data));
                  Cstruct.blit_from_string data 0 buf 4 (String.length data);
                  TLS.write tls_flow buf >>= handle_res ()
                in
                let rec send_data push =
                  push () >>= function
                  | None ->
                      (* send trailing 0 byte chunk *)
                      let buf = Cstruct.create 4 in
                      Cstruct.BE.set_uint32 buf 0 0l;
                      TLS.write tls_flow buf >>= handle_res ()
                  | Some data -> (
                      write_one data >>= function
                      | Error err -> Lwt.return (Error err)
                      | Ok () -> send_data push)
                in
                (match push with
                | None -> Lwt.return (Ok ())
                | Some f -> send_data f)
                >>= function
                | Ok () -> (
                    TLS.read tls_flow >>= fun r ->
                    match r with
                    | Ok (`Data d) ->
                        set_online t;
                        f tls_flow (Cstruct.to_string d)
                    | Ok `Eof ->
                        TLS.close tls_flow >|= fun () ->
                        let err =
                          Fmt.str "eof from albatross %s %a:%u querying %a %a"
                            (Configuration.name_to_str config.name)
                            Ipaddr.pp config.server_ip config.server_port
                            Vmm_core.Name.pp name
                            (Vmm_commands.pp ~verbose:false)
                            cmd
                        in
                        t.status <-
                          Status.update t.status (Status.make `Transient err);
                        Error err
                    | Error e ->
                        TLS.close tls_flow >|= fun () ->
                        let err =
                          Fmt.str
                            "albatross %s received error reading from %a:%u \
                             querying %a %a: %a"
                            (Configuration.name_to_str config.name)
                            Ipaddr.pp config.server_ip config.server_port
                            Vmm_core.Name.pp name
                            (Vmm_commands.pp ~verbose:false)
                            cmd TLS.pp_error e
                        in
                        t.status <-
                          Status.update t.status (Status.make `Transient err);
                        Error err)
                | Error err ->
                    t.status <-
                      Status.update t.status (Status.make `Transient err);
                    Lwt.return_error err)))

  let init stack (configuration : Configuration.t) =
    let open Lwt.Infix in
    let state =
      { status = Status.Online; policies = Vmm_trie.empty; configuration }
    in
    let cmd = `Policy_cmd `Policy_info in
    match
      key_cert ~is_ca:false ~cmd configuration.private_key "."
        (X509.Certificate.subject configuration.certificate)
    with
    | Error s ->
        Logs.warn (fun m ->
            m "init: key_cert failed for %s: %s"
              (Configuration.name_to_str configuration.name)
              s);
        let error =
          Status.make `Configuration
            (Fmt.str "Initialisation error: key_cert failed for %s with %s"
               (Configuration.name_to_str configuration.name)
               s)
        in
        state.status <- Offline [ error ];
        Lwt.return state
    | Ok (key, cert) -> (
        let certificates = `Single ([ cert ], key) in
        raw_query stack state certificates cmd reply >>= function
        | Ok (_hdr, `Success (`Policies ps)) ->
            let policies =
              List.fold_left
                (fun trie (name, p) -> fst (Vmm_trie.insert name p trie))
                Vmm_trie.empty ps
            in
            state.policies <- policies;
            Lwt.return state
        | Ok w ->
            let err =
              Fmt.str "Unexpected policy reply for %s: %a"
                (Configuration.name_to_str configuration.name)
                (Vmm_commands.pp_wire ~verbose:false)
                w
            in
            Logs.warn (fun m -> m "%s" err);
            let error = Status.make `Internal err in
            state.status <- Status.update state.status error;
            Lwt.return state
        | Error str ->
            Logs.warn (fun m ->
                m "init: raw_query failed for %s: %s"
                  (Configuration.name_to_str configuration.name)
                  str);
            let error =
              Status.make `Configuration
                (Fmt.str "Initialisation error: raw_qeury failed for %s with %s"
                   (Configuration.name_to_str configuration.name)
                   str)
            in
            state.status <- Status.update state.status error;
            Lwt.return state)

  let init_all stack (configs : Configuration.t list) =
    let open Lwt.Infix in
    Lwt_list.fold_left_s
      (fun acc_map (configuration : Configuration.t) ->
        init stack configuration >|= fun state ->
        Albatross_map.add configuration.name state acc_map)
      Albatross_map.empty configs

  let find_instance_by_name (albatross_map : a_map) name =
    match Albatross_map.find_opt name albatross_map with
    | Some instance_state -> Ok instance_state
    | None ->
        Error
          (Fmt.str "No albatross instance found with the name %s"
             (Configuration.name_to_str name))

  let certs t domain name cmd =
    match
      Result.bind (Vmm_core.Name.path_of_string domain) (fun path ->
          Vmm_core.Name.create path name)
    with
    | Error (`Msg msg) -> Error msg
    | Ok vmm_name ->
        Result.map (fun c -> (vmm_name, c)) (gen_cert t ~domain cmd name)

  let query stack t ~domain ?(name = ".") ?push cmd =
    match certs t domain name cmd with
    | Error str ->
        t.status <- Status.update t.status (Status.make `Configuration str);
        Lwt.return (Error str)
    | Ok (vmm_name, certificates) ->
        set_online t;
        raw_query stack t ~name:vmm_name certificates cmd ?push reply

  let query_console stack t ~domain ~name f =
    let cmd = `Console_cmd (`Console_subscribe (`Count 40)) in
    match certs t domain name cmd with
    | Error str -> Lwt.return (Error str)
    | Ok (vmm_name, certificates) ->
        raw_query stack t ~name:vmm_name certificates cmd (console vmm_name f)

  let query_block_dump stack t ~domain ~name compression f =
    let cmd = `Block_cmd (`Block_dump compression) in
    match certs t domain name cmd with
    | Error str -> Lwt.return (Error str)
    | Ok (vmm_name, certificates) ->
        raw_query stack t ~name:vmm_name certificates cmd
          (block_data vmm_name f)

  let set_policy stack t ~domain policy =
    let open Lwt.Infix in
    match Vmm_core.Name.path_of_string domain with
    | Error (`Msg msg) -> Lwt.return (Error ("couldn't set policy: " ^ msg))
    | Ok p -> (
        let old_policies = t.policies in
        let name = Vmm_core.Name.create_of_path p in
        (* we set it locally - which is then used for the next command *)
        t.policies <- fst (Vmm_trie.insert name policy t.policies);
        (* now we tell albatross about it, using a command for throwing it away *)
        (* note that the 'certs' / 'gen_cert' uses the policies for intermediate certificates *)
        query stack t ~domain (`Unikernel_cmd `Unikernel_info) >|= function
        | Ok _ -> Ok (name, policy)
        | Error msg ->
            Logs.warn (fun m -> m "error updating policies: %s" msg);
            t.policies <- old_policies;
            Error msg)
end
