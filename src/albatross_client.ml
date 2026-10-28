let ( let* ) = Result.bind

module Status = struct
  type category = [ `Certificate | `Network | `Credentials | `Incompatible ]
  type error = { timestamp : Ptime.t; category : category; details : string }
  type t = Online | Degraded of { retries : int; log : error list }

  let to_string = function
    | Online -> "online"
    | Degraded { retries; _ } -> Fmt.str "degraded (%d retries)" retries

  let make category details = { timestamp = Utils.now (); category; details }

  let update status new_error =
    match status with
    | Online -> Degraded { retries = 1; log = [ new_error ] }
    | Degraded { retries; log } ->
        Degraded { retries = retries + 1; log = new_error :: log }

  let pp_error ppf { timestamp; category; details } =
    let category_str =
      match category with
      | `Certificate -> "certificate"
      | `Network -> "network"
      | `Credentials -> "credentials"
      | `Incompatible -> "incompatible"
    in
    Fmt.pf ppf "[%a] %s: %s" Ptime.pp timestamp category_str details
end

type t = {
  mutable policies : Vmm_core.Policy.t Vmm_trie.t;
  configuration : Models.Configuration.t;
  mutable status : Status.t;
}

let set_online t = t.status <- Online

module String_set = Set.Make (String)

type a_map = t Utils.LM.t

let empty_policy =
  Vmm_core.Policy.
    {
      unikernels = 0;
      cpuids = Vmm_core.IS.empty;
      memory = 0;
      block = None;
      bridges = Vmm_core.String_set.empty;
    }

let dot_name = Result.get_ok (Vmm_core.Name.Label.of_string ".")

let policy ?domain t =
  let path =
    Option.value ~default:Vmm_core.Name.root
      (Option.map
         (fun d -> Vmm_core.Name.make_of_path (Vmm_core.Name.Path.of_label d))
         domain)
  in
  Vmm_trie.find path t.policies

let all_policies ?domain t = Utils.LM.map (policy ?domain) t

let policy_resource_avalaible t =
  match policy t with
  | None ->
      Logs.err (fun m -> m "policy error: empty root policy");
      Error "root policy is empty"
  | Some root_policy ->
      let unikernels_used, memory_used, storage_used =
        Vmm_trie.fold Vmm_core.Name.Path.root t.policies
          (fun name_ policy (total_vms, total_memory, total_block) ->
            if not Vmm_core.Name.(equal name_ root) then
              ( total_vms + policy.Vmm_core.Policy.unikernels,
                total_memory + policy.memory,
                total_block + Option.value ~default:0 policy.block )
            else (total_vms, total_memory, total_block))
          (0, 0, 0)
      in
      Ok
        Vmm_core.Policy.
          {
            unikernels = root_policy.unikernels - unikernels_used;
            cpuids = root_policy.cpuids;
            memory = root_policy.memory - memory_used;
            block = Option.map (fun b -> b - storage_used) root_policy.block;
            bridges = root_policy.bridges;
          }

let key_ids exts pub issuer =
  let open X509 in
  let auth = (Some (Public_key.id issuer), General_name.empty, None) in
  Extension.(
    add Subject_key_id
      (false, Public_key.id pub)
      (add Authority_key_id (false, auth) exts))

let timestamps validity =
  let now = Utils.now () in
  match
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
    Extension.(add Basic_constraints (true, bc) (add Key_usage (true, ku) ext))
  in
  let tmp_key = Private_key.generate key_type in
  match
    let name =
      [
        Distinguished_name.(
          Relative_distinguished_name.singleton
            (CN (Common_name.v (Vmm_core.Name.Label.to_string name))));
      ]
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
        Signing_request.sign csr ~valid_from ~valid_until ~extensions key issuer
      with
      | Error e ->
          Error
            (Fmt.str "albatross: failed to sign %ssigning request: %a"
               (if is_ca then "CA " else "")
               X509.Validation.pp_signature_error e)
      | Ok mycert -> Ok (tmp_key, mycert))

let gen_cert t ?domain cmd name =
  let c = t.configuration in
  let* ikey, cert, certs =
    match domain with
    | None -> Ok (c.private_key, c.certificate, [])
    | Some domain ->
        let policy = Option.value ~default:empty_policy (policy ~domain t) in
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

let rec continue_reading name f flow =
  match Vmm_tls_mnet.read_tls flow with
  | Ok wire -> (
      match f (Ok wire) with
      | Ok () -> continue_reading name f flow
      | Error () ->
          Vmm_tls_mnet.close flow;
          Ok ())
  | Error `Eof | Error `Tls_eof ->
      Logs.info (fun m -> m "albatross stream EOF %a" Vmm_core.Name.pp name);
      Vmm_tls_mnet.close flow;
      Ok ()
  | Error `Exception | Error `Toomuch ->
      Logs.err (fun m -> m "albatross stream error %a" Vmm_core.Name.pp name);
      Vmm_tls_mnet.close flow;
      Error "albatross received an error"

let reply tls_flow =
  let res =
    match Vmm_tls_mnet.read_tls tls_flow with
    | Ok wire -> Ok wire
    | Error `Eof | Error `Tls_eof -> Error "EOF while reading reply"
    | Error `Exception -> Error "Exception while reading reply"
    | Error `Toomuch -> Error "Read too much data from albatross"
  in
  Vmm_tls_mnet.close tls_flow;
  res

let console name f tls_flow =
  let dec = function
    | Error s ->
        Logs.err (fun m ->
            m "albatross stop reading console %a: error %s" Vmm_core.Name.pp
              name s);
        Error ()
    | Ok (_, `Data (`Console_data (ts, data))) -> f (ts, data)
    | Ok (_, `Success (`String _)) -> Ok ()
    | Ok w ->
        Logs.warn (fun m ->
            m "albatross unexpected reply, need console output, got %a"
              (Vmm_commands.pp_wire ~verbose:false)
              w);
        Ok ()
  in
  continue_reading name dec tls_flow

let block_data name f tls_flow =
  let dec = function
    | Error s ->
        Logs.err (fun m ->
            m "albatross stop reading block data %a: error %s" Vmm_core.Name.pp
              name s);
        Error ()
    | Ok (_, `Data (`Block_data d)) -> f d
    | Ok (_, `Success (`Block_device_image _)) -> Ok ()
    | Ok w ->
        Logs.warn (fun m ->
            m "albatross unexpected reply, need block data, got %a"
              (Vmm_commands.pp_wire ~verbose:false)
              w);
        Ok ()
  in
  continue_reading name dec tls_flow

let stats_data name
    (f : Vmm_core.Name.t -> Vmm_core.Stats.t -> (unit, unit) result) tls_flow =
  let dec = function
    | Error s ->
        Logs.err (fun m ->
            m "albatross stop reading stats data %a: error %s" Vmm_core.Name.pp
              name s);
        Error ()
    | Ok (hdr, `Data (`Stats_data d)) -> f hdr.Vmm_commands.name d
    | Ok (hdr, `Data (`Old_stats_data (ru, mem, _vmm, ifd))) ->
        f hdr.Vmm_commands.name (ru, mem, ifd)
    | Ok (_, `Success (`String _)) -> Ok ()
    | Ok w ->
        Logs.warn (fun m ->
            m "albatross unexpected reply, need stats data, got %a"
              (Vmm_commands.pp_wire ~verbose:false)
              w);
        Ok ()
  in
  continue_reading name dec tls_flow

let get_unikernel_binary name f tls_flow =
  let dec = function
    | Error s ->
        Logs.err (fun m ->
            m "albatross stop reading unikernel binary %a: error %s"
              Vmm_core.Name.pp name s);
        Error ()
    | Ok (_hdr, `Success (`Unikernel_image (_compressed, data))) ->
        let _ = f data in
        Error ()
    | Ok w ->
        Logs.warn (fun m ->
            m "albatross unexpected reply, need unikernel binary, got %a"
              (Vmm_commands.pp_wire ~verbose:false)
              w);
        Ok ()
  in
  continue_reading name dec tls_flow

let connect_tls tcp t certificates =
  let config = t.configuration in
  let config_name = Models.Configuration.name_to_str config.name in
  let tcp_flow =
    try
      Ok
        (Mnet.TCP.connect ~kind:Mnet.TCP.direct tcp
           (config.server_ip, config.server_port))
    with
    | Mnet.TCP.Net_unreach ->
        let err =
          Fmt.str "Network unreachable connecting to %a:%u" Ipaddr.pp
            config.server_ip config.server_port
        in
        Logs.err (fun m -> m "albatross %s: %s" config_name err);
        t.status <- Status.update t.status (Status.make `Network err);
        Error err
    | Mnet.TCP.Connection_refused ->
        let err =
          Fmt.str "Connection refused connecting to %a:%u" Ipaddr.pp
            config.server_ip config.server_port
        in
        Logs.err (fun m -> m "albatross %s: %s" config_name err);
        t.status <- Status.update t.status (Status.make `Network err);
        Error err
    | exn ->
        let err = Fmt.str "TCP connection error: %s" (Printexc.to_string exn) in
        Logs.err (fun m -> m "albatross %s: %s" config_name err);
        t.status <- Status.update t.status (Status.make `Network err);
        Error err
  in
  match tcp_flow with
  | Error _ as e -> e
  | Ok tcp_flow -> (
      let authenticator =
        X509.Authenticator.chain_of_trust
          ~time:(fun () -> Some (Utils.now ()))
          [ config.certificate ]
      in
      match Tls.Config.client ~authenticator ~certificates () with
      | Error (`Msg msg) ->
          Mnet.TCP.close tcp_flow;
          let err = Fmt.str "TLS configuration error: %s" msg in
          Logs.err (fun m -> m "albatross %s: %s" config_name err);
          t.status <- Status.update t.status (Status.make `Credentials err);
          Error err
      | Ok tls_config -> (
          try Ok (Mnet_tls.client_of_fd tls_config tcp_flow) with
          | End_of_file ->
              Mnet.TCP.close tcp_flow;
              let err = "EOF during TLS handshake" in
              Logs.err (fun m -> m "albatross %s: %s" config_name err);
              t.status <- Status.update t.status (Status.make `Credentials err);
              Error err
          | exn ->
              Mnet.TCP.close tcp_flow;
              let err =
                Fmt.str "TLS handshake error: %s" (Printexc.to_string exn)
              in
              Logs.err (fun m -> m "albatross %s: %s" config_name err);
              t.status <- Status.update t.status (Status.make `Credentials err);
              Error err))

let send_push t tls_flow push_fn =
  let config_name = Models.Configuration.name_to_str t.configuration.name in
  let rec loop () =
    match push_fn () with
    | None -> (
        match Vmm_tls_mnet.write_tls_chunk tls_flow "" with
        | Ok () -> Ok ()
        | Error `Exception ->
            let err = "Error sending trailing chunk" in
            Logs.err (fun m -> m "albatross %s: %s" config_name err);
            t.status <- Status.update t.status (Status.make `Network err);
            Error err)
    | Some data -> (
        match Vmm_tls_mnet.write_tls_chunk tls_flow data with
        | Ok () -> loop ()
        | Error `Exception ->
            let err = "Error pushing data chunk" in
            Logs.err (fun m -> m "albatross %s: %s" config_name err);
            t.status <- Status.update t.status (Status.make `Network err);
            Error err)
  in
  loop ()

let raw_query tcp t ?(name = Vmm_core.Name.root) certificates cmd ?push f =
  let config_name = Models.Configuration.name_to_str t.configuration.name in
  let* tls_flow = connect_tls tcp t certificates in
  let push_res =
    match push with
    | None -> Ok ()
    | Some push_fn -> send_push t tls_flow push_fn
  in
  match push_res with
  | Error _ as e ->
      Vmm_tls_mnet.close tls_flow;
      e
  | Ok () -> (
      match f tls_flow with
      | Ok res -> Ok res
      | Error msg ->
          Vmm_tls_mnet.close tls_flow;
          Logs.err (fun m ->
              m "albatross %s: error querying %a %a: %s" config_name
                Vmm_core.Name.pp name
                (Vmm_commands.pp ~verbose:false)
                cmd msg);
          t.status <- Status.update t.status (Status.make `Incompatible msg);
          Error msg)

let init tcp (configuration : Models.Configuration.t) =
  let state =
    { status = Status.Online; policies = Vmm_trie.empty; configuration }
  in
  let cmd = `Policy_cmd `Policy_info in
  match
    key_cert ~is_ca:false ~cmd configuration.private_key dot_name
      (X509.Certificate.subject configuration.certificate)
  with
  | Error s ->
      let err = Fmt.str "Initialisation error: key_cert failed %s" s in
      Logs.warn (fun m ->
          m "albatross %s: %s"
            (Models.Configuration.name_to_str configuration.name)
            err);
      state.status <- Status.update state.status (Status.make `Certificate err);
      Error (state, err)
  | Ok (key, cert) -> (
      let certificates = `Single ([ cert ], key) in
      match raw_query tcp state certificates cmd reply with
      | Ok (_hdr, `Success (`Policies ps)) ->
          let policies =
            List.fold_left
              (fun trie (name, p) -> fst (Vmm_trie.insert name p trie))
              Vmm_trie.empty ps
          in
          state.policies <- policies;
          Ok state
      | Ok w ->
          let err =
            Fmt.str "Initialisation error: unexpected policy reply: %a"
              (Vmm_commands.pp_wire ~verbose:false)
              w
          in
          Logs.warn (fun m ->
              m "albatross %s: %s"
                (Models.Configuration.name_to_str configuration.name)
                err);
          state.status <-
            Status.update state.status (Status.make `Incompatible err);
          Error (state, err)
      | Error str ->
          let err =
            Fmt.str "Initialisation error: raw_query failed with %s" str
          in
          Logs.warn (fun m ->
              m "albatross %s: %s"
                (Models.Configuration.name_to_str configuration.name)
                err);
          Error (state, err))

let init_all tcp (configs : Models.Configuration.t Utils.LM.t) : a_map =
  Utils.LM.fold
    (fun _ (configuration : Models.Configuration.t) acc_map ->
      match init tcp configuration with
      | Error (state, msg) ->
          Logs.err (fun m ->
              m "albatross: failed to init instance %s: %s"
                (Models.Configuration.name_to_str configuration.name)
                msg);
          Utils.LM.add configuration.name state acc_map
      | Ok state -> Utils.LM.add configuration.name state acc_map)
    configs Utils.LM.empty

let find_instance_by_name (albatross_map : a_map) name =
  match Utils.LM.find_opt name albatross_map with
  | Some instance_state -> Ok instance_state
  | None ->
      Error
        (Fmt.str "No albatross instance found with the name %s"
           (Models.Configuration.name_to_str name))

let certs t domain name cmd =
  let vmm_name = Vmm_core.Name.make (Vmm_core.Name.Path.of_label domain) name in
  Result.map_error
    (fun msg ->
      t.status <- Status.update t.status (Status.make `Certificate msg);
      msg)
    (Result.map (fun c -> (vmm_name, c)) (gen_cert t ~domain cmd name))

let query tcp t ~domain ?(name = dot_name) ?push cmd =
  match certs t domain name cmd with
  | Error str -> Error str
  | Ok (vmm_name, certificates) ->
      raw_query tcp t ~name:vmm_name certificates cmd ?push reply

let query_console tcp t ~domain ~name f =
  let cmd = `Console_cmd (`Console_subscribe (`Count 40)) in
  match certs t domain name cmd with
  | Error str -> Error str
  | Ok (vmm_name, certificates) ->
      raw_query tcp t ~name:vmm_name certificates cmd (console vmm_name f)

let query_block_dump tcp t ~domain ~name compression f =
  let cmd = `Block_cmd (`Block_dump compression) in
  match certs t domain name cmd with
  | Error str -> Error str
  | Ok (vmm_name, certificates) ->
      raw_query tcp t ~name:vmm_name certificates cmd (block_data vmm_name f)

let query_unikernel_binary tcp t ~domain ~name f =
  let cmd = `Unikernel_cmd (`Unikernel_get 4) in
  match certs t domain name cmd with
  | Error str -> Error str
  | Ok (vmm_name, certificates) ->
      raw_query tcp t ~name:vmm_name certificates cmd
        (get_unikernel_binary vmm_name f)

let query_stats tcp t f =
  let cmd = `Stats_cmd `Stats_subscribe in
  let vmm_name = Vmm_core.Name.root in
  match gen_cert t cmd dot_name with
  | Error err ->
      t.status <- Status.update t.status (Status.make `Certificate err);
      Error err
  | Ok certificates ->
      set_online t;
      raw_query tcp t ~name:vmm_name certificates cmd (stats_data vmm_name f)

let set_policy tcp t ~domain policy =
  let old_policies = t.policies in
  let name = Vmm_core.Name.make_of_path (Vmm_core.Name.Path.of_label domain) in
  t.policies <- fst (Vmm_trie.insert name policy t.policies);
  match query tcp t ~domain (`Unikernel_cmd `Unikernel_info) with
  | Ok (_hdr, `Failure msg) ->
      Logs.warn (fun m -> m "error updating policies: %s" msg);
      t.policies <- old_policies;
      Error msg
  | Ok _ -> Ok (name, policy)
  | Error msg ->
      Logs.warn (fun m -> m "error updating policies: %s" msg);
      t.policies <- old_policies;
      Error msg

let make_device store_device get_tcp =
  let finally _ = () in
  let open Vifu.Device in
  v ~name:"albatross" ~finally [ value store_device ]
  @@ fun (store : Storage.t) env ->
  ref (init_all (get_tcp env) store.configurations)

let user_blocks_by_instance tcp (state : t) user_name =
  match query tcp state ~domain:user_name (`Block_cmd `Block_info) with
  | Error _msg -> []
  | Ok (_hdr, `Success (`Block_devices blocks)) ->
      set_online state;
      blocks
  | Ok reply ->
      Logs.err (fun m ->
          m "Expected block info reply, got %a"
            (Vmm_commands.pp_wire ~verbose:false)
            reply);
      []

let user_unikernels_by_instance tcp (state : t) user_name =
  match query tcp state ~domain:user_name (`Unikernel_cmd `Unikernel_info) with
  | Error _msg -> []
  | Ok (_hdr, `Success (`Old_unikernel_info4 unikernels))
  | Ok (_hdr, `Success (`Old_unikernel_info5 unikernels))
  | Ok (_hdr, `Success (`Unikernel_info unikernels)) ->
      set_online state;
      unikernels
  | Ok reply ->
      Logs.err (fun m ->
          m "Expected unikernel info reply, got %a"
            (Vmm_commands.pp_wire ~verbose:false)
            reply);
      []

let user_deceased_by_instance tcp (albatross_instances : a_map) user_name =
  Utils.LM.map
    (fun (instance : t) ->
      match
        query tcp instance ~domain:user_name ~name:dot_name
          (`Console_cmd `Console_list_inactive)
      with
      | Error _msg -> []
      | Ok (_hdr, `Success (`Consoles names)) ->
          set_online instance;
          names
      | Ok _reply -> [])
    albatross_instances

let user_unikernels tcp (albatross_instances : a_map) user_name =
  Utils.LM.map
    (fun (instance : t) ->
      match
        query tcp instance ~domain:user_name (`Unikernel_cmd `Unikernel_info)
      with
      | Error _msg -> []
      | Ok (_hdr, `Success (`Old_unikernel_info4 unikernels))
      | Ok (_hdr, `Success (`Old_unikernel_info5 unikernels))
      | Ok (_hdr, `Success (`Unikernel_info unikernels)) ->
          set_online instance;
          unikernels
      | Ok _reply -> [])
    albatross_instances

let user_max_allowed_unikernel_instances albatross user_name =
  Option.fold ~none:0
    ~some:(fun (user_policy : Vmm_core.Policy.t) -> user_policy.unikernels)
    (policy albatross ~domain:user_name)

let user_unikernel tcp state ~user_name ~unikernel_name =
  match
    query tcp state ~domain:user_name ~name:unikernel_name
      (`Unikernel_cmd `Unikernel_info)
  with
  | Error err ->
      Error
        (Fmt.str "Error fetching '%s' from '%s': %s"
           (Models.Configuration.name_to_str unikernel_name)
           (Models.Configuration.name_to_str state.configuration.name)
           err)
  | Ok (_hdr, `Success (`Unikernel_info [ unikernel ]))
  | Ok (_hdr, `Success (`Old_unikernel_info4 [ unikernel ]))
  | Ok (_hdr, `Success (`Old_unikernel_info5 [ unikernel ])) ->
      set_online state;
      Ok unikernel
  | Ok (_hdr, `Success (`Unikernel_info unikernels))
  | Ok (_hdr, `Success (`Old_unikernel_info4 unikernels))
  | Ok (_hdr, `Success (`Old_unikernel_info5 unikernels)) ->
      let message =
        Fmt.str "Expected one unikernel, but got %u" (List.length unikernels)
      in
      Error message
  | Ok reply ->
      let message =
        Fmt.str "Expected unikernel info for '%s', got %a"
          (Models.Configuration.name_to_str unikernel_name)
          (Vmm_commands.pp_wire ~verbose:false)
          reply
      in
      Error message
