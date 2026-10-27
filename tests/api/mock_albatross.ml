open Test_utils

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
        started = Ptime_clock.now ();
      }
  in
  let info = Vmm_core.Unikernel.info (fun _ -> None) dummy_u in
  (name, info)

let known_unikernel_label = label_of_string_exn "hello"

let success_config =
  {
    mock_albatross_config with
    server_ip = Ipaddr.of_string_exn "127.0.0.1";
    server_port = 1025;
  }

let failure_config =
  {
    mock_albatross_config with
    name = label_of_string_exn "failing";
    server_ip = Ipaddr.of_string_exn "127.0.0.1";
    server_port = 1026;
  }

let make_default_policies ~domain ?(unikernels = 10) ?(memory = 1024)
    ?(block = Some 2048) ?(bridges = [ "service" ]) ?(cpuids = [ 0; 1 ]) () =
  let path = Vmm_core.Name.Path.of_label domain in
  let name = Vmm_core.Name.make_of_path path in
  let root_policy : Vmm_core.Policy.t =
    {
      unikernels = unikernels * 2;
      cpuids = Vmm_core.IS.of_list cpuids;
      memory = memory * 2;
      block;
      bridges = Vmm_core.String_set.of_list bridges;
    }
  in
  let policy : Vmm_core.Policy.t =
    {
      unikernels;
      cpuids = Vmm_core.IS.of_list cpuids;
      memory;
      block = Option.map (fun b -> b / 2) block;
      bridges = Vmm_core.String_set.of_list bridges;
    }
  in
  let trie =
    fst (Vmm_trie.insert Vmm_core.Name.root root_policy Vmm_trie.empty)
  in
  fst (Vmm_trie.insert name policy trie)

let default_instances ?(policies = Vmm_trie.empty) () =
  let make configuration : Albatross_client.t =
    { configuration; policies; status = Albatross_client.Status.Online }
  in
  Utils.LM.empty
  |> Utils.LM.add success_config.name (make success_config)
  |> Utils.LM.add failure_config.name (make failure_config)

let eval_success (instance : Albatross_client.t) (name : Vmm_core.Name.t)
    (cmd : Vmm_commands.t) :
    (Vmm_commands.header * Vmm_commands.res, string) result =
  let hdr = Vmm_commands.header name in
  if
    String.starts_with ~prefix:"failing"
      (Models.Configuration.name_to_str instance.configuration.name)
    || instance.configuration.server_port = failure_config.server_port
  then Ok (hdr, `Failure "albatross failure")
  else
    match cmd with
    | `Unikernel_cmd `Unikernel_info -> (
        let hello_name =
          Vmm_core.Name.make (Vmm_core.Name.path name) known_unikernel_label
        in
        match Vmm_core.Name.name name with
        | None -> Ok (hdr, `Success (`Unikernel_info [ dummy_info hello_name ]))
        | Some label ->
            if Vmm_core.Name.Label.equal label known_unikernel_label then
              Ok (hdr, `Success (`Unikernel_info [ dummy_info hello_name ]))
            else Ok (hdr, `Success (`Unikernel_info [])))
    | `Unikernel_cmd (`Unikernel_create _) ->
        Ok (hdr, `Success (`String "unikernel created"))
    | `Unikernel_cmd (`Unikernel_force_create _) ->
        Ok (hdr, `Success (`String "unikernel force created"))
    | `Unikernel_cmd `Unikernel_destroy ->
        Ok (hdr, `Success (`String "unikernel destroyed"))
    | `Unikernel_cmd (`Unikernel_restart _) ->
        Ok (hdr, `Success (`String "unikernel restarted"))
    | `Block_cmd `Block_info -> Ok (hdr, `Success (`Block_devices []))
    | `Block_cmd (`Block_add _) ->
        Ok (hdr, `Success (`String "block device added"))
    | `Block_cmd (`Block_set _) ->
        Ok (hdr, `Success (`String "block device set"))
    | `Block_cmd `Block_remove ->
        Ok (hdr, `Success (`String "block device removed"))
    | `Block_cmd (`Block_dump _) ->
        Ok
          ( hdr,
            `Data (`Block_data (Some "mock block dump content\n\r\n\rEOF\n\r"))
          )
    | `Policy_cmd `Policy_info ->
        let policies = Vmm_trie.all instance.policies in
        Ok (hdr, `Success (`Policies policies))
    | _ -> Ok (hdr, `Success `Empty)
