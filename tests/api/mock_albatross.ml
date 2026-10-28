open Test_utils

let make_default_policies ~domain ?(unikernels = 10) ?(memory = 1024)
    ?(block = Some 2048) ?(cpuids = [ 0; 1 ]) () =
  let path = Vmm_core.Name.Path.of_label domain in
  let name = Vmm_core.Name.make_of_path path in
  let root_policy : Vmm_core.Policy.t =
    {
      unikernels = unikernels * 2;
      cpuids = Vmm_core.IS.of_list cpuids;
      memory = memory * 2;
      block;
      bridges = Vmm_core.String_set.of_list [ "service" ];
    }
  in
  let policy : Vmm_core.Policy.t =
    {
      unikernels;
      cpuids = Vmm_core.IS.of_list cpuids;
      memory;
      block = Option.map (fun b -> b / 2) block;
      bridges = Vmm_core.String_set.of_list [ "service" ];
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
  |> Utils.LM.add success_cfg.name (make success_cfg)
  |> Utils.LM.add failure_cfg.name (make failure_cfg)
