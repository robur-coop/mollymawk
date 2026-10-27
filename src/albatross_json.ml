let j_obj xs =
  Jsont.Json.object'
    (List.map (fun (k, v) -> Jsont.Json.mem (Jsont.Json.name k) v) xs)

let get_mem key mems = Option.map snd (Jsont.Json.find_mem key mems)

let int_of_json = function
  | Jsont.Number (f, _) when Float.is_integer f -> Some (int_of_float f)
  | _ -> None

let fail_behaviour = function
  | `Quit -> Jsont.Json.string "quit"
  | `Restart ex ->
      let els = Option.value ~default:[] (Option.map Vmm_core.IS.elements ex) in
      j_obj
        [
          ("restart", Jsont.Json.null ());
          ("exit_code", Jsont.Json.list (List.map Jsont.Json.int els));
          ("all_exit_codes", Jsont.Json.bool (ex = None));
        ]

let cpuids cs =
  Jsont.Json.list (List.map Jsont.Json.int (Vmm_core.IS.elements cs))

let memory m = Jsont.Json.int m

let argv args =
  Jsont.Json.list (List.map Jsont.Json.string (Option.value ~default:[] args))

let block_devices bs =
  let block
      { Vmm_core.Unikernel.unikernel_device; host_device; sector_size; size } =
    j_obj
      [
        ("name", Jsont.Json.string unikernel_device);
        ("host_device", Jsont.Json.string host_device);
        ("sector_size", Jsont.Json.int sector_size);
        ("size", Jsont.Json.int size);
      ]
  in
  Jsont.Json.list (List.map block bs)

let bridges bs =
  let bridge
      ({ Vmm_core.Unikernel.unikernel_device; host_device; _ } :
        Vmm_core.Unikernel.net_info) =
    j_obj
      [
        ("name", Jsont.Json.string unikernel_device);
        ("host_device", Jsont.Json.string host_device);
      ]
  in
  Jsont.Json.list (List.map bridge bs)

let unikernel_info (unikernel_name, info) =
  j_obj
    [
      ( "name",
        Jsont.Json.string
          (Option.value ~default:""
             (Option.map Vmm_core.Name.Label.to_string
                (Vmm_core.Name.name unikernel_name))) );
      ("fail_behaviour", fail_behaviour info.Vmm_core.Unikernel.fail_behaviour);
      ("cpuids", cpuids info.cpuids);
      ("memory", memory info.memory);
      ("block_devices", block_devices info.block_devices);
      ("network_interfaces", bridges info.bridges);
      ("arguments", argv info.argv);
      ("numcpus", Jsont.Json.int info.numcpus);
      ( "linux_boot_partition",
        Jsont.Json.option Jsont.Json.string info.linux_boot_partition );
    ]

let unikernel_info_to_pretty_string u =
  Result.get_ok
    (Jsont_bytesrw.encode_string ~format:Jsont.Indent Jsont.json
       (unikernel_info u))

let unikernel_infos is = Jsont.Json.list (List.map unikernel_info is)

let block_info (name, size, used) =
  j_obj
    [
      ("name", Jsont.Json.string (Vmm_core.Name.to_string name));
      ("size", Jsont.Json.int size);
      ("used", Jsont.Json.bool used);
    ]

let block_infos bs = Jsont.Json.list (List.map block_info bs)

let policy_info (name, policy) =
  j_obj
    [
      ("name", Jsont.Json.string (Vmm_core.Name.to_string name));
      ("allowed_unikernels", Jsont.Json.int policy.Vmm_core.Policy.unikernels);
      ( "allowed_cpuids",
        Jsont.Json.list
          (List.map Jsont.Json.int (Vmm_core.IS.elements policy.cpuids)) );
      ("allowed_memory", Jsont.Json.int policy.memory);
      ( "allowed_block_size",
        Jsont.Json.int (Option.value ~default:0 policy.block) );
      ( "allowed_bridges",
        Jsont.Json.list
          (List.map Jsont.Json.string
             (Vmm_core.String_set.elements policy.bridges)) );
    ]

let policy_infos ps = Jsont.Json.list (List.map policy_info ps)

let success = function
  | `Empty -> Jsont.Json.null ()
  | `String m -> Jsont.Json.string m
  | `Policies ps -> policy_infos ps
  | `Old_unikernel_info4 is -> unikernel_infos is
  | `Old_unikernel_info5 is -> unikernel_infos is
  | `Unikernel_info is -> unikernel_infos is
  | `Block_devices bs -> block_infos bs
  | `Unikernel_image _ -> Jsont.Json.string "unikernel image not supported"
  | `Old_block_device_image (_, bd) -> Jsont.Json.string bd
  | `Block_device_image _compressed -> Jsont.Json.string "block device image"
  | `Consoles _names -> Jsont.Json.string "consoles"

let console_data_to_json (ts, data) =
  j_obj
    [
      ("timestamp", Jsont.Json.string (Ptime.to_rfc3339 ts));
      ("line", Jsont.Json.string data);
    ]

let console_data_to_string (ts, data) =
  Result.get_ok
    (Jsont_bytesrw.encode_string Jsont.json (console_data_to_json (ts, data)))

let res = function
  | `Command _ -> Error (`String "command not supported")
  | `Success s -> Ok (success s)
  | `Failure f -> Error (`String ("failure: " ^ String.escaped f))
  | `Data (`Console_data (ts, data)) -> Ok (console_data_to_json (ts, data))
  | `Data (`Utc_console_data (ts, data)) -> Ok (console_data_to_json (ts, data))
  | `Data (`Log_data _) -> Error (`String "logs not supported")
  | `Data (`Stats_data _) -> Error (`String "stats not supported")
  | `Data (`Old_stats_data _) -> Error (`String "stats not supported")
  | `Data (`Block_data _) -> Error (`String "block data not supported")

let res_to_string r =
  Result.map
    (fun j -> Result.get_ok (Jsont_bytesrw.encode_string Jsont.json j))
    (res r)

let fail_behaviour_of_json (js : Jsont.json) =
  let ( let* ) = Result.bind in
  match js with
  | Jsont.String ("quit", _) -> Ok `Quit
  | Jsont.Object (rs, _) -> (
      match
        ( get_mem "restart" rs,
          get_mem "exit_code" rs,
          get_mem "all_exit_codes" rs )
      with
      | None, _, _ -> Error (`Msg "expected a restart")
      | Some _, Some (Jsont.Array (codes, _)), _ ->
          let* codes =
            List.fold_left
              (fun acc i ->
                let* acc = acc in
                match int_of_json i with
                | Some n -> Ok (n :: acc)
                | None ->
                    Error (`Msg "fail_behaviour exit code is not an integer"))
              (Ok []) codes
          in
          Ok (`Restart (Some (Vmm_core.IS.of_list codes)))
      | Some _, Some _, _ ->
          Error (`Msg "expected a list of integers as exit_code payload")
      | Some _, _, _ -> Ok (`Restart None))
  | _ -> Error (`Msg "fail behaviour must be quit or restart")

let bridge_of_json (js : Jsont.json) =
  (* we support [ "foo" ] as well as [ { name: "foo" ; host_device: "myfoo" ; mac: "aa:bb:cc:dd:ee:ff" *)
  let ( let* ) = Result.bind in
  match js with
  | Jsont.String (bridge, _) -> Ok (bridge, None, None)
  | Jsont.Object (xs, _) -> (
      match (get_mem "name" xs, get_mem "host_device" xs, get_mem "mac" xs) with
      | None, _, _ -> Error (`Msg "name must be present in the json")
      | ( Some (Jsont.String (name, _)),
          (None | Some (Jsont.Null _)),
          (None | Some (Jsont.Null _)) ) ->
          Ok (name, None, None)
      | ( Some (Jsont.String (name, _)),
          Some (Jsont.String (host, _)),
          (None | Some (Jsont.Null _)) ) ->
          Ok (name, Some host, None)
      | ( Some (Jsont.String (name, _)),
          (None | Some (Jsont.Null _)),
          Some (Jsont.String (mac, _)) ) ->
          let* mac = Macaddr.of_string mac in
          Ok (name, None, Some mac)
      | ( Some (Jsont.String (name, _)),
          Some (Jsont.String (host, _)),
          Some (Jsont.String (mac, _)) ) ->
          let* mac = Macaddr.of_string mac in
          Ok (name, Some host, Some mac)
      | _, _, _ -> Error (`Msg "couldn't decode json"))
  | _ -> Error (`Msg "bad json, either string or assoc")

let block_device_of_json (js : Jsont.json) =
  (* we support [ "foo" ] as well as [ { name: "foo" ; host_device: "myfoo" ; sector_size: 20 *)
  match js with
  | Jsont.String (name, _) -> Ok (name, None, None)
  | Jsont.Object (xs, _) -> (
      match
        (get_mem "name" xs, get_mem "host_device" xs, get_mem "sector_size" xs)
      with
      | None, _, _ -> Error (`Msg "name must be present in the json")
      | ( Some (Jsont.String (name, _)),
          (None | Some (Jsont.Null _)),
          (None | Some (Jsont.Null _)) ) ->
          Ok (name, None, None)
      | ( Some (Jsont.String (name, _)),
          Some (Jsont.String (host, _)),
          (None | Some (Jsont.Null _)) ) ->
          Ok (name, Some host, None)
      | Some (Jsont.String (name, _)), (None | Some (Jsont.Null _)), Some s -> (
          match int_of_json s with
          | Some sector_size -> Ok (name, None, Some sector_size)
          | None -> Error (`Msg "couldn't decode json"))
      | Some (Jsont.String (name, _)), Some (Jsont.String (host, _)), Some s
        -> (
          match int_of_json s with
          | Some sector_size -> Ok (name, Some host, Some sector_size)
          | None -> Error (`Msg "couldn't decode json"))
      | _, _, _ -> Error (`Msg "couldn't decode json"))
  | _ -> Error (`Msg "bad json, either string or assoc")

let config_of_jsont (json : Jsont.json) =
  let ( let* ) = Result.bind in
  let* dict =
    match json with
    | Jsont.Object (r, _) -> Ok r
    | _ -> Error (`Msg "not a json assoc")
  in
  let* fail_behaviour =
    Option.fold ~none:(Ok `Quit) ~some:fail_behaviour_of_json
      (get_mem "fail_behaviour" dict)
  in
  let* cpuids =
    match get_mem "cpuids" dict with
    | None ->
        (* backward compatibility for data on disk *)
        Option.fold
          ~none:(Error (`Msg "cpuids must be a list of integers"))
          ~some:(fun j ->
            match int_of_json j with
            | Some n -> Ok (Vmm_core.IS.singleton n)
            | None -> Error (`Msg "cpuid must be an integer"))
          (get_mem "cpuid" dict)
    | Some (Jsont.Array (l, _)) ->
        let* ids =
          List.fold_left
            (fun acc i ->
              let* acc = acc in
              match int_of_json i with
              | Some i -> Ok (i :: acc)
              | None -> Error (`Msg "CPUid is not an int"))
            (Ok []) l
        in
        if ids = [] then Error (`Msg "CPUids cannot be an empty list")
        else Ok (Vmm_core.IS.of_list ids)
    | Some _ -> Error (`Msg "CPUids must be a list of integers")
  in
  let* numcpus =
    Option.fold ~none:(Ok 1) (* Default to 1 vCPU *)
      ~some:(fun j ->
        match int_of_json j with
        | Some n when n > 0 -> Ok n
        | _ -> Error (`Msg "vcpus must be a positive integer greater than 0"))
      (get_mem "vcpus" dict)
  in
  let* linux_boot_partition =
    match get_mem "linux_boot_partition" dict with
    | None | Some (Jsont.Null _) | Some (Jsont.String ("", _)) -> Ok None
    | Some (Jsont.String (s, _)) -> Ok (Some s)
    | Some _ -> Error (`Msg "linux_boot_partition must be a string")
  in
  let* memory =
    Option.fold ~none:(Ok 32)
      ~some:(fun j ->
        match int_of_json j with
        | Some n -> Ok n
        | None -> Error (`Msg "memory must be an integer"))
      (get_mem "memory" dict)
  in
  let* bridges =
    let bridge bs =
      List.fold_left
        (fun r x ->
          let* r = r in
          let* b = bridge_of_json x in
          Ok (b :: r))
        (Ok []) bs
    in
    match get_mem "network_interfaces" dict with
    | None -> Ok []
    | Some (Jsont.Array (bs, _)) -> bridge bs
    | Some _ -> Error (`Msg "expected a list of network devices")
  in
  let* block_devices =
    match get_mem "block_devices" dict with
    | None -> Ok []
    | Some (Jsont.Array (bs, _)) ->
        List.fold_left
          (fun r x ->
            let* r = r in
            let* b = block_device_of_json x in
            Ok (b :: r))
          (Ok []) bs
    | Some _ -> Error (`Msg "expected a list of block devices")
  in
  let* argv =
    Option.fold ~none:(Ok None)
      ~some:(function
        | Jsont.Array (bs, _) ->
            let* args =
              List.fold_left
                (fun acc arg ->
                  let* acc = acc in
                  match arg with
                  | Jsont.String (n, _) -> Ok (n :: acc)
                  | _ -> Error (`Msg "argument is not a string"))
                (Ok []) bs
            in
            if args = [] then Ok None else Ok (Some (List.rev args))
        | _ -> Error (`Msg "arguments must be a list of strings"))
      (get_mem "arguments" dict)
  in
  let* startup =
    Option.fold ~none:(Ok None)
      ~some:(fun j ->
        match int_of_json j with
        | Some n when n >= 0 && n <= 100 ->
            if n = 50 then Ok None else Ok (Some n)
        | _ ->
            Error
              (`Msg
                 "startup must be an integer between 1 and 100, default is 50"))
      (get_mem "startup" dict)
  in
  let* typ =
    match get_mem "typ" dict with
    | None -> Error (`Msg "typ must be provided")
    | Some (Jsont.String (t, _)) -> (
        match t with
        | "solo5" -> Ok `Solo5
        | "bhyve" -> Ok `BHyve
        | _ -> Error (`Msg "only 'solo5' and 'bhyve' supported as typ"))
    | Some _ -> Error (`Msg "typ must be a string")
  in
  Ok
    {
      Vmm_core.Unikernel.typ;
      compressed = false;
      image = "";
      fail_behaviour;
      add_name = true;
      startup;
      cpuids;
      memory;
      block_devices;
      bridges;
      argv;
      numcpus;
      linux_boot_partition;
    }

let config_of_json str =
  let ( let* ) = Result.bind in
  let* json =
    Jsont_bytesrw.decode_string Jsont.json str
    |> Result.map_error (fun e -> `Msg e)
  in
  config_of_jsont json

let typ = function `Solo5 -> "solo5" | `BHyve -> "bhyve"

let config_to_json (cfg : Vmm_core.Unikernel.config) =
  let block_devices bs =
    let block (name, host_device, sector_size) =
      j_obj
        [
          ("name", Jsont.Json.string name);
          ("host_device", Jsont.Json.option Jsont.Json.string host_device);
          ("sector_size", Jsont.Json.option Jsont.Json.int sector_size);
        ]
    in
    Jsont.Json.list (List.map block bs)
  and bridges bs =
    let bridge (name, host_device, mac) =
      j_obj
        [
          ("name", Jsont.Json.string name);
          ("host_device", Jsont.Json.option Jsont.Json.string host_device);
          ( "mac",
            Jsont.Json.option
              (fun ?meta m -> Jsont.Json.string ?meta (Macaddr.to_string m))
              mac );
        ]
    in
    Jsont.Json.list (List.map bridge bs)
  in
  j_obj
    [
      ("typ", Jsont.Json.string (typ cfg.typ));
      ("compressed", Jsont.Json.bool cfg.compressed);
      ("fail_behaviour", fail_behaviour cfg.fail_behaviour);
      ("cpuids", cpuids cfg.cpuids);
      ("memory", memory cfg.memory);
      ("block_devices", block_devices cfg.block_devices);
      ("network_interfaces", bridges cfg.bridges);
      ("arguments", argv cfg.argv);
      ("numcpus", Jsont.Json.int cfg.numcpus);
      ( "linux_boot_partition",
        Jsont.Json.option Jsont.Json.string cfg.linux_boot_partition );
    ]

let config_json : Vmm_core.Unikernel.config Jsont.t =
  Jsont.map
    ~dec:(fun j ->
      match config_of_jsont j with
      | Ok cfg -> cfg
      | Error (`Msg msg) -> Jsont.Error.msg Jsont.Meta.none msg)
    ~enc:config_to_json Jsont.json

let unikernel_info_to_arguments_json info =
  j_obj
    [
      ("typ", Jsont.Json.string (typ info.Vmm_core.Unikernel.typ));
      ("fail_behaviour", fail_behaviour info.fail_behaviour);
      ( "startup",
        Jsont.Json.int (Option.fold ~none:50 ~some:Fun.id info.startup) );
      (* add_name is not found in Vmm_core.Unikernel.info *)
      ("add_name", Jsont.Json.bool true);
      ("cpuids", cpuids info.cpuids);
      ("memory", memory info.memory);
      ("block_devices", block_devices info.block_devices);
      ("network_interfaces", bridges info.bridges);
      ("arguments", argv info.argv);
      ("numcpus", Jsont.Json.int info.numcpus);
      ( "linux_boot_partition",
        Jsont.Json.string
          (Option.fold ~none:"" ~some:Fun.id info.linux_boot_partition) );
    ]

let unikernel_info_to_arguments_pretty_string info =
  Result.get_ok
    (Jsont_bytesrw.encode_string ~format:Jsont.Indent Jsont.json
       (unikernel_info_to_arguments_json info))

let arguments_of_json str =
  match config_of_json str with
  | Ok config ->
      Ok
        {
          Vmm_core.Unikernel.fail_behaviour = config.fail_behaviour;
          add_name = config.add_name;
          startup = config.startup;
          cpuids = config.cpuids;
          memory = config.memory;
          block_devices = config.block_devices;
          bridges = config.bridges;
          argv = config.argv;
          numcpus = config.numcpus;
          linux_boot_partition = config.linux_boot_partition;
        }
  | Error err -> Error err
