open Utils.Json

let fail_behaviour = function
  | `Quit -> `String "quit"
  | `Restart ex ->
      let els = Option.value ~default:[] (Option.map Vmm_core.IS.elements ex) in
      `Assoc
        [
          ("restart", `Null);
          ("exit_code", `List (List.map (fun e -> `Int e) els));
          ("all_exit_codes", `Bool (ex = None));
        ]

let cpuid c = `Int c
let memory m = `Int m

let argv args =
  `List (List.map (fun a -> `String a) (Option.value ~default:[] args))

let unikernel_info (unikernel_name, info) =
  let block_devices bs =
    let block
        { Vmm_core.Unikernel.unikernel_device; host_device; sector_size; size }
        =
      `Assoc
        [
          ("name", `String unikernel_device);
          ("host_device", `String host_device);
          ("sector_size", `Int sector_size);
          ("size", `Int size);
        ]
    in
    `List (List.map block bs)
  and bridges bs =
    let bridge
        ({ Vmm_core.Unikernel.unikernel_device; host_device; _ } :
          Vmm_core.Unikernel.net_info) =
      `Assoc
        [
          ("name", `String unikernel_device);
          ("host_device", `String host_device);
        ]
    in
    `List (List.map bridge bs)
  in
  `Assoc
    [
      ( "name",
        `String (Option.value ~default:"" (Vmm_core.Name.name unikernel_name))
      );
      ("fail_behaviour", fail_behaviour info.Vmm_core.Unikernel.fail_behaviour);
      ("cpuid", cpuid info.cpuid);
      ("memory", memory info.memory);
      ("block_devices", block_devices info.block_devices);
      ("network_interfaces", bridges info.bridges);
      ("arguments", argv info.argv);
    ]

let unikernel_infos is = `List (List.map unikernel_info is)

let block_info (name, size, used) =
  `Assoc
    [
      ("name", `String (Vmm_core.Name.to_string name));
      ("size", `Int size);
      ("used", `Bool used);
    ]

let block_infos bs = `List (List.map block_info bs)

let policy_info (name, policy) =
  `Assoc
    [
      ("name", `String (Vmm_core.Name.to_string name));
      ("allowed_unikernels", `Int policy.Vmm_core.Policy.unikernels);
      ( "allowed_cpuids",
        `List
          (List.map (fun id -> `Int id) (Vmm_core.IS.elements policy.cpuids)) );
      ("allowed_memory", `Int policy.memory);
      ("allowed_block_size", `Int (Option.value ~default:0 policy.block));
      ( "allowed_bridges",
        `List
          (List.map
             (fun b -> `String b)
             (Vmm_core.String_set.elements policy.bridges)) );
    ]

let policy_infos ps = `List (List.map policy_info ps)

let success = function
  | `Empty -> `Null
  | `String m -> `String m
  | `Policies ps -> policy_infos ps
  | `Old_unikernel_info2 is -> unikernel_infos is
  | `Old_unikernel_info3 is -> unikernel_infos is
  | `Unikernel_info is -> unikernel_infos is
  | `Block_devices bs -> block_infos bs
  | `Old_unikernels _ -> `String "old unikernels not supported"
  | `Unikernel_image _ -> `String "unikernel image not supported"
  | `Old_block_device_image (_, bd) -> `String bd
  | `Block_device_image _compressed -> `String "block device image"

let console_data_to_json (ts, data) =
  `Assoc
    [ ("timestamp", `String (Ptime.to_rfc3339 ts)); ("line", `String data) ]

let res = function
  | `Command _ -> Error (`String "command not supported")
  | `Success s -> Ok (success s)
  | `Failure f -> Error (`String ("failure: " ^ f))
  | `Data (`Console_data (ts, data)) -> Ok (console_data_to_json (ts, data))
  | `Data (`Utc_console_data (ts, data)) -> Ok (console_data_to_json (ts, data))
  | `Data (`Stats_data _) -> Error (`String "stats not supported")
  | `Data (`Block_data _) -> Error (`String "block data not supported")

let fail_behaviour_of_json js =
  let ( let* ) = Result.bind in
  match js with
  | `String "quit" -> Ok `Quit
  | `Assoc rs -> (
      match (get "restart" rs, get "exit_code" rs, get "all_exit_codes" rs) with
      | None, _, _ -> Error (`Msg "expected a restart")
      | Some _, Some (`List codes), _ ->
          let* codes =
            try
              Ok
                (List.map
                   (function `Int n -> n | _ -> failwith "not an integer")
                   codes)
            with Failure s ->
              Error
                (`Msg ("expected integer values as exit codes, error: " ^ s))
          in
          Ok (`Restart (Some (Vmm_core.IS.of_list codes)))
      | Some _, Some _, _ ->
          Error (`Msg "expected a list of integers as exit_code payload")
      | Some _, _, _ -> Ok (`Restart None))
  | _ -> Error (`Msg "fail behaviour must be quit or restart")

let bridge_of_json (js : Yojson.Basic.t) =
  (* we support [ "foo" ] as well as [ { name: "foo" ; host_device: "myfoo" ; mac: "aa:bb:cc:dd:ee:ff" *)
  let ( let* ) = Result.bind in
  match js with
  | `String bridge -> Ok (bridge, None, None)
  | `Assoc xs -> (
      match (get "name" xs, get "host_device" xs, get "mac" xs) with
      | None, _, _ -> Error (`Msg "name must be present in the json")
      | Some (`String name), (None | Some `Null), (None | Some `Null) ->
          Ok (name, None, None)
      | Some (`String name), Some (`String host), (None | Some `Null) ->
          Ok (name, Some host, None)
      | Some (`String name), (None | Some `Null), Some (`String mac) ->
          let* mac = Macaddr.of_string mac in
          Ok (name, None, Some mac)
      | Some (`String name), Some (`String host), Some (`String mac) ->
          let* mac = Macaddr.of_string mac in
          Ok (name, Some host, Some mac)
      | _, _, _ -> Error (`Msg "couldn't decode json"))
  | _ -> Error (`Msg "bad json, either string or assoc")

let block_device_of_json js =
  (* we support [ "foo" ] as well as [ { name: "foo" ; host_device: "myfoo" ; sector_size: 20 *)
  match js with
  | `String name -> Ok (name, None, None)
  | `Assoc xs -> (
      match (get "name" xs, get "host_device" xs, get "sector_size" xs) with
      | None, _, _ -> Error (`Msg "name must be present in the json")
      | Some (`String name), (None | Some `Null), (None | Some `Null) ->
          Ok (name, None, None)
      | Some (`String name), Some (`String host), (None | Some `Null) ->
          Ok (name, Some host, None)
      | Some (`String name), (None | Some `Null), Some (`Int sector_size) ->
          Ok (name, None, Some sector_size)
      | Some (`String name), Some (`String host), Some (`Int sector_size) ->
          Ok (name, Some host, Some sector_size)
      | _, _, _ -> Error (`Msg "couldn't decode json"))
  | _ -> Error (`Msg "bad json, either string or assoc")

let policy_of_json json_dict =
  match
    Utils.Json.
      ( get "unikernels" json_dict,
        get "memory" json_dict,
        get "block" json_dict,
        get "cpuids" json_dict,
        get "bridges" json_dict )
  with
  | ( Some (`Int unikernels),
      Some (`Int memory),
      Some (`Int block),
      Some (`String cpuids),
      Some (`String bridges) ) ->
      let policy =
        {
          Vmm_core.Policy.unikernels;
          memory;
          block = (if block = 0 then None else Some block);
          cpuids =
            (let parsed_cpuids =
               List.filter_map
                 (fun s ->
                   match int_of_string_opt s with
                   | Some i -> Some i
                   | None ->
                       Logs.warn (fun m -> m "Ignoring invalid CPU id: %s" s);
                       None)
                 (String.split_on_char ',' cpuids)
             in
             Vmm_core.IS.of_list parsed_cpuids);
          bridges =
            Vmm_core.String_set.of_list (String.split_on_char ',' bridges);
        }
      in
      let ( let* ) = Result.bind in
      let* () = Vmm_core.Policy.usable policy in
      Ok policy
  | _ ->
      Error
        (`Msg
           (Fmt.str "policy: unexpected types, got %s"
              (Yojson.Basic.to_string (`Assoc json_dict))))

let config_of_json str =
  let ( let* ) = Result.bind in
  let* json =
    try Ok (Yojson.Basic.from_string str)
    with Yojson.Json_error s -> Error (`Msg s)
  in
  let* dict =
    match json with `Assoc r -> Ok r | _ -> Error (`Msg "not a json assoc")
  in
  let* fail_behaviour =
    Option.fold ~none:(Ok `Quit) ~some:fail_behaviour_of_json
      (get "fail_behaviour" dict)
  in
  let* cpuid =
    Option.fold ~none:(Ok 0)
      ~some:(function
        | `Int n -> Ok n | _ -> Error (`Msg "cpuid must be an integer"))
      (get "cpuid" dict)
  in
  let* memory =
    Option.fold ~none:(Ok 32)
      ~some:(function
        | `Int n -> Ok n | _ -> Error (`Msg "memory must be an integer"))
      (get "memory" dict)
  in
  let* bridges =
    match get "network_interfaces" dict with
    | None -> Ok []
    | Some (`List bs) ->
        List.fold_left
          (fun r x ->
            let* r = r in
            let* b = bridge_of_json x in
            Ok (b :: r))
          (Ok []) bs
    | Some _ -> Error (`Msg "expected a list of network devices")
  in
  let* block_devices =
    match get "block_devices" dict with
    | None -> Ok []
    | Some (`List bs) ->
        List.fold_left
          (fun r x ->
            let* r = r in
            let* b = block_device_of_json x in
            Ok (b :: r))
          (Ok []) bs
    | Some _ -> Error (`Msg "expected a list of block devices")
  in
  let* argv =
    try
      Option.fold ~none:(Ok None)
        ~some:(function
          | `List bs ->
              Ok
                (Some
                   (List.map
                      (function
                        | `String n -> n
                        | _ -> failwith "argument is not a string")
                      bs))
          | _ -> Error (`Msg "arguments must be a list of strings"))
        (get "arguments" dict)
    with Failure s -> Error (`Msg ("expected strings as argv, error: " ^ s))
  in
  Ok
    {
      Vmm_core.Unikernel.typ = `Solo5;
      compressed = false;
      image = "";
      fail_behaviour;
      cpuid;
      memory;
      block_devices;
      bridges;
      argv;
    }

let config_to_json (cfg : Vmm_core.Unikernel.config) =
  let block_devices bs =
    let block (name, host_device, sector_size) =
      `Assoc
        [
          ("name", `String name);
          ( "host_device",
            match host_device with
            | Some host_device -> `String host_device
            | None -> `Null );
          ( "sector_size",
            match sector_size with
            | Some sector_size -> `Int sector_size
            | None -> `Null );
        ]
    in
    `List (List.map block bs)
  and bridges bs =
    let bridge (name, host_device, mac) =
      `Assoc
        [
          ("name", `String name);
          ( "host_device",
            match host_device with
            | Some host_device -> `String host_device
            | None -> `Null );
          ( "mac",
            match mac with
            | Some mac -> `String (Macaddr.to_string mac)
            | None -> `Null );
        ]
    in
    `List (List.map bridge bs)
  in
  `Assoc
    [
      ("typ", `String "solo5");
      ("compressed", `Bool cfg.compressed);
      ("fail_behaviour", fail_behaviour cfg.fail_behaviour);
      ("cpuid", cpuid cfg.cpuid);
      ("memory", memory cfg.memory);
      ("block_devices", block_devices cfg.block_devices);
      ("network_interfaces", bridges cfg.bridges);
      ("arguments", argv cfg.argv);
    ]
