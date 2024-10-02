open Utils.Json

let unikernel_info (unikernel_name, info) =
  let typ = function `Solo5 -> `String "solo5"
  and fail_behaviour = function
    | `Quit -> `String "quit"
    | `Restart ex ->
        let els =
          Option.value ~default:[] (Option.map Vmm_core.IS.elements ex)
        in
        `Assoc
          [
            ("restart", `Null);
            ("exit_code", `List (List.map (fun e -> `Int e) els));
            ("all_exit_codes", `Bool (ex = None));
          ]
  and cpuid c = `Int c
  and memory m = `Int m
  and block_devices bs =
    let block (name, dev, sec) =
      let dev = Option.value ~default:"none specified (name is used)" dev in
      let s = Option.value ~default:(-1) sec in
      `Assoc
        [
          ("name", `String name);
          ("host_device", `String dev);
          ("sector_size", `Int s);
        ]
    in
    `List (List.map block bs)
  and bridges bs =
    let bridge (name, dev, mac) =
      let dev = Option.value ~default:name dev in
      let mac =
        Option.value ~default:(Vmm_core.Name.mac unikernel_name dev) mac
      in
      `Assoc
        [
          ("name", `String name);
          ("host_device", `String dev);
          ("mac", `String (Macaddr.to_string mac));
        ]
    in
    `List (List.map bridge bs)
  and argv args =
    `List (List.map (fun a -> `String a) (Option.value ~default:[] args))
  and digest d = `String (Ohex.encode d) in
  `Assoc
    [
      ("name", `String (Vmm_core.Name.to_string unikernel_name));
      ("typ", typ info.Vmm_core.Unikernel.typ);
      ("fail_behaviour", fail_behaviour info.fail_behaviour);
      ("cpuid", cpuid info.cpuid);
      ("memory", memory info.memory);
      ("block_devices", block_devices info.block_devices);
      ("network_interfaces", bridges info.bridges);
      ("arguments", argv info.argv);
      ("digest", digest info.digest);
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
      ("allowed_vms", `Int policy.Vmm_core.Policy.vms);
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
  | `Old_unikernel_info is -> unikernel_infos is
  | `Unikernel_info is -> unikernel_infos is
  | `Block_devices bs -> block_infos bs
  | `Old_unikernels _ -> `String "old unikernels not supported"
  | `Unikernel_image _ -> `String "unikernel image not supported"
  | `Block_device_image _ -> `String "block device image not supported"

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
      | Some (`String name), None, None -> Ok (name, None, None)
      | Some (`String name), Some (`String host), None ->
          Ok (name, Some host, None)
      | Some (`String name), None, Some (`String mac) ->
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
      | Some (`String name), None, None -> Ok (name, None, None)
      | Some (`String name), Some (`String host), None ->
          Ok (name, Some host, None)
      | Some (`String name), None, Some (`Int sector_size) ->
          Ok (name, None, Some sector_size)
      | Some (`String name), Some (`String host), Some (`Int sector_size) ->
          Ok (name, Some host, Some sector_size)
      | _, _, _ -> Error (`Msg "couldn't decode json"))
  | _ -> Error (`Msg "bad json, either string or assoc")

let policy_of_json js =
  match js with
  | `Assoc xs -> (
      match
        Utils.Json.
          ( get "vms" xs,
            get "memory" xs,
            get "block" xs,
            get "cpuids" xs,
            get "bridges" xs )
      with
      | ( Some (`Int vms),
          Some (`Int memory),
          Some (`Int block),
          Some (`String cpuids),
          Some (`String bridges) ) ->
          let policy =
            {
              Vmm_core.Policy.vms;
              memory;
              block = (if block = 0 then None else Some block);
              cpuids =
                (let parsed_cpuids =
                   List.filter_map
                     (fun s ->
                       match int_of_string_opt s with
                       | Some i -> Some i
                       | None ->
                           Logs.err (fun m -> m "Invalid CPU id: %s" s);
                           None)
                     (String.split_on_char ',' cpuids)
                 in
                 if parsed_cpuids = [] then Vmm_core.IS.empty
                 else Vmm_core.IS.of_list parsed_cpuids);
              bridges =
                   Vmm_core.String_set.of_list
                     (String.split_on_char ',' bridges);
            }
          in
          let ( let* ) = Result.bind in
          let* () = Vmm_core.Policy.usable policy in
          Ok policy
      | _ ->
          Error
            (`Msg
              (Fmt.str "policy: unexpected types, got %s"
                 (Yojson.Basic.to_string (`Assoc xs)))))
  | _ -> Error (`Msg "policy: Expected a dictionary")

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
