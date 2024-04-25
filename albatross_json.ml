let unikernel_info (unikernel_name, info) =
  let typ = function `Solo5 -> `String "solo5"
  and fail_behaviour = function
    | `Quit -> `String "quit"
    | `Restart ex ->
      let els = Option.value ~default:[] (Option.map Vmm_core.IS.elements ex) in
      `Assoc [("restart", `Null) ; ("exit_code", `List (List.map (fun e -> `Int e) els))]
  and cpuid c = `Int c
  and memory m = `Int m
  and block_devices bs =
    let block (name, dev, sec) =
      let dev = Option.value ~default:name dev in
      let s = Option.value ~default:0 sec in
      `Assoc [("name", `String name) ; ("host_device", `String dev); ("sector_size", `Int s)]
    in
    `List (List.map block bs)
  and bridges bs =
    let bridge (name, dev, mac) =
      let dev = Option.value ~default:name dev in
      let mac = Option.value ~default:(Vmm_core.Name.mac unikernel_name dev) mac in
      `Assoc [("name", `String name) ; ("host_device", `String dev); ("mac", `String (Macaddr.to_string mac)) ]
    in
    `List (List.map bridge bs)
  and argv args =
    `List (List.map (fun a -> `String a) (Option.value ~default:[] args))
  and digest d =
    `String (Cstruct.to_hex_string d)
  in
  `Assoc [
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
