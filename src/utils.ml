module LM = Map.Make (Vmm_core.Name.Label)
module SM = Map.Make (String)
module UM = Map.Make (Uuidm)

let now () = try Mkernel.now () with _ -> Ptime.epoch

module Json = struct
  let get key mems = Option.map snd (Jsont.Json.find_mem key mems)

  let to_string (t : Jsont.json) =
    Result.get_ok (Jsont_bytesrw.encode_string Jsont.json t)

  let from_string data =
    Jsont_bytesrw.decode_string' Jsont.json data
    |> Result.map_error (fun (_, _, k) -> `Msg (Jsont.Error.kind_to_string k))

  let encode codec v = Result.get_ok (Jsont.Json.encode codec v)

  let decode codec j =
    Jsont.Json.decode' codec j
    |> Result.map_error (fun (_, _, k) -> `Msg (Jsont.Error.kind_to_string k))

  let of_string_result ~enc dec =
    Jsont.Base.string
      (Jsont.Base.map
         ~dec:(fun meta s ->
           match dec s with
           | Ok v -> v
           | Error (`Msg msg) -> Jsont.Error.msg meta msg)
         ~enc ())

  let uuid_json =
    of_string_result ~enc:Uuidm.to_string (fun s ->
        Option.to_result ~none:(`Msg ("invalid UUID: " ^ s)) (Uuidm.of_string s))

  let mailbox_json =
    of_string_result ~enc:Emile.to_string Mrmime.Mailbox.of_string

  let ipaddr_json = of_string_result ~enc:Ipaddr.to_string Ipaddr.of_string
end

module TimeHelper = struct
  let ptime_of_string (t_str : string) : (Ptime.t, [> `Msg of string ]) result =
    match Ptime.of_rfc3339 t_str with
    | Ok (ptime, _, _) -> Ok ptime
    | Error (`RFC3339 (_, err)) ->
        Logs.err (fun m -> m "string_to_ptime: %a" Ptime.pp_rfc3339_error err);
        Error
          (`Msg
             (Format.asprintf "invalid created_at format: %a"
                Ptime.pp_rfc3339_error err))

  let string_of_ptime (t : Ptime.t) : string =
    Ptime.to_rfc3339 ~space:true t ~frac_s:0

  let ptime_json = Json.of_string_result ~enc:string_of_ptime ptime_of_string

  (* calculate the diff between two timestamps *)
  let diff_in_seconds ~current_time ~check_time =
    match Ptime.Span.to_int_s (Ptime.diff current_time check_time) with
    | Some s -> s
    | None -> 0

  let is_within_seconds ~current_time ~check_time ttl =
    diff_in_seconds ~current_time ~check_time < ttl

  let time_ago ~current_time ~check_time =
    let diff = diff_in_seconds ~current_time ~check_time in
    if diff < 60 then Printf.sprintf "%d s ago" diff
    else if diff < 3600 then Printf.sprintf "%d m ago" (diff / 60)
    else if diff < 86400 then Printf.sprintf "%d h ago" (diff / 3600)
    else if diff < 604800 then Printf.sprintf "%d d ago" (diff / 86400)
    else if diff < 2592000 then Printf.sprintf "%d w ago" (diff / 604800)
    else if diff < 31536000 then Printf.sprintf "%d mo ago" (diff / 2419200)
    else Printf.sprintf "%d y ago" (diff / 31536000)
end

let bytes_to_megabytes bytes =
  let megabytes = float_of_int bytes /. (1024.0 *. 1024.0) in
  Printf.sprintf "%.2f MB" megabytes

(*10 minutes for a rollback to be valid*)
let rollback_seconds_limit = 600

let cpuid_to_array_string lst =
  if lst = [] then "[]"
  else "[\"" ^ String.concat "\", \"" (List.map string_of_int lst) ^ "\"]"

type user_policy_usage = {
  deployed_unikernels : int;
  total_block_used : int;
  total_free_space : int;
  total_memory_used : int;
  cpu_usage_count : (int * int) list;
  bridge_usage_count : (string * int) list;
}

let deployed_unikernels unikernels = List.length unikernels

let cpu_usage_count policy unikernels =
  let cpuid_count = Hashtbl.create (List.length unikernels) in
  List.iter
    (fun (_, unikernel) ->
      Vmm_core.IS.iter
        (fun cpuid ->
          let count =
            Hashtbl.find_opt cpuid_count cpuid |> Option.value ~default:0
          in
          Hashtbl.replace cpuid_count cpuid (count + 1))
        unikernel.Vmm_core.Unikernel.cpuids)
    unikernels;
  let policy_cpuids = Vmm_core.IS.elements policy.Vmm_core.Policy.cpuids in
  List.map
    (fun cpuid ->
      let count =
        Hashtbl.find_opt cpuid_count cpuid |> Option.value ~default:0
      in
      (cpuid, count))
    policy_cpuids

let total_block_used blocks =
  List.fold_left (fun total_size (_, size, _) -> total_size + size) 0 blocks

let total_free_space policy total_block_used =
  Option.value ~default:0 policy.Vmm_core.Policy.block - total_block_used

let total_memory_used unikernels =
  List.fold_left
    (fun total_memory (_, unikernel) ->
      total_memory + unikernel.Vmm_core.Unikernel.memory)
    0 unikernels

let bridge_usage_count policy unikernels =
  let bridge_count =
    Hashtbl.create (Vmm_core.String_set.cardinal policy.Vmm_core.Policy.bridges)
  in
  Vmm_core.String_set.iter
    (fun bridge -> Hashtbl.replace bridge_count bridge 0)
    policy.bridges;
  List.iter
    (fun (_, unikernel) ->
      List.iter
        (fun { Vmm_core.Unikernel.host_device; _ } ->
          if Vmm_core.String_set.mem host_device policy.bridges then
            let count =
              Hashtbl.find_opt bridge_count host_device
              |> Option.value ~default:0
            in
            Hashtbl.replace bridge_count host_device (count + 1))
        unikernel.Vmm_core.Unikernel.bridges)
    unikernels;
  let bridge_usage_list =
    Hashtbl.fold
      (fun bridge count acc -> (bridge, count) :: acc)
      bridge_count []
  in
  List.sort (fun (b1, _) (b2, _) -> String.compare b1 b2) bridge_usage_list

let user_policy_usage policy unikernels blocks : user_policy_usage =
  let deployed_unikernels = deployed_unikernels unikernels in
  let total_block_used = total_block_used blocks in
  let total_free_space = total_free_space policy total_block_used in
  let total_memory_used = total_memory_used unikernels in
  let cpu_usage_count = cpu_usage_count policy unikernels in
  let bridge_usage_count = bridge_usage_count policy unikernels in
  {
    deployed_unikernels;
    total_block_used;
    total_free_space;
    total_memory_used;
    cpu_usage_count;
    bridge_usage_count;
  }
