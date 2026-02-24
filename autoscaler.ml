let poll_interval = Duration.of_min 5 (* check every 5 minutes *)
let trigger_ticks = 5
(* number of times a unikernel is checked and is overloaded before spawning a new clone i.e trigger_ticks x poll_interval determines how load a unikernel is overloaded for before we spawn. Currently this will be 25 minutes. *)

let threshold_percent =
  90.0 (* if CPU usage >= 90% for 5 consercutive checks then we spawn. *)

let cooldown_period = 600.0
(* after spawning a new clone, wait for 10 minutes before we start checking this unikernel again *)

let death_timeout = 900.0
(* Timeout in seconds. If no stats for 15 minutes, assume destroyed. *)

module Cpu_monitor = struct
  let rusage_to_float (sec, usec) =
    let s = Int64.to_float sec in
    let u = float_of_int usec in
    s +. (u /. 1_000_000.0)

  let get_total_cpu_time (r : Vmm_core.Stats.rusage) =
    let user_t = rusage_to_float r.utime in
    let sys_t = rusage_to_float r.stime in
    user_t +. sys_t

  type t = { last_cpu_time : float; last_wall_time : Ptime.t }

  let create now (initial_rusage : Vmm_core.Stats.rusage) =
    { last_cpu_time = get_total_cpu_time initial_rusage; last_wall_time = now }

  let measure t now (current_rusage : Vmm_core.Stats.rusage) =
    let curr_cpu_time = get_total_cpu_time current_rusage in
    let curr_wall_time = now in
    let cpu_delta = curr_cpu_time -. t.last_cpu_time in
    let wall_span = Ptime.diff curr_wall_time t.last_wall_time in
    let wall_delta = Ptime.Span.to_float_s wall_span in
    { last_cpu_time = curr_cpu_time; last_wall_time = curr_wall_time }
    |> fun t ->
    if wall_delta <= 0.000001 then 0.0
    else if cpu_delta < 0.0 then 0.0
    else
      let pct = cpu_delta /. wall_delta *. 100.0 in
      Float.min 100.0 pct
end

type t = { mutable monitor : Cpu_monitor.t; mutable last_cpu_usage : float }

type status =
  | Overloaded of
      t (* the usage of the clone or vm that is checked in the group *)
  | Pending of int * t
    (* We have a high load but waiting for required number of polls to complete*)
  | Cooldown of t
  | Normal of t

let create now initial_rusage =
  {
    monitor = Cpu_monitor.create now initial_rusage;
    last_cpu_usage =
      Cpu_monitor.measure
        (Cpu_monitor.create now initial_rusage)
        now initial_rusage;
  }

module Cluster_manager = struct
  type group = {
    primary : string * t;
    mutable clones : (string * t) list;
    mutable last_scale_action : Ptime.t option;
    mutable last_stats_received : Ptime.t;
    mutable next_id : int;
    mutable consecutive_high_ticks : int;
  }

  let clusters : (string, group) Hashtbl.t = Hashtbl.create 10

  let in_cooldown now = function
    | { last_scale_action = None; _ } -> false
    | { last_scale_action = Some t; _ } ->
        let span = Ptime.diff now t in
        Ptime.Span.to_float_s span < cooldown_period

  let get_or_create primary =
    match Hashtbl.find_opt clusters (fst primary) with
    | Some g -> g
    | None ->
        let g =
          {
            primary;
            clones = [];
            consecutive_high_ticks = 0;
            last_scale_action = None;
            last_stats_received = Mirage_ptime.now ();
            next_id = 1;
          }
        in
        Hashtbl.add clusters (fst primary) g;
        g

  let find_group_by_name primary_name = Hashtbl.find_opt clusters primary_name

  let extract_primary name =
    match List.rev (String.split_on_char '-' name) with
    | _id_str :: "clone" :: primary_parts_rev ->
        Some (String.concat "-" (List.rev primary_parts_rev))
    | _ -> None

  let get_primary_from_clone clone = extract_primary (fst clone)
  let is_clone name = Option.is_some (extract_primary name)

  let find_or_create_group ~name ~key t =
    if is_clone name then
      match get_primary_from_clone (name, ()) with
      | Some primary_name -> (
          match find_group_by_name primary_name with
          | Some g -> Ok (g, name)
          | None ->
              Error
                (Fmt.str
                   "No primary group found for primary '%s' (derived from \
                    clone '%s')."
                   primary_name name))
      | None ->
          Error (Fmt.str "Could not derive primary name from clone '%s'." name)
    else
      match find_group_by_name key with
      | Some g -> Ok (g, key)
      | None -> get_or_create (key, t) |> fun g -> Ok (g, key)

  let next_clone_name vm =
    if is_clone (fst vm) then
      match get_primary_from_clone vm with
      | Some primary_name -> (
          match find_group_by_name primary_name with
          | Some g ->
              let id = g.next_id in
              g.next_id <- g.next_id + 1;
              Ok (Fmt.str "%s-clone-%d" primary_name id)
          | None ->
              Error
                (Fmt.str
                   "No primary group found for primary '%s' (derived from \
                    clone '%s')."
                   primary_name (fst vm)))
      | None ->
          Error
            (Fmt.str "Could not derive primary name from clone '%s'." (fst vm))
    else
      let g = get_or_create vm in
      let id = g.next_id in
      g.next_id <- g.next_id + 1;
      Ok (Fmt.str "%s-clone-%d" (fst g.primary) id)

  let register_clone clone =
    match get_primary_from_clone clone with
    | None ->
        Error
          (Fmt.str "Invalid clone name '%s'. Cannot extract primary name."
             (fst clone))
    | Some primary_name -> (
        match find_group_by_name primary_name with
        | Some g ->
            g.clones <- clone :: g.clones;
            g.last_scale_action <- Some (Mirage_ptime.now ());
            Ok ()
        | None ->
            Error
              (Fmt.str
                 "No primary group found for primary '%s' (derived from clone \
                  '%s')."
                 primary_name (fst clone)))

  let remove_clone clone =
    match get_primary_from_clone clone with
    | Some primary_name -> (
        match find_group_by_name primary_name with
        | Some g ->
            g.clones <- List.filter (fun c -> fst c <> fst clone) g.clones;
            g.last_scale_action <- Some (Mirage_ptime.now ());
            Ok ()
        | None -> Error "Primary group not found during removal")
    | None -> Error "Could not derive primary name for removal"

  let record_action primary =
    let g = get_or_create primary in
    g.last_scale_action <- Some (Mirage_ptime.now ())

  let check_group_average group key now rusage =
    let all_instances = group.primary :: group.clones in
    match
      List.find_opt (fun (name, _) -> String.equal name key) all_instances
    with
    | None -> Error "Current VM not found in group during average check"
    | Some (_, vm) ->
        let current_vm_usage = Cpu_monitor.measure vm.monitor now rusage in
        let new_monitor = Cpu_monitor.create now rusage in
        let current_vm_state =
          { monitor = new_monitor; last_cpu_usage = current_vm_usage }
        in
        vm.monitor <- new_monitor;
        vm.last_cpu_usage <- current_vm_usage;
        let other_vms_usage =
          List.fold_left
            (fun acc (name, vm) ->
              if key = name then acc else acc +. vm.last_cpu_usage)
            0.0 all_instances
        in
        let total_usage = current_vm_usage +. other_vms_usage in
        let average_usage =
          total_usage /. float_of_int (List.length all_instances)
        in
        Ok (average_usage, current_vm_state)

  let check_group_status group key now rusage =
    match check_group_average group key now rusage with
    | Error e -> Error e
    | Ok (average_usage, current_vm_state) ->
        if in_cooldown now group then (
          group.consecutive_high_ticks <- 0;
          Ok (Cooldown current_vm_state))
        else if average_usage > threshold_percent then (
          group.consecutive_high_ticks <- group.consecutive_high_ticks + 1;
          if group.consecutive_high_ticks >= trigger_ticks then (
            group.consecutive_high_ticks <- 0;
            group.last_scale_action <- Some now;
            Ok (Overloaded current_vm_state))
          else Ok (Pending (group.consecutive_high_ticks, current_vm_state)))
        else (
          group.consecutive_high_ticks <- 0;
          Ok (Normal current_vm_state))

  let prune_dead_clusters now =
    let dead_keys =
      Hashtbl.fold
        (fun primary_name group acc ->
          let span = Ptime.diff now group.last_stats_received in
          if Ptime.Span.to_float_s span > death_timeout then primary_name :: acc
          else acc)
        clusters []
    in
    List.iter
      (fun key ->
        Logs.warn (fun m -> m "[Cluster Manager] Pruning dead cluster: %s" key);
        Hashtbl.remove clusters key)
      dead_keys
end
