let a_logs = Logs.Src.create "autoscaling-logs"
let poll_interval = Duration.of_min 5

(** number of times a cluster is checked before deciding if it's overloaded. *)
let scale_up_trigger_ticks = 3

(** if CPU usage >= 90% for [scale_up_trigger_ticks] consecutive checks then
    this vm can be cloned. *)
let scale_up_threshold_percent = 90.0

let scale_down_threshold_percent = 40.0
let scale_down_trigger_ticks = 5

(** after spawning a new vm, wait [cooldown_period] before checking that vm
    again. *)
let cooldown_period = 600.0

(** if no stats are gotten for a particular vm after [death_timeout] then
    consider the vm destroyed and prune it *)
let death_timeout = 900.0

module Cpu_monitor = struct
  type t = { last_cpu_time : float; last_wall_time : Ptime.t }

  let timeval_to_float (sec, usec) =
    let s = Int64.to_float sec in
    let u = float_of_int usec in
    s +. (u /. 1_000_000.0)

  let get_total_cpu_time (r : Vmm_core.Stats.rusage) =
    let user_t = timeval_to_float r.utime in
    let sys_t = timeval_to_float r.stime in
    user_t +. sys_t

  let create now (initial_rusage : Vmm_core.Stats.rusage) =
    { last_cpu_time = get_total_cpu_time initial_rusage; last_wall_time = now }

  let measure t now (current_rusage : Vmm_core.Stats.rusage) =
    let curr_cpu_time = get_total_cpu_time current_rusage in
    let cpu_delta = curr_cpu_time -. t.last_cpu_time in
    let elasped_time_difference = Ptime.diff now t.last_wall_time in
    let elasped_time_in_seconds =
      Ptime.Span.to_float_s elasped_time_difference
    in
    if elasped_time_in_seconds <= 0.000001 then 0.0
    else if cpu_delta < 0.0 then 0.0
    else
      let pct = cpu_delta /. elasped_time_in_seconds *. 100.0 in
      (* TODO: use numcpus to cap it at 100.0% if the vm has more than 1 cpu. Now most
         vms use 1 cpu, so capping at 100% is fine. *)
      Float.min 100.0 pct
end

type t = {
  mutable monitor : Cpu_monitor.t;
  mutable last_cpu_usage : float;
  mutable last_stats_received : Ptime.t;
}

type scale_action = [ `Spawn | `Prune ]

type status =
  | Overloaded of t
  | Pending of scale_action * int * t
  | Underloaded of string * t
  | Cooldown of t
  | Normal of t

let create now initial_rusage =
  {
    monitor = Cpu_monitor.create now initial_rusage;
    last_cpu_usage =
      Cpu_monitor.measure
        (Cpu_monitor.create now initial_rusage)
        now initial_rusage;
    last_stats_received = now;
  }

module Cluster_manager = struct
  type vm = string * t

  type group = {
    primary : vm;
    mutable clones : vm list;
    mutable last_scale_action : Ptime.t option;
    mutable last_tick_update : Ptime.t option;
    mutable next_id : int;
    mutable consecutive_high_ticks : int;
    mutable consecutive_low_ticks : int;
  }

  let clusters : (string, group) Hashtbl.t = Hashtbl.create 10

  let in_cooldown now = function
    | { last_scale_action = None; _ } -> false
    | { last_scale_action = Some t; _ } ->
        let span = Ptime.diff now t in
        Ptime.Span.to_float_s span < cooldown_period

  let should_tick now group =
    match group.last_tick_update with
    | None -> true
    | Some t ->
        let span = Ptime.diff now t in
        Ptime.Span.to_float_s span >= Duration.to_f poll_interval

  let key ~user_name ~unikernel_name = Fmt.str "%s-%s" user_name unikernel_name

  let find_group_by_name ~user_name ~unikernel_name =
    Hashtbl.find_opt clusters (key ~user_name ~unikernel_name)

  let get_or_create ~user_name primary =
    match find_group_by_name ~user_name ~unikernel_name:(fst primary) with
    | Some g -> g
    | None ->
        let g =
          {
            primary;
            clones = [];
            last_scale_action = None;
            consecutive_high_ticks = 0;
            consecutive_low_ticks = 0;
            last_tick_update = None;
            next_id = 1;
          }
        in
        Hashtbl.add clusters (key ~user_name ~unikernel_name:(fst primary)) g;
        g

  (* TODO: in unikernel_create, forbid users from creating unikernels which end with [-clone-ID] *)
  let extract_name_and_clone_id name =
    match List.rev (String.split_on_char '-' name) with
    | id_str :: "clone" :: primary_parts_rev -> (
        match int_of_string_opt id_str with
        | Some id -> Some (String.concat "-" (List.rev primary_parts_rev), id)
        | None -> None)
    | _ -> None

  let add_clone_to_group g clone clone_id =
    if not (List.mem_assoc (fst clone) g.clones) then begin
      g.clones <- clone :: g.clones;
      g.next_id <- max g.next_id (clone_id + 1)
    end

  let register_clone user_name clone =
    match extract_name_and_clone_id (fst clone) with
    | Some (primary_name, _) -> (
        match find_group_by_name ~user_name ~unikernel_name:primary_name with
        | Some g ->
            g.next_id <- g.next_id + 1;
            let new_name = Fmt.str "%s-clone-%d" primary_name g.next_id in
            add_clone_to_group g (new_name, snd clone) g.next_id;
            g.last_scale_action <- Some (Mirage_ptime.now ());
            Ok new_name
        | None ->
            Error (Fmt.str "No primary group found for '%s'." primary_name))
    | None ->
        let g = get_or_create ~user_name clone in
        let new_name = Fmt.str "%s-clone-%d" (fst clone) g.next_id in
        add_clone_to_group g (new_name, snd clone) g.next_id;
        Ok new_name

  let find_or_create_group ~user_name ~unikernel_name t =
    match extract_name_and_clone_id unikernel_name with
    | Some (primary_name, clone_id) -> (
        match find_group_by_name ~user_name ~unikernel_name:primary_name with
        | Some g ->
            (* stats arrived before register_clone was called, recover silently *)
            add_clone_to_group g (unikernel_name, t) clone_id;
            Ok g
        | None ->
            Error
              (Fmt.str "No primary group found for '%s' (derived from '%s')."
                 primary_name unikernel_name))
    | None -> (
        match find_group_by_name ~user_name ~unikernel_name with
        | Some g -> Ok g
        | None ->
            let g = get_or_create ~user_name (unikernel_name, t) in
            Ok g)

  let remove_clone ~user_name clone =
    match extract_name_and_clone_id (fst clone) with
    | Some (primary_name, _) -> (
        match find_group_by_name ~user_name ~unikernel_name:primary_name with
        | Some g ->
            g.clones <-
              List.filter
                (fun c -> not (String.equal (fst c) (fst clone)))
                g.clones;
            g.last_scale_action <- Some (Mirage_ptime.now ());
            Ok ()
        | None ->
            Logs.debug ~src:a_logs (fun m ->
                m
                  "[Cluster Manager] No primary group found during removal for \
                   primary '%s' (derived from clone '%s')."
                  primary_name (fst clone));
            Error "Primary group not found during removal")
    | None ->
        Logs.debug ~src:a_logs (fun m ->
            m "[Cluster Manager] Invalid clone name '%s'." (fst clone));
        Error "Invalid clone name"

  let check_group_average group key now rusage =
    let all_instances = group.primary :: group.clones in
    let current_vm_state = ref None in
    let total_usage =
      List.fold_left
        (fun acc (name, vm) ->
          if String.equal name key then begin
            let current_vm_usage = Cpu_monitor.measure vm.monitor now rusage in
            let new_monitor = Cpu_monitor.create now rusage in
            vm.monitor <- new_monitor;
            vm.last_cpu_usage <- current_vm_usage;
            vm.last_stats_received <- now;
            current_vm_state := Some vm;
            acc +. current_vm_usage
          end
          else acc +. vm.last_cpu_usage)
        0.0 all_instances
    in
    match !current_vm_state with
    | None -> Error "Current VM not found in group during average check"
    | Some state ->
        let average_usage =
          total_usage /. float_of_int (List.length all_instances)
        in
        Ok (average_usage, state)

  let check_group_status group key now rusage =
    match check_group_average group key now rusage with
    | Error e -> Error e
    | Ok (average_usage, current_vm_state) ->
        let is_cooldown = in_cooldown now group in
        let is_high = average_usage > scale_up_threshold_percent in
        let is_low =
          average_usage < scale_down_threshold_percent && group.clones <> []
        in
        if should_tick now group then begin
          group.last_tick_update <- Some now;
          group.consecutive_high_ticks <-
            (if is_high && not is_cooldown then group.consecutive_high_ticks + 1
             else 0);
          group.consecutive_low_ticks <-
            (if is_low && not is_cooldown then group.consecutive_low_ticks + 1
             else 0)
        end;
        let trigger state =
          group.consecutive_high_ticks <- 0;
          group.consecutive_low_ticks <- 0;
          group.last_scale_action <- Some now;
          Ok state
        in
        if is_cooldown then Ok (Cooldown current_vm_state)
        else if group.consecutive_high_ticks >= scale_up_trigger_ticks then
          trigger (Overloaded current_vm_state)
        else if group.consecutive_low_ticks >= scale_down_trigger_ticks then
          let clone_to_kill = fst (List.hd group.clones) in
          trigger (Underloaded (clone_to_kill, current_vm_state))
        else if is_high then
          Ok (Pending (`Spawn, group.consecutive_high_ticks, current_vm_state))
        else if is_low then
          Ok (Pending (`Prune, group.consecutive_low_ticks, current_vm_state))
        else Ok (Normal current_vm_state)

  let is_dead now (vm : t) =
    Ptime.Span.to_float_s (Ptime.diff now vm.last_stats_received)
    > death_timeout

  let prune_dead_clusters now =
    let dead_keys =
      Hashtbl.fold
        (fun primary_name group acc ->
          if is_dead now (snd group.primary) then primary_name :: acc
          else begin
            let alive_clones, dead_clones =
              List.partition (fun (_, vm) -> not (is_dead now vm)) group.clones
            in
            match dead_clones with
            | [] -> acc
            | _ ->
                Logs.debug ~src:a_logs (fun m ->
                    m "[Cluster Manager] Pruning %d dead clones"
                      (List.length dead_clones));
                group.clones <- alive_clones;
                acc
          end)
        clusters []
    in
    List.iter
      (fun key ->
        Logs.debug ~src:a_logs (fun m ->
            m "[Cluster Manager] Pruning dead cluster: %s" key);
        Hashtbl.remove clusters key)
      dead_keys
end
