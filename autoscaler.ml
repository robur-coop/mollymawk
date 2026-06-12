let a_logs = Logs.Src.create "autoscaling-logs"

(** The interval at which a cluster's metrics are evaluated for scaling. *)
let poll_interval = Duration.of_min 5

(** A span representation of [poll_interval]. *)
let poll_interval_span = Ptime.Span.of_int_s (Duration.to_sec poll_interval)

(** number of times a cluster is checked before deciding if it's overloaded. *)
let scale_up_trigger_ticks = 3

(** if CPU usage >= 90% for [scale_up_trigger_ticks] consecutive checks then
    this vm can be cloned. *)
let scale_up_threshold_percent = 90.0

(** if average CPU usage < 40% for [scale_down_trigger_ticks] consecutive
    checks, one of the clone VMs can be pruned. *)
let scale_down_threshold_percent = 40.0

(** number of times a cluster is checked before deciding if it's underloaded. *)
let scale_down_trigger_ticks = 5

(** after spawning a new vm, wait [cooldown_period] before checking that vm
    again. *)
let cooldown_period = Ptime.Span.of_int_s 600

(** if no stats are gotten for a particular vm after [death_timeout] then
    consider the vm destroyed and prune it *)
let death_timeout = Ptime.Span.of_int_s 900

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
    let elapsed_time_difference = Ptime.diff now t.last_wall_time in
    let elapsed_time_in_seconds =
      Ptime.Span.to_float_s elapsed_time_difference
    in
    if elapsed_time_in_seconds <= 0.0 then 0.0
      (* cpu_delta can be negative if the VM was rebooted or stats counter reset *)
    else if cpu_delta < 0.0 then 0.0
    else
      let pct = cpu_delta /. elapsed_time_in_seconds *. 100.0 in
      (* TODO: use numcpus to cap it at 100.0% if the vm has more than 1 cpu. Now most
         vms use 1 cpu, so capping at 100% is fine. *)
      Float.min 100.0 pct
end

type t = {
  monitor : Cpu_monitor.t;
  last_cpu_usage : float;
  last_stats_received : Ptime.t;
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
    clones : vm list;
    last_scale_action : Ptime.t;
    last_tick_update : Ptime.t;
    next_id : int;
    consecutive_high_ticks : int;
    consecutive_low_ticks : int;
  }

  let in_cooldown now group =
    let span = Ptime.diff now group.last_scale_action in
    Ptime.Span.compare span cooldown_period < 0

  let should_tick now group =
    let span = Ptime.diff now group.last_tick_update in
    Ptime.Span.compare span poll_interval_span >= 0

  let extract_name_and_clone_id name =
    match List.rev (String.split_on_char '-' name) with
    | id_str :: "clone" :: primary_parts_rev -> (
        match int_of_string_opt id_str with
        | Some id -> Some (String.concat "-" (List.rev primary_parts_rev), id)
        | None -> None)
    | _ -> None

  let update_next_id clones =
    let max_id =
      List.fold_left
        (fun acc (name, _) ->
          match extract_name_and_clone_id name with
          | Some (_, id) -> max acc id
          | None -> acc)
        0 clones
    in
    max_id + 1

  let create_group primary =
    {
      primary;
      clones = [];
      last_scale_action = Ptime.epoch;
      consecutive_high_ticks = 0;
      consecutive_low_ticks = 0;
      last_tick_update = Ptime.epoch;
      next_id = 1;
    }

  let next_clone_name group =
    let primary_name = fst group.primary in
    Fmt.str "%s-clone-%d" primary_name group.next_id

  let add_clone_to_group g clone clone_id =
    if not (List.mem_assoc (fst clone) g.clones) then
      let new_clones = clone :: g.clones in
      { g with clones = new_clones; next_id = max g.next_id (clone_id + 1) }
    else g

  let register_clone group clone =
    let name = fst clone in
    if String.length name > 63 then
      Error (Fmt.str "Clone name '%s' exceeds the 63-character limit" name)
    else
      match extract_name_and_clone_id name with
      | Some (primary_name, clone_id) ->
          if String.equal primary_name (fst group.primary) then
            Ok
              {
                (add_clone_to_group group clone clone_id) with
                last_scale_action = Mirage_ptime.now ();
              }
          else
            Error
              (Fmt.str "Clone '%s' does not match group primary '%s'" name
                 (fst group.primary))
      | None -> Error (Fmt.str "Clone name '%s' is not a valid format" name)

  let remove_clone group clone_name =
    match extract_name_and_clone_id clone_name with
    | Some (primary_name, _) ->
        if String.equal primary_name (fst group.primary) then
          let new_clones =
            List.filter
              (fun (n, _) -> not (String.equal n clone_name))
              group.clones
          in
          let next_id = update_next_id new_clones in
          Ok
            {
              group with
              clones = new_clones;
              next_id;
              last_scale_action = Mirage_ptime.now ();
            }
        else
          Error
            (Fmt.str "Clone '%s' does not match group primary '%s'" clone_name
               (fst group.primary))
    | None -> Error (Fmt.str "Clone name '%s' is not a valid format" clone_name)

  (** [check_group_average group key now rusage] calculates the average CPU
      usage across all instances in the [group] (primary and clones). Since
      stats arrive for a single VM at a time (identified by [key]), we: 1.
      Calculate the current CPU usage for the VM [key] using the new [rusage].
      2. Use the last cached CPU usage for all other VMs in the group. 3. Update
      the state of VM [key] with the new measurements. 4. Return the computed
      average, the updated VM state, and the updated group. *)
  let check_group_average group key now rusage =
    let all_instances = group.primary :: group.clones in
    match List.assoc_opt key all_instances with
    | None -> Error "Current VM not found in group during average check"
    | Some vm ->
        let current_vm_usage = Cpu_monitor.measure vm.monitor now rusage in
        let state =
          {
            monitor = Cpu_monitor.create now rusage;
            last_cpu_usage = current_vm_usage;
            last_stats_received = now;
          }
        in
        let total_usage =
          List.fold_left
            (fun acc (name, v) ->
              if String.equal name key then acc +. current_vm_usage
              else acc +. v.last_cpu_usage)
            0.0 all_instances
        in
        let average_usage =
          total_usage /. float_of_int (List.length all_instances)
        in
        (* Construct the new updated group state *)
        let updated_group =
          if String.equal key (fst group.primary) then
            { group with primary = (key, state) }
          else
            let new_clones =
              List.map
                (fun (n, v) ->
                  if String.equal n key then (n, state) else (n, v))
                group.clones
            in
            { group with clones = new_clones }
        in
        Ok (average_usage, state, updated_group)

  (** [check_group_status group key now rusage] evaluates the scaling status of
      the [group] when new stats [rusage] at time [now] arrive for the VM [key].
      It updates the average load of the group, ticks the consecutive high/low
      counters, and determines if a scaling action (spawn or prune) is
      triggered. *)
  let check_group_status group key now rusage =
    match check_group_average group key now rusage with
    | Error e -> Error e
    | Ok (average_usage, current_vm_state, updated_group) ->
        let is_cooldown = in_cooldown now updated_group in
        let is_high = average_usage > scale_up_threshold_percent in
        let is_low =
          average_usage < scale_down_threshold_percent
          && updated_group.clones <> []
        in
        let updated_group =
          if should_tick now updated_group then
            {
              updated_group with
              last_tick_update = now;
              consecutive_high_ticks =
                (if is_high && not is_cooldown then
                   updated_group.consecutive_high_ticks + 1
                 else 0);
              consecutive_low_ticks =
                (if is_low && not is_cooldown then
                   updated_group.consecutive_low_ticks + 1
                 else 0);
            }
          else updated_group
        in
        let trigger state updated_group =
          let final_group =
            {
              updated_group with
              consecutive_high_ticks = 0;
              consecutive_low_ticks = 0;
              last_scale_action = now;
            }
          in
          Ok (state, final_group)
        in
        if is_cooldown then Ok (Cooldown current_vm_state, updated_group)
        else if updated_group.consecutive_high_ticks >= scale_up_trigger_ticks
        then trigger (Overloaded current_vm_state) updated_group
        else if updated_group.consecutive_low_ticks >= scale_down_trigger_ticks
        then
          let clone_to_kill = fst (List.hd updated_group.clones) in
          trigger (Underloaded (clone_to_kill, current_vm_state)) updated_group
        else if is_high then
          Ok
            ( Pending
                (`Spawn, updated_group.consecutive_high_ticks, current_vm_state),
              updated_group )
        else if is_low then
          Ok
            ( Pending
                (`Prune, updated_group.consecutive_low_ticks, current_vm_state),
              updated_group )
        else Ok (Normal current_vm_state, updated_group)

  (** [sync_group group active_vm_names] checks if the primary is still alive
      and prunes any clones that are no longer present in [active_vm_names].
      Returns the updated group, or Error `Primary_dead if the primary VM has
      died. *)
  let sync_group group active_vm_names =
    let primary_name = fst group.primary in
    if not (List.mem primary_name active_vm_names) then Error `Primary_dead
    else
      let new_clones =
        List.filter
          (fun (name, _) -> List.mem name active_vm_names)
          group.clones
      in
      let next_id = update_next_id new_clones in
      Ok { group with clones = new_clones; next_id }
end
