module Cpu_monitor = struct
  let rusage_to_float (sec, usec) =
    let s = Int64.to_float sec in
    let u = float_of_int usec in
    s +. (u /. 1_000_000.0)

  let get_total_cpu_time (r : Vmm_core.Stats.rusage) =
    let user_t = rusage_to_float r.utime in
    let sys_t = rusage_to_float r.stime in
    user_t +. sys_t

  type t = { mutable last_cpu_time : float; mutable last_wall_time : Ptime.t }

  let create now (initial_rusage : Vmm_core.Stats.rusage) =
    { last_cpu_time = get_total_cpu_time initial_rusage; last_wall_time = now }

  let measure t now (current_rusage : Vmm_core.Stats.rusage) =
    let curr_cpu_time = get_total_cpu_time current_rusage in
    let curr_wall_time = now in
    let cpu_delta = curr_cpu_time -. t.last_cpu_time in
    let wall_span = Ptime.diff curr_wall_time t.last_wall_time in
    let wall_delta = Ptime.Span.to_float_s wall_span in
    t.last_cpu_time <- curr_cpu_time;
    t.last_wall_time <- curr_wall_time;
    if wall_delta <= 0.000001 then 0.0
    else
      let pct = cpu_delta /. wall_delta *. 100.0 in
      Float.min 100.0 (Float.max 0.0 pct)
end

type t = {
  monitor : Cpu_monitor.t;
  mutable consecutive_high_ticks : int;
  mutable last_spawn_time : Ptime.t option;
}

type status =
  | Overloaded
  | Pending of int * float
    (* We have a high load but waiting for required number of polls to complete*)
  | Cooldown of float
  | Normal of float

let poll_interval = 300.0 (* check every 5 minutes *)
let trigger_ticks = 5
(* number of times a unikernel is checked and is overloaded before spawning a new clone i.e trigger_ticks x poll_interval determines how load a unikernel is overloaded for before we spawn. Currently this will be 25 minutes. *)

let threshold_percent =
  90.0 (* if CPU usage >= 90% for 5 consercutive checks then we spawn. *)

let cooldown_period = 600.0
(* after spawning a new clone, wait for 10 minutes before we start checking this unikernel again *)

let create now initial_rusage =
  {
    monitor = Cpu_monitor.create now initial_rusage;
    consecutive_high_ticks = 0;
    last_spawn_time = None;
  }

let in_cooldown now = function
  | { last_spawn_time = None; _ } -> false
  | { last_spawn_time = Some t; _ } ->
      let span = Ptime.diff now t in
      Ptime.Span.to_float_s span < cooldown_period

let check t now current_rusage =
  let usage = Cpu_monitor.measure t.monitor now current_rusage in
  if in_cooldown now t then (
    t.consecutive_high_ticks <- 0;
    Cooldown usage)
  else if usage > threshold_percent then (
    t.consecutive_high_ticks <- t.consecutive_high_ticks + 1;
    if t.consecutive_high_ticks >= trigger_ticks then (
      t.consecutive_high_ticks <- 0;
      t.last_spawn_time <- Some now;
      Overloaded)
    else Pending (t.consecutive_high_ticks, usage))
  else (
    t.consecutive_high_ticks <- 0;
    Normal usage)
