open Controller

let ( let* ) = Result.bind

let scaling_groups : Autoscaler.Cluster_manager.group Utils.LM.t Utils.LM.t ref
    =
  ref Utils.LM.empty

module Log = (val Logs.src_log Autoscaler.a_logs : Logs.LOG)

let least_used_cpuid tcp albatross_inst user_name =
  let unikernels =
    Albatross_client.user_unikernels_by_instance tcp albatross_inst user_name
  in
  let* p =
    Option.to_result ~none:"No policy found for user"
      (Albatross_client.policy ~domain:user_name albatross_inst)
  in
  match
    Utils.cpu_usage_count p unikernels
    |> List.sort (fun (_, c1) (_, c2) -> Int.compare c1 c2)
  with
  | (id, _) :: _ -> Ok id
  | [] -> Error "No cpuids available in user policy"

let put_group ~(user_name : Vmm_core.Name.Label.t)
    ~(unikernel_name : Vmm_core.Name.Label.t) group =
  let unikernel_map =
    Option.value ~default:Utils.LM.empty
      (Utils.LM.find_opt user_name !scaling_groups)
  in
  scaling_groups :=
    Utils.LM.add user_name
      (Utils.LM.add unikernel_name group unikernel_map)
      !scaling_groups

let spawn_clone tcp store albatross_inst ~unikernel_name ~clone_name ~user_name
    group scaler =
  let* user =
    Option.to_result ~none:"User not found"
      (Storage.find_by_name store user_name)
  in
  let* _, unikernel =
    Albatross_client.user_unikernel tcp albatross_inst ~user_name
      ~unikernel_name
    |> Result.map_error (fun err ->
        Fmt.str "An error occured trying to fetch %s from albatross: %s"
          (Models.Configuration.name_to_str unikernel_name)
          err)
  in
  if unikernel.block_devices <> [] then (
    Log.info (fun m ->
        m "spawn_clone: Aborting clone of %s because it has block devices."
          (Models.Configuration.name_to_str unikernel_name));
    Error "Unikernels with block devices cannot be safely cloned")
  else
    let bridges =
      List.map
        (fun { Vmm_core.Unikernel.unikernel_device; host_device; _ } ->
          (unikernel_device, Some host_device, None))
        unikernel.bridges
    in
    let* cpuid = least_used_cpuid tcp albatross_inst user_name in
    let argv =
      Option.map
        (fun args ->
          List.map
            (fun arg ->
              String.split_on_char ' ' arg
              |> List.filter (fun s ->
                  not (String.starts_with ~prefix:"--name=" s))
              |> String.concat " ")
            args
          |> List.filter (fun s -> s <> ""))
        unikernel.argv
    in
    let config =
      Models.Unikernel.config_of_info ~compressed:true
        ~cpuids:(Vmm_core.IS.singleton cpuid)
        ~block_devices:[] ~bridges ~argv unikernel
    in
    let buf = Buffer.create 65536 in
    let* () =
      Albatross_client.query_unikernel_binary tcp albatross_inst
        ~domain:user_name ~name:unikernel_name (fun binary_str ->
          Buffer.add_string buf binary_str;
          Ok ())
      |> Result.map_error (fun err ->
          put_group ~user_name ~unikernel_name
            (Autoscaler.Cluster_manager.remove_cooldown group);
          Log.err (fun m ->
              m "Error getting binary for %s: %s"
                (Models.Configuration.name_to_str unikernel_name)
                err);
          err)
    in
    let push = make_string_stream (Buffer.contents buf) in
    let* () =
      Unikernel.force_create_unikernel tcp albatross_inst
        ~unikernel_name:clone_name ~push config user
      |> Result.map_error (fun (`Msg err, _status) ->
          put_group ~user_name ~unikernel_name
            (Autoscaler.Cluster_manager.remove_cooldown group);
          err)
    in
    let* spawn_group =
      Autoscaler.Cluster_manager.register_clone group (clone_name, scaler)
      |> Result.map_error (fun e ->
          Log.err (fun m ->
              m "Error registering clone %s: %s"
                (Models.Configuration.name_to_str clone_name)
                e);
          Fmt.str "Error registering clone %s: %s"
            (Models.Configuration.name_to_str clone_name)
            e)
    in
    Log.debug (fun m ->
        m "Successfully cloned %s to %s"
          (Models.Configuration.name_to_str unikernel_name)
          (Models.Configuration.name_to_str clone_name));
    put_group ~user_name ~unikernel_name spawn_group;
    Ok ()

let prune_clone tcp albatross_inst group ~unikernel_name ~clone_to_kill
    ~user_name =
  let log_prune_err err =
    Log.err (fun m ->
        m "Error pruning unikernel %s: %s"
          (Models.Configuration.name_to_str clone_to_kill)
          err);
    err
  in
  let* _hdr, res =
    Albatross_client.query tcp albatross_inst ~domain:user_name
      ~name:clone_to_kill (`Unikernel_cmd `Unikernel_destroy)
    |> Result.map_error log_prune_err
  in
  let* _res =
    Albatross_json.res res
    |> Result.map_error (fun (`String err) -> log_prune_err err)
  in
  let* post_prune_group =
    Autoscaler.Cluster_manager.remove_clone group clone_to_kill
    |> Result.map_error (fun e ->
        Log.err (fun m ->
            m "Error removing clone %s: %s"
              (Models.Configuration.name_to_str clone_to_kill)
              e);
        e)
  in
  Log.debug (fun m ->
      m "Succesfully pruned %s" (Models.Configuration.name_to_str clone_to_kill));
  put_group ~user_name ~unikernel_name post_prune_group;
  Ok ()

let evaluate_scaling tcp store albatross_inst group current_time user_name
    unikernel_name =
  match Autoscaler.Cluster_manager.evaluate_scaling group current_time with
  | Error err ->
      Error
        (Fmt.str "Error evaluating scaling for cluster %s/%s: %s"
           (Models.Configuration.name_to_str user_name)
           (Models.Configuration.name_to_str unikernel_name)
           err)
  | Ok (status, group) -> (
      put_group ~user_name ~unikernel_name group;
      match status with
      | Autoscaler.Normal _scaler -> Ok ()
      | Autoscaler.Pending (`Spawn, ticks, scaler) ->
          Log.info (fun m ->
              m "%s/%s: high usage (%d%%), [ticks: %d/%d]"
                (Models.Configuration.name_to_str user_name)
                (Models.Configuration.name_to_str unikernel_name)
                scaler.last_cpu_usage ticks Autoscaler.scale_up_trigger_ticks);
          Ok ()
      | Autoscaler.Cooldown scaler ->
          Log.info (fun m ->
              m "%s/%s: cooldown (usage: %d%%)"
                (Models.Configuration.name_to_str user_name)
                (Models.Configuration.name_to_str unikernel_name)
                scaler.last_cpu_usage);
          Ok ()
      | Autoscaler.Pending (`Prune, ticks, scaler) ->
          Log.info (fun m ->
              m "%s/%s: low usage (%d%%), [ticks: %d/%d]"
                (Models.Configuration.name_to_str user_name)
                (Models.Configuration.name_to_str unikernel_name)
                scaler.last_cpu_usage ticks Autoscaler.scale_down_trigger_ticks);
          Ok ()
      | Autoscaler.Overloaded scaler -> (
          match Autoscaler.Cluster_manager.next_clone_name group with
          | Error (`Msg e) -> Error (Fmt.str "Error: %s" e)
          | Ok next_name ->
              Log.info (fun m ->
                  m "%s/%s: overloaded (%d%%), spawning new clone: %s"
                    (Models.Configuration.name_to_str user_name)
                    (Models.Configuration.name_to_str unikernel_name)
                    scaler.last_cpu_usage
                    (Models.Configuration.name_to_str next_name));
              spawn_clone tcp store albatross_inst ~unikernel_name
                ~clone_name:next_name ~user_name group scaler)
      | Autoscaler.Underloaded (clone_to_kill, scaler) ->
          Log.info (fun m ->
              m "[%s/%s]: underloaded (%d%%), pruning: %s"
                (Models.Configuration.name_to_str user_name)
                (Models.Configuration.name_to_str unikernel_name)
                scaler.last_cpu_usage
                (Models.Configuration.name_to_str clone_to_kill));
          prune_clone tcp albatross_inst group ~unikernel_name ~clone_to_kill
            ~user_name)

let handle_stats tcp state ((rusage, _, _) : Vmm_core.Stats.t) name store =
  match Vmm_core.Name.name name with
  | None -> Error "VM name has no unikernel label"
  | Some unikernel_name -> (
      match Vmm_core.Name.Path.to_labels (Vmm_core.Name.path name) with
      | [ user_label ] -> (
          match Storage.find_by_name store user_label with
          | None ->
              Error
                (Fmt.str "User %s not found."
                   (Models.Configuration.name_to_str user_label))
          | Some user ->
              let primary_name =
                Option.value ~default:unikernel_name
                  (Option.map fst
                     (Autoscaler.Cluster_manager.extract_name_and_clone_id
                        unikernel_name))
              in
              let is_scaling_enabled =
                Models.User.Scaling_policy_map.exists
                  (fun (p_name, _) _ ->
                    Vmm_core.Name.Label.equal p_name primary_name)
                  user.scaling_policies
              in
              if is_scaling_enabled then (
                Log.debug (fun m ->
                    m "[Stats] Received stats for %s as %a"
                      (Models.Configuration.name_to_str unikernel_name)
                      Vmm_core.Stats.pp_rusage rusage);
                let current_time = now () in
                let group =
                  let unikernel_map =
                    Option.value ~default:Utils.LM.empty
                      (Utils.LM.find_opt user.name !scaling_groups)
                  in
                  match Utils.LM.find_opt primary_name unikernel_map with
                  | Some g -> g
                  | None ->
                      let primary_vm =
                        (primary_name, Autoscaler.create current_time rusage)
                      in
                      Autoscaler.Cluster_manager.create_group primary_vm
                in
                let group =
                  if
                    (not
                       (Vmm_core.Name.Label.equal unikernel_name primary_name))
                    && not (List.mem_assoc unikernel_name group.clones)
                  then (
                    let clone_vm =
                      (unikernel_name, Autoscaler.create current_time rusage)
                    in
                    match
                      Autoscaler.Cluster_manager.register_clone group clone_vm
                    with
                    | Ok updated_g -> updated_g
                    | Error err ->
                        Log.err (fun m ->
                            m "Error registering clone %s: %s"
                              (Models.Configuration.name_to_str unikernel_name)
                              err);
                        group)
                  else group
                in
                match
                  Autoscaler.Cluster_manager.update_vm_metrics group
                    unikernel_name current_time rusage
                with
                | Error err ->
                    Error
                      (Fmt.str "Error updating metrics for unikernel %s: %s"
                         (Models.Configuration.name_to_str unikernel_name)
                         err)
                | Ok (_, updated_group) ->
                    put_group ~user_name:user.name ~unikernel_name:primary_name
                      updated_group;
                    if Vmm_core.Name.Label.equal unikernel_name primary_name
                    then
                      evaluate_scaling tcp store state updated_group
                        current_time user.name primary_name
                    else Ok ())
              else Ok ())
      | _ -> Error "VM path must contain exactly one user domain level")

let unikernels_stats tcp (instance : Albatross_client.t) store =
  let cb name st =
    (match handle_stats tcp instance st name store with
    | Ok () -> ()
    | Error err ->
        Log.err (fun m ->
            m "Error handling stats for %s: %s"
              (Vmm_core.Name.to_string name)
              err));
    Ok ()
  in
  match Albatross_client.query_stats tcp instance cb with
  | Error err ->
      Error
        (Fmt.str "Error fetching stats for albatross instance %s: %s"
           (Models.Configuration.name_to_str instance.configuration.name)
           err)
  | Ok () -> Ok ()

let seconds_until_next_midnight () =
  let current_time = now () in
  let _, ((hour, min, sec), _) = Ptime.to_date_time current_time in
  let passed = (hour * 3600) + (min * 60) + sec in
  86_400 - passed

let check_user_unikernel_updates tcp albatross_instances
    (user : Models.User.user) he =
  let unikernels_per_instance =
    Albatross_client.user_unikernels tcp albatross_instances user.name
  in
  let available_updates =
    Utils.LM.filter_map
      (fun _instance_name unikernels ->
        let updates =
          List.filter_map
            (fun (name, unikernel) ->
              match Update_flow.check_for_update name unikernel he with
              | Ok (Update_flow.Update_available (name, unikernel, comparison))
                ->
                  Some (name, unikernel, comparison)
              | _ -> None)
            unikernels
        in
        if updates = [] then None else Some updates)
      unikernels_per_instance
  in
  { Update_flow.user; available_updates }

let check_available_unikernel_updates tcp albatross_instances
    (users : Models.User.user Utils.UM.t) he =
  Utils.UM.fold
    (fun _ user acc ->
      let update_report =
        check_user_unikernel_updates tcp albatross_instances user he
      in
      if Utils.LM.is_empty update_report.available_updates then acc
      else update_report :: acc)
    users []

let run_background_update_check he users tcp email_config albatross_instances =
  match email_config with
  | None ->
      Logs.info (fun m ->
          m "Email not configured, skipping background update check")
  | Some email_config -> (
      try
        Logs.info (fun m ->
            m "Starting background update check for all users' unikernels");
        let reports =
          check_available_unikernel_updates tcp albatross_instances users he
        in
        List.iter
          (fun (report : Update_flow.user_unikernel_available_updates) ->
            let references =
              Fmt.str "updates-%s@robur.coop" (Uuidm.to_string report.user.uuid)
            in
            match
              Email_client.send_update_report_email ~references he email_config
                report
            with
            | Ok () ->
                Logs.info (fun m ->
                    m "Sent update report email to %s"
                      (Emile.to_string report.user.email))
            | Error err ->
                Logs.err (fun m ->
                    m "Failed to send update report email to %s: %s"
                      (Emile.to_string report.user.email)
                      err))
          reports
      with exn ->
        Logs.err (fun m ->
            m "Background update check failed: %s" (Printexc.to_string exn)))

let start_background_scheduler he tcp store albatross_instances_ref =
  let rec loop () =
    let delay = seconds_until_next_midnight () in
    Logs.info (fun m -> m "Next background update in %d seconds" delay);
    Mkernel.sleep (Int64.to_int (Duration.of_sec delay));
    (try
       (* prune expired cookie sessions everyday *)
       Logs.info (fun m -> m "Pruning expired sessions...");
       ignore (Storage.prune_expired_sessions store (now ()));
       Logs.info (fun m -> m "Starting background update...");
       run_background_update_check he (Storage.users store) tcp
         (Storage.email store) !albatross_instances_ref
     with exn ->
       Logs.err (fun m ->
           m "Background update failed: %s" (Printexc.to_string exn)));
    loop ()
  in
  loop ()

let start_background_scaler_scheduler tcp store albatross_instances_ref =
  let active_streams = ref Vmm_core.String_set.empty in
  let orphans = Miou.orphans () in
  let rec clean_orphans () =
    match Miou.care orphans with
    | Some (Some prm) ->
        let _ = Miou.await prm in
        clean_orphans ()
    | _ -> ()
  in
  let spawn_stats_stream key (instance : Albatross_client.t) =
    Fun.protect
      ~finally:(fun () ->
        active_streams := Vmm_core.String_set.remove key !active_streams)
      (fun () ->
        let rec stream_loop () =
          match
            try Ok (unikernels_stats tcp instance store) with exn -> Error exn
          with
          | Ok (Ok ()) ->
              Mkernel.sleep (Int64.to_int (Duration.of_sec 10));
              (stream_loop [@tailcall]) ()
          | Ok (Error err) ->
              Log.err (fun m ->
                  m "Stats stream for %s failed: %s"
                    (Models.Configuration.name_to_str
                       instance.configuration.name)
                    err)
          | Error exn ->
              Log.err (fun m ->
                  m "Exception in stats stream for %s: %s. Retrying in 10s..."
                    (Models.Configuration.name_to_str
                       instance.configuration.name)
                    (Printexc.to_string exn));
              Mkernel.sleep (Int64.to_int (Duration.of_sec 10));
              (stream_loop [@tailcall]) ()
        in
        stream_loop ())
  in
  let rec loop () =
    clean_orphans ();
    let current_instances = !albatross_instances_ref in
    Utils.LM.iter
      (fun instance_name instance ->
        let key = Models.Configuration.name_to_str instance_name in
        if not (Vmm_core.String_set.mem key !active_streams) then (
          Log.debug (fun m ->
              m "Spawning new stats stream for albatross instance %s" key);
          active_streams := Vmm_core.String_set.add key !active_streams;
          ignore
            (Miou.async ~orphans (fun () -> spawn_stats_stream key instance))))
      current_instances;
    Mkernel.sleep (Int64.to_int (Duration.of_sec 30));
    loop ()
  in
  loop ()

let run_prune_dead_clusters tcp albatross_instances_ref =
  let rec loop () =
    Log.debug (fun m ->
        m "Pruning dead clusters in %a" Ptime.Span.pp Autoscaler.death_timeout);
    let sleep_ns =
      Int64.to_int
        (Duration.of_sec
           (Option.value ~default:600
              (Ptime.Span.to_int_s Autoscaler.death_timeout)))
    in
    (try
       Log.debug (fun m -> m "Starting background pruning...");
       Utils.LM.iter
         (fun user_name unikernel_map ->
           let results =
             Albatross_client.user_unikernels tcp !albatross_instances_ref
               user_name
           in
           let active_vm_names =
             Utils.LM.fold
               (fun _instance_name vms acc ->
                 let names =
                   List.filter_map
                     (fun (name, _info) -> Vmm_core.Name.name name)
                     vms
                 in
                 acc @ names)
               results []
           in
           Utils.LM.iter
             (fun unikernel_name group ->
               match
                 Autoscaler.Cluster_manager.sync_group group active_vm_names
               with
               | Ok updated_group ->
                   put_group ~user_name ~unikernel_name updated_group
               | Error `Primary_dead ->
                   Log.warn (fun m ->
                       m "Primary dead for %s/%s. Removing cluster."
                         (Models.Configuration.name_to_str user_name)
                         (Models.Configuration.name_to_str unikernel_name));
                   let updated_unikernel_map =
                     Utils.LM.remove unikernel_name unikernel_map
                   in
                   if Utils.LM.is_empty updated_unikernel_map then
                     scaling_groups := Utils.LM.remove user_name !scaling_groups
                   else
                     scaling_groups :=
                       Utils.LM.add user_name updated_unikernel_map
                         !scaling_groups)
             unikernel_map)
         !scaling_groups
     with exn ->
       Log.err (fun m ->
           m "Background pruning failed: %s" (Printexc.to_string exn)));
    Mkernel.sleep sleep_ns;
    loop ()
  in
  loop ()

let device : (env, unit Miou.t * unit Miou.t * unit Miou.t) Vifu.Device.device =
  let finally (p1, p2, p3) =
    Miou.cancel p1;
    Miou.cancel p2;
    Miou.cancel p3
  in
  let open Vifu.Device in
  v ~name:"scheduler" ~finally
    [ value storage; value happy_eyeballs; value albatross ]
  @@ fun store he albatross_instances_ref env ->
  let p1 =
    Miou.async (fun () : unit ->
        start_background_scheduler he env.tcp store albatross_instances_ref)
  in
  let p2 =
    Miou.async (fun () : unit ->
        start_background_scaler_scheduler env.tcp store albatross_instances_ref)
  in
  let p3 =
    Miou.async (fun () : unit ->
        run_prune_dead_clusters env.tcp albatross_instances_ref)
  in
  (p1, p2, p3)
