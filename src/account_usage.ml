let account_usage_layout instance_name policy unikernels blocks =
  let user_policy_usage = Utils.user_policy_usage policy unikernels blocks in
  let avail_unikernels =
    policy.Vmm_core.Policy.unikernels - user_policy_usage.deployed_unikernels
  in
  let free_memory = policy.memory - user_policy_usage.total_memory_used in
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        h1
          ~a:[ a_class [ "text-3xl font-bold" ] ]
          [
            txt
              ("Resources for " ^ Models.Configuration.name_to_str instance_name);
          ];
        section
          ~a:[ a_id "usage-display"; a_class [ "grid grid-cols-3 gap-4" ] ]
          [
            div
              ~a:[ a_class [ "mx-auto text-center my-4" ] ]
              [
                h1
                  ~a:[ a_class [ "text-xl font-bold" ] ]
                  [ txt "Number of Unikernels" ];
                Ui.pie_chart ~canvas_id:"unikernelChart"
                  ~dataset_label:"Allocated"
                  ~labels:
                    [
                      "Available (" ^ string_of_int avail_unikernels ^ ")";
                      "Running ("
                      ^ string_of_int user_policy_usage.deployed_unikernels
                      ^ ")";
                    ]
                  ~data:
                    [ avail_unikernels; user_policy_usage.deployed_unikernels ];
              ];
            div
              ~a:[ a_class [ "mx-auto text-center my-4" ] ]
              [
                h1 ~a:[ a_class [ "text-xl font-bold" ] ] [ txt "Storage" ];
                Ui.pie_chart ~canvas_id:"storageChart" ~dataset_label:"Size"
                  ~labels:
                    [
                      "Free ("
                      ^ string_of_int user_policy_usage.total_free_space
                      ^ "MB)";
                      "Used ("
                      ^ string_of_int user_policy_usage.total_block_used
                      ^ "MB)";
                    ]
                  ~data:
                    [
                      user_policy_usage.total_free_space;
                      user_policy_usage.total_block_used;
                    ];
              ];
            div
              ~a:[ a_class [ "mx-auto text-center my-4" ] ]
              [
                h1 ~a:[ a_class [ "text-xl font-bold" ] ] [ txt "Memory" ];
                Ui.pie_chart ~canvas_id:"memoryChart" ~dataset_label:"Allocated"
                  ~labels:
                    [
                      "Free (" ^ string_of_int free_memory ^ "MB)";
                      "Assigned ("
                      ^ string_of_int user_policy_usage.total_memory_used
                      ^ "MB)";
                    ]
                  ~data:[ free_memory; user_policy_usage.total_memory_used ];
              ];
          ];
        section
          ~a:[ a_id "cpu-display"; a_class [ "block" ] ]
          [
            div
              ~a:[ a_class [ "mx-auto text-center my-4 mx-4" ] ]
              [
                h1
                  ~a:[ a_class [ "text-xl font-bold" ] ]
                  [ txt "CPU ID usage in Unikernels" ];
                (if user_policy_usage.cpu_usage_count = [] then
                   p
                     ~a:[ a_class [ "text-sm text-gray-400 italic mt-1" ] ]
                     [
                       txt
                         "More data will appear here as unikernels are \
                          instantiated and assigned CPU resources.";
                     ]
                 else p []);
                Ui.bar_chart ~canvas_id:"cpuChart" ~dataset_label:"Usage count"
                  ~labels:
                    (List.map string_of_int
                       (Vmm_core.IS.elements policy.cpuids))
                  ~data:(List.map snd user_policy_usage.cpu_usage_count)
                  ~x_title:"CPU ID" ~y_title:"Number of Unikernels";
              ];
            div
              ~a:[ a_class [ "mx-auto text-center my-4 mx-4" ] ]
              [
                h1
                  ~a:[ a_class [ "text-xl font-bold" ] ]
                  [ txt "Bridge usage in Unikernels" ];
                (if user_policy_usage.bridge_usage_count = [] then
                   p
                     ~a:[ a_class [ "text-sm text-gray-400 italic mt-1" ] ]
                     [
                       txt
                         "More data will appear here as unikernels deploy and \
                          map to network bridges.";
                     ]
                 else p []);
                Ui.bar_chart ~canvas_id:"bridgeChart"
                  ~dataset_label:"Usage Count"
                  ~labels:(Vmm_core.String_set.elements policy.bridges)
                  ~data:(List.map snd user_policy_usage.bridge_usage_count)
                  ~x_title:"Bridge Name" ~y_title:"Number of Unikernels";
              ];
          ];
      ])
