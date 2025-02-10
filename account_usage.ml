let account_usage_layout policy unikernels blocks =
  let deployed_unikernels = List.length unikernels in
  let total_volume_used =
    List.fold_left (fun total_size (_, size, _) -> total_size + size) 0 blocks
  in
  let total_free_space =
    Option.value ~default:0 policy.Vmm_core.Policy.block - total_volume_used
  in
  let total_memory_used =
    List.fold_left
      (fun total_memory (_, unikernel) ->
        total_memory + unikernel.Vmm_core.Unikernel.memory)
      0 unikernels
  in
  let count_cpuid_usage =
    let cpuid_count = Hashtbl.create (List.length unikernels) in
    (* count usage of each cpuid in unikernels *)
    List.iter
      (fun (_, unikernel) ->
        let cpuid = unikernel.Vmm_core.Unikernel.cpuid in
        let count =
          Hashtbl.find_opt cpuid_count cpuid |> Option.value ~default:0
        in
        Hashtbl.replace cpuid_count cpuid (count + 1))
      unikernels;
    (* Prepare list of all cpuids from policy *)
    let policy_cpuids = Vmm_core.IS.elements policy.cpuids in
    (* Generate the list with all policy.cpuids and their respective counts *)
    List.map
      (fun cpuid ->
        let count =
          Hashtbl.find_opt cpuid_count cpuid |> Option.value ~default:0
        in
        (cpuid, count))
      policy_cpuids
  in
  let count_bridge_usage =
    let bridge_count =
      Hashtbl.create (Vmm_core.String_set.cardinal policy.bridges)
    in
    (* Initialize all policy bridges with a count of 0 *)
    Vmm_core.String_set.iter
      (fun bridge -> Hashtbl.replace bridge_count bridge 0)
      policy.bridges;

    (* Count each bridge usage from unikernel.bridges, but only if it's in policy.bridges *)
    List.iter
      (fun (_, unikernel) ->
        List.iter
          (fun { Vmm_core.Unikernel.host_device; _ } ->
            (* Only count if the bridge is in policy.bridges *)
            if Vmm_core.String_set.mem host_device policy.bridges then
              let count =
                Hashtbl.find_opt bridge_count host_device
                |> Option.value ~default:0
              in
              Hashtbl.replace bridge_count host_device (count + 1))
          unikernel.Vmm_core.Unikernel.bridges)
      unikernels;

    (* Convert the hash table to a sorted list of (bridge_name, count) *)
    let bridge_usage_list =
      Hashtbl.fold
        (fun bridge count acc -> (bridge, count) :: acc)
        bridge_count []
    in
    (* Sort the list by bridge name *)
    List.sort (fun (b1, _) (b2, _) -> String.compare b1 b2) bridge_usage_list
  in
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        h1 ~a:[ a_class [ "text-3xl font-bold" ] ] [ txt "Resources" ];
        section
          ~a:[ a_id "usage-display"; a_class [ "grid grid-cols-3 gap-4" ] ]
          [
            div
              ~a:[ a_class [ "mx-auto text-center my-4" ] ]
              [
                h1
                  ~a:[ a_class [ "text-xl font-bold" ] ]
                  [ txt "Number of Unikernels" ];
                div
                  ~a:
                    [
                      Unsafe.string_attrib "x-data" "chart: null";
                      Unsafe.string_attrib "x-init"
                        ("\n\
                         \                  chart = new \
                          Chart(document.getElementById('unikernelChart').getContext('2d'), \
                          {\n\
                         \                  type: 'pie',\n\
                         \                  data: {\n\
                         \                    labels: ['Available ("
                        ^ string_of_int (policy.unikernels - deployed_unikernels)
                        ^ ")','Running ("
                        ^ string_of_int deployed_unikernels
                        ^ ")'],\n\
                          \                    datasets: [{\n\
                          \                      label: 'Allocated',\n\
                          \                      data: ["
                        ^ string_of_int (policy.unikernels - deployed_unikernels)
                        ^ ", "
                        ^ string_of_int deployed_unikernels
                        ^ "],\n\
                          \                      backgroundColor: ['rgb(156, \
                           156, 156)','rgb(54, 156, 140)'],\n\
                          \                      hoverOffset: 4,\n\
                          \                    }]\n\
                          \                  },\n\
                          \                  options: {}\n\
                          \                });\n\
                          \              ");
                      a_class [ "flex justify-center items-center" ];
                    ]
                  [ canvas ~a:[ a_id "unikernelChart" ] [] ];
              ];
            div
              ~a:[ a_class [ "mx-auto text-center my-4" ] ]
              [
                h1 ~a:[ a_class [ "text-xl font-bold" ] ] [ txt "Storage" ];
                div
                  ~a:
                    [
                      Unsafe.string_attrib "x-data" "chart: null";
                      Unsafe.string_attrib "x-init"
                        ("\n\
                         \                  chart = new \
                          Chart(document.getElementById('storageChart').getContext('2d'), \
                          {\n\
                         \                  type: 'pie',\n\
                         \                  data: {\n\
                         \                    labels: ['Free ("
                        ^ string_of_int total_free_space
                        ^ "MB)','Used ("
                        ^ string_of_int total_volume_used
                        ^ "MB)'],\n\
                          \                    datasets: [{\n\
                          \                      label: 'Size',\n\
                          \                      data: ["
                        ^ string_of_int total_free_space
                        ^ ", "
                        ^ string_of_int total_volume_used
                        ^ "],\n\
                          \                      backgroundColor: ['rgb(156, \
                           156, 156)','rgb(54, 156, 140)'],\n\
                          \                      hoverOffset: 4,\n\
                          \                    }]\n\
                          \                  },\n\
                          \                  options: {}\n\
                          \                });\n\
                          \              ");
                      a_class [ "flex justify-center items-center" ];
                    ]
                  [ canvas ~a:[ a_id "storageChart" ] [] ];
              ];
            div
              ~a:[ a_class [ "mx-auto text-center my-4" ] ]
              [
                h1 ~a:[ a_class [ "text-xl font-bold" ] ] [ txt "Memory" ];
                div
                  ~a:
                    [
                      Unsafe.string_attrib "x-data" "chart: null";
                      Unsafe.string_attrib "x-init"
                        ("\n\
                         \                  chart = new \
                          Chart(document.getElementById('memoryChart').getContext('2d'), \
                          {\n\
                         \                  type: 'pie',\n\
                         \                  data: {\n\
                         \                    labels: ['Free ("
                        ^ string_of_int (policy.memory - total_memory_used)
                        ^ "MB)','Assigned ("
                        ^ string_of_int total_memory_used
                        ^ "MB)'],\n\
                          \                    datasets: [{\n\
                          \                      label: 'Allocated',\n\
                          \                      data: ["
                        ^ string_of_int (policy.memory - total_memory_used)
                        ^ ", "
                        ^ string_of_int total_memory_used
                        ^ "],\n\
                          \                      backgroundColor: ['rgb(156, \
                           156, 156)','rgb(54, 156, 140)'],\n\
                          \                      hoverOffset: 4,\n\
                          \                    }]\n\
                          \                  },\n\
                          \                  options: {}\n\
                          \                });\n\
                          \              ");
                      a_class [ "flex justify-center items-center" ];
                    ]
                  [ canvas ~a:[ a_id "memoryChart" ] [] ];
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
                div
                  ~a:
                    [
                      Unsafe.string_attrib "x-data" "chart: null";
                      Unsafe.string_attrib "x-init"
                        ("\n\
                         \                          chart = new \
                          Chart(document.getElementById('cpuChart').getContext('2d'), \
                          {\n\
                         \                            type: 'bar',\n\
                         \                            data: {\n\
                         \                              labels: [\n\
                         \                                "
                        ^ String.concat ", "
                            (List.map
                               (fun id -> "\"" ^ string_of_int id ^ "\"")
                               (Vmm_core.IS.elements policy.cpuids))
                        ^ "\n\
                          \                              ],\n\
                          \                              datasets: [{\n\
                          \                                label: 'Usage count',\n\
                          \                                data: [\n\
                          \                                  "
                        ^ String.concat ", "
                            (List.map
                               (fun (_, count) ->
                                 "\"" ^ string_of_int count ^ "\"")
                               count_cpuid_usage)
                        ^ "\n\
                          \                                ],\n\
                          \                                backgroundColor: \
                           ['rgb(54, 156, 140)'],\n\
                          \                                hoverOffset: 4\n\
                          \                              }]\n\
                          \                            },\n\
                          \                            options: {\n\
                          \                              scales: {\n\
                          \                                x: {\n\
                          \                                  title: {\n\
                          \                                    display: true,\n\
                          \                                    text: 'CPU ID'\n\
                          \                                  }\n\
                          \                                },\n\
                          \                                y: {\n\
                          \                                  title: {\n\
                          \                                    display: true,\n\
                          \                                    text: 'Number \
                           of Unikernels'\n\
                          \                                  },\n\
                          \                                  ticks: {\n\
                          \                                    stepSize: 1,\n\
                          \                                    callback: \
                           function(value) {\n\
                          \                                      if \
                           (Number.isInteger(value)) {\n\
                          \                                        return value;\n\
                          \                                      }\n\
                          \                                      return null;\n\
                          \                                    }\n\
                          \                                  },\n\
                          \                                  beginAtZero: true\n\
                          \                                }\n\
                          \                              }\n\
                          \                            }\n\
                          \                          });\n\
                          \                        ");
                      a_class [ "flex justify-center items-center" ];
                    ]
                  [
                    div
                      ~a:
                        [
                          a_class [ "chart-container" ];
                          a_style "position:relative; height:40vh; width:80vh;";
                        ]
                      [ canvas ~a:[ a_id "cpuChart" ] [] ];
                  ];
              ];
            div
              ~a:[ a_class [ "mx-auto text-center my-4 mx-4" ] ]
              [
                h1
                  ~a:[ a_class [ "text-xl font-bold" ] ]
                  [ txt "Bridge usage in Unikernels" ];
                div
                  ~a:
                    [
                      Unsafe.string_attrib "x-data" "chart: null";
                      Unsafe.string_attrib "x-init"
                        ("\n\
                         \    chart = new \
                          Chart(document.getElementById('bridgeChart').getContext('2d'), \
                          {\n\
                         \      type: 'bar',\n\
                         \      data: {\n\
                         \        labels: [\n\
                         \          "
                        ^ String.concat ", "
                            (List.map
                               (fun bridge -> "\"" ^ bridge ^ "\"")
                               (Vmm_core.String_set.elements policy.bridges))
                        ^ "\n\
                          \        ],\n\
                          \        datasets: [{\n\
                          \          label: 'Usage Count',\n\
                          \          data: [\n\
                          \            "
                        ^ String.concat ", "
                            (List.map
                               (fun (_, count) ->
                                 "\"" ^ string_of_int count ^ "\"")
                               count_bridge_usage)
                        ^ "\n\
                          \          ],\n\
                          \          backgroundColor: ['rgb(54, 156, 140)'],\n\
                          \          hoverOffset: 4\n\
                          \        }]\n\
                          \      },\n\
                          \      options: {\n\
                          \        scales: {\n\
                          \          x: {\n\
                          \            title: {\n\
                          \              display: true,\n\
                          \              text: 'Bridge Name'\n\
                          \            }\n\
                          \          },\n\
                          \          y: {\n\
                          \            title: {\n\
                          \              display: true,\n\
                          \              text: 'Number of Unikernels'\n\
                          \            },\n\
                          \            ticks: {\n\
                          \              stepSize: 1,\n\
                          \              callback: function(value) {\n\
                          \                if (Number.isInteger(value)) {\n\
                          \                  return value;\n\
                          \                }\n\
                          \                return null;\n\
                          \              }\n\
                          \            },\n\
                          \            beginAtZero: true\n\
                          \          }\n\
                          \        }\n\
                          \      }\n\
                          \    });\n\
                          \  ");
                      a_class [ "flex justify-center items-center" ];
                    ]
                  [
                    div
                      ~a:
                        [
                          a_class [ "chart-container" ];
                          a_style "position:relative; height:40vh; width:80vh;";
                        ]
                      [ canvas ~a:[ a_id "bridgeChart" ] [] ];
                  ];
              ];
          ];
      ])
