let account_usage_layout instance_name policy unikernels blocks =
  let user_policy_usage = Utils.user_policy_usage policy unikernels blocks in
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        h1
          ~a:[ a_class [ "text-3xl font-bold" ] ]
          [ txt ("Resources for " ^ instance_name) ];
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
                        ^ string_of_int
                            (policy.unikernels
                           - user_policy_usage.deployed_unikernels)
                        ^ ")','Running ("
                        ^ string_of_int user_policy_usage.deployed_unikernels
                        ^ ")'],\n\
                          \                    datasets: [{\n\
                          \                      label: 'Allocated',\n\
                          \                      data: ["
                        ^ string_of_int
                            (policy.unikernels
                           - user_policy_usage.deployed_unikernels)
                        ^ ", "
                        ^ string_of_int user_policy_usage.deployed_unikernels
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
                        ^ string_of_int user_policy_usage.total_free_space
                        ^ "MB)','Used ("
                        ^ string_of_int user_policy_usage.total_volume_used
                        ^ "MB)'],\n\
                          \                    datasets: [{\n\
                          \                      label: 'Size',\n\
                          \                      data: ["
                        ^ string_of_int user_policy_usage.total_free_space
                        ^ ", "
                        ^ string_of_int user_policy_usage.total_volume_used
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
                        ^ string_of_int
                            (policy.memory - user_policy_usage.total_memory_used)
                        ^ "MB)','Assigned ("
                        ^ string_of_int user_policy_usage.total_memory_used
                        ^ "MB)'],\n\
                          \                    datasets: [{\n\
                          \                      label: 'Allocated',\n\
                          \                      data: ["
                        ^ string_of_int
                            (policy.memory - user_policy_usage.total_memory_used)
                        ^ ", "
                        ^ string_of_int user_policy_usage.total_memory_used
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
                               user_policy_usage.cpu_usage_count)
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
                               user_policy_usage.bridge_usage_count)
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
