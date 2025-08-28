let account_usage_layout policies unikernels blocks =
  let user_policy_usages =
    Utils.user_policy_usages policies unikernels blocks
  in
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        h1 ~a:[ a_class [ "text-3xl font-bold" ] ] [ txt "Resources" ];
        section
          ~a:[ a_id "usage-display"; a_class [ "grid grid-cols-1 gap-4" ] ]
          (List.map
             (fun (instance_name, (user_policy_usage : Utils.user_policy_usage))
                ->
               let policy =
                 match policies with
                 | Some ps -> (
                     match List.assoc_opt instance_name ps with
                     | Some (Ok (Some p)) -> p
                     | _ -> failwith "Policy not found")
                 | None -> failwith "No policies"
               in
               div
                 ~a:[ a_class [ "mx-auto text-center my-4" ] ]
                 [
                   h2
                     ~a:[ a_class [ "text-xl font-bold" ] ]
                     [ txt ("Policy: " ^ instance_name) ];
                   (* Unikernel chart *)
                   div
                     ~a:
                       [
                         Unsafe.string_attrib "x-data" "chart: null";
                         Unsafe.string_attrib "x-init"
                           ("\n\
                             chart = new \
                             Chart(document.getElementById('unikernelChart-"
                          ^ instance_name
                          ^ "').getContext('2d'), {\n\
                             type: 'pie',\n\
                             data: {\n\
                             labels: ['Available ("
                           ^ string_of_int
                               (policy.unikernels
                              - user_policy_usage.deployed_unikernels)
                           ^ ")','Running ("
                           ^ string_of_int user_policy_usage.deployed_unikernels
                           ^ ")'],\ndatasets: [{\nlabel: 'Allocated',\ndata: ["
                           ^ string_of_int
                               (policy.unikernels
                              - user_policy_usage.deployed_unikernels)
                           ^ ", "
                           ^ string_of_int user_policy_usage.deployed_unikernels
                           ^ "],\n\
                              backgroundColor: ['rgb(156, 156, 156)','rgb(54, \
                              156, 140)'],\n\
                              hoverOffset: 4,\n\
                              }]\n\
                              },\n\
                              options: {}\n\
                              });\n");
                         a_class [ "flex justify-center items-center" ];
                       ]
                     [
                       canvas ~a:[ a_id ("unikernelChart-" ^ instance_name) ] [];
                     ];
                   (* Storage chart *)
                   div
                     ~a:
                       [
                         Unsafe.string_attrib "x-data" "chart: null";
                         Unsafe.string_attrib "x-init"
                           ("\n\
                             chart = new \
                             Chart(document.getElementById('storageChart-"
                          ^ instance_name
                          ^ "').getContext('2d'), {\n\
                             type: 'pie',\n\
                             data: {\n\
                             labels: ['Free ("
                           ^ string_of_int user_policy_usage.total_free_space
                           ^ "MB)','Used ("
                           ^ string_of_int user_policy_usage.total_volume_used
                           ^ "MB)'],\ndatasets: [{\nlabel: 'Size',\ndata: ["
                           ^ string_of_int user_policy_usage.total_free_space
                           ^ ", "
                           ^ string_of_int user_policy_usage.total_volume_used
                           ^ "],\n\
                              backgroundColor: ['rgb(156, 156, 156)','rgb(54, \
                              156, 140)'],\n\
                              hoverOffset: 4,\n\
                              }]\n\
                              },\n\
                              options: {}\n\
                              });\n");
                         a_class [ "flex justify-center items-center" ];
                       ]
                     [ canvas ~a:[ a_id ("storageChart-" ^ instance_name) ] [] ];
                   (* Memory chart *)
                   div
                     ~a:
                       [
                         Unsafe.string_attrib "x-data" "chart: null";
                         Unsafe.string_attrib "x-init"
                           ("\n\
                             chart = new \
                             Chart(document.getElementById('memoryChart-"
                          ^ instance_name
                          ^ "').getContext('2d'), {\n\
                             type: 'pie',\n\
                             data: {\n\
                             labels: ['Free ("
                           ^ string_of_int
                               (policy.memory
                              - user_policy_usage.total_memory_used)
                           ^ "MB)','Assigned ("
                           ^ string_of_int user_policy_usage.total_memory_used
                           ^ "MB)'],\n\
                              datasets: [{\n\
                              label: 'Allocated',\n\
                              data: ["
                           ^ string_of_int
                               (policy.memory
                              - user_policy_usage.total_memory_used)
                           ^ ", "
                           ^ string_of_int user_policy_usage.total_memory_used
                           ^ "],\n\
                              backgroundColor: ['rgb(156, 156, 156)','rgb(54, \
                              156, 140)'],\n\
                              hoverOffset: 4,\n\
                              }]\n\
                              },\n\
                              options: {}\n\
                              });\n");
                         a_class [ "flex justify-center items-center" ];
                       ]
                     [ canvas ~a:[ a_id ("memoryChart-" ^ instance_name) ] [] ];
                   (* CPU chart *)
                   div
                     ~a:
                       [
                         Unsafe.string_attrib "x-data" "chart: null";
                         Unsafe.string_attrib "x-init"
                           ("\n\
                             chart = new \
                             Chart(document.getElementById('cpuChart-"
                          ^ instance_name
                          ^ "').getContext('2d'), {\n\
                             type: 'bar',\n\
                             data: {\n\
                             labels: ["
                           ^ String.concat ", "
                               (List.map
                                  (fun id -> "\"" ^ string_of_int id ^ "\"")
                                  (Vmm_core.IS.elements policy.cpuids))
                           ^ "],\ndatasets: [{\nlabel: 'Usage count',\ndata: ["
                           ^ String.concat ", "
                               (List.map
                                  (fun (_, count) ->
                                    "\"" ^ string_of_int count ^ "\"")
                                  user_policy_usage.cpu_usage_count)
                           ^ "],\n\
                              backgroundColor: ['rgb(54, 156, 140)'],\n\
                              hoverOffset: 4\n\
                              }]\n\
                              },\n\
                              options: {\n\
                              scales: {\n\
                              x: {\n\
                              title: {\n\
                              display: true,\n\
                              text: 'CPU ID'\n\
                              }\n\
                              },\n\
                              y: {\n\
                              title: {\n\
                              display: true,\n\
                              text: 'Number of Unikernels'\n\
                              },\n\
                              ticks: {\n\
                              stepSize: 1,\n\
                              callback: function(value) {\n\
                              if (Number.isInteger(value)) {\n\
                              return value;\n\
                              }\n\
                              return null;\n\
                              }\n\
                              },\n\
                              beginAtZero: true\n\
                              }\n\
                              }\n\
                              }\n\
                              });\n");
                         a_class [ "flex justify-center items-center" ];
                       ]
                     [
                       div
                         ~a:
                           [
                             a_class [ "chart-container" ];
                             a_style
                               "position:relative; height:40vh; width:80vh;";
                           ]
                         [ canvas ~a:[ a_id ("cpuChart-" ^ instance_name) ] [] ];
                     ];
                   (* Bridge chart *)
                   div
                     ~a:
                       [
                         Unsafe.string_attrib "x-data" "chart: null";
                         Unsafe.string_attrib "x-init"
                           ("\n\
                             chart = new \
                             Chart(document.getElementById('bridgeChart-"
                          ^ instance_name
                          ^ "').getContext('2d'), {\n\
                             type: 'bar',\n\
                             data: {\n\
                             labels: ["
                           ^ String.concat ", "
                               (List.map
                                  (fun bridge -> "\"" ^ bridge ^ "\"")
                                  (Vmm_core.String_set.elements policy.bridges))
                           ^ "],\ndatasets: [{\nlabel: 'Usage Count',\ndata: ["
                           ^ String.concat ", "
                               (List.map
                                  (fun (_, count) ->
                                    "\"" ^ string_of_int count ^ "\"")
                                  user_policy_usage.bridge_usage_count)
                           ^ "],\n\
                              backgroundColor: ['rgb(54, 156, 140)'],\n\
                              hoverOffset: 4\n\
                              }]\n\
                              },\n\
                              options: {\n\
                              scales: {\n\
                              x: {\n\
                              title: {\n\
                              display: true,\n\
                              text: 'Bridge Name'\n\
                              }\n\
                              },\n\
                              y: {\n\
                              title: {\n\
                              display: true,\n\
                              text: 'Number of Unikernels'\n\
                              },\n\
                              ticks: {\n\
                              stepSize: 1,\n\
                              callback: function(value) {\n\
                              if (Number.isInteger(value)) {\n\
                              return value;\n\
                              }\n\
                              return null;\n\
                              }\n\
                              },\n\
                              beginAtZero: true\n\
                              }\n\
                              }\n\
                              }\n\
                              });\n");
                         a_class [ "flex justify-center items-center" ];
                       ]
                     [
                       div
                         ~a:
                           [
                             a_class [ "chart-container" ];
                             a_style
                               "position:relative; height:40vh; width:80vh;";
                           ]
                         [
                           canvas
                             ~a:[ a_id ("bridgeChart-" ^ instance_name) ]
                             [];
                         ];
                     ];
                 ])
             user_policy_usages);
      ])
