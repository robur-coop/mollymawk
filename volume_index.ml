let volume_index_layout
    (volumes_by_instance : (string * (Vmm_core.Name.t * int * bool) list) list)
    (policies : (string * Vmm_core.Policy.t) list) =
  let instance_data_json =
    let open Yojson.Basic in
    let data =
      List.filter_map
        (fun (name, policy) ->
          let volumes =
            List.assoc_opt name volumes_by_instance |> Option.value ~default:[]
          in
          let total_volume_used =
            List.fold_left (fun acc (_, size, _) -> acc + size) 0 volumes
          in
          let total_free_space =
            Option.value ~default:0 policy.Vmm_core.Policy.block
            - total_volume_used
          in
          let volumes_json =
            `List
              (List.map
                 (fun (n, size, used) ->
                   `Assoc
                     [
                       ("name", `String (Vmm_core.Name.to_string n));
                       ("size", `Int size);
                       ("used", `Bool used);
                     ])
                 volumes)
          in
          Some
            ( name,
              `Assoc
                [
                  ("volumes", volumes_json);
                  ("policy", Albatross_json.policy_to_json policy);
                  ( "stats",
                    `Assoc
                      [
                        ("total_volume_used", `Int total_volume_used);
                        ("total_free_space", `Int total_free_space);
                        ("volume_count", `Int (List.length volumes));
                      ] );
                ] ))
        policies
    in
    `Assoc data |> to_string
  in
  let initial_instance_name =
    match policies with (name, _) :: _ -> name | [] -> ""
  in
  Tyxml_html.(
    section
      ~a:
        [
          a_class [ "col-span-7 p-4 bg-gray-50 my-1" ];
          Unsafe.string_attrib "x-data"
            (Fmt.str
               "{ instanceData: %s, selectedInstance: '%s', chart: null, get \
                selected() { return this.instanceData[this.selectedInstance] \
                }, updateChart() { if (this.chart) { this.chart.destroy(); } \
                const stats = this.selected.stats; this.chart = new \
                Chart(document.getElementById('usageChart').getContext('2d'), \
                { type: 'pie', data: { labels: [`Free \
                (${stats.total_free_space}MB)`, `Used \
                (${stats.total_volume_used}MB)`], datasets: [{ data: \
                [stats.total_free_space, stats.total_volume_used], \
                backgroundColor: ['rgb(156, 156, 156)','rgb(54, 156, 140)'], \
                hoverOffset: 4 }] } }); } }"
               instance_data_json initial_instance_name);
          Unsafe.string_attrib "x-init"
            "$watch('selectedInstance', () => updateChart()); updateChart()";
        ]
      [
        section
          ~a:[ a_id "block-display"; a_class [ "block" ] ]
          [
            div
              ~a:[ a_class [ "px-3 flex justify-between items-center" ] ]
              [
                div
                  ~a:[ a_class [ "flex items-center space-x-4" ] ]
                  [
                    p
                      ~a:[ a_class [ "font-bold text-gray-700" ] ]
                      [
                        txt "Volumes (";
                        span
                          ~a:
                            [
                              Unsafe.string_attrib "x-text"
                                "selected.stats.volume_count";
                            ]
                          [];
                        txt ")";
                      ];
                    select
                      ~a:
                        [
                          Unsafe.string_attrib "x-model" "selectedInstance";
                          a_class
                            [
                              "rounded py-2 px-3 border border-primary-200 \
                               focus:border-primary-500 outline-0";
                            ];
                        ]
                      (List.map
                         (fun (name, _) ->
                           option ~a:[ a_value name ] (txt name))
                         policies);
                  ];
                div
                  ~a:[ Unsafe.string_attrib "x-show" "selected" ]
                  [
                    Modal_dialog.modal_dialog ~modal_title:"Create a volume"
                      ~button_content:(txt "Create block device")
                      ~content:
                        (div
                           ~a:
                             [
                               Unsafe.string_attrib "x-data"
                                 "{ free_space: \
                                  selected.stats.total_free_space }";
                             ]
                           [ Volume_ui.create_volume ])
                      ();
                  ];
              ];
            div
              ~a:[ a_class [ "mx-auto text-center" ] ]
              [
                div
                  ~a:
                    [
                      a_class [ "flex justify-center items-center" ];
                      a_style "position: relative; height:30vh; width:70vw;";
                    ]
                  [ canvas ~a:[ a_id "usageChart" ] [] ];
              ];
            div
              [
                input
                  ~a:
                    [
                      a_onkeyup "filterData()";
                      a_placeholder "search";
                      a_id "searchQuery";
                      a_name "searchQuery";
                      a_input_type `Text;
                      a_class
                        [
                          "rounded py-2 px-3 border border-primary-200 \
                           focus:border-primary-500 outline-0";
                        ];
                    ]
                  ();
              ];
            hr ~a:[ a_class [ "border border-primary-500 my-5" ] ] ();
            p ~a:[ a_id "form-alert"; a_class [ "my-4 hidden" ] ] [];
            div
              ~a:[ a_class [ "flex flex-col" ] ]
              [
                div
                  ~a:[ a_class [ "-m-1.5 overflow-x-auto" ] ]
                  [
                    div
                      ~a:
                        [
                          a_class
                            [ "p-1.5 min-w-full inline-block align-middle" ];
                        ]
                      [
                        div
                          ~a:
                            [
                              Unsafe.string_attrib "x-data" "sort_data()";
                              a_class [ "overflow-hidden" ];
                            ]
                          [
                            table
                              ~a:
                                [
                                  a_id "data-table";
                                  a_class
                                    [
                                      "table-auto min-w-full divide-y \
                                       divide-gray-200";
                                    ];
                                ]
                              ~thead:
                                (thead
                                   [
                                     tr
                                       [
                                         th
                                           ~a:
                                             [
                                               Unsafe.string_attrib "x-on:click"
                                                 "sortByColumn";
                                               a_class
                                                 [
                                                   "px-6 py-3 text-start \
                                                    text-xs font-bold \
                                                    text-primary-600 uppercase \
                                                    cursor-pointer select-none";
                                                 ];
                                             ]
                                           [
                                             txt "Host Device";
                                             span
                                               ~a:[ a_class [ "px-2" ] ]
                                               [
                                                 i
                                                   ~a:
                                                     [
                                                       a_class
                                                         [ "fa-solid fa-sort" ];
                                                     ]
                                                   [];
                                               ];
                                           ];
                                         th
                                           ~a:
                                             [
                                               Unsafe.string_attrib "x-on:click"
                                                 "sortByColumn";
                                               a_class
                                                 [
                                                   "px-6 py-3 text-start \
                                                    text-xs font-bold \
                                                    text-primary-600 uppercase \
                                                    cursor-pointer select-none";
                                                 ];
                                             ]
                                           [
                                             txt "Size (MB)";
                                             span
                                               ~a:[ a_class [ "px-2" ] ]
                                               [
                                                 i
                                                   ~a:
                                                     [
                                                       a_class
                                                         [ "fa-solid fa-sort" ];
                                                     ]
                                                   [];
                                               ];
                                           ];
                                         th
                                           ~a:
                                             [
                                               Unsafe.string_attrib "x-on:click"
                                                 "sortByColumn";
                                               a_class
                                                 [
                                                   "px-6 py-3 text-start \
                                                    text-xs font-bold \
                                                    text-primary-600 uppercase \
                                                    cursor-pointer select-none";
                                                 ];
                                             ]
                                           [
                                             txt "Used";
                                             span
                                               ~a:[ a_class [ "px-2" ] ]
                                               [
                                                 i
                                                   ~a:
                                                     [
                                                       a_class
                                                         [ "fa-solid fa-sort" ];
                                                     ]
                                                   [];
                                               ];
                                           ];
                                         th
                                           ~a:
                                             [
                                               a_class
                                                 [
                                                   "px-6 py-3 text-start \
                                                    text-xs font-bold \
                                                    text-primary-600 uppercase \
                                                    cursor-pointer select-none";
                                                 ];
                                             ]
                                           [ txt "Action" ];
                                       ];
                                   ])
                              (List.map
                                 (fun ((name, size, used) :
                                        Vmm_core.Name.t * int * bool) ->
                                   let name =
                                     Option.value ~default:"no name"
                                       (Vmm_core.Name.name name)
                                   in
                                   tr
                                     ~a:
                                       [
                                         a_class [ "border-b border-gray-200" ];
                                       ]
                                     [
                                       td
                                         ~a:
                                           [
                                             a_class
                                               [
                                                 "px-6 py-4 whitespace-nowrap \
                                                  text-sm font-medium \
                                                  text-gray-800";
                                               ];
                                           ]
                                         [ txt name ];
                                       td
                                         ~a:
                                           [
                                             a_class
                                               [
                                                 "px-6 py-4 whitespace-nowrap \
                                                  text-sm font-medium \
                                                  text-gray-800";
                                               ];
                                           ]
                                         [ txt (string_of_int size ^ "MB") ];
                                       td
                                         ~a:
                                           [
                                             a_class
                                               [
                                                 "px-6 py-4 whitespace-nowrap \
                                                  text-sm font-medium \
                                                  text-gray-800";
                                               ];
                                           ]
                                         [ txt (string_of_bool used) ];
                                       td
                                         ~a:
                                           [
                                             a_class
                                               [
                                                 "px-6 py-4 whitespace-nowrap \
                                                  text-sm font-medium \
                                                  text-gray-800 flex space-x-5";
                                               ];
                                           ]
                                         [
                                           Modal_dialog.modal_dialog
                                             ~modal_title:
                                               ("Upload data to " ^ name)
                                             ~button_content:(txt "Upload")
                                             ~button_type:`Primary_outlined
                                             ~content:
                                               (Volume_ui.upload_to_volume name)
                                             ();
                                           Modal_dialog.modal_dialog
                                             ~modal_title:"Download volume"
                                             ~button_content:(txt "Download")
                                             ~content:
                                               (Volume_ui.download_volume name)
                                             ();
                                           Utils.button_component
                                             ~attribs:
                                               [
                                                 a_id
                                                   ("delete-block-button-"
                                                  ^ name);
                                                 a_onclick
                                                   ("deleteVolume('" ^ name
                                                  ^ "')");
                                               ]
                                             ~content:(txt "Delete")
                                             ~btn_type:`Danger_full ();
                                         ];
                                     ])
                                 volumes);
                          ];
                      ];
                  ];
              ];
          ];
      ])
