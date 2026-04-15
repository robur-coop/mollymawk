let instance_unikernels instance_name albatross_instance_unikernels current_time
    =
  Tyxml_html.(
    List.map
      (fun (name, unikernel) ->
        let name =
          Option.value ~default:"no name"
            (Option.map Vmm_core.Name.Label.to_string (Vmm_core.Name.name name))
        in
        tr
          ~a:[ a_class [ "border-b border-gray-200" ] ]
          [
            td
              ~a:
                [
                  a_class
                    [
                      "px-6 py-4 whitespace-nowrap text-sm font-medium \
                       text-gray-800";
                    ];
                ]
              [
                txt name;
                p
                  ~a:[ a_class [ "text-xs text-gray-500" ] ]
                  [ txt ("from: " ^ Configuration.name_to_str instance_name) ];
                p
                  ~a:[ a_class [ "text-xs text-gray-500" ] ]
                  [
                    txt
                      ("type: "
                      ^
                      match unikernel.Vmm_core.Unikernel.typ with
                      | `Solo5 -> "Solo5"
                      | `BHyve -> "BHyve");
                  ];
              ];
            td
              ~a:
                [
                  a_class
                    [
                      "px-6 py-4 whitespace-nowrap text-sm font-medium \
                       text-gray-800";
                    ];
                ]
              [
                txt
                  (Option.value ~default:"50 (default)"
                     (Option.map string_of_int unikernel.startup));
              ];
            td
              ~a:
                [
                  a_class
                    [
                      "px-6 py-4 whitespace-nowrap text-sm font-medium \
                       text-gray-800";
                    ];
                ]
              [
                txt
                  (String.concat ", "
                     (List.map string_of_int
                        (Vmm_core.IS.elements unikernel.cpuids)));
              ];
            td
              ~a:
                [
                  a_class
                    [
                      "px-6 py-4 whitespace-nowrap text-sm font-medium \
                       text-gray-800";
                    ];
                ]
              [ txt (string_of_int unikernel.memory ^ " MB") ];
            td
              ~a:
                [
                  a_class
                    [
                      "px-6 py-4 whitespace-nowrap text-sm font-medium \
                       text-gray-800";
                    ];
                ]
              [
                txt
                  (Utils.TimeHelper.time_ago ~current_time
                     ~check_time:unikernel.started);
              ];
            td
              ~a:
                [
                  a_class
                    [
                      "px-6 py-4 whitespace-nowrap text-sm font-medium \
                       text-gray-800";
                    ];
                ]
              [
                a
                  ~a:
                    [
                      a_href
                        ("/unikernel/info?unikernel=" ^ name ^ "&instance="
                        ^ Configuration.name_to_str instance_name);
                      a_class
                        [
                          "inline-flex items-center gap-x-2 text-sm \
                           font-semibold rounded-lg border border-1 py-1 px-2 \
                           border-primary-400 text-primary-600 \
                           hover:text-primary-500 focus:outline-none \
                           focus:text-primary-800 disabled:opacity-50 \
                           disabled:pointer-events-none";
                        ];
                    ]
                  [ txt "View" ];
              ];
          ])
      albatross_instance_unikernels)

let deceased_unikernels_table deceased_unikernels_by_albatross_instance =
  Tyxml_html.(
    table
      ~a:
        [
          a_class
            [ "data-table table-auto min-w-full divide-y divide-gray-200 mt-4" ];
        ]
      ~thead:
        (thead
           [
             tr
               [
                 th
                   ~a:
                     [
                       a_class
                         [
                           "px-6 py-3 text-start text-xs font-bold \
                            text-primary-600 uppercase";
                         ];
                     ]
                   [ txt "Instance" ];
                 th
                   ~a:
                     [
                       a_class
                         [
                           "px-6 py-3 text-start text-xs font-bold \
                            text-primary-600 uppercase";
                         ];
                     ]
                   [ txt "Name" ];
                 th
                   ~a:
                     [
                       a_class
                         [
                           "px-6 py-3 text-start text-xs font-bold \
                            text-primary-600 uppercase";
                         ];
                     ]
                   [ txt "Action" ];
               ];
           ])
      (List.map
         (fun (instance_name, names) ->
           List.map
             (fun name ->
               let name_str =
                 Option.value ~default:"no name"
                   (Option.map Vmm_core.Name.Label.to_string
                      (Vmm_core.Name.name name))
               in
               tr
                 [
                   td
                     ~a:
                       [
                         a_class
                           [
                             "px-6 py-4 whitespace-nowrap text-sm text-gray-500";
                           ];
                       ]
                     [ txt (Configuration.name_to_str instance_name) ];
                   td
                     ~a:
                       [
                         a_class
                           [
                             "px-6 py-4 whitespace-nowrap text-sm font-medium \
                              text-gray-800";
                           ];
                       ]
                     [ txt name_str ];
                   td
                     ~a:
                       [
                         a_class
                           [
                             "px-6 py-4 whitespace-nowrap text-sm font-medium \
                              text-gray-800";
                           ];
                       ]
                     [
                       a
                         ~a:
                           [
                             a_href
                               (Fmt.str
                                  "/unikernel/console?unikernel=%s&instance=%s"
                                  name_str
                                  (Configuration.name_to_str instance_name));
                             a_target "_blank";
                             a_class
                               [
                                 "py-2 px-2 rounded border border-1 \
                                  border-primary-400 text-primary-600 \
                                  hover:text-gray-50 focus:outline-none \
                                  hover:bg-primary-800 font-semibold";
                               ];
                           ]
                         [ txt "View logs" ];
                     ];
                 ])
             names)
         deceased_unikernels_by_albatross_instance
      |> List.flatten))

let unikernel_index_layout unikernels_by_albatross_instance
    deceased_unikernels_by_albatross_instance current_time =
  let online_instances_count, total_unikernels_count =
    List.fold_left
      (fun (count, total) (_, us) ->
        if us = [] then (count, total) else (count + 1, total + List.length us))
      (0, 0) unikernels_by_albatross_instance
  in
  let deceased_instances_count, total_deceased_count =
    List.fold_left
      (fun (count, total) (_, us) ->
        if us = [] then (count, total) else (count + 1, total + List.length us))
      (0, 0) deceased_unikernels_by_albatross_instance
  in
  Tyxml_html.(
    section
      ~a:
        [
          a_class [ "col-span-7 p-4 bg-gray-50 my-1" ];
          Unsafe.string_attrib "x-data" "{ tab: 'online' }";
        ]
      [
        div
          ~a:[ a_class [ "flex border-b border-gray-200 mb-4" ] ]
          [
            button
              ~a:
                [
                  Unsafe.string_attrib "x-on:click" "tab = 'online'";
                  Unsafe.string_attrib ":class"
                    "tab === 'online' ? 'border-primary-500 text-primary-600' \
                     : 'border-transparent text-gray-500 hover:text-gray-700 \
                     hover:border-gray-300'";
                  a_class
                    [
                      "py-2 px-4 border-b-2 font-medium text-sm \
                       focus:outline-none transition-colors duration-150";
                    ];
                ]
              [ txt "Online Unikernels" ];
            button
              ~a:
                [
                  Unsafe.string_attrib "x-on:click" "tab = 'deceased'";
                  Unsafe.string_attrib ":class"
                    "tab === 'deceased' ? 'border-primary-500 \
                     text-primary-600' : 'border-transparent text-gray-500 \
                     hover:text-gray-700 hover:border-gray-300'";
                  a_class
                    [
                      "py-2 px-4 border-b-2 font-medium text-sm \
                       focus:outline-none transition-colors duration-150";
                    ];
                ]
              [ txt "Deceased Unikernels" ];
          ];
        div
          ~a:[ a_class [ "px-3 flex justify-between items-center" ] ]
          [
            div
              [
                p
                  ~a:
                    [
                      a_class [ "font-bold text-gray-700" ];
                      Unsafe.string_attrib "x-show" "tab === 'online'";
                    ]
                  [
                    txt
                      (Fmt.str "Showing %u unikernels from %u online instances"
                         total_unikernels_count online_instances_count);
                  ];
                p
                  ~a:
                    [
                      a_class [ "font-bold text-gray-700" ];
                      Unsafe.string_attrib "x-show" "tab === 'deceased'";
                      Unsafe.string_attrib "style" "display: none;";
                    ]
                  [
                    txt
                      (Fmt.str
                         "Showing %u deceased unikernels from %u instances"
                         total_deceased_count deceased_instances_count);
                  ];
              ];
            div
              ~a:[ a_class [ "flex gap-4" ] ]
              [
                select
                  ~a:
                    [
                      Unsafe.string_attrib "x-show" "tab === 'online'";
                      a_onchange "filterAlbatrossInstance(event)";
                      a_id "filterAlbatrossIntances";
                      a_class
                        [
                          "rounded py-2 px-3 border border-primary-200 \
                           focus:border-primary-500 outline-0";
                        ];
                    ]
                  (option ~a:[ a_value "all" ] (txt "All instances")
                  :: List.map
                       (fun (name, unikernels) ->
                         option
                           ~a:[ a_value (Configuration.name_to_str name) ]
                           (txt
                              (Fmt.str "%s (%u unikernels)"
                                 (Configuration.name_to_str name)
                                 (List.length unikernels))))
                       unikernels_by_albatross_instance);
                select
                  ~a:
                    [
                      Unsafe.string_attrib "x-show" "tab === 'deceased'";
                      Unsafe.string_attrib "style" "display: none;";
                      a_onchange "filterAlbatrossInstance(event)";
                      a_id "filterAlbatrossIntancesDeceased";
                      a_class
                        [
                          "rounded py-2 px-3 border border-primary-200 \
                           focus:border-primary-500 outline-0";
                        ];
                    ]
                  (option ~a:[ a_value "all" ] (txt "All instances")
                  :: List.map
                       (fun (name, deceased_unikernels) ->
                         option
                           ~a:[ a_value (Configuration.name_to_str name) ]
                           (txt
                              (Fmt.str "%s (%u unikernels)"
                                 (Configuration.name_to_str name)
                                 (List.length deceased_unikernels))))
                       deceased_unikernels_by_albatross_instance);
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
          ];
        hr ~a:[ a_class [ "border border-primary-500 my-5" ] ] ();
        div
          ~a:[ Unsafe.string_attrib "x-show" "tab === 'online'" ]
          [
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
                                      "data-table table-auto min-w-full \
                                       divide-y divide-gray-200";
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
                                             txt "Name";
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
                                           [ txt "Startup Priority" ];
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
                                             txt "CPU";
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
                                             txt "Memory";
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
                                             txt "Created";
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
                                 (fun (instance_name, unikernels) ->
                                   instance_unikernels instance_name unikernels
                                     current_time)
                                 unikernels_by_albatross_instance
                              |> List.flatten);
                          ];
                      ];
                  ];
              ];
          ];
        div
          ~a:
            [
              Unsafe.string_attrib "x-show" "tab === 'deceased'";
              Unsafe.string_attrib "style" "display: none;";
            ]
          [
            deceased_unikernels_table deceased_unikernels_by_albatross_instance;
          ];
      ])
