let volume_index_layout volumes policy =
  let total_volume_used =
    List.fold_left (fun total_size (_, size, _) -> total_size + size) 0 volumes
  in
  let total_free_space =
    match policy with
    | Some policy ->
        Option.value ~default:0 policy.Vmm_core.Policy.block - total_volume_used
    | None -> 0
  in
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        section
          ~a:[ a_id "block-display"; a_class [ "block" ] ]
          [
            div
              ~a:[ a_class [ "px-3 flex justify-between items-center" ] ]
              [
                div
                  [
                    p
                      ~a:[ a_class [ "font-bold text-gray-700" ] ]
                      [
                        txt
                          ("Volumes ("
                          ^ string_of_int (List.length volumes)
                          ^ ")");
                      ];
                  ];
                div
                  ~a:[ Unsafe.string_attrib "x-data" "{modalIsOpen: false}" ]
                  [
                    button
                      ~a:
                        [
                          Unsafe.string_attrib "x-on:click" "modalIsOpen = true";
                          a_class
                            [
                              "py-3 px-3 rounded bg-primary-500 \
                               hover:bg-primary-800 w-full text-gray-50 \
                               font-semibold";
                            ];
                        ]
                      [ txt "Create block device" ];
                    div
                      ~a:
                        [
                          Unsafe.string_attrib "x-cloak" "";
                          Unsafe.string_attrib "x-show" "modalIsOpen";
                          Unsafe.string_attrib
                            "x-transition.opacity.duration.200ms" "";
                          Unsafe.string_attrib "x-trap.inert.noscroll"
                            "modalIsOpen";
                          Unsafe.string_attrib "x-on:keydown.esc.window"
                            "modalIsOpen = false";
                          Unsafe.string_attrib "x-on:click.self"
                            "modalIsOpen = false";
                          a_class
                            [
                              "fixed inset-0 z-30 flex items-end \
                               justify-center bg-black/20 p-4 backdrop-blur-md \
                               sm:items-center";
                            ];
                          a_role [ "dialog" ];
                          a_aria "modal" [ "true" ];
                        ]
                      [
                        div
                          ~a:
                            [
                              Unsafe.string_attrib "x-show" "modalIsOpen";
                              Unsafe.string_attrib "x-transition:enter"
                                "transition ease-out duration-200 delay-100 \
                                 motion-reduce:transition-opacity";
                              Unsafe.string_attrib "x-transition:enter-start"
                                "opacity-0 scale-50";
                              Unsafe.string_attrib "x-transition:enter-end"
                                "opacity-100 scale-100";
                              a_class
                                [
                                  "flex max-w-xl flex-col gap-4 \
                                   overflow-hidden rounded-md border \
                                   border-neutral-300 bg-gray-50";
                                ];
                            ]
                          [
                            div
                              ~a:
                                [
                                  a_class
                                    [
                                      "flex items-center justify-between \
                                       border-b p-4";
                                    ];
                                ]
                              [
                                h3
                                  ~a:[ a_class [ "font-bold text-gray-700" ] ]
                                  [ txt "Create a volume" ];
                                i
                                  ~a:
                                    [
                                      a_class
                                        [
                                          "fa-solid fa-x text-sm cursor-pointer";
                                        ];
                                      Unsafe.string_attrib "x-on:click"
                                        "modalIsOpen = false";
                                    ]
                                  [];
                              ];
                            div
                              ~a:[ a_class [ "px-4" ] ]
                              [ Volume_create.create_volume total_free_space ];
                          ];
                      ];
                  ];
              ];
            div
              ~a:[ a_class [ "mx-auto text-center" ] ]
              [
                div
                  ~a:
                    [
                      Unsafe.string_attrib "x-data" "chart: null";
                      Unsafe.string_attrib "x-init"
                        ("\n\
                         \                  chart = new \
                          Chart(document.getElementById('usageChart').getContext('2d'), \
                          {\n\
                         \                  type: 'pie',\n\
                         \                  data: {\n\
                         \                    labels: ['Free storage ("
                        ^ string_of_int total_free_space
                        ^ "MB)','Used storage ("
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
                                                  text-gray-800 space-x-5";
                                               ];
                                           ]
                                         [
                                           (if used then
                                              a
                                                ~a:
                                                  [
                                                    a_href
                                                      ("/unikernel/info/" ^ name);
                                                    a_class
                                                      [
                                                        "inline-flex \
                                                         items-center gap-x-2 \
                                                         text-sm font-semibold \
                                                         rounded border \
                                                         border-1 py-1 px-2 \
                                                         border-primary-400 \
                                                         text-primary-600 \
                                                         hover:text-primary-500 \
                                                         focus:outline-none \
                                                         focus:text-primary-800 \
                                                         disabled:opacity-50 \
                                                         disabled:pointer-events-none";
                                                      ];
                                                  ]
                                                [ txt "View" ]
                                            else
                                              span
                                                ~a:
                                                  [
                                                    a_class
                                                      [ "gap-x-2 py-1 px-2" ];
                                                  ]
                                                []);
                                           button
                                             ~a:
                                               [
                                                 a_id
                                                   ("delete-block-button-"
                                                  ^ name);
                                                 a_onclick
                                                   ("deleteVolume('" ^ name
                                                  ^ "')");
                                                 a_class
                                                   [
                                                     "py-1 px-2 mx-2 rounded \
                                                      bg-secondary-500 \
                                                      hover:bg-secondary-800 \
                                                      text-gray-50 \
                                                      font-semibold";
                                                   ];
                                               ]
                                             [ txt "Delete" ];
                                         ];
                                     ])
                                 volumes);
                          ];
                      ];
                  ];
              ];
          ];
      ])
