let volume_index_layout instance_name volumes policy =
  let total_volume_used =
    List.fold_left (fun total_size (_, size, _) -> total_size + size) 0 volumes
  in
  let total_free_space =
    match policy with
    | Some policy ->
        Option.value ~default:0 policy.Vmm_core.Policy.block - total_volume_used
    | None -> 0
  in
  if List.length volumes = 0 then
    Tyxml_html.(
      section
        ~a:
          [
            a_class
              [
                "col-span-7 p-4 bg-gray-50 my-1 flex flex-col items-center \
                 justify-center text-center space-y-6";
              ];
            a_style "min-height: 60vh;";
          ]
        [
          div
            ~a:[ a_class [ "text-gray-400 mt-12" ] ]
            [ i ~a:[ a_class [ "fa-solid fa-hard-drive fa-4x mb-4" ] ] [] ];
          h2
            ~a:[ a_class [ "text-2xl font-bold text-gray-700" ] ]
            [ txt "No Volumes Found" ];
          p
            ~a:[ a_class [ "text-gray-500 max-w-xl text-sm" ] ]
            [
              txt
                "Note: \"Volumes\" and \"Block devices\" refer to the same \
                 concept in Mollymawk — persistent storage units. You haven't \
                 provisioned any block devices yet.";
            ];
          div
            ~a:[ a_class [ "mt-4" ] ]
            [
              Modal_dialog.modal_dialog ~modal_title:"Create a volume"
                ~button_content:(txt "Create your first block device")
                ~button_type:`Primary_full
                ~content:
                  (Volume_ui.create_volume instance_name total_free_space)
                ();
            ];
        ])
  else
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
                            (string_of_int (List.length volumes)
                            ^ " volumes found on instance "
                            ^ Configuration.name_to_str instance_name);
                        ];
                      p
                        ~a:
                          [ a_class [ "text-sm text-gray-500 mt-1 max-w-2xl" ] ]
                        [
                          txt
                            "Note: \"Volumes\" and \"Block devices\" refer to \
                             the same concept in Mollymawk — persistent \
                             storage units. The \"Host Device\" block exposes \
                             this storage to a unikernel.";
                        ];
                    ];
                  div
                    [
                      Modal_dialog.modal_dialog ~modal_title:"Create a volume"
                        ~button_content:(txt "Create block device")
                        ~content:
                          (Volume_ui.create_volume instance_name
                             total_free_space)
                        ();
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
                                                 Unsafe.string_attrib
                                                   "x-on:click" "sortByColumn";
                                                 Unsafe.string_attrib ":class"
                                                   "sortBy === 'Host Device' ? \
                                                    'border-b-2 \
                                                    border-primary-500 \
                                                    text-primary-800 \
                                                    bg-primary-100' : \
                                                    'hover:bg-primary-100 \
                                                    text-primary-600'";
                                                 a_class
                                                   [
                                                     "px-6 py-3 text-start \
                                                      text-xs font-bold \
                                                      uppercase cursor-pointer \
                                                      select-none";
                                                   ];
                                               ]
                                             [
                                               span [ txt "Host Device" ];
                                               span
                                                 ~a:
                                                   [
                                                     a_class [ "px-2" ];
                                                     Unsafe.string_attrib
                                                       "x-show"
                                                       "sortBy === 'Host \
                                                        Device'";
                                                   ]
                                                 [
                                                   i
                                                     ~a:
                                                       [
                                                         Unsafe.string_attrib
                                                           ":class"
                                                           "sortAsc ? \
                                                            'fa-solid \
                                                            fa-sort-up' : \
                                                            'fa-solid \
                                                            fa-sort-down'";
                                                       ]
                                                     [];
                                                 ];
                                             ];
                                           th
                                             ~a:
                                               [
                                                 Unsafe.string_attrib
                                                   "x-on:click" "sortByColumn";
                                                 Unsafe.string_attrib ":class"
                                                   "sortBy === 'Size (MB)' ? \
                                                    'border-b-2 \
                                                    border-primary-500 \
                                                    text-primary-800 \
                                                    bg-primary-100' : \
                                                    'hover:bg-primary-100 \
                                                    text-primary-600'";
                                                 a_class
                                                   [
                                                     "px-6 py-3 text-start \
                                                      text-xs font-bold \
                                                      uppercase cursor-pointer \
                                                      select-none";
                                                   ];
                                               ]
                                             [
                                               span [ txt "Size (MB)" ];
                                               span
                                                 ~a:
                                                   [
                                                     a_class [ "px-2" ];
                                                     Unsafe.string_attrib
                                                       "x-show"
                                                       "sortBy === 'Size (MB)'";
                                                   ]
                                                 [
                                                   i
                                                     ~a:
                                                       [
                                                         Unsafe.string_attrib
                                                           ":class"
                                                           "sortAsc ? \
                                                            'fa-solid \
                                                            fa-sort-up' : \
                                                            'fa-solid \
                                                            fa-sort-down'";
                                                       ]
                                                     [];
                                                 ];
                                             ];
                                           th
                                             ~a:
                                               [
                                                 Unsafe.string_attrib
                                                   "x-on:click" "sortByColumn";
                                                 Unsafe.string_attrib ":class"
                                                   "sortBy === 'Used' ? \
                                                    'border-b-2 \
                                                    border-primary-500 \
                                                    text-primary-800 \
                                                    bg-primary-100' : \
                                                    'hover:bg-primary-100 \
                                                    text-primary-600'";
                                                 a_class
                                                   [
                                                     "px-6 py-3 text-start \
                                                      text-xs font-bold \
                                                      uppercase cursor-pointer \
                                                      select-none";
                                                   ];
                                               ]
                                             [
                                               span [ txt "Used" ];
                                               span
                                                 ~a:
                                                   [
                                                     a_class [ "px-2" ];
                                                     Unsafe.string_attrib
                                                       "x-show"
                                                       "sortBy === 'Used'";
                                                   ]
                                                 [
                                                   i
                                                     ~a:
                                                       [
                                                         Unsafe.string_attrib
                                                           ":class"
                                                           "sortAsc ? \
                                                            'fa-solid \
                                                            fa-sort-up' : \
                                                            'fa-solid \
                                                            fa-sort-down'";
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
                                                      text-primary-600 \
                                                      uppercase cursor-pointer \
                                                      select-none";
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
                                         (Option.map
                                            Vmm_core.Name.Label.to_string
                                            (Vmm_core.Name.name name))
                                     in
                                     tr
                                       ~a:
                                         [
                                           a_class
                                             [ "border-b border-gray-200" ];
                                         ]
                                       [
                                         td
                                           ~a:
                                             [
                                               a_class
                                                 [
                                                   "px-6 py-4 \
                                                    whitespace-nowrap text-sm \
                                                    font-medium text-gray-800";
                                                 ];
                                             ]
                                           [ txt name ];
                                         td
                                           ~a:
                                             [
                                               a_class
                                                 [
                                                   "px-6 py-4 \
                                                    whitespace-nowrap text-sm \
                                                    font-medium text-gray-800";
                                                 ];
                                             ]
                                           [ txt (string_of_int size ^ "MB") ];
                                         td
                                           ~a:
                                             [
                                               a_class
                                                 [
                                                   "px-6 py-4 \
                                                    whitespace-nowrap text-sm \
                                                    font-medium text-gray-800";
                                                 ];
                                             ]
                                           [ txt (string_of_bool used) ];
                                         td
                                           ~a:
                                             [
                                               a_class
                                                 [
                                                   "px-6 py-4 \
                                                    whitespace-nowrap text-sm \
                                                    font-medium text-gray-800 \
                                                    flex space-x-5";
                                                 ];
                                             ]
                                           [
                                             Modal_dialog.modal_dialog
                                               ~modal_title:
                                                 ("Upload data to " ^ name)
                                               ~button_content:(txt "Upload")
                                               ~button_type:`Primary_outlined
                                               ~content:
                                                 (Volume_ui.upload_to_volume
                                                    ~instance_name
                                                    ~block_name:name)
                                               ();
                                             Modal_dialog.modal_dialog
                                               ~modal_title:"Download volume"
                                               ~button_content:(txt "Download")
                                               ~content:
                                                 (Volume_ui.download_volume
                                                    ~instance_name
                                                    ~block_name:name)
                                               ();
                                             Utils.button_component
                                               ~attribs:
                                                 [
                                                   a_id
                                                     ("delete-block-button-"
                                                    ^ name);
                                                   a_onclick
                                                     ("deleteVolume('" ^ name
                                                    ^ "','"
                                                     ^ Configuration.name_to_str
                                                         instance_name
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
