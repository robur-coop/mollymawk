let unikernel_single_layout ~unikernel_name ~instance_name
    ?(last_update_time = None) ~current_time unikernel =
  let u_name, data = unikernel in
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-10 p-4 bg-gray-50 my-1" ] ]
      [
        div
          ~a:[ a_id "unikernel-container"; a_class [ "p-4 rounded-md" ] ]
          [
            div
              ~a:[ a_id "info-container" ]
              [
                div
                  ~a:[ a_class [ "flex justify-between" ] ]
                  [
                    div
                      [
                        div
                          ~a:[ a_class [ "flex space-x-2 items-end" ] ]
                          [
                            h2
                              ~a:
                                [
                                  a_id "unikernel-name";
                                  a_class [ "text-xl font-bold uppercase" ];
                                ]
                              [ txt (Vmm_core.Name.to_string u_name) ];
                            p
                              ~a:[ a_class [ "text-sm" ] ]
                              [
                                txt
                                  ("created "
                                  ^ Utils.TimeHelper.time_ago ~current_time
                                      ~check_time:
                                        data.Vmm_core.Unikernel.started);
                              ];
                          ];
                        p
                          ~a:[ a_class [ "text-sm" ] ]
                          [ txt (Ohex.encode data.digest) ];
                        p
                          ~a:[ a_class [ "text-sm" ] ]
                          [ txt ("albatross: " ^ instance_name) ];
                      ];
                    div
                      ~a:[ a_class [ "flex space-x-2 items-center" ] ]
                      [
                        div
                          [
                            Utils.button_component
                              ~attribs:
                                [
                                  a_onclick
                                    ("restartUnikernel('" ^ unikernel_name
                                   ^ "', '" ^ instance_name ^ "')");
                                ]
                              ~content:(txt "Restart")
                              ~btn_type:`Primary_outlined ();
                          ];
                        div
                          [
                            a
                              ~a:
                                [
                                  a_href
                                    ("/unikernel/update?unikernel="
                                   ^ unikernel_name ^ "&instance="
                                   ^ instance_name);
                                  a_class
                                    [
                                      "py-2 px-2 rounded border border-1 \
                                       border-primary-400 text-primary-600 \
                                       hover:text-gray-50 focus:outline-none \
                                       hover:bg-primary-800 font-semibold";
                                    ];
                                ]
                              [ txt "Update" ];
                          ];
                        (* check that the last update happened less than 10 minutes ago (600 seconds)*)
                        (match last_update_time with
                        | Some check_time
                          when Utils.TimeHelper.diff_in_seconds ~current_time
                                 ~check_time
                               < Utils.rollback_seconds_limit ->
                            div
                              [
                                Utils.button_component
                                  ~attribs:
                                    [
                                      a_id "unikernel-rollback";
                                      a_onclick
                                        ("rollbackUnikernel('" ^ unikernel_name
                                       ^ "', '" ^ instance_name ^ "')");
                                    ]
                                  ~content:(txt "Rollback")
                                  ~btn_type:`Primary_outlined ();
                              ]
                        | _ -> div []);
                        div
                          [
                            Utils.button_component
                              ~attribs:
                                [
                                  a_onclick
                                    ("destroyUnikernel('" ^ unikernel_name
                                   ^ "', '" ^ instance_name ^ "')");
                                ]
                              ~content:(txt "Destroy")
                              ~btn_type:`Danger_outlined ();
                          ];
                      ];
                  ];
                p ~a:[ a_id "form-alert"; a_class [ "my-3" ] ] [];
                div
                  ~a:[ a_class [ "grid grid-cols-3 gap-3 text-white my-3" ] ]
                  [
                    div
                      ~a:[ a_class [ "p-4 rounded border border-primary-700" ] ]
                      [
                        div
                          ~a:[ a_class [ "flex justify-between" ] ]
                          [
                            i
                              ~a:
                                [
                                  a_class
                                    [
                                      "fa-solid fa-microchip text-xl \
                                       text-primary-500";
                                    ];
                                ]
                              [];
                            p ~a:[ a_class [ "text-md" ] ] [ txt "CPU" ];
                          ];
                        p
                          ~a:[ a_class [ "text-3xl text-right" ] ]
                          [ txt (string_of_int data.cpuid) ];
                      ];
                    div
                      ~a:[ a_class [ "p-4 rounded border border-primary-700" ] ]
                      [
                        div
                          ~a:[ a_class [ "flex justify-between" ] ]
                          [
                            i
                              ~a:
                                [
                                  a_class
                                    [
                                      "fa-solid fa-hard-drive text-xl \
                                       text-primary-500";
                                    ];
                                ]
                              [];
                            p ~a:[ a_class [ "text-md" ] ] [ txt "Memory" ];
                          ];
                        p
                          ~a:[ a_class [ "text-3xl text-right" ] ]
                          [ txt (string_of_int data.memory ^ "MB") ];
                      ];
                    div
                      ~a:[ a_class [ "p-4 rounded border border-primary-700" ] ]
                      [
                        div
                          ~a:[ a_class [ "flex justify-between" ] ]
                          [
                            i
                              ~a:
                                [
                                  a_class
                                    [
                                      "fa-solid fa-warehouse text-xl \
                                       text-primary-500";
                                    ];
                                ]
                              [];
                            p ~a:[ a_class [ "text-md" ] ] [ txt "Type" ];
                          ];
                        p
                          ~a:[ a_class [ "text-3xl text-right" ] ]
                          [ txt (match data.typ with `Solo5 -> "Solo5") ];
                      ];
                  ];
                div
                  ~a:[ a_class [ "grid grid-cols-2" ] ]
                  [
                    div
                      ~a:
                        [
                          a_class
                            [
                              "rounded my-4 text-white p-4 divide-y border \
                               border-primary-700";
                            ];
                        ]
                      [
                        div
                          [
                            p
                              ~a:[ a_class [ "text-xl font-semibold" ] ]
                              [ txt "Arguments" ];
                            table
                              ~a:
                                [
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
                                               a_class
                                                 [
                                                   "px-6 py-2 text-start \
                                                    text-xs font-bold \
                                                    text-primary-600 uppercase";
                                                 ];
                                             ]
                                           [ txt "Argument" ];
                                       ];
                                   ])
                              (List.map
                                 (fun arg ->
                                   tr
                                     [
                                       td
                                         ~a:
                                           [
                                             a_class
                                               [
                                                 "px-6 py-1 text-sm \
                                                  font-medium text-gray-800";
                                               ];
                                           ]
                                         [ txt arg ];
                                     ])
                                 (Option.value data.argv ~default:[]));
                          ];
                        div
                          ~a:[ a_class [ "my-4" ] ]
                          [
                            p
                              ~a:[ a_class [ "text-xl font-semibold" ] ]
                              [ txt "Block Devices" ];
                            table
                              ~a:
                                [
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
                                               a_class
                                                 [
                                                   "px-6 py-2 text-start \
                                                    text-xs font-bold \
                                                    text-primary-600 uppercase";
                                                 ];
                                             ]
                                           [ txt "Unikernel block device" ];
                                         th
                                           ~a:
                                             [
                                               a_class
                                                 [
                                                   "px-6 py-2 text-start \
                                                    text-xs font-bold \
                                                    text-primary-600 uppercase";
                                                 ];
                                             ]
                                           [ txt "Host device" ];
                                         th
                                           ~a:
                                             [
                                               a_class
                                                 [
                                                   "px-6 py-2 text-start \
                                                    text-xs font-bold \
                                                    text-primary-600 uppercase";
                                                 ];
                                             ]
                                           [ txt "Sector size" ];
                                         th
                                           ~a:
                                             [
                                               a_class
                                                 [
                                                   "px-6 py-2 text-start \
                                                    text-xs font-bold \
                                                    text-primary-600 uppercase";
                                                 ];
                                             ]
                                           [ txt "Size" ];
                                       ];
                                   ])
                              (List.map
                                 (fun {
                                        Vmm_core.Unikernel.unikernel_device;
                                        host_device;
                                        sector_size;
                                        size;
                                      } ->
                                   tr
                                     [
                                       td
                                         ~a:
                                           [
                                             a_class
                                               [
                                                 "px-6 py-1 whitespace-nowrap \
                                                  text-sm font-medium \
                                                  text-gray-800";
                                               ];
                                           ]
                                         [ txt unikernel_device ];
                                       td
                                         ~a:
                                           [
                                             a_class
                                               [
                                                 "px-6 py-1 whitespace-nowrap \
                                                  text-sm font-medium \
                                                  text-gray-800";
                                               ];
                                           ]
                                         [ txt host_device ];
                                       td
                                         ~a:
                                           [
                                             a_class
                                               [
                                                 "px-6 py-1 whitespace-nowrap \
                                                  text-sm font-medium \
                                                  text-gray-800";
                                               ];
                                           ]
                                         [
                                           txt
                                             (string_of_int sector_size
                                            ^ " bytes");
                                         ];
                                       td
                                         ~a:
                                           [
                                             a_class
                                               [
                                                 "px-6 py-1 whitespace-nowrap \
                                                  text-sm font-medium \
                                                  text-gray-800";
                                               ];
                                           ]
                                         [ txt (string_of_int size ^ " MB") ];
                                     ])
                                 data.block_devices);
                          ];
                        div
                          ~a:[ a_class [ "my-4" ] ]
                          [
                            p
                              ~a:[ a_class [ "text-xl font-semibold" ] ]
                              [ txt "Network Devices" ];
                            table
                              ~a:
                                [
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
                                               a_class
                                                 [
                                                   "px-6 py-2 text-start \
                                                    text-xs font-bold \
                                                    text-primary-600 uppercase";
                                                 ];
                                             ]
                                           [ txt "Unikernel network device" ];
                                         th
                                           ~a:
                                             [
                                               a_class
                                                 [
                                                   "px-6 py-2 text-start \
                                                    text-xs font-bold \
                                                    text-primary-600 uppercase";
                                                 ];
                                             ]
                                           [ txt "Host device" ];
                                         th
                                           ~a:
                                             [
                                               a_class
                                                 [
                                                   "px-6 py-2 text-start \
                                                    text-xs font-bold \
                                                    text-primary-600 uppercase";
                                                 ];
                                             ]
                                           [ txt "MAC Address" ];
                                       ];
                                   ])
                              (List.map
                                 (fun {
                                        Vmm_core.Unikernel.unikernel_device;
                                        host_device;
                                        mac;
                                      } ->
                                   tr
                                     [
                                       td
                                         ~a:
                                           [
                                             a_class
                                               [
                                                 "px-6 py-1 whitespace-nowrap \
                                                  text-sm font-medium \
                                                  text-gray-800";
                                               ];
                                           ]
                                         [ txt unikernel_device ];
                                       td
                                         ~a:
                                           [
                                             a_class
                                               [
                                                 "px-6 py-1 whitespace-nowrap \
                                                  text-sm font-medium \
                                                  text-gray-800";
                                               ];
                                           ]
                                         [ txt host_device ];
                                       td
                                         ~a:
                                           [
                                             a_class
                                               [
                                                 "px-6 py-1 whitespace-nowrap \
                                                  text-sm font-medium \
                                                  text-gray-800";
                                               ];
                                           ]
                                         [ txt (Macaddr.to_string mac) ];
                                     ])
                                 data.bridges);
                          ];
                        div
                          ~a:[ a_class [ "my-4" ] ]
                          [
                            p
                              ~a:[ a_class [ "text-xl font-semibold" ] ]
                              [ txt "Fail Behaviour" ];
                            (match data.fail_behaviour with
                            | `Quit -> p [ txt "Quit" ]
                            | `Restart None -> p [ txt "Restart" ]
                            | `Restart (Some codes) ->
                                p
                                  [
                                    txt
                                      (String.concat ", "
                                         (List.map string_of_int
                                            (Vmm_core.IS.elements codes)));
                                  ]);
                          ];
                      ];
                    div
                      ~a:[ a_class [ "text-left p-4" ] ]
                      [
                        p
                          ~a:[ a_class [ "text-xl font-semibold" ] ]
                          [ txt "Console Output" ];
                        div
                          ~a:[ a_id "console-container" ]
                          [
                            textarea
                              ~a:
                                [
                                  a_id "console-output";
                                  a_rows 42;
                                  a_readonly ();
                                  a_disabled ();
                                  a_class
                                    [
                                      "w-full bg-transparent border-0 h-screen \
                                       overflow-auto whitespace-pre-wrap \
                                       font-mono";
                                    ];
                                ]
                              (txt "");
                          ];
                      ];
                  ];
              ];
          ];
      ])
