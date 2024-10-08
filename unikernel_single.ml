let unikernel_single_layout unikernel now console_output =
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
                                  ^ Utils.TimeHelper.time_ago now
                                      data.Vmm_core.Unikernel.started);
                              ];
                          ];
                        p
                          ~a:[ a_class [ "text-sm" ] ]
                          [ txt (Ohex.encode data.digest) ];
                      ];
                    div
                      [
                        button
                          ~a:
                            [
                              a_onclick
                                ("destroyUnikernel('"
                                ^ Option.value ~default:""
                                    (Vmm_core.Name.name u_name)
                                ^ "')");
                              a_class
                                [
                                  "my-3 py-2 px-3 rounded bg-secondary-500 \
                                   text-white hover:bg-secondary-700 \
                                   text-secondary-50 font-semibold";
                                ];
                            ]
                          [ txt "Destroy" ];
                      ];
                  ];
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
                                                 "px-6 py-1 whitespace-nowrap \
                                                  text-sm font-medium \
                                                  text-gray-800";
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
                                           [ txt "Name" ];
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
                                       ];
                                   ])
                              (List.map
                                 (fun (name, device, size) ->
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
                                         [ txt name ];
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
                                           txt (Option.value device ~default:"");
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
                                         [
                                           txt
                                             (string_of_int
                                                (Option.value size ~default:512)
                                              (* TODO: Read the default from albatross json *)
                                             ^ " bytes");
                                         ];
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
                                           [ txt "Name" ];
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
                                 (fun (name, device, mac) ->
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
                                         [ txt name ];
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
                                             (Option.value device ~default:name);
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
                                         [
                                           txt
                                             (Macaddr.to_string
                                                (Option.value mac
                                                   ~default:
                                                     (Vmm_core.Name.mac u_name
                                                        (Option.value device
                                                           ~default:name))));
                                         ];
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
                      ~a:[ a_class [ "text-left p-4 overflow-hidden" ] ]
                      [
                        p
                          ~a:[ a_class [ "text-xl font-semibold" ] ]
                          [ txt "Console Output" ];
                        div
                          ~a:[ a_id "console-container" ]
                          [
                            div
                              ~a:
                                [
                                  a_class
                                    [
                                      "w-full bg-transparent border-0 h-screen \
                                       overflow-hidden";
                                    ];
                                ]
                              (List.map
                                 (fun (ts, data) ->
                                   p
                                     ~a:
                                       [
                                         a_class
                                           [ "text-white font-mono text-sm" ];
                                       ]
                                     [ txt (Ptime.to_rfc3339 ts); txt data ])
                                 console_output);
                          ];
                      ];
                  ];
              ];
          ];
      ])
