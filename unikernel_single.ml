let unikernel_single_layout unikernel =
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
                        h2 [ txt "ExampleUnikernel" ];
                        p
                          [
                            txt
                              (Cstruct.to_string
                                 unikernel.Vmm_core.Unikernel.digest);
                          ];
                      ];
                    div
                      [
                        button
                          ~a:
                            [
                              a_onclick "destroyUnikernel()";
                              a_class
                                [
                                  "my-3 py-2 px-3 rounded bg-secondary-500 \
                                   text-white hover:bg-secondary-700 \
                                   text-secondary-50 font-semibold";
                                ];
                            ]
                          [ txt "Destory" ];
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
                          ~a:[ a_class [ "text-md" ] ]
                          [ txt (string_of_int unikernel.cpuid) ];
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
                          ~a:[ a_class [ "text-md" ] ]
                          [ txt (string_of_int unikernel.memory) ];
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
                          ~a:[ a_class [ "text-md" ] ]
                          [ txt (match unikernel.typ with `Solo5 -> "Solo5") ];
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
                                      "items-center bg-transparent w-full \
                                       border-collapse";
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
                                                   "border-t-0 px-6 \
                                                    align-middle border-l-0 \
                                                    border-r-0 text-md \
                                                    whitespace-nowrap p-4";
                                                 ];
                                             ]
                                           [ txt "Host device" ];
                                         th
                                           ~a:
                                             [
                                               a_class
                                                 [
                                                   "border-t-0 px-6 \
                                                    align-middle border-l-0 \
                                                    border-r-0 text-md \
                                                    whitespace-nowrap p-4";
                                                 ];
                                             ]
                                           [ txt "Name" ];
                                         th
                                           ~a:
                                             [
                                               a_class
                                                 [
                                                   "border-t-0 px-6 \
                                                    align-middle border-l-0 \
                                                    border-r-0 text-md \
                                                    whitespace-nowrap p-4";
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
                                         ~a:[ a_class [ "text-center" ] ]
                                         [ txt name ];
                                       td
                                         ~a:[ a_class [ "text-center" ] ]
                                         [
                                           txt (Option.value device ~default:"");
                                         ];
                                       td
                                         ~a:[ a_class [ "text-center" ] ]
                                         [
                                           txt
                                             (string_of_int
                                                (Option.value size ~default:0)
                                             ^ "MB");
                                         ];
                                     ])
                                 unikernel.block_devices);
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
                                      "items-center bg-transparent w-full \
                                       border-collapse";
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
                                                   "border-t-0 px-6 \
                                                    align-middle border-l-0 \
                                                    border-r-0 text-md \
                                                    whitespace-nowrap p-4";
                                                 ];
                                             ]
                                           [ txt "Host device" ];
                                         th
                                           ~a:
                                             [
                                               a_class
                                                 [
                                                   "border-t-0 px-6 \
                                                    align-middle border-l-0 \
                                                    border-r-0 text-md \
                                                    whitespace-nowrap p-4";
                                                 ];
                                             ]
                                           [ txt "Name" ];
                                         th
                                           ~a:
                                             [
                                               a_class
                                                 [
                                                   "border-t-0 px-6 \
                                                    align-middle border-l-0 \
                                                    border-r-0 text-md \
                                                    whitespace-nowrap p-4";
                                                 ];
                                             ]
                                           [ txt "Sector size" ];
                                       ];
                                   ])
                              (List.map
                                 (fun (name, device, mac) ->
                                   tr
                                     [
                                       td
                                         ~a:[ a_class [ "text-center" ] ]
                                         [ txt name ];
                                       td
                                         ~a:[ a_class [ "text-center" ] ]
                                         [
                                           txt (Option.value device ~default:"");
                                         ];
                                       td
                                         ~a:[ a_class [ "text-center" ] ]
                                         [
                                           txt
                                             (Macaddr.to_string
                                                (Option.value mac
                                                   ~default:
                                                     (Macaddr.of_string_exn
                                                        "00-00-00-00-00-00")));
                                         ];
                                     ])
                                 unikernel.bridges);
                          ];
                        div
                          ~a:[ a_class [ "my-4" ] ]
                          [
                            p
                              ~a:[ a_class [ "text-xl font-semibold" ] ]
                              [ txt "Fail Behaviour" ];
                            (match unikernel.fail_behaviour with
                            | `Quit -> p [ txt "Quit" ]
                            | `Restart None -> p [ txt "Restart" ]
                            | `Restart (Some _codes) ->
                              p [txt ""]
                                (* List.map
                                  (fun code -> p [ txt (string_of_int code) ])
                                  (Vmm_core.IS.elements codes); *) )
                          ];
                      ];
                  ];
              ];
          ];
      ])
