open Tyxml

let unikernel_index_layout unikernels current_time =
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 px-4 py-6 bg-gray-50 my-6" ] ]
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
                      ("Unikernels ("
                      ^ string_of_int (List.length unikernels)
                      ^ ")");
                  ];
              ];
            div
              [
                input
                  ~a:
                    [
                      a_onkeyup "filterUnikernels()";
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
          ~a:[ a_class [ "flex flex-col" ] ]
          [
            div
              ~a:[ a_class [ "-m-1.5 overflow-x-auto" ] ]
              [
                div
                  ~a:
                    [ a_class [ "p-1.5 min-w-full inline-block align-middle" ] ]
                  [
                    div
                      ~a:[ a_class [ "overflow-hidden" ] ]
                      [
                        table
                          ~a:
                            [
                              a_id "unikernel-table";
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
                                               "px-6 py-3 text-start text-xs \
                                                font-bold text-primary-600 \
                                                uppercase";
                                             ];
                                         ]
                                       [ txt "Name" ];
                                     th
                                       ~a:
                                         [
                                           a_class
                                             [
                                               "px-6 py-3 text-start text-xs \
                                                font-bold text-primary-600 \
                                                uppercase";
                                             ];
                                         ]
                                       [ txt "Type" ];
                                     th
                                       ~a:
                                         [
                                           a_class
                                             [
                                               "px-6 py-3 text-start text-xs \
                                                font-bold text-primary-600 \
                                                uppercase";
                                             ];
                                         ]
                                       [ txt "CPU" ];
                                     th
                                       ~a:
                                         [
                                           a_class
                                             [
                                               "px-6 py-3 text-start text-xs \
                                                font-bold text-primary-600 \
                                                uppercase";
                                             ];
                                         ]
                                       [ txt "Memory" ];
                                     th
                                       ~a:
                                         [
                                           a_class
                                             [
                                               "px-6 py-3 text-start text-xs \
                                                font-bold text-primary-600 \
                                                uppercase";
                                             ];
                                         ]
                                       [ txt "Created" ];
                                     th
                                       ~a:
                                         [
                                           a_class
                                             [
                                               "px-6 py-3 text-start text-xs \
                                                font-bold text-primary-600 \
                                                uppercase";
                                             ];
                                         ]
                                       [ txt "Action" ];
                                   ];
                               ])
                          (List.map
                             (fun (name, unikernel) ->
                               let name =
                                 Option.value ~default:"no name"
                                   (Vmm_core.Name.name name)
                               in
                               tr
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
                                     [
                                       txt
                                         (match
                                            unikernel.Vmm_core.Unikernel.typ
                                          with
                                         | `Solo5 -> "solo5");
                                     ];
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
                                     [ txt (string_of_int unikernel.cpuid) ];
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
                                     [
                                       txt
                                         (string_of_int unikernel.memory ^ " MB");
                                     ];
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
                                     [
                                       txt
                                         (Utils.TimeHelper.time_ago current_time
                                            unikernel.started);
                                     ];
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
                                     [
                                       a
                                         ~a:
                                           [
                                             a_href ("/unikernel/info/" ^ name);
                                             a_class
                                               [
                                                 "inline-flex items-center \
                                                  gap-x-2 text-sm \
                                                  font-semibold rounded-lg \
                                                  border border-1 py-1 px-2 \
                                                  border-primary-400 \
                                                  text-primary-600 \
                                                  hover:text-primary-500 \
                                                  focus:outline-none \
                                                  focus:text-primary-800 \
                                                  disabled:opacity-50 \
                                                  disabled:pointer-events-none";
                                               ];
                                           ]
                                         [ txt "View" ];
                                     ];
                                 ])
                             unikernels);
                      ];
                  ];
              ];
          ];
      ])
