let th_cell name =
  Tyxml_html.(
    th
      ~a:
        [
          Unsafe.string_attrib "x-on:click" "sortByColumn";
          a_class
            [
              "px-6 py-3 text-start text-xs font-bold text-primary-600 \
               uppercase cursor-pointer select-none";
            ];
        ]
      [
        txt name;
        span
          ~a:[ a_class [ "px-2" ] ]
          [ i ~a:[ a_class [ "fa-solid fa-sort" ] ] [] ];
      ])

let td_cell content =
  Tyxml_html.(
    td
      ~a:
        [
          a_class
            [ "px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-800" ];
        ]
      content)

let unikernel_index_layout
    (unikernels_by_instance :
      (string * (Vmm_core.Name.t * Vmm_core.Unikernel.info) list) list)
    current_time =
  let instance_names = List.map fst unikernels_by_instance in
  let total_unikernels =
    List.fold_left
      (fun acc (_, uks) -> acc + List.length uks)
      0 unikernels_by_instance
  in

  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        div
          ~a:[ a_class [ "px-3 flex justify-between items-center" ] ]
          [
            div
              [
                p
                  ~a:[ a_class [ "font-bold text-gray-700" ] ]
                  [
                    txt ("Unikernels (" ^ string_of_int total_unikernels ^ ")");
                  ];
              ];
            div
              ~a:[ a_class [ "flex space-x-4 items-center" ] ]
              [
                div
                  [
                    select
                      ~a:
                        [
                          a_id "instanceFilter";
                          a_name "instanceFilter";
                          Unsafe.string_attrib "x-model" "selectedInstance";
                          a_class
                            [
                              "rounded py-2 px-3 border border-primary-200 \
                               focus:border-primary-500 outline-0";
                            ];
                        ]
                      ([ option ~a:[ a_value "all" ] (txt "All Instances") ]
                      @ List.map
                          (fun name -> option ~a:[ a_value name ] (txt name))
                          instance_names);
                  ];
                input
                  ~a:
                    [
                      a_onkeyup "filterData()";
                      a_placeholder "search";
                      a_id "searchQuery";
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
                      ~a:
                        [
                          Unsafe.string_attrib "x-data"
                            "{ ...sort_data(), selectedInstance: 'all' }";
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
                                   divide-gray-20";
                                ];
                            ]
                          ~thead:
                            (thead
                               [
                                 tr
                                   [
                                     th_cell "Instance";
                                     th_cell "Name";
                                     th_cell "Type";
                                     th_cell "CPU";
                                     th_cell "Memory";
                                     th_cell "Created";
                                     th_cell "Action";
                                   ];
                               ])
                          (List.concat_map
                             (fun (instance_name, unikernels) ->
                               List.map
                                 (fun (name, unikernel) ->
                                   let unikernel_name =
                                     Option.value ~default:"no name"
                                       (Vmm_core.Name.name name)
                                   in
                                   tr
                                     ~a:
                                       [
                                         a_class [ "border-b border-gray-200" ];
                                         Unsafe.string_attrib "x-show"
                                           (Fmt.str
                                              "selectedInstance === 'all' || \
                                               selectedInstance === '%s'"
                                              instance_name);
                                       ]
                                     [
                                       td_cell [ txt instance_name ];
                                       td_cell [ txt unikernel_name ];
                                       td_cell
                                         [
                                           txt
                                             (match
                                                unikernel.Vmm_core.Unikernel.typ
                                              with
                                             | `Solo5 -> "solo5");
                                         ];
                                       td_cell
                                         [ txt (string_of_int unikernel.cpuid) ];
                                       td_cell
                                         [
                                           txt
                                             (string_of_int unikernel.memory
                                             ^ " MB");
                                         ];
                                       td_cell
                                         [
                                           txt
                                             (Utils.TimeHelper.time_ago
                                                ~current_time
                                                ~check_time:unikernel.started);
                                         ];
                                       td
                                         ~a:[ a_class [ "px-6 py-4" ] ]
                                         [
                                           a
                                             ~a:
                                               [
                                                 a_href
                                                   ("/unikernel/info/"
                                                  ^ unikernel_name);
                                                 a_class
                                                   [
                                                     "inline-flex items-center \
                                                      gap-x-2 text-sm \
                                                      font-semibold rounded-lg \
                                                      border border-1 py-1 \
                                                      px-2 border-primary-400 \
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
                                 unikernels)
                             unikernels_by_instance);
                      ];
                  ];
              ];
          ];
      ])
