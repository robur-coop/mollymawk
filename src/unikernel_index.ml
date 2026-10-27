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
            Ui.td_cell
              [
                txt name;
                p
                  ~a:[ a_class [ "text-xs text-gray-500" ] ]
                  [
                    txt
                      ("from: " ^ Models.Configuration.name_to_str instance_name);
                  ];
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
            Ui.td_txt
              (Option.value ~default:"50 (default)"
                 (Option.map string_of_int unikernel.startup));
            Ui.td_txt (string_of_int unikernel.memory ^ " MB");
            Ui.td_txt
              (Utils.TimeHelper.time_ago ~current_time
                 ~check_time:unikernel.started);
            Ui.td_cell
              [
                Ui.link_button
                  ~href:
                    ("/unikernel/info?unikernel=" ^ name ^ "&instance="
                    ^ Models.Configuration.name_to_str instance_name)
                  ~content:(txt "View") ~btn_type:`Primary_outlined ();
              ];
          ])
      albatross_instance_unikernels)

let deceased_unikernels_table deceased_unikernels_by_albatross_instance =
  Tyxml_html.(
    Ui.simple_table
      ~headers:
        [ Ui.static_th "Instance"; Ui.static_th "Name"; Ui.static_th "Action" ]
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
                   Ui.td_txt (Models.Configuration.name_to_str instance_name);
                   Ui.td_txt name_str;
                   Ui.td_cell
                     [
                       Ui.link_button
                         ~href:
                           (Fmt.str
                              "/unikernel/console?unikernel=%s&instance=%s"
                              name_str
                              (Models.Configuration.name_to_str instance_name))
                         ~attribs:[ a_target "_blank" ]
                         ~content:(txt "View logs") ~btn_type:`Primary_outlined
                         ();
                     ];
                 ])
             names)
         deceased_unikernels_by_albatross_instance
      |> List.flatten))

let unikernel_index_layout unikernels_by_albatross_instance
    deceased_unikernels_by_albatross_instance current_time =
  let online_instances_count =
    List.fold_left
      (fun count (_, unikernels) ->
        if List.length unikernels > 0 then count + 1 else count)
      0 unikernels_by_albatross_instance
  in
  let total_unikernels_count =
    List.fold_left
      (fun acc (_, us) -> acc + List.length us)
      0 unikernels_by_albatross_instance
  in
  let deceased_instances_count =
    List.fold_left
      (fun count (_, unikernels) ->
        if List.length unikernels > 0 then count + 1 else count)
      0 deceased_unikernels_by_albatross_instance
  in
  let total_deceased_count =
    List.fold_left
      (fun acc (_, us) -> acc + List.length us)
      0 deceased_unikernels_by_albatross_instance
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
            Ui.button_component
              ~attribs:
                [
                  Unsafe.string_attrib "x-on:click" "tab = 'online'";
                  Unsafe.string_attrib ":class"
                    "tab === 'online' ? 'border-primary-500 text-primary-600' \
                     : 'border-transparent text-gray-500 hover:text-gray-700 \
                     hover:border-gray-300'";
                ]
              ~btn_type:
                (`Custom
                   "py-2 px-4 border-b-2 font-medium text-sm \
                    focus:outline-none transition-colors duration-150")
              ~content:(txt "Online Unikernels") ();
            Ui.button_component
              ~attribs:
                [
                  Unsafe.string_attrib "x-on:click" "tab = 'deceased'";
                  Unsafe.string_attrib ":class"
                    "tab === 'deceased' ? 'border-primary-500 \
                     text-primary-600' : 'border-transparent text-gray-500 \
                     hover:text-gray-700 hover:border-gray-300'";
                ]
              ~btn_type:
                (`Custom
                   "py-2 px-4 border-b-2 font-medium text-sm \
                    focus:outline-none transition-colors duration-150")
              ~content:(txt "Deceased Unikernels")
              ();
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
                Ui.raw_select
                  ~attribs:
                    [
                      a_id "filterAlbatrossIntances";
                      Unsafe.string_attrib "x-show" "tab === 'online'";
                      a_onchange "filterAlbatrossInstance(event)";
                    ]
                  (option ~a:[ a_value "all" ] (txt "All instances")
                  :: List.map
                       (fun (name, unikernels) ->
                         option
                           ~a:
                             [ a_value (Models.Configuration.name_to_str name) ]
                           (txt
                              (Fmt.str "%s (%u unikernels)"
                                 (Models.Configuration.name_to_str name)
                                 (List.length unikernels))))
                       unikernels_by_albatross_instance);
                Ui.raw_select
                  ~attribs:
                    [
                      a_id "filterAlbatrossIntancesDeceased";
                      Unsafe.string_attrib "x-show" "tab === 'deceased'";
                      Unsafe.string_attrib "style" "display: none;";
                      a_onchange "filterAlbatrossInstance(event)";
                    ]
                  (option ~a:[ a_value "all" ] (txt "All instances")
                  :: List.map
                       (fun (name, deceased_unikernels) ->
                         option
                           ~a:
                             [ a_value (Models.Configuration.name_to_str name) ]
                           (txt
                              (Fmt.str "%s (%u unikernels)"
                                 (Models.Configuration.name_to_str name)
                                 (List.length deceased_unikernels))))
                       deceased_unikernels_by_albatross_instance);
                Ui.search_input ();
              ];
          ];
        hr ~a:[ a_class [ "border border-primary-500 my-5" ] ] ();
        div
          ~a:[ Unsafe.string_attrib "x-show" "tab === 'online'" ]
          [
            Ui.table_wrapper
              (if total_unikernels_count = 0 then
                 div
                   ~a:
                     [
                       a_class
                         [
                           "text-center py-16 px-4 bg-white rounded-lg \
                            shadow-sm border border-dashed border-gray-200 \
                            mt-6 max-w-2xl mx-auto";
                         ];
                     ]
                   [
                     i
                       ~a:
                         [
                           a_class
                             [
                               "fa-solid fa-cube text-6xl text-gray-300 mb-6 \
                                block";
                             ];
                         ]
                       [];
                     h3
                       ~a:
                         [
                           a_class [ "text-lg font-medium text-gray-900 mb-2" ];
                         ]
                       [ txt "No unikernels yet" ];
                     p
                       ~a:[ a_class [ "text-gray-500 mb-8 max-w-sm mx-auto" ] ]
                       [
                         txt
                           "You don't have any unikernel instances running. \
                            Deploy your first workload by grabbing a verified \
                            image from the registry.";
                       ];
                     a
                       ~a:
                         [
                           a_href "/unikernel/deploy";
                           a_class
                             [
                               "inline-flex items-center px-4 py-2 \
                                bg-primary-600 text-white font-medium rounded \
                                shadow hover:bg-primary-700 transition";
                             ];
                         ]
                       [
                         i ~a:[ a_class [ "fa-solid fa-rocket mr-2" ] ] [];
                         txt "Deploy your first unikernel";
                       ];
                     div
                       ~a:[ a_class [ "mt-6" ] ]
                       [
                         a
                           ~a:
                             [
                               a_href
                                 "https://robur-coop.github.io/mollymawk-handbook/index.html";
                               a_class
                                 [
                                   "text-sm text-primary-500 \
                                    hover:text-primary-600 font-medium \
                                    hover:underline";
                                 ];
                             ]
                           [ txt "Learn more about unikernels" ];
                       ];
                   ]
               else
                 Ui.simple_table
                   ~attribs:[ a_id "data-table" ]
                   ~headers:
                     [
                       Ui.sortable_th "Name";
                       Ui.sortable_th "Startup Priority";
                       Ui.sortable_th "Memory";
                       Ui.sortable_th "Created";
                       Ui.static_th "Action";
                     ]
                   (List.map
                      (fun (instance_name, unikernels) ->
                        instance_unikernels instance_name unikernels
                          current_time)
                      unikernels_by_albatross_instance
                   |> List.flatten));
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
