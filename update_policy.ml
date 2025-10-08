let update_policy_layout (user : User_model.user) ~user_policy
    ~unallocated_resources instance_name =
  Tyxml_html.(
    section
      ~a:[ a_id "policy-form" ]
      [
        h2
          ~a:[ a_class [ "font-semibold text-2xl" ] ]
          [
            txt
              ("Set Policy for " ^ user.name ^ "on albatross instance "
              ^ Configuration.name_to_str instance_name);
          ];
        p ~a:[ a_id "form-alert"; a_class [ "my-4" ] ] [];
        p ~a:[ a_id "user_id"; a_class [ "hidden" ] ] [ txt user.uuid ];
        div
          ~a:[ a_class [ "py-3" ] ]
          [
            Utils.increment_or_decrement_ui ~id:"f_allowed_unikernels"
              ~default_value:user_policy.Vmm_core.Policy.unikernels ~min_value:0
              ~max_value:
                Vmm_core.Policy.(
                  unallocated_resources.unikernels + user_policy.unikernels)
              ~figure_unit:"VMs" ~label':"Allowed Unikernels" ();
          ];
        hr ();
        div
          ~a:[ a_class [ "py-3" ] ]
          [
            Utils.increment_or_decrement_ui ~id:"f_allowed_memory"
              ~default_value:user_policy.Vmm_core.Policy.memory ~step:32
              ~min_value:0
              ~max_value:
                Vmm_core.Policy.(
                  unallocated_resources.memory + user_policy.memory)
              ~figure_unit:"MB" ~label':"Allowed Memory" ();
          ];
        hr ();
        div
          ~a:[ a_class [ "py-3" ] ]
          [
            Utils.increment_or_decrement_ui ~id:"f_allowed_storage"
              ~default_value:
                (Option.value ~default:0 user_policy.Vmm_core.Policy.block)
              ~min_value:0
              ~max_value:
                Vmm_core.Policy.(
                  Option.value ~default:0 unallocated_resources.block
                  + Option.value ~default:0 user_policy.block)
              ~figure_unit:"MB" ~step:32 ~label':"Allowed Storage" ();
          ];
        hr ();
        div
          ~a:[ a_class [ "my-3" ] ]
          [
            label ~a:[ a_class [ "block font-medium" ] ] [ txt "CPU IDs" ];
            div
              ~a:
                [
                  Unsafe.string_attrib "x-data"
                    ("multiselect("
                    ^ Utils.cpuid_to_array_string
                        (Vmm_core.IS.elements user_policy.cpuids)
                    ^ ", "
                    ^ Utils.cpuid_to_array_string
                        (Vmm_core.IS.elements unallocated_resources.cpuids)
                    ^ ")");
                  a_class [ "multiselect border my-4 p-4 rounded" ];
                ]
              [
                div
                  ~a:[ a_class [ "selected-items" ] ]
                  [
                    template
                      ~a:
                        [
                          Unsafe.string_attrib "x-for"
                            "(item, index) in selected";
                          Unsafe.string_attrib ":key" "item";
                        ]
                      [
                        span
                          ~a:
                            [
                              a_class
                                [
                                  "selected-tag rounded bg-primary-100 p-1 ml-1";
                                ];
                            ]
                          [
                            span ~a:[ Unsafe.string_attrib "x-text" "item" ] [];
                            (*This button should be rounded-full w-6 h-6 text-center bg-secondary-300*)
                            Utils.button_component
                              ~attribs:
                                [
                                  Unsafe.string_attrib "x-on:click"
                                    "removeItem(index)";
                                ]
                              ~content:(txt "x")
                              ~extra_css:
                                "rounded-full w-6 h-6 text-center \
                                 justify-center inline-flex items-center"
                              ~btn_type:`Danger_full ();
                          ];
                      ];
                  ];
                div
                  ~a:
                    [
                      Unsafe.string_attrib "x-on:click" "toggleDropdown";
                      a_class [ "dropdown my-3" ];
                    ]
                  [
                    Utils.button_component
                      ~attribs:
                        [
                          Unsafe.string_attrib "x-on:click" "removeItem(index)";
                        ]
                      ~content:
                        (span
                           [
                             span
                               ~a:[ a_class [ "px-2" ] ]
                               [ txt "Assign CPUs" ];
                             i ~a:[ a_class [ "fa-solid fa-caret-down" ] ] [];
                           ])
                      ~btn_type:`Primary_outlined ();
                  ];
                div
                  ~a:
                    [
                      Unsafe.string_attrib "x-show" "isOpen";
                      a_class [ "dropdown-list" ];
                    ]
                  [
                    template
                      ~a:
                        [
                          Unsafe.string_attrib "x-for"
                            "(option, index) in options";
                          Unsafe.string_attrib ":key" "option";
                        ]
                      [
                        p
                          ~a:[ a_class [ "py-2" ] ]
                          [
                            input
                              ~a:
                                [
                                  a_input_type `Checkbox;
                                  a_class [ "accent-primary-500 mr-2" ];
                                  Unsafe.string_attrib ":value" "option";
                                  Unsafe.string_attrib "x-on:change"
                                    "updateSelection($event, option)";
                                  Unsafe.string_attrib ":checked"
                                    "selected.includes(option)";
                                ]
                              ();
                            span [ txt "CPU " ];
                            span
                              ~a:[ Unsafe.string_attrib "x-text" "option" ]
                              [];
                          ];
                      ];
                  ];
                input
                  ~a:
                    [
                      a_input_type `Hidden;
                      Unsafe.string_attrib ":value" "selected.join(',')";
                      a_id "selectedCPUs";
                    ]
                  ();
              ];
          ];
        div
          ~a:[ a_class [ "my-3" ] ]
          [
            label
              ~a:[ a_class [ "block font-medium" ] ]
              [ txt "Network interfaces" ];
            div
              ~a:
                (let bridges_to_array_string set =
                   if Vmm_core.String_set.is_empty set then "[]"
                   else
                     "[\""
                     ^ String.concat "\", \"" (Vmm_core.String_set.elements set)
                     ^ "\"]"
                 in

                 [
                   Unsafe.string_attrib "x-data"
                     ("multiselect("
                     ^ bridges_to_array_string user_policy.bridges
                     ^ ", "
                     ^ bridges_to_array_string unallocated_resources.bridges
                     ^ ")");
                   a_class [ "multiselect border my-4 p-4 rounded" ];
                 ])
              [
                div
                  ~a:[ a_class [ "selected-items" ] ]
                  [
                    template
                      ~a:
                        [
                          Unsafe.string_attrib "x-for"
                            "(item, index) in selected";
                          Unsafe.string_attrib ":key" "item";
                        ]
                      [
                        span
                          ~a:
                            [
                              a_class
                                [
                                  "selected-tag rounded bg-primary-100 p-1 ml-1";
                                ];
                            ]
                          [
                            span ~a:[ Unsafe.string_attrib "x-text" "item" ] [];
                            Utils.button_component
                              ~attribs:
                                [
                                  Unsafe.string_attrib "x-on:click"
                                    "removeItem(index)";
                                ]
                              ~extra_css:
                                "rounded-full w-6 h-6 text-center \
                                 justify-center inline-flex items-center"
                              ~content:(txt "x") ~btn_type:`Danger_full ();
                          ];
                      ];
                  ];
                div
                  ~a:
                    [
                      Unsafe.string_attrib "x-on:click" "toggleDropdown";
                      a_class [ "dropdown my-3" ];
                    ]
                  [
                    Utils.button_component ~attribs:[]
                      ~content:
                        (span
                           [
                             span
                               ~a:[ a_class [ "px-2" ] ]
                               [ txt "Assign Bridges" ];
                             i ~a:[ a_class [ "fa-solid fa-caret-down" ] ] [];
                           ])
                      ~btn_type:`Primary_outlined ();
                  ];
                div
                  ~a:
                    [
                      Unsafe.string_attrib "x-show" "isOpen";
                      a_class [ "dropdown-list" ];
                    ]
                  [
                    template
                      ~a:
                        [
                          Unsafe.string_attrib "x-for"
                            "(option, index) in options";
                          Unsafe.string_attrib ":key" "option";
                        ]
                      [
                        p
                          ~a:[ a_class [ "py-2" ] ]
                          [
                            input
                              ~a:
                                [
                                  a_input_type `Checkbox;
                                  a_class [ "accent-primary-500 mr-2" ];
                                  Unsafe.string_attrib ":value" "option";
                                  Unsafe.string_attrib "x-on:change"
                                    "updateSelection($event, option)";
                                  Unsafe.string_attrib ":checked"
                                    "selected.includes(option)";
                                ]
                              ();
                            span
                              ~a:[ Unsafe.string_attrib "x-text" "option" ]
                              [];
                          ];
                      ];
                  ];
                input
                  ~a:
                    [
                      a_input_type `Hidden;
                      Unsafe.string_attrib ":value" "selected.join(',')";
                      a_id "selectedBridges";
                    ]
                  ();
              ];
          ];
        hr ();
        div
          ~a:[ a_class [ "my-3" ] ]
          [
            Utils.button_component
              ~attribs:
                [
                  a_onclick
                    ("updatePolicy('"
                    ^ Configuration.name_to_str instance_name
                    ^ "')");
                  a_id "set-policy-btn";
                ]
              ~content:(txt "Set Policy") ~btn_type:`Primary_full ();
          ];
      ])
