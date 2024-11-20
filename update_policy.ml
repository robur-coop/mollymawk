let update_policy_layout (user : User_model.user) ~user_policy
    ~unallocated_resources =
  Tyxml_html.(
    section
      ~a:[ a_id "policy-form" ]
      [
        h2
          ~a:[ a_class [ "font-semibold text-2xl" ] ]
          [ txt ("Set Policy for " ^ user.name) ];
        p ~a:[ a_id "form-alert"; a_class [ "my-4" ] ] [];
        p ~a:[ a_id "user_id"; a_class [ "hidden" ] ] [ txt user.uuid ];
        div
          ~a:[ a_class [ "my-3" ] ]
          [
            label
              ~a:[ a_class [ "block font-medium" ] ]
              [ txt "Allowed Unikernels" ];
            small
              ~a:[ a_class [ "text-sm" ] ]
              [
                txt
                  ("can assign up to: "
                  ^ string_of_int
                      Vmm_core.Policy.(
                        unallocated_resources.unikernels
                        + user_policy.unikernels));
              ];
            div
              ~a:
                [
                  a_class [ "space-x-5 my-4" ];
                  Unsafe.string_attrib "x-data"
                    ("{count : "
                    ^ string_of_int user_policy.Vmm_core.Policy.unikernels
                    ^ "}");
                ]
              [
                Utils.button_component
                  ~attribs:[ Unsafe.string_attrib "x-on:click" "count++" ]
                  ~content:(i ~a:[ a_class [ "fa-solid fa-plus" ] ] [])
                  ~btn_type:`Primary_outlined ();
                span
                  ~a:
                    [
                      a_id "f_allowed_unikernels";
                      a_contenteditable true;
                      a_class [ "text-4xl border px-4" ];
                      a_user_data "x-on:keydown.enter.prevent" "";
                      Unsafe.string_attrib "x-on:input"
                        "let value = \
                         $event.target.innerText.replace(/[^0-9]/g,'');\n\
                        \                                     \
                         $event.target.innerText = value;\n\
                        \                                     count = \
                         parseInt(value) || 0;";
                      Unsafe.string_attrib "x-text" "count";
                      Unsafe.string_attrib "x-on:blur"
                        "$event.target.innerText = count;";
                    ]
                  [];
                Utils.button_component
                  ~attribs:
                    [
                      Unsafe.string_attrib "x-on:click" "if (count > 0) count--";
                    ]
                  ~content:(i ~a:[ a_class [ "fa-solid fa-minus" ] ] [])
                  ~btn_type:`Danger_outlined ();
              ];
          ];
        hr ();
        div
          ~a:[ a_class [ "my-3" ] ]
          [
            label
              ~a:[ a_class [ "block font-medium" ] ]
              [ txt "Allowed Memory" ];
            small
              ~a:[ a_class [ "text-sm" ] ]
              [
                txt
                  ("can assign up to: "
                  ^ string_of_int
                      Vmm_core.Policy.(
                        unallocated_resources.memory + user_policy.memory)
                  ^ " MB");
              ];
            div
              ~a:
                [
                  a_class [ "space-x-5 my-4" ];
                  Unsafe.string_attrib "x-data"
                    ("{count : "
                    ^ string_of_int user_policy.Vmm_core.Policy.memory
                    ^ "}");
                ]
              [
                Utils.button_component
                  ~attribs:
                    [ Unsafe.string_attrib "x-on:click" "count = count + 50" ]
                  ~content:(i ~a:[ a_class [ "fa-solid fa-plus" ] ] [])
                  ~btn_type:`Primary_outlined ();
                span
                  ~a:
                    [
                      a_id "f_allowed_memory";
                      a_contenteditable true;
                      a_class [ "text-4xl border px-4" ];
                      a_user_data "x-on:keydown.enter.prevent" "";
                      Unsafe.string_attrib "x-on:input"
                        "let value = \
                         $event.target.innerText.replace(/[^0-9]/g,'');\n\
                        \                                     \
                         $event.target.innerText = value;\n\
                        \                                     count = \
                         parseInt(value) || 0;";
                      Unsafe.string_attrib "x-text" "count";
                      Unsafe.string_attrib "x-on:blur"
                        "$event.target.innerText = count;";
                    ]
                  [];
                span ~a:[ a_class [ "text-4xl" ] ] [ txt " MB" ];
                Utils.button_component
                  ~attribs:
                    [
                      Unsafe.string_attrib "x-on:click"
                        "if (count > 0) count = count - 50";
                    ]
                  ~content:(i ~a:[ a_class [ "fa-solid fa-minus" ] ] [])
                  ~btn_type:`Danger_outlined ();
              ];
          ];
        hr ();
        div
          ~a:[ a_class [ "my-3" ] ]
          [
            label
              ~a:[ a_class [ "block font-medium" ] ]
              [ txt "Allowed Storage" ];
            small
              ~a:[ a_class [ "text-sm" ] ]
              [
                txt
                  ("can assign up to: "
                  ^ string_of_int
                      Vmm_core.Policy.(
                        match
                          (unallocated_resources.block, user_policy.block)
                        with
                        | Some unallocated, Some user_block ->
                            unallocated + user_block
                        | Some unallocated, None -> unallocated
                        | _ -> 0)
                  ^ " MB");
              ];
            div
              ~a:
                [
                  a_class [ "space-x-5 my-4" ];
                  Unsafe.string_attrib "x-data"
                    ("{count : "
                    ^ (match user_policy.Vmm_core.Policy.block with
                      | None -> string_of_int 0
                      | Some x -> string_of_int x)
                    ^ "}");
                ]
              [
                Utils.button_component
                  ~attribs:
                    [ Unsafe.string_attrib "x-on:click" "count = count + 50" ]
                  ~content:(i ~a:[ a_class [ "fa-solid fa-plus" ] ] [])
                  ~btn_type:`Primary_outlined ();
                span
                  ~a:
                    [
                      a_id "f_allowed_storage";
                      a_contenteditable true;
                      a_class [ "text-4xl border px-4" ];
                      a_user_data "x-on:keydown.enter.prevent" "";
                      Unsafe.string_attrib "x-on:input"
                        "let value = \
                         $event.target.innerText.replace(/[^0-9]/g,'');\n\
                        \                                     \
                         $event.target.innerText = value;\n\
                        \                                     count = \
                         parseInt(value) || 0;";
                      Unsafe.string_attrib "x-text" "count";
                      Unsafe.string_attrib "x-on:blur"
                        "$event.target.innerText = count;";
                    ]
                  [];
                span ~a:[ a_class [ "text-4xl" ] ] [ txt "MB" ];
                Utils.button_component
                  ~attribs:
                    [
                      Unsafe.string_attrib "x-on:click"
                        "if (count > 0) count = count - 50";
                    ]
                  ~content:(i ~a:[ a_class [ "fa-solid fa-minus" ] ] [])
                  ~btn_type:`Danger_outlined ();
              ];
          ];
        div
          ~a:[ a_class [ "my-3" ] ]
          [
            label ~a:[ a_class [ "block font-medium" ] ] [ txt "CPU IDs" ];
            div
              ~a:
                (let cpuid_to_array_string lst =
                   if lst = [] then "[]"
                   else
                     "[\""
                     ^ String.concat "\", \"" (List.map string_of_int lst)
                     ^ "\"]"
                 in

                 [
                   Unsafe.string_attrib "x-data"
                     ("multiselect("
                     ^ cpuid_to_array_string
                         (Vmm_core.IS.elements user_policy.cpuids)
                     ^ ", "
                     ^ cpuid_to_array_string
                         (Vmm_core.IS.elements unallocated_resources.cpuids)
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
              ~attribs:[ a_onclick "updatePolicy()"; a_id "set-policy-btn" ]
              ~content:(txt "Set Policy") ~btn_type:`Primary_full ();
          ];
      ])
