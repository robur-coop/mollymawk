let update_policy_layout (user : User_model.user) ~user_policy ~root_policy =
  let empty_policy =
    Vmm_core.Policy.
      {
        vms = 0;
        cpuids = Vmm_core.IS.of_list [];
        memory = 0;
        block = Some 0;
        bridges = Vmm_core.String_set.of_list [];
      }
  in
  let root_policy =
    match root_policy with Some p -> p | None -> empty_policy
  in
  let policy =
    match user_policy with None -> empty_policy | Some policy -> policy
  in
  Tyxml_html.(
    section
      ~a:[ a_id "policy-form" ]
      [
        h2
          ~a:[ a_class [ "font-semibold text-xl" ] ]
          [ txt ("Set Policy for " ^ user.name) ];
        p ~a:[ a_id "form-alert"; a_class [ "my-4" ] ] [];
        p ~a:[ a_id "user_id"; a_class [ "hidden" ] ] [ txt user.uuid ];
        div
          ~a:[ a_class [ "my-4" ] ]
          [
            label
              ~a:[ a_class [ "block text-sm font-medium" ] ]
              [ txt "Allowed VMs" ];
            p
              [
                txt ("total: " ^ string_of_int root_policy.Vmm_core.Policy.vms);
              ];
            div
              ~a:
                [
                  a_class [ "space-x-5 my-4" ];
                  Unsafe.string_attrib "x-data"
                    ("{count : "
                    ^ string_of_int policy.Vmm_core.Policy.vms
                    ^ "}");
                ]
              [
                button
                  ~a:
                    [
                      a_class
                        [
                          "border py-2 px-3 border-primary-500 \
                           hover:bg-primary-100 rounded-md";
                        ];
                      Unsafe.string_attrib "x-on:click" "count++";
                    ]
                  [ i ~a:[ a_class [ "fa-solid fa-plus" ] ] [] ];
                span
                  ~a:
                    [
                      a_id "f_allowed_vms";
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
                button
                  ~a:
                    [
                      a_class
                        [
                          "border py-2 px-3 border-secondary-500 \
                           hover:bg-secondary-100 rounded-md";
                        ];
                      Unsafe.string_attrib "x-on:click" "if (count > 0) count--";
                    ]
                  [ i ~a:[ a_class [ "fa-solid fa-minus" ] ] [] ];
              ];
          ];
        hr ();
        div
          ~a:[ a_class [ "my-4" ] ]
          [
            label
              ~a:[ a_class [ "block text-sm font-medium" ] ]
              [ txt "Allowed Memory" ];
            div
              ~a:
                [
                  a_class [ "space-x-5 my-4" ];
                  Unsafe.string_attrib "x-data"
                    ("{count : "
                    ^ string_of_int policy.Vmm_core.Policy.memory
                    ^ "}");
                ]
              [
                button
                  ~a:
                    [
                      a_class
                        [
                          "border py-2 px-3 border-primary-500 \
                           hover:bg-primary-100 rounded-md";
                        ];
                      Unsafe.string_attrib "x-on:click" "count = count + 50";
                    ]
                  [ i ~a:[ a_class [ "fa-solid fa-plus" ] ] [] ];
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
                span ~a:[ a_class [ "text-4xl" ] ] [ txt "MB" ];
                button
                  ~a:
                    [
                      a_class
                        [
                          "border py-2 px-3 border-secondary-500 \
                           hover:bg-secondary-100 rounded-md";
                        ];
                      Unsafe.string_attrib "x-on:click"
                        "if (count > 0) count = count - 50";
                    ]
                  [ i ~a:[ a_class [ "fa-solid fa-minus" ] ] [] ];
              ];
          ];
        hr ();
        div
          ~a:[ a_class [ "my-4" ] ]
          [
            label
              ~a:[ a_class [ "block text-sm font-medium" ] ]
              [ txt "Allowed Storage" ];
            div
              ~a:
                [
                  a_class [ "space-x-5 my-4" ];
                  Unsafe.string_attrib "x-data"
                    ("{count : "
                    ^ (match policy.Vmm_core.Policy.block with
                      | None -> string_of_int 0
                      | Some x -> string_of_int x)
                    ^ "}");
                ]
              [
                button
                  ~a:
                    [
                      a_class
                        [
                          "border py-2 px-3 border-primary-500 \
                           hover:bg-primary-100 rounded-md";
                        ];
                      Unsafe.string_attrib "x-on:click" "count = count + 50";
                    ]
                  [ i ~a:[ a_class [ "fa-solid fa-plus" ] ] [] ];
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
                button
                  ~a:
                    [
                      a_class
                        [
                          "border py-2 px-3 border-secondary-500 \
                           hover:bg-secondary-100 rounded-md";
                        ];
                      Unsafe.string_attrib "x-on:click"
                        "if (count > 0) count = count - 50";
                    ]
                  [ i ~a:[ a_class [ "fa-solid fa-minus" ] ] [] ];
              ];
          ];
        div
          ~a:[ a_class [ "my-4" ] ]
          [
            label
              ~a:[ a_class [ "block text-sm font-medium" ] ]
              [ txt "CPU IDs" ];
            div
              ~a:
                [
                  Unsafe.string_attrib "x-data"
                    "multiselect([\n                                    ])";
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
                            button
                              ~a:
                                [
                                  Unsafe.string_attrib "x-on:click"
                                    "removeItem(index)";
                                  a_class
                                    [
                                      "rounded-full bg-secondary-300 \
                                       text-secondary-700 w-6 h-6 text-center";
                                    ];
                                ]
                              [ txt "x" ];
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
                    button
                      ~a:
                        [
                          a_class
                            [ "dropdown-button flex justify-between space-x-4" ];
                        ]
                      [
                        span [ txt "Assign CPUs" ];
                        i ~a:[ a_class [ "fa-solid fa-caret-down" ] ] [];
                      ];
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
                                  a_class [ "text-primary-500 bg-primary-500" ];
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
          ~a:[ a_class [ "my-4" ] ]
          [
            label
              ~a:[ a_class [ "block text-sm font-medium" ] ]
              [ txt "Bridges" ];
            div
              ~a:
                [
                  Unsafe.string_attrib "x-data" "multiselect()";
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
                            button
                              ~a:
                                [
                                  Unsafe.string_attrib "x-on:click"
                                    "removeItem(index)";
                                  a_class
                                    [
                                      "rounded-full bg-secondary-300 \
                                       text-secondary-700 w-6 h-6 text-center";
                                    ];
                                ]
                              [ txt "x" ];
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
                    button
                      ~a:
                        [
                          a_class
                            [ "dropdown-button flex justify-between space-x-4" ];
                        ]
                      [
                        span [ txt "Assign Bridges" ];
                        i ~a:[ a_class [ "fa-solid fa-caret-down" ] ] [];
                      ];
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
                                  a_class [ "text-primary-500 bg-primary-500" ];
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
          ~a:[ a_class [ "my-4" ] ]
          [
            button
              ~a:
                [
                  a_onclick "updatePolicy()";
                  a_class [ "bg-primary-500 py-1 px-2 text-primary-50 rounded" ];
                ]
              [ txt "Set Policy" ];
          ];
      ])
