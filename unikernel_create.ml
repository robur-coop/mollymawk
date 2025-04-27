let unikernel_create_layout ~(user_policy : Vmm_core.Policy.t) unikernels
    (blocks : (Vmm_core.Name.t * int * bool) list) =
  let _, _, _, total_memory_used, count_cpuid_usage, _ =
    Utils.user_policy_usage user_policy unikernels blocks
  in
  let memory_left = user_policy.memory - total_memory_used in
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        div
          ~a:[ a_class [ "px-3 flex justify-between items-center" ] ]
          [
            p
              ~a:[ a_class [ "font-bold text-gray-700" ] ]
              [ txt "Deploy a Unikernel" ];
          ];
        hr ();
        div
          ~a:[ a_class [ "space-y-6 mt-8 p-6 max-w-5xl mx-auto" ] ]
          [
            p ~a:[ a_id "form-alert"; a_class [ "my-4 hidden" ] ] [];
            div
              ~a:[ a_class [ "grid grid-cols-2 gap-3" ] ]
              [
                div
                  [
                    label ~a:[ a_class [ "block font-medium" ] ] [ txt "Name*" ];
                    input
                      ~a:
                        [
                          a_input_type `Text;
                          a_name "name";
                          a_required ();
                          a_id "unikernel-name";
                          a_class
                            [
                              "ring-primary-100 mt-1.5 transition \
                               appearance-none block w-full px-3 py-3 \
                               rounded-xl shadow-sm border \
                               hover:border-primary-200\n\
                              \                                           \
                               focus:border-primary-300 bg-primary-50 \
                               bg-opacity-0 hover:bg-opacity-50 \
                               focus:bg-opacity-50 ring-primary-200 \
                               focus:ring-primary-200\n\
                              \                                           \
                               focus:ring-[1px] focus:outline-none";
                            ];
                        ]
                      ();
                  ];
                div
                  [
                    label
                      ~a:[ a_class [ "block font-medium" ] ]
                      [ txt "CPU Id" ];
                    select
                      ~a:
                        [
                          a_id "cpuid";
                          a_name "cpuid";
                          a_class
                            [
                              "ring-primary-100 mt-1.5 transition block w-full \
                               px-3 py-3 rounded-xl shadow-sm border \
                               hover:border-primary-200\n\
                              \                                           \
                               focus:border-primary-300 bg-primary-50 \
                               bg-opacity-0 hover:bg-opacity-50 \
                               focus:bg-opacity-50 ring-primary-200 \
                               focus:ring-primary-200\n\
                              \                                           \
                               focus:ring-[1px] focus:outline-none";
                            ];
                        ]
                      (List.map
                         (fun (cpu_id, count) ->
                           option
                             ~a:[ a_value (string_of_int cpu_id) ]
                             (txt
                                ("CPU " ^ string_of_int cpu_id ^ " (used by "
                               ^ string_of_int count ^ " unikernels)")))
                         count_cpuid_usage);
                  ];
                div
                  ~a:[ a_class [ "" ] ]
                  [
                    label
                      ~a:[ a_class [ "block font-medium" ] ]
                      [ txt "Fail Behaviour" ];
                    div
                      ~a:[ a_class [ "" ] ]
                      [
                        Utils.switch_button ~switch_id:"restart-on-fail"
                          ~switch_label:"Restart"
                          (div
                             [
                               label
                                 ~a:
                                   [
                                     a_class [ "block text-sm font-medium" ];
                                     a_label_for "restart_description";
                                   ]
                                 [
                                   txt
                                     "This unikernel will automatically \
                                      restart on fail";
                                 ];
                             ]);
                      ];
                  ];
                div
                  ~a:
                    [
                      a_class [ "space-x-5" ];
                      Unsafe.string_attrib "x-data"
                        ("{count : "
                        ^ (if memory_left >= 32 then string_of_int 32
                           else string_of_int memory_left)
                        ^ "}");
                    ]
                  [
                    label
                      ~a:[ a_class [ "block font-medium" ] ]
                      [
                        txt ("Memory (max " ^ string_of_int memory_left ^ "MB)");
                      ];
                    Utils.button_component
                      ~attribs:
                        [
                          Unsafe.string_attrib "x-on:click" "count = count + 32";
                        ]
                      ~content:(i ~a:[ a_class [ "fa-solid fa-plus" ] ] [])
                      ~btn_type:`Primary_outlined ();
                    span
                      ~a:
                        [
                          a_id "unikernel-memory";
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
                            "if (count > 32) count = count - 32";
                        ]
                      ~content:(i ~a:[ a_class [ "fa-solid fa-minus" ] ] [])
                      ~btn_type:`Danger_outlined ();
                  ];
              ];
            hr ();
            div
              [
                label
                  ~a:[ a_class [ "block font-medium" ] ]
                  [ txt "Network Interfaces" ];
                Utils.dynamic_dropdown_form
                  (Vmm_core.String_set.elements user_policy.bridges)
                  ~get_label:(fun bridge -> bridge)
                  ~get_value:(fun bridge -> bridge)
                  ~id:"network";
              ];
            hr ();
            div
              [
                label
                  ~a:[ a_class [ "block font-medium" ] ]
                  [ txt "Block devices" ];
                Utils.dynamic_dropdown_form blocks
                  ~get_label:(fun (name, size, used) ->
                    Option.value ~default:"" (Vmm_core.Name.name name)
                    ^ " - " ^ Int.to_string size ^ "MB (used: "
                    ^ Bool.to_string used ^ ")")
                  ~get_value:(fun (name, _, _) ->
                    Option.value ~default:"" (Vmm_core.Name.name name))
                  ~id:"block";
              ];
            hr ();
            div
              [
                label ~a:[ a_class [ "block font-medium" ] ] [ txt "Arguments" ];
                p
                  [
                    txt "Write arguments seperated by a whitespace e.g ";
                    code [ txt "--ip=127.0.0.1 --port=8180" ];
                  ];
                textarea
                  ~a:
                    [
                      a_rows 4;
                      a_required ();
                      a_name "arguments";
                      a_id "unikernel-arguments";
                      a_class
                        [
                          "ring-primary-100 mt-1.5 transition appearance-none \
                           block w-full px-3 py-3 rounded-xl shadow-sm border \
                           hover:border-primary-200\n\
                          \                                           \
                           focus:border-primary-300 bg-primary-50 bg-opacity-0 \
                           hover:bg-opacity-50 focus:bg-opacity-50 \
                           ring-primary-200 focus:ring-primary-200\n\
                          \                                           \
                           focus:ring-[1px] focus:outline-none";
                        ];
                    ]
                  (txt "");
              ];
            hr ();
            div
              [
                label
                  ~a:[ a_class [ "block font-medium" ] ]
                  [ txt "Unikernel Image Binary*" ];
                input
                  ~a:
                    [
                      a_input_type `File;
                      a_name "binary";
                      a_required ();
                      a_id "unikernel-binary";
                      a_class
                        [
                          "ring-primary-100 mt-1.5 transition appearance-none \
                           block w-full px-3 py-3 rounded-xl shadow-sm border \
                           hover:border-primary-200\n\
                          \                                           \
                           focus:border-primary-300 bg-primary-50 bg-opacity-0 \
                           hover:bg-opacity-50 focus:bg-opacity-50 \
                           ring-primary-200 focus:ring-primary-200\n\
                          \                                           \
                           focus:ring-[1px] focus:outline-none";
                        ];
                    ]
                  ();
              ];
            div
              ~a:[ a_class [ "flex justify-items-center mx-auto w-60" ] ]
              [
                Utils.button_component
                  ~attribs:
                    [ a_id "deploy-button"; a_onclick "deployUnikernel()" ]
                  ~content:(txt "Deploy") ~btn_type:`Primary_full ();
              ];
          ];
      ])
