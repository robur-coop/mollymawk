let unikernel_create_layout ~(policies : (string * Vmm_core.Policy.t) list)
    unikernels_by_instance
    (blocks : (string * (Vmm_core.Name.t * int * bool) list) list) =
  let instance_data =
    List.map
      (fun (name, policy) ->
        let unikernels_on_instance =
          Option.value ~default:[] (List.assoc_opt name unikernels_by_instance)
        in
        let total_memory_used =
          Utils.total_memory_used unikernels_on_instance
        in
        let cpu_usage_count =
          Utils.cpu_usage_count policy unikernels_on_instance
        in
        let memory_left = policy.memory - total_memory_used in
        (name, (policy, memory_left, cpu_usage_count)))
      policies
  in
  let instance_data_json =
    let open Yojson.Basic in
    `Assoc
      (List.map
         (fun (name, (policy, memory_left, cpu_usage)) ->
           let cpu_usage_json =
             `List
               (List.map
                  (fun (cpu, count) ->
                    `Assoc [ ("cpu", `Int cpu); ("count", `Int count) ])
                  cpu_usage)
           in
           ( name,
             `Assoc
               [
                 ("policy", Albatross_json.policy_to_json policy);
                 ( "stats",
                   `Assoc
                     [
                       ("memory_left", `Int memory_left);
                       ("cpu_usage_count", cpu_usage_json);
                     ] );
               ] ))
         instance_data)
    |> to_string
  in
  let initial_instance_name, _ = List.hd policies in
  Tyxml_html.(
    section
      ~a:
        [
          a_class [ "col-span-7 p-4 bg-gray-50 my-1" ];
          Unsafe.string_attrib "x-data"
            (Fmt.str
               "{ instanceData: %s, selectedInstance: '%s', get selected() { \
                return this.instanceData[this.selectedInstance] } }"
               instance_data_json initial_instance_name);
        ]
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
                    label
                      ~a:[ a_class [ "block font-medium" ] ]
                      [ txt "Albatross Instance*" ];
                    select
                      ~a:
                        [
                          a_id "albatross-instance";
                          Unsafe.string_attrib "x-model" "selectedInstance";
                          a_class
                            [
                              "ring-primary-100 mt-1.5 transition \
                               appearance-none block w-full px-3 py-3 \
                               rounded-xl shadow-sm border \
                               hover:border-primary-200 \
                               focus:border-primary-300 bg-primary-50 \
                               bg-opacity-0 hover:bg-opacity-50 \
                               focus:bg-opacity-50 ring-primary-200 \
                               focus:ring-primary-200 focus:ring-[1px] \
                               focus:outline-none";
                            ];
                        ]
                      (List.map
                         (fun (name, _) ->
                           option ~a:[ a_value name ] (txt name))
                         policies);
                  ];
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
                               hover:border-primary-200 \
                               focus:border-primary-300 bg-primary-50 \
                               bg-opacity-0 hover:bg-opacity-50 \
                               focus:bg-opacity-50 ring-primary-200 \
                               focus:ring-primary-200 focus:ring-[1px] \
                               focus:outline-none";
                            ];
                        ]
                      ();
                  ];
                div
                  [
                    label
                      ~a:[ a_class [ "block font-medium" ] ]
                      [ txt "CPU ID" ];
                    select
                      ~a:
                        [
                          a_id "cpuid";
                          a_name "cpuid";
                          a_class
                            [
                              "ring-primary-100 mt-1.5 transition \
                               appearance-none block w-full px-3 py-3 \
                               rounded-xl shadow-sm border \
                               hover:border-primary-200 \
                               focus:border-primary-300 bg-primary-50 \
                               bg-opacity-0 hover:bg-opacity-50 \
                               focus:bg-opacity-50 ring-primary-200 \
                               focus:ring-primary-200 focus:ring-[1px] \
                               focus:outline-none";
                            ];
                        ]
                      [
                        Unsafe.data
                          "<template x-for=\"cpu in \
                           selected.stats.cpu_usage_count\">\n\
                          \             <option :value=\"cpu.cpu\" \
                           :text=\"`CPU ${cpu.cpu} (used by \
                           ${cpu.count})`\"></option>\n\
                          \           </template>";
                      ];
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
                      Unsafe.string_attrib "x-data" "{ count: 32 }";
                    ]
                  [
                    label
                      ~a:[ a_class [ "block font-medium" ] ]
                      [
                        txt "Memory (max ";
                        span
                          ~a:
                            [
                              Unsafe.string_attrib "x-text"
                                "selected.stats.memory_left";
                            ]
                          [];
                        txt "MB available)";
                      ];
                    Utils.button_component
                      ~attribs:
                        [
                          Unsafe.string_attrib "x-on:click"
                            "if (count > 32) count = count - 32";
                        ]
                      ~content:(i ~a:[ a_class [ "fa-solid fa-minus" ] ] [])
                      ~btn_type:`Danger_outlined ();
                    span
                      ~a:
                        [
                          a_id "unikernel-memory";
                          a_contenteditable true;
                          a_class [ "text-4xl border px-4" ];
                          Unsafe.string_attrib "@keydown.enter.prevent" "";
                          Unsafe.string_attrib "x-text" "count";
                          Unsafe.string_attrib "x-on:blur"
                            "\n\
                            \                        count = \
                             parseInt($event.target.innerText.replace(/[^0-9]/g,'')) \
                             || 0;\n\
                            \                        let value = \
                             $event.target.innerText.replace(/[^0-9]/g,'');count \
                             = parseInt(value) || 0;$event.target.innerText = \
                             count;";
                          Unsafe.string_attrib "x-init" "$el.innerText = count";
                        ]
                      [];
                    span ~a:[ a_class [ "text-4xl" ] ] [ txt " MB" ];
                    Utils.button_component
                      ~attribs:
                        [
                          Unsafe.string_attrib "x-on:click" "count = count + 32";
                        ]
                      ~content:(i ~a:[ a_class [ "fa-solid fa-plus" ] ] [])
                      ~btn_type:`Primary_outlined ();
                  ];
              ];
            hr ();
            div
              [
                label
                  ~a:[ a_class [ "block font-medium" ] ]
                  [ txt "Network Interfaces" ];
                div
                  (List.map
                     (fun (name, (policy : Vmm_core.Policy.t)) ->
                       div
                         ~a:
                           [
                             Unsafe.string_attrib "x-show"
                               (Fmt.str "selectedInstance === '%s'" name);
                           ]
                         [
                           Utils.dynamic_dropdown_form
                             (Vmm_core.String_set.elements policy.bridges)
                             ~get_label:(fun b -> b)
                             ~get_value:(fun b -> b)
                             ~id:("network-" ^ name);
                         ])
                     policies);
              ];
            hr ();
            div
              [
                label
                  ~a:[ a_class [ "block font-medium" ] ]
                  [ txt "Block devices" ];
                div
                  (List.map
                     (fun (instance_name, blocks_list) ->
                       div
                         ~a:
                           [
                             Unsafe.string_attrib "x-show"
                               (Fmt.str "selectedInstance === '%s'"
                                  instance_name);
                           ]
                         [
                           Utils.dynamic_dropdown_form blocks_list
                             ~get_label:(fun (name, size, used) ->
                               Vmm_core.Name.to_string name
                               ^ " - " ^ Int.to_string size ^ "MB (used: "
                               ^ Bool.to_string used ^ ")")
                             ~get_value:(fun (name, _, _) ->
                               Vmm_core.Name.to_string name)
                             ~id:("block-" ^ instance_name);
                         ])
                     blocks);
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
              ~a:[ a_class [ "" ] ]
              [
                label
                  ~a:[ a_class [ "block font-medium" ] ]
                  [ txt "Force create" ];
                div
                  ~a:[ a_class [ "" ] ]
                  [
                    Utils.switch_button ~initial_state:true
                      ~switch_id:"force-create"
                      ~switch_label:"Force create this unikernel"
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
                                 "If a unikernel exist with this same name, it \
                                  will be destroyed first and this one \
                                  created.";
                             ];
                         ]);
                  ];
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
