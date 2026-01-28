let unikernel_create_layout ~(user_policy : Vmm_core.Policy.t) unikernels
    (blocks : (Vmm_core.Name.t * int * bool) list) albatross_instance =
  let total_memory_used = Utils.total_memory_used unikernels in
  let cpu_usage_count = Utils.cpu_usage_count user_policy unikernels in
  let memory_left = user_policy.memory - total_memory_used in
  let input_classes =
    "ring-primary-100 mt-1.5 transition appearance-none block w-full px-3 py-3 \
     rounded-xl shadow-sm border hover:border-primary-200 \
     focus:border-primary-300 bg-primary-50 bg-opacity-0 hover:bg-opacity-50 \
     focus:bg-opacity-50 ring-primary-200 focus:ring-primary-200 \
     focus:ring-[1px] focus:outline-none"
  in
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
                          a_class [ input_classes ];
                        ]
                      ();
                  ];
                (* Type Selector *)
                div
                  [
                    label ~a:[ a_class [ "block font-medium" ] ] [ txt "Type" ];
                    select
                      ~a:
                        [
                          a_id "unikernel-type";
                          a_name "typ";
                          a_class [ input_classes ];
                          a_onchange "toggleType()";
                        ]
                      [
                        option
                          ~a:[ a_value "solo5"; a_selected () ]
                          (txt "Solo5");
                        option ~a:[ a_value "bhyve" ] (txt "BHyve");
                      ];
                  ];
                (* CPU Ids - Multi-select *)
                div
                  [
                    label
                      ~a:[ a_class [ "block font-medium" ] ]
                      [ txt "CPU Ids (Multi-select)" ];
                    select
                      ~a:
                        [
                          a_id "cpuids";
                          a_name "cpuids";
                          a_multiple ();
                          a_class [ input_classes ];
                          a_style "height: auto; min-height: 3rem;";
                        ]
                      (List.map
                         (fun (cpu_id, count) ->
                           option
                             ~a:[ a_value (string_of_int cpu_id) ]
                             (txt
                                ("CPU " ^ string_of_int cpu_id ^ " (used by "
                               ^ string_of_int count ^ " unikernels)")))
                         (List.sort
                            (fun (_, count1) (_, count2) ->
                              Int.compare count1 count2)
                            cpu_usage_count));
                  ];
                Utils.increment_or_decrement_ui ~id:"unikernel-memory"
                  ~max_value:memory_left ~min_value:0 ~default_value:32
                  ~figure_unit:"MB" ~step:32 ~label':"Memory" ();
                Utils.increment_or_decrement_ui ~id:"startup-priority"
                  ~max_value:100 ~min_value:0 ~default_value:50
                  ~label':"Startup Priority" ();
              ];
            (* BHyve Specific Options *)
            div
              ~a:
                [
                  a_id "bhyve-options";
                  a_class [ "hidden grid grid-cols-2 gap-3" ];
                ]
              [
                Utils.increment_or_decrement_ui ~id:"numcpus" ~max_value:16
                  ~min_value:1 ~default_value:1 ~label':"vCPUs (BHyve)" ();
                div
                  [
                    label
                      ~a:[ a_class [ "block font-medium" ] ]
                      [ txt "Linux Boot Partition" ];
                    input
                      ~a:
                        [
                          a_input_type `Text;
                          a_name "linux_boot_partition";
                          a_id "linux-boot-partition";
                          a_placeholder "Optional (e.g. /dev/sda1)";
                          a_class [ input_classes ];
                        ]
                      ();
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
                    Option.value ~default:""
                      (Option.map Vmm_core.Name.Label.to_string
                         (Vmm_core.Name.name name))
                    ^ " - " ^ Int.to_string size ^ "MB (used: "
                    ^ Bool.to_string used ^ ")")
                  ~get_value:(fun (name, _, _) ->
                    Option.value ~default:""
                      (Option.map Vmm_core.Name.Label.to_string
                         (Vmm_core.Name.name name)))
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
                      a_class [ input_classes ];
                    ]
                  (txt "");
              ];
            hr ();
            (* Image Binary (Solo5 only) *)
            div
              ~a:[ a_id "solo5-options" ]
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
                      a_class [ input_classes ];
                    ]
                  ();
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
                                 "This unikernel will automatically restart on \
                                  fail";
                             ];
                         ]);
                  ];
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
                    [
                      a_id "deploy-button";
                      a_onclick
                        ("deployUnikernel('"
                        ^ Configuration.name_to_str albatross_instance
                        ^ "')");
                    ]
                  ~content:(txt "Deploy") ~btn_type:`Primary_full ();
              ];
          ];
        script
          (txt
             "function toggleType() { \n\
             \  var type = document.getElementById('unikernel-type').value; \n\
             \  var bhyveOpts = document.getElementById('bhyve-options'); \n\
             \  var solo5Opts = document.getElementById('solo5-options'); \n\
             \  var binInput = document.getElementById('unikernel-binary'); \n\n\
             \  if (type === 'bhyve') { \n\
             \    bhyveOpts.classList.remove('hidden'); \n\
             \    solo5Opts.classList.add('hidden'); \n\
             \    binInput.removeAttribute('required'); \n\
             \  } else { \n\
             \    bhyveOpts.classList.add('hidden'); \n\
             \    solo5Opts.classList.remove('hidden'); \n\
             \    binInput.setAttribute('required', ''); \n\
             \  } \n\
              }");
      ])
