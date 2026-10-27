let cpu_multiselect cpu_usage_count =
  let sorted_cpus =
    cpu_usage_count |> List.sort (fun (_, c1) (_, c2) -> Int.compare c1 c2)
  in
  let default_cpu =
    match sorted_cpus with (id, _) :: _ -> string_of_int id | [] -> ""
  in
  let options =
    sorted_cpus
    |> List.map (fun (id, cnt) ->
        Printf.sprintf "{id:%d, txt:'CPU %d (%d)'}" id id cnt)
    |> String.concat "," |> Printf.sprintf "[%s]"
  in

  Tyxml_html.(
    div
      ~a:[ a_class [ "my-3" ] ]
      [
        label ~a:[ a_class [ "block font-medium" ] ] [ txt "CPU Ids" ];
        div
          ~a:
            [
              a_class [ "relative" ];
              Unsafe.string_attrib "x-on:click.outside" "open = false";
              Unsafe.string_attrib "x-data"
                (Printf.sprintf
                   "{ open: false, sel: [%s], opts: %s, toggle(id) { \
                    this.sel.includes(id) ? this.sel = this.sel.filter(x => \
                    x!==id) : this.sel.push(id) } }"
                   default_cpu options);
            ]
          [
            div
              ~a:
                [
                  a_class
                    [
                      Ui.input_classes;
                      "min-h-[3rem] h-auto flex flex-wrap gap-1 cursor-pointer";
                    ];
                  Unsafe.string_attrib "x-on:click" "open = !open";
                ]
              [
                span
                  ~a:
                    [
                      a_class [ "text-gray-500" ];
                      Unsafe.string_attrib "x-show" "!sel.length";
                    ]
                  [ txt "Select CPUs..." ];
                template
                  ~a:[ Unsafe.string_attrib "x-for" "id in sel" ]
                  [
                    span
                      ~a:
                        [
                          a_class
                            [
                              "bg-primary-100 text-primary-700 px-2 rounded \
                               text-sm flex items-center";
                            ];
                        ]
                      [
                        span ~a:[ Unsafe.string_attrib "x-text" "'CPU '+id" ] [];
                        Ui.button_component
                          ~attribs:
                            [
                              Unsafe.string_attrib "x-on:click.stop"
                                "toggle(id)";
                            ]
                          ~btn_type:
                            (`Custom "ml-1 text-primary-500 hover:text-red-500")
                          ~content:(txt "x") ();
                      ];
                  ];
              ];
            div
              ~a:
                [
                  Unsafe.string_attrib "x-show" "open";
                  a_style
                    "display: none; background-color: white; position: \
                     absolute; z-index: 50; width: 100%;";
                  a_class
                    [
                      "w-full border rounded shadow-lg max-h-48 overflow-auto \
                       mt-1";
                    ];
                ]
              [
                template
                  ~a:
                    [
                      Unsafe.string_attrib "x-for" "o in opts";
                      Unsafe.string_attrib ":key" "o.id";
                    ]
                  [
                    div
                      ~a:
                        [
                          a_class
                            [
                              "p-2 hover:bg-gray-100 cursor-pointer flex \
                               items-center";
                            ];
                          Unsafe.string_attrib "x-on:click" "toggle(o.id)";
                        ]
                      [
                        Ui.checkbox_input
                          ~attribs:
                            [
                              Unsafe.string_attrib ":checked"
                                "sel.includes(o.id)";
                              a_style "pointer-events: none";
                            ]
                          ();
                        span ~a:[ Unsafe.string_attrib "x-text" "o.txt" ] [];
                      ];
                  ];
              ];
            Ui.hidden_input
              ~attribs:
                [
                  a_id "cpuids";
                  a_name "cpuids";
                  Unsafe.string_attrib ":value" "sel.join(',') ";
                ]
              ();
          ];
      ])

let type_and_name_section () =
  let open Tyxml_html in
  div
    ~a:[ a_class [ "grid grid-cols-2 gap-3" ] ]
    [
      Ui.form_input ~label_text:"Name*"
        ~description:
          "Must be lowercase alphanumeric, dashes are allowed (spaces are not)."
        ~attribs:[ a_required () ]
        "unikernel-name";
      Ui.form_select ~label_text:"Type"
        ~description:"The type of unikernel to create (e.g. solo5 or bhyve)."
        ~attribs:
          [
            Unsafe.string_attrib "x-model" "typ";
            Unsafe.string_attrib ":disabled" "deploy_mode === 'builder'";
          ]
        "unikernel-type"
        [
          option ~a:[ a_value "solo5" ] (txt "Solo5");
          option ~a:[ a_value "bhyve" ] (txt "BHyve");
        ];
    ]

let builder_source_section builder_jobs available_networks free_space
    free_blocks_count =
  let open Tyxml_html in
  div
    ~a:
      [
        a_id "builder-options";
        a_class [ "space-y-4 pt-4" ];
        Unsafe.string_attrib "x-show" "deploy_mode === 'builder'";
      ]
    [
      div
        ~a:
          [
            a_id "builder-web-container";
            a_class [ "mt-4 p-4 border rounded bg-white" ];
          ]
        [
          Ui.form_select ~label_text:"Select a reproducible build"
            ~attribs:
              [
                Unsafe.string_attrib "@change"
                  "let job = window.builderJobs[$event.target.value]; if(job) \
                   { selectedSynopsis = \
                   job.synopsis;document.getElementById('unikernel-name').value \
                   = $event.target.value;let d = { network: job.network, \
                   block: job.block, job_name: $event.target.value }; \
                   window.dispatchEvent(new CustomEvent('populate-manifest', \
                   {detail: d})); } else { selectedSynopsis = ''; \
                   window.dispatchEvent(new CustomEvent('populate-manifest', \
                   {detail: {}})); }";
              ]
            "builder-job-select"
            (option ~a:[ a_value "" ] (txt "Select a build")
            :: List.map
                 (fun (j : Builder_web.job) ->
                   let req_nets =
                     List.fold_left
                       (fun acc (d : Builder_web.device) ->
                         match d with Network _ -> acc + 1 | _ -> acc)
                       0 j.manifest.devices
                   in
                   let req_blocks =
                     List.fold_left
                       (fun acc (d : Builder_web.device) ->
                         match d with Block _ -> acc + 1 | _ -> acc)
                       0 j.manifest.devices
                   in
                   let net_disabled = req_nets > available_networks in
                   let block_disabled =
                     req_blocks > 0 && free_space <= 0 && free_blocks_count = 0
                   in
                   let disabled_attr =
                     if net_disabled then
                       [
                         a_disabled ();
                         a_title
                           "Requires more network interfaces than available";
                       ]
                     else if block_disabled then
                       [
                         a_disabled ();
                         a_title
                           "Requires a block device, but you have no free \
                            block devices or quota";
                       ]
                     else []
                   in
                   option
                     ~a:(a_value j.name :: disabled_attr)
                     (txt
                        (Fmt.str "%s (%s.%d)" j.name j.abi_target j.abi_version)))
                 builder_jobs);
        ];
      div
        ~a:
          [
            Unsafe.string_attrib "x-show" "selectedSynopsis !== ''";
            a_class [ "mt-2 p-3" ];
          ]
        [ p ~a:[ Unsafe.string_attrib "x-text" "selectedSynopsis" ] [] ];
    ]

let manual_source_section () =
  let open Tyxml_html in
  div
    ~a:
      [
        a_id "manual-upload-options";
        a_class [ "space-y-4 pt-4" ];
        Unsafe.string_attrib "x-show" "deploy_mode === 'manual'";
      ]
    [
      Ui.form_input ~label_text:"Unikernel Image Binary*"
        ~attribs:
          [
            a_input_type `File;
            Unsafe.string_attrib ":required" "deploy_mode === 'manual'";
          ]
        "unikernel-binary";
    ]

let solo5_config_section () =
  let open Tyxml_html in
  div
    ~a:
      [
        Unsafe.string_attrib "x-show" "typ === 'solo5'";
        a_class [ "pt-4 space-y-4" ];
      ]
    [
      label ~a:[ a_class [ "block font-medium" ] ] [ txt "Arguments" ];
      p
        ~a:[ a_class [ "text-sm text-gray-500 mb-1" ] ]
        [
          txt "Write arguments separated by a whitespace e.g ";
          code [ txt "--ip=127.0.0.1 --port=8180" ];
        ];
      Ui.raw_textarea
        ~attribs:[ a_rows 2; a_name "arguments"; a_id "unikernel-arguments" ]
        "";
    ]

let advanced_config_section memory_left cpu_usage_count =
  let open Tyxml_html in
  div
    [
      div
        ~a:
          [
            a_class
              [
                "mt-4 bg-white border border-gray-200 rounded-lg shadow-sm p-4 \
                 text-center mx-auto";
              ];
          ]
        [
          h3
            ~a:[ a_class [ "text-lg font-bold text-gray-800" ] ]
            [ txt "Advanced Configuration" ];
          p
            ~a:
              [ a_class [ "text-sm text-gray-500 mt-2 mb-4 mx-auto max-w-xl" ] ]
            [
              txt
                "Configure network or block devices, customize hardware \
                 allocation, or set failure recovery strategies.";
            ];
          Ui.button_component
            ~attribs:
              [
                a_button_type `Button;
                Unsafe.string_attrib "x-on:click" "advanced = !advanced";
              ]
            ~btn_type:
              (`Custom
                 "inline-flex items-center px-4 py-2 border border-primary-300 \
                  shadow-sm text-sm font-medium rounded-md text-primary-700 \
                  bg-primary-50 hover:bg-primary-100 focus:outline-none \
                  focus:ring-2 focus:ring-offset-2 focus:ring-primary-500")
            ~content:
              (span
                 ~a:
                   [
                     Unsafe.string_attrib "x-text"
                       "advanced ? 'Hide Advanced Settings' : 'Show Advanced \
                        Settings'";
                   ]
                 [])
            ();
        ];
      div
        ~a:
          [
            a_class
              [
                "space-y-6 mt-6 p-6 border border-gray-200 rounded-lg bg-white";
              ];
            Unsafe.string_attrib "x-show" "advanced";
            Unsafe.string_attrib "x-transition" "";
            Unsafe.string_attrib "x-cloak" "";
          ]
        [
          div
            ~a:[ a_class [ "grid grid-cols-3 gap-3" ] ]
            [
              cpu_multiselect cpu_usage_count;
              Ui.increment_or_decrement_ui ~id:"unikernel-memory"
                ~max_value:memory_left ~min_value:0 ~default_value:32
                ~figure_unit:"MB" ~step:32 ~label':"Memory" ();
              div
                [
                  Ui.increment_or_decrement_ui ~id:"startup-priority"
                    ~max_value:100 ~min_value:0 ~default_value:50
                    ~label':"Startup Priority" ();
                  small
                    [
                      txt
                        "The order to start unikernels. Smallest number is \
                         started first.";
                    ];
                ];
            ];
          div
            ~a:
              [
                a_id "bhyve-options";
                a_class [ "grid grid-cols-2 gap-3" ];
                Unsafe.string_attrib "x-show" "typ === 'bhyve'";
              ]
            [
              Ui.increment_or_decrement_ui ~id:"numcpus" ~max_value:16
                ~min_value:1 ~default_value:1 ~label':"vCPUs" ();
              Ui.form_input ~label_text:"Linux Boot Partition"
                ~attribs:[ a_placeholder "Optional (e.g. /dev/sda1)" ]
                "linux-boot-partition";
            ];
          hr ();
          div
            ~a:[ a_class [ "" ] ]
            [
              label
                ~a:[ a_class [ "block font-medium" ] ]
                [ txt "Fail Behaviour" ];
              div
                ~a:[ a_class [ "" ] ]
                [
                  Ui.switch_button ~switch_id:"restart-on-fail"
                    ~switch_label:"Restart" ~initial_state:true
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
                  Ui.switch_button ~initial_state:false
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
                               "If a unikernel exists with this same name, it \
                                will be destroyed first and this one created.";
                           ];
                       ]);
                ];
            ];
        ];
    ]

let deploy_mode_toggles () =
  let open Tyxml_html in
  div
    ~a:[ a_class [ "grid grid-cols-2 gap-4 mb-6" ] ]
    [
      label
        ~a:
          [
            a_class
              [
                "flex flex-col items-center justify-center p-6 border-2 \
                 rounded-lg cursor-pointer transition-colors duration-200";
              ];
            Unsafe.string_attrib ":class"
              "deploy_mode === 'builder' ? 'border-primary-500 bg-primary-100 \
               text-primary-700' : 'border-gray-200 hover:bg-gray-50 bg-white'";
          ]
        [
          Ui.radio_input
            ~attribs:
              [
                a_name "deploy_mode";
                a_value "builder";
                Unsafe.string_attrib "x-model" "deploy_mode";
              ]
            ();
          span
            ~a:[ a_class [ "font-bold text-lg mb-2" ] ]
            [ txt "Robur Builder" ];
          span
            ~a:[ a_class [ "text-sm text-center opacity-80" ] ]
            [ txt "Deploy directly from builds.robur.coop" ];
        ];
      label
        ~a:
          [
            a_class
              [
                "flex flex-col items-center justify-center p-6 border-2 \
                 rounded-lg cursor-pointer transition-colors duration-200";
              ];
            Unsafe.string_attrib ":class"
              "deploy_mode === 'manual' ? 'border-primary-500 bg-primary-100 \
               text-primary-700' : 'border-gray-200 hover:bg-gray-50 bg-white'";
          ]
        [
          Ui.radio_input
            ~attribs:
              [
                a_name "deploy_mode";
                a_value "manual";
                Unsafe.string_attrib "x-model" "deploy_mode";
              ]
            ();
          span
            ~a:[ a_class [ "font-bold text-lg mb-2" ] ]
            [ txt "Manual Upload" ];
          span
            ~a:[ a_class [ "text-sm text-center opacity-80" ] ]
            [ txt "Upload a local unikernel binary image." ];
        ];
    ]

let unikernel_create_layout ~(user_policy : Vmm_core.Policy.t) unikernels
    (blocks : (Vmm_core.Name.t * int * bool) list) albatross_instance
    (builder_jobs : Builder_web.job list) =
  let total_memory_used = Utils.total_memory_used unikernels in
  let cpu_usage_count = Utils.cpu_usage_count user_policy unikernels in
  let memory_left = user_policy.memory - total_memory_used in
  let free_space =
    Utils.total_free_space user_policy (Utils.total_block_used blocks)
  in
  let free_blocks_count =
    List.fold_left
      (fun acc (_, _, used) -> if not used then acc + 1 else acc)
      0 blocks
  in
  let available_networks = Vmm_core.String_set.cardinal user_policy.bridges in
  let builder_jobs_json = Builder_web.jobs_to_json_string builder_jobs in
  let alpine_state =
    "{ advanced: false, deploy_mode: 'builder', typ: 'solo5', \
     selectedSynopsis: '' }"
  in
  Tyxml_html.(
    div
      [
        script
          (Unsafe.data
             (Printf.sprintf "window.builderJobs = %s;" builder_jobs_json));
        section
          ~a:
            [
              a_id "unikernel-create-form";
              a_class [ "col-span-7 p-4 bg-gray-50 my-1" ];
              Unsafe.string_attrib "x-data" alpine_state;
              Unsafe.string_attrib "x-init"
                "$watch('deploy_mode', val => { if(val === 'builder') typ = \
                 'solo5'; })";
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
                deploy_mode_toggles ();
                type_and_name_section ();
                builder_source_section builder_jobs available_networks
                  free_space free_blocks_count;
                manual_source_section ();
                solo5_config_section ();
                hr ();
                div
                  [
                    label
                      ~a:[ a_class [ "block font-medium" ] ]
                      [ txt "Network Interfaces" ];
                    Ui.dynamic_dropdown_form
                      (Vmm_core.String_set.elements user_policy.bridges)
                      ~get_label:Fun.id ~get_value:Fun.id ~id:"network" ();
                  ];
                hr ();
                div
                  [
                    label
                      ~a:[ a_class [ "block font-medium" ] ]
                      [ txt "Block devices" ];
                    Ui.dynamic_dropdown_form blocks
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
                      ~id:"block" ~manual_entry:true ();
                  ];
                hr ();
                advanced_config_section memory_left cpu_usage_count;
                div
                  ~a:
                    [
                      a_class [ "pt-6 flex justify-items-center mx-auto w-64" ];
                    ]
                  [
                    Ui.button_component
                      ~attribs:
                        [
                          a_id "deploy-button";
                          a_onclick
                            ("deployUnikernel('"
                            ^ Models.Configuration.name_to_str
                                albatross_instance
                            ^ "')");
                        ]
                      ~content:(txt "Deploy") ~btn_type:`Primary_full ();
                  ];
              ];
          ];
      ])
