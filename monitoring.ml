open Tyxml_html

let check_monitoring_response_format str =
  let str = String.trim str in
  if String.length str > 4 && String.sub str 0 4 = "ok: " then
    Ok (String.sub str 4 (String.length str - 4))
  else
    Error
      (Fmt.str
         "Response from monitoring does not start with \"ok: \". Received: %s"
         str)

let split_key_value_sources str =
  String.split_on_char ',' str
  |> List.fold_left
       (fun acc s ->
         match acc with
         | Error err -> Error err
         | Ok lst -> (
             match String.split_on_char ':' (String.trim s) with
             | [ name; value ] ->
                 Ok ((String.trim name, String.trim value) :: lst)
             | _ -> Error "Invalid key-value source format"))
       (Ok [])
  |> function
  | Ok parsed_list -> Ok (List.rev parsed_list)
  | Error err -> Error err

let parse_monitoring_response str =
  match check_monitoring_response_format str with
  | Ok str -> (
      match split_key_value_sources str with
      | Ok lst -> Ok lst
      | Error err -> Error err)
  | Error err -> Error err

let log_levels = [ "debug"; "info"; "warning"; "error"; "app"; "quiet" ]

let render_log_picker ~source ~default_level ~value =
  div
    ~a:
      [
        a_class
          [
            "flex items-center justify-between gap-3 px-3 py-2 rounded \
             bg-gray-700 hover:bg-gray-600 transition-colors";
          ];
      ]
    [
      span
        ~a:
          [
            a_class [ "font-mono truncate flex-1 font-semibold" ];
            a_title source;
          ]
        [
          txt (if String.equal source "*" then "Default Log level" else source);
        ];
      select
        ~a:
          [
            Unsafe.string_attrib "x-model" value;
            Unsafe.string_attrib ":style"
              (Fmt.str
                 "`background-color: ${getColor(%s)}; color: \
                  ${getTextColor(%s)}`"
                 value value);
            a_class
              [
                "text-xs border border-gray-600 rounded px-2 py-1 \
                 focus:outline-none cursor-pointer shrink-0 rounded-xl";
              ];
          ]
        (List.map
           (fun l ->
             option
               ~a:
                 ([ a_value l ]
                 @ if l = default_level then [ a_selected () ] else [])
               (txt l))
           log_levels);
    ]

let render_metric_picker ~source ~value index =
  label
    ~a:
      [
        a_class
          [
            "flex items-center gap-2 cursor-pointer px-3 py-2 rounded \
             transition-colors";
          ];
        Unsafe.string_attrib ":class"
          (Fmt.str "%s ? 'bg-primary-900' : 'bg-gray-700 hover:bg-gray-600'"
             value);
      ]
    [
      input
        ~a:
          [
            a_input_type `Checkbox;
            a_id (Fmt.str "metric-%d" index);
            a_name (Fmt.str "metric-%s-%d" source index);
            a_class [ "accent-primary-500 shrink-0 cursor-pointer" ];
            Unsafe.string_attrib "x-model" value;
          ]
        ();
      span
        ~a:[ a_class [ "font-semibold font-mono px-2 truncate" ] ]
        [ txt source ];
      span
        ~a:
          [
            Unsafe.string_attrib "x-show" value;
            a_class [ "ml-auto font-mono text-primary-400 shrink-0" ];
          ]
        [ txt "enabled" ];
      span
        ~a:
          [
            Unsafe.string_attrib "x-show" (Fmt.str "!%s" value);
            a_class [ "ml-auto font-mono text-gray-500 shrink-0" ];
          ]
        [ txt "disabled" ];
    ]

let render_log_sources log_entries unikernel_name =
  let default_log_level =
    match List.assoc_opt "*" log_entries with Some l -> l | None -> "info"
  in
  let other_logs =
    List.filter (fun (s, _) -> not (String.equal s "*")) log_entries
  in
  let logs_dict =
    String.concat ", "
      (List.map
         (fun (s, l) ->
           Printf.sprintf "'%s': '%s'" (String.escaped s) (String.escaped l))
         other_logs)
  in
  let log_data =
    Printf.sprintf
      "{ advancedLogs: false, defaultLevel: '%s', logs: { %s }, \
       getColor(level) { const colors = {'info': '#166534', 'warning': \
       '#854d0e', 'error': '#7f1d1d'}; return colors[level] || '#27272a'; }, \
       getTextColor(level) { const colors = {'info': '#bbf7d0', 'warning': \
       '#fef08a', 'error': '#fecaca'}; return colors[level] || '#a1a1aa'; }, \
       getCommand() { let cmd = 'L*:' + this.defaultLevel; for (const [k, v] \
       of Object.entries(this.logs)) { cmd += ',' + k + ':' + v; }  return \
       cmd; } }"
      default_log_level logs_dict
  in
  match log_entries with
  | [] -> div [ txt "No log sources available for this unikernel." ]
  | _ ->
      div
        ~a:[ a_class [ "space-y-3" ]; Unsafe.string_attrib "x-data" log_data ]
        [
          div
            ~a:[ a_class [ "flex items-center gap-2 mb-2" ] ]
            [
              i
                ~a:
                  [
                    a_class
                      [ "fa-solid fa-list-check text-primary-400 text-sm px-2" ];
                  ]
                [];
              p
                ~a:[ a_class [ "text-xl font-semibold text-primary-400" ] ]
                [ txt "Log Sources" ];
              span
                ~a:
                  [
                    a_class
                      [
                        "ml-auto text-xs font-semibold px-2 py-0.5 rounded-full";
                      ];
                  ]
                [ txt (Fmt.str "%d sources" (List.length log_entries)) ];
            ];
          form
            ~a:
              [
                a_enctype "multipart/form-data";
                Unsafe.string_attrib "hx-post"
                  "/api/unikernel/monitoring/update";
                Unsafe.string_attrib "hx-swap" "innerHTML";
                Unsafe.string_attrib "hx-include" "#molly-csrf";
                Unsafe.string_attrib "hx-target" "#monitoring-container";
              ]
            [
              input
                ~a:
                  [
                    a_input_type `Hidden;
                    a_name "unikernel_name";
                    a_value unikernel_name;
                  ]
                ();
              input
                ~a:
                  [
                    a_input_type `Hidden;
                    a_name "command";
                    Unsafe.string_attrib ":value" "getCommand()";
                  ]
                ();
              div
                ~a:
                  [
                    a_class
                      [
                        "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-2 \
                         max-h-72 overflow-y-auto pr-1 scrollbar-thin";
                      ];
                  ]
                [
                  render_log_picker ~source:"*" ~default_level:default_log_level
                    ~value:"defaultLevel";
                ];
              (if other_logs <> [] then
                 div
                   [
                     div
                       ~a:[ a_class [ "mt-4 mb-2" ] ]
                       [
                         button
                           ~a:
                             [
                               a_button_type `Button;
                               a_class
                                 [
                                   "font-semibold text-primary-400 \
                                    hover:text-primary-300";
                                 ];
                               a_style
                                 "cursor: pointer; text-decoration: underline;";
                               Unsafe.string_attrib "@click"
                                 "advancedLogs = !advancedLogs";
                             ]
                           [
                             span
                               ~a:
                                 [
                                   Unsafe.string_attrib "x-text"
                                     "advancedLogs ? 'Less logging options' : \
                                      'More logging options'";
                                 ]
                               [];
                           ];
                       ];
                     div
                       ~a:
                         [
                           Unsafe.string_attrib "x-show" "advancedLogs";
                           a_class
                             [
                               "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 \
                                gap-2 max-h-72 overflow-y-auto pr-1 \
                                scrollbar-thin";
                             ];
                         ]
                       (List.map
                          (fun (s, l) ->
                            render_log_picker ~source:s ~default_level:l
                              ~value:(Printf.sprintf "logs['%s']" s))
                          other_logs);
                   ]
               else div []);
              div
                ~a:[ a_class [ "mt-4" ] ]
                [
                  Utils.button_component ~extra_css:"font-bold"
                    ~btn_type:`Primary_full ~attribs:[]
                    ~content:(txt "Update Logging") ();
                ];
            ];
        ]

let render_metric_sources metric_entries unikernel_name =
  let other_metrics =
    List.filter (fun (s, _) -> not (String.equal s "*")) metric_entries
  in
  let metrics_dict =
    String.concat ", "
      (List.map
         (fun (s, l) ->
           let is_en = if String.equal l "enabled" then "true" else "false" in
           Printf.sprintf "'%s': %s" (String.escaped s) is_en)
         other_metrics)
  in
  let metric_data =
    Printf.sprintf
      "{ metrics: { %s }, get allMetrics() { return \
       Object.values(this.metrics).every(v => v); }, set allMetrics(value) { \
       for (const k in this.metrics) { this.metrics[k] = value; } }, \
       getCommand() { const values = Object.values(this.metrics); if \
       (values.every(v => v)) { return 'M*:enable'; } if (values.every(v => \
       !v)) { return 'M*:disable'; } let cmd = 'M*:disable'; for (const [k, v] \
       of Object.entries(this.metrics)) { if (v) { cmd += ',' + k + ':enable'; \
       } } return cmd; } }"
      metrics_dict
  in
  match metric_entries with
  | [] -> div [ txt "No metric sources available for this unikernel." ]
  | _ ->
      div
        ~a:
          [ a_class [ "space-y-3" ]; Unsafe.string_attrib "x-data" metric_data ]
        [
          div
            ~a:[ a_class [ "flex items-center gap-2 mb-2" ] ]
            [
              i
                ~a:
                  [
                    a_class
                      [ "fa-solid fa-chart-line text-primary-400 text-sm px-2" ];
                  ]
                [];
              p
                ~a:[ a_class [ "text-xl font-semibold text-primary-400" ] ]
                [ txt "Metrics Sources" ];
              span
                ~a:
                  [
                    a_class
                      [
                        "ml-auto text-xs font-semibold px-2 py-0.5 rounded-full";
                      ];
                  ]
                [
                  txt
                    (Fmt.str "%d/%d enabled"
                       (List.length
                          (List.filter
                             (fun (_, s) -> String.equal s "enabled")
                             metric_entries))
                       (List.length metric_entries));
                ];
            ];
          form
            ~a:
              [
                a_enctype "multipart/form-data";
                Unsafe.string_attrib "hx-post"
                  "/api/unikernel/monitoring/update";
                Unsafe.string_attrib "hx-swap" "innerHTML";
                Unsafe.string_attrib "hx-include" "#molly-csrf";
                Unsafe.string_attrib "hx-target" "#monitoring-container";
              ]
            [
              input
                ~a:
                  [
                    a_input_type `Hidden;
                    a_name "unikernel_name";
                    a_value unikernel_name;
                  ]
                ();
              input
                ~a:
                  [
                    a_input_type `Hidden;
                    a_name "command";
                    Unsafe.string_attrib ":value" "getCommand()";
                  ]
                ();
              div
                ~a:
                  [
                    a_class
                      [
                        "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-2 \
                         max-h-72 overflow-y-auto pr-1 scrollbar-thin";
                      ];
                  ]
                (render_metric_picker ~source:"All metrics" ~value:"allMetrics"
                   (-1)
                :: List.mapi
                     (fun i (s, l) ->
                       render_metric_picker ~source:s
                         ~value:(Printf.sprintf "metrics['%s']" s)
                         i)
                     other_metrics);
              div
                ~a:[ a_class [ "mt-4" ] ]
                [
                  Utils.button_component ~extra_css:"font-bold"
                    ~btn_type:`Primary_full ~attribs:[]
                    ~content:(txt "Update Metrics") ();
                ];
            ];
        ]

let monitoring_status_html ~name ~logs ~metrics =
  let log_entries = parse_monitoring_response logs in
  let metric_entries = parse_monitoring_response metrics in
  div
    ~a:[ a_id "monitoring-container"; a_class [ "space-y-6 py-4" ] ]
    [
      (match (log_entries, metric_entries) with
      | Error err, _ | _, Error err ->
          div
            ~a:[ a_class [ "text-center py-8" ] ]
            [
              i
                ~a:
                  [
                    a_class
                      [
                        "fa-solid fa-circle-exclamation text-gray-500 text-2xl \
                         mb-2";
                      ];
                  ]
                [];
              p
                ~a:[ a_class [ "text-secondary-500 mt-2" ] ]
                [ txt (Fmt.str "Monitoring parse error: %s" err) ];
            ]
      | Ok log_entries, Ok metric_entries ->
          div
            [
              render_log_sources log_entries name;
              hr ();
              render_metric_sources metric_entries name;
            ]);
    ]
