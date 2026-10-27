let arg_modal ~unikernel_name ~instance_name
    ~(to_be_updated_unikernel : Builder_web.build)
    ~(currently_running_unikernel : Builder_web.build)
    (unikernel : Vmm_core.Name.t * Vmm_core.Unikernel.info) =
  let instance_name_str = Models.Configuration.name_to_str instance_name in
  Tyxml_html.(
    form
      ~a:
        [
          Unsafe.string_attrib "hx-post" "/api/unikernel/update";
          Unsafe.string_attrib "hx-ext" "form-json";
          Unsafe.string_attrib "hx-include" "#molly-csrf";
          Unsafe.string_attrib "hx-swap" "none";
          Unsafe.string_attrib "hx-on::config-request"
            (Fmt.str
               "const p = event.detail.parameters; p.job = '%s'; \
                p.to_be_updated_unikernel = '%s'; \
                p.currently_running_unikernel = '%s'; p.unikernel_name = '%s'; \
                p.albatross_instance = '%s'; p.http_liveliness_address = \
                document.getElementById('http-toggle').checked ? \
                (document.getElementById('http_address').value.trim() || null) \
                : null; p.dns_liveliness = \
                document.getElementById('dns-toggle').checked ? { dns_address: \
                document.getElementById('dns_address').value.trim() || null, \
                dns_name: document.getElementById('dns_name').value.trim() || \
                null } : null; if \
                (document.getElementById('arguments-toggle').checked) { \
                p.unikernel_arguments = JSON.parse(p.arguments); } delete \
                p.arguments; delete p['liveliness-toggle']; delete \
                p['http-toggle']; delete p['dns-toggle']; delete \
                p['arguments-toggle']; delete p.http_address; delete \
                p.dns_name; delete p.dns_address;"
               to_be_updated_unikernel.job to_be_updated_unikernel.uuid
               currently_running_unikernel.uuid unikernel_name instance_name_str);
          Unsafe.string_attrib "hx-on::after-request"
            (Fmt.str
               "window.parseXhrResponse(event.detail.xhr, \
                '/unikernel/info?unikernel=%s&instance=%s')"
               unikernel_name instance_name_str);
        ]
      [
        p ~a:[ a_id "unikernel-arguments-alert"; a_class [ "my-4 hidden" ] ] [];
        div
          ~a:[ a_class [ "my-4" ] ]
          [
            Ui.switch_button ~switch_id:"liveliness-toggle"
              ~switch_label:"Perform a liveliness check after updating"
              (div
                 ~a:[ a_class [ "px-4" ] ]
                 [
                   div
                     [
                       Ui.switch_button ~switch_id:"http-toggle"
                         ~switch_label:"HTTP check"
                         (Ui.form_input
                            ~label_text:"HTTP address for liveliness check"
                            ~attribs:[ a_placeholder "https://example.com" ]
                            "http_address");
                       hr ();
                       Ui.switch_button ~switch_id:"dns-toggle"
                         ~switch_label:"DNS check"
                         (div
                            [
                              Ui.form_input
                                ~label_text:"Domain name for liveliness check"
                                ~attribs:[ a_placeholder "e.g robur.coop" ]
                                "dns_name";
                              Ui.form_input
                                ~label_text:
                                  "Domain IP address for liveliness check"
                                ~attribs:[ a_placeholder "e.g 127.0.0.1" ]
                                "dns_address";
                            ]);
                     ];
                 ]);
            hr ();
            Ui.switch_button ~switch_id:"arguments-toggle"
              ~switch_label:"Update the configuration for this build"
              (div
                 [
                   small
                     ~a:[ a_class [ "my-1" ] ]
                     [
                       txt
                         "Use json syntax to provide arguments for the latest \
                          build";
                     ];
                   Ui.raw_textarea
                     ~attribs:
                       [
                         a_rows 15;
                         a_required ();
                         a_name "arguments";
                         a_id "unikernel-arguments";
                       ]
                     (Albatross_json.unikernel_info_to_pretty_string unikernel);
                 ]);
          ];
        hr ();
        div
          ~a:[ a_class [ "my-4" ] ]
          [
            Ui.button_component
              ~attribs:[ a_id "update-unikernel-button"; a_button_type `Submit ]
              ~content:(txt "Proceed to update") ~btn_type:`Primary_full ();
          ];
      ])

let build_table (build : Builder_web.build) =
  let kv_row k v = Tyxml_html.tr [ Ui.td_txt k; Ui.td_txt v ] in
  Ui.simple_table
    ~headers:[ Ui.static_th "Info" ]
    [
      kv_row "Name" build.job;
      kv_row "UUID" build.uuid;
      kv_row "Platform" build.platform;
      kv_row "Start time:" (Utils.TimeHelper.string_of_ptime build.start_time);
      kv_row "Finish time" (Utils.TimeHelper.string_of_ptime build.finish_time);
      kv_row "Has binary" (string_of_bool build.main_binary);
      kv_row "Binary size:"
        (match build.main_binary_size with
        | None -> "-"
        | Some size -> Utils.bytes_to_megabytes size);
    ]

let package_table (packages : Builder_web.package list) =
  Ui.simple_table
    ~headers:[ Ui.static_th "Packages" ]
    (List.map
       (fun (package : Builder_web.package) ->
         Tyxml_html.tr [ Ui.td_txt (package.name ^ " - " ^ package.version) ])
       packages)

let duniverse_table (packages : Builder_web.duniverse list) =
  Ui.simple_table
    ~headers:[ Ui.static_th "Packages" ]
    (List.map
       (fun (package : Builder_web.duniverse) ->
         Tyxml_html.tr [ Ui.td_txt (package.name ^ " - " ^ package.value_) ])
       packages)

let opam_version_table (packages : Builder_web.package_version_diff list) =
  Ui.simple_table
    ~headers:
      [
        Ui.static_th "Opam Package";
        Ui.static_th "Deployed Unikernel";
        Ui.static_th "Latest Build";
      ]
    (List.map
       (fun (package : Builder_web.package_version_diff) ->
         Tyxml_html.tr
           [
             Ui.td_txt package.name;
             Ui.td_txt package.version_left;
             Ui.td_txt package.version_right;
           ])
       packages)

let opam_diff_table (diffs : Builder_web.o_diff list) =
  Tyxml_html.(
    Ui.simple_table
      ~headers:[ Ui.static_th "Package" ]
      (List.map
         (fun (diff : Builder_web.o_diff) ->
           tr
             ~a:[ a_class [ "border" ] ]
             [
               Ui.td_cell
                 [
                   p
                     ~a:[ a_class [ "text-primary-500 font-bold text-md" ] ]
                     [ txt diff.package_version ];
                   p
                     [
                       txt
                         ("Effectively equal: "
                         ^ string_of_bool diff.effectively_equal);
                     ];
                   pre [ code ~a:[ a_class [ "code-diff" ] ] [ txt diff.diff ] ];
                 ];
             ])
         diffs))

let unikernel_update_layout ~unikernel_name ~instance_name unikernel
    current_time (build_comparison : Builder_web.compare) =
  let u_name, u_data = unikernel in
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-10 p-4 bg-gray-50 my-1" ] ]
      [
        p
          ~a:[ a_id "unikernel-update-form-alert"; a_class [ "my-4 hidden" ] ]
          [];
        div
          ~a:[ a_id "unikernel-container"; a_class [ "p-4 rounded-md" ] ]
          [
            div
              ~a:[ a_id "info-container" ]
              [
                div
                  ~a:[ a_class [ "flex justify-between" ] ]
                  [
                    div
                      [
                        div
                          ~a:[ a_class [ "flex space-x-2 items-end" ] ]
                          [
                            h2
                              ~a:
                                [
                                  a_id "unikernel-name";
                                  a_class [ "text-xl font-bold uppercase" ];
                                ]
                              [ txt (Vmm_core.Name.to_string u_name) ];
                            p
                              ~a:[ a_class [ "text-sm" ] ]
                              [
                                txt
                                  ("created "
                                  ^ Utils.TimeHelper.time_ago ~current_time
                                      ~check_time:
                                        u_data.Vmm_core.Unikernel.started);
                              ];
                          ];
                        p
                          ~a:[ a_class [ "text-sm" ] ]
                          [ txt (Ohex.encode u_data.digest) ];
                        p
                          ~a:[ a_class [ "text-sm" ] ]
                          [
                            txt
                              ("albatross: "
                              ^ Models.Configuration.name_to_str instance_name);
                          ];
                      ];
                    (if build_comparison.right.main_binary then
                       Modal_dialog.modal_dialog
                         ~modal_title:"Unikernel Configuration"
                         ~button_content:(txt "Update to Latest")
                         ~content:
                           (arg_modal ~unikernel_name ~instance_name
                              ~to_be_updated_unikernel:build_comparison.right
                              ~currently_running_unikernel:build_comparison.left
                              unikernel)
                         ()
                     else
                       p
                         ~a:[ a_class [ "text-secondary-500 font-semibold" ] ]
                         [ txt "Can't update. No binary in latest build." ]);
                  ];
                div
                  ~a:[ a_class [ "grid grid-cols-2 divide-x-2 gap-4" ] ]
                  [
                    div
                      ~a:
                        [
                          a_class
                            [
                              "rounded my-4 text-white p-4 divide-y border \
                               border-primary-700";
                            ];
                        ]
                      [
                        div
                          [
                            p
                              ~a:[ a_class [ "text-xl font-semibold mt-4" ] ]
                              [ txt "Deployed Unikernel" ];
                            build_table build_comparison.left;
                            hr ();
                            p
                              ~a:[ a_class [ "text-md font-semibold mt-4" ] ]
                              [ txt "Packages" ];
                            package_table
                              build_comparison.package_diff.left_packages;
                            hr ();
                            p
                              ~a:[ a_class [ "text-md font-semibold mt-4" ] ]
                              [ txt "Environment" ];
                            package_table
                              build_comparison.env_diff.left_packages;
                            hr ();
                            p
                              ~a:[ a_class [ "text-md font-semibold mt-4" ] ]
                              [ txt "Opam" ];
                            package_table
                              build_comparison.opam_diff.only_in_left;
                            hr ();
                            p
                              ~a:[ a_class [ "text-md font-semibold mt-4" ] ]
                              [ txt "Duniverse" ];
                            duniverse_table
                              build_comparison.opam_diff.duniverse_diff.left;
                          ];
                      ];
                    div
                      ~a:
                        [
                          a_class
                            [
                              "rounded my-4 text-white p-4 divide-y border \
                               border-primary-700";
                            ];
                        ]
                      [
                        div
                          [
                            p
                              ~a:[ a_class [ "text-xl font-semibold mt-4" ] ]
                              [ txt "Latest Build" ];
                            build_table build_comparison.right;
                            hr ();
                            p
                              ~a:[ a_class [ "text-md font-semibold mt-4" ] ]
                              [ txt "Packages" ];
                            package_table
                              build_comparison.package_diff.right_packages;
                            hr ();
                            p
                              ~a:[ a_class [ "text-md font-semibold mt-4" ] ]
                              [ txt "Environment" ];
                            package_table
                              build_comparison.env_diff.right_packages;
                            hr ();
                            p
                              ~a:[ a_class [ "text-md font-semibold mt-4" ] ]
                              [ txt "Opam" ];
                            package_table
                              build_comparison.opam_diff.only_in_right;
                            hr ();
                            p
                              ~a:[ a_class [ "text-md font-semibold mt-4" ] ]
                              [ txt "Duniverse" ];
                            duniverse_table
                              build_comparison.opam_diff.duniverse_diff.right;
                          ];
                      ];
                  ];
                div
                  [
                    p
                      ~a:[ a_class [ "text-xl font-semibold mt-4" ] ]
                      [ txt "Opam Differences" ];
                    div
                      [
                        p
                          ~a:[ a_class [ "text-md font-semibold" ] ]
                          [ txt "Duniverse Differences" ];
                        div
                          ~a:[ a_class [ "flex gap-4" ] ]
                          (List.map
                             (fun (package : string) -> p [ txt package ])
                             build_comparison.opam_diff.duniverse_diff
                               .detailed_diff);
                      ];
                    p
                      ~a:[ a_class [ "text-md font-semibold mt-4" ] ]
                      [ txt "Version Differences" ];
                    opam_version_table build_comparison.opam_diff.version_diff;
                    p
                      ~a:[ a_class [ "text-md font-semibold mt-4" ] ]
                      [ txt "Opam Differences" ];
                    opam_diff_table build_comparison.opam_diff.opam_diff;
                  ];
              ];
          ];
      ])
