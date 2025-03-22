let arg_modal ~unikernel_name ~(to_be_updated_unikernel : Builder_web.build)
    ~(currently_running_unikernel : Builder_web.build)
    (unikernel : Vmm_core.Name.t * Vmm_core.Unikernel.info) =
  Tyxml_html.(
    section
      [
        p ~a:[ a_id "unikernel-arguments-alert"; a_class [ "my-4 hidden" ] ] [];
        div
          ~a:[ a_class [ "my-4" ] ]
          [
            Utils.switch_button ~switch_id:"liveliness-toggle"
              ~switch_label:"Perform a liveliness check after updating"
              (div
                 ~a:[ a_class [ "px-4" ] ]
                 [
                   div
                     [
                       Utils.switch_button ~switch_id:"http-toggle"
                         ~switch_label:"HTTP check"
                         (div
                            [
                              div
                                [
                                  label
                                    ~a:
                                      [
                                        a_class [ "block text-sm font-medium" ];
                                        a_label_for "http_address";
                                      ]
                                    [ txt "HTTP address for liveliness check" ];
                                  input
                                    ~a:
                                      [
                                        a_autocomplete `On;
                                        a_input_type `Text;
                                        a_name "http_address";
                                        a_id "http-address";
                                        a_class
                                          [
                                            "ring-primary-100 mt-1.5 \
                                             transition appearance-none block \
                                             w-full px-3 py-3 rounded-xl \
                                             shadow-sm border \
                                             hover:border-primary-200\n\
                                            \                                           \
                                             focus:border-primary-300 \
                                             bg-primary-50 bg-opacity-0 \
                                             hover:bg-opacity-50 \
                                             focus:bg-opacity-50 \
                                             ring-primary-200 \
                                             focus:ring-primary-200\n\
                                            \                                           \
                                             focus:ring-[1px] \
                                             focus:outline-none";
                                          ];
                                      ]
                                    ();
                                ];
                            ]);
                       hr ();
                       Utils.switch_button ~switch_id:"dns-toggle"
                         ~switch_label:"DNS check"
                         (div
                            [
                              div
                                [
                                  label
                                    ~a:
                                      [
                                        a_class [ "block text-sm font-medium" ];
                                        a_label_for "dns_name";
                                      ]
                                    [ txt "Domain name for liveliness check" ];
                                  input
                                    ~a:
                                      [
                                        a_autocomplete `On;
                                        a_input_type `Text;
                                        a_name "dns_name";
                                        a_id "dns-address";
                                        a_class
                                          [
                                            "ring-primary-100 mt-1.5 \
                                             transition appearance-none block \
                                             w-full px-3 py-3 rounded-xl \
                                             shadow-sm border \
                                             hover:border-primary-200\n\
                                            \                                           \
                                             focus:border-primary-300 \
                                             bg-primary-50 bg-opacity-0 \
                                             hover:bg-opacity-50 \
                                             focus:bg-opacity-50 \
                                             ring-primary-200 \
                                             focus:ring-primary-200\n\
                                            \                                           \
                                             focus:ring-[1px] \
                                             focus:outline-none";
                                          ];
                                      ]
                                    ();
                                ];
                            ]);
                       hr ();
                       Utils.switch_button ~switch_id:"tcp-toggle"
                         ~switch_label:"TCP check"
                         (div
                            [
                              div
                                [
                                  label
                                    ~a:
                                      [
                                        a_class [ "block text-sm font-medium" ];
                                        a_label_for "http_link";
                                      ]
                                    [ txt "TCP address for liveliness check" ];
                                  input
                                    ~a:
                                      [
                                        a_autocomplete `On;
                                        a_input_type `Text;
                                        a_name "tcp_address";
                                        a_id "tcp-address";
                                        a_class
                                          [
                                            "ring-primary-100 mt-1.5 \
                                             transition appearance-none block \
                                             w-full px-3 py-3 rounded-xl \
                                             shadow-sm border \
                                             hover:border-primary-200\n\
                                            \                                           \
                                             focus:border-primary-300 \
                                             bg-primary-50 bg-opacity-0 \
                                             hover:bg-opacity-50 \
                                             focus:bg-opacity-50 \
                                             ring-primary-200 \
                                             focus:ring-primary-200\n\
                                            \                                           \
                                             focus:ring-[1px] \
                                             focus:outline-none";
                                          ];
                                      ]
                                    ();
                                ];
                            ]);
                     ];
                 ]);
            hr ();
            Utils.switch_button ~switch_id:"arguments-toggle"
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
                   textarea
                     ~a:
                       [
                         a_rows 15;
                         a_required ();
                         a_name "arguments";
                         a_id "unikernel-arguments";
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
                     (txt
                        (Albatross_json.unikernel_info unikernel
                        |> Yojson.Basic.pretty_to_string));
                 ]);
          ];
        hr ();
        div
          ~a:[ a_class [ "my-4" ] ]
          [
            Utils.button_component
              ~attribs:
                [
                  a_id "update-unikernel-button";
                  a_onclick
                    ("updateUnikernel('" ^ to_be_updated_unikernel.job ^ "','"
                   ^ to_be_updated_unikernel.uuid ^ "','"
                   ^ currently_running_unikernel.uuid ^ "','" ^ unikernel_name
                   ^ "')");
                ]
              ~content:(txt "Proceed to update") ~btn_type:`Primary_full ();
          ];
      ])

let build_table (build : Builder_web.build) =
  Tyxml_html.(
    table
      ~a:[ a_class [ "table-auto min-w-full divide-y divide-gray-200" ] ]
      ~thead:
        (thead
           [
             tr
               [
                 th
                   ~a:
                     [
                       a_class
                         [
                           "px-6 py-2 text-start text-xs font-bold \
                            text-primary-600 uppercase";
                         ];
                     ]
                   [ txt "Info" ];
               ];
           ])
      [
        tr
          [
            td
              ~a:[ a_class [ "px-6 py-1 text-sm font-medium text-gray-800" ] ]
              [ txt "Name" ];
            td
              ~a:[ a_class [ "px-6 py-1 text-sm font-medium text-gray-800" ] ]
              [ txt build.job ];
          ];
        tr
          [
            td
              ~a:[ a_class [ "px-6 py-1 text-sm font-medium text-gray-800" ] ]
              [ txt "UUID" ];
            td
              ~a:[ a_class [ "px-6 py-1 text-sm font-medium text-gray-800" ] ]
              [ txt build.uuid ];
          ];
        tr
          [
            td
              ~a:[ a_class [ "px-6 py-1 text-sm font-medium text-gray-800" ] ]
              [ txt "Platform" ];
            td
              ~a:[ a_class [ "px-6 py-1 text-sm font-medium text-gray-800" ] ]
              [ txt build.platform ];
          ];
        tr
          [
            td
              ~a:[ a_class [ "px-6 py-1 text-sm font-medium text-gray-800" ] ]
              [ txt "Start time:" ];
            td
              ~a:[ a_class [ "px-6 py-1 text-sm font-medium text-gray-800" ] ]
              [ txt (Utils.TimeHelper.string_of_ptime build.start_time) ];
          ];
        tr
          [
            td
              ~a:[ a_class [ "px-6 py-1 text-sm font-medium text-gray-800" ] ]
              [ txt "Finish time" ];
            td
              ~a:[ a_class [ "px-6 py-1 text-sm font-medium text-gray-800" ] ]
              [ txt (Utils.TimeHelper.string_of_ptime build.finish_time) ];
          ];
        tr
          [
            td
              ~a:[ a_class [ "px-6 py-1 text-sm font-medium text-gray-800" ] ]
              [ txt "Has binary" ];
            td
              ~a:[ a_class [ "px-6 py-1 text-sm font-medium text-gray-800" ] ]
              [ txt (string_of_bool build.main_binary) ];
          ];
        tr
          [
            td
              ~a:[ a_class [ "px-6 py-1 text-sm font-medium text-gray-800" ] ]
              [ txt "Binary size:" ];
            (match build.main_binary_size with
            | None -> td [ txt "-" ]
            | Some size ->
                td
                  ~a:
                    [
                      a_class [ "px-6 py-1 text-sm font-medium text-gray-800" ];
                    ]
                  [ txt (Utils.bytes_to_megabytes size) ]);
          ];
      ])

let package_table (packages : Builder_web.package list) =
  Tyxml_html.(
    table
      ~a:[ a_class [ "table-auto min-w-full divide-y divide-gray-200" ] ]
      ~thead:
        (thead
           [
             tr
               [
                 th
                   ~a:
                     [
                       a_class
                         [
                           "px-6 py-2 text-start text-xs font-bold \
                            text-primary-600 uppercase";
                         ];
                     ]
                   [ txt "Packages" ];
               ];
           ])
      (List.map
         (fun (package : Builder_web.package) ->
           tr
             [
               td
                 ~a:
                   [ a_class [ "px-6 py-1 text-sm font-medium text-gray-800" ] ]
                 [ txt (package.name ^ " - " ^ package.version) ];
             ])
         packages))

let duniverse_table (packages : Builder_web.duniverse list) =
  Tyxml_html.(
    table
      ~a:[ a_class [ "table-auto min-w-full divide-y divide-gray-200" ] ]
      ~thead:
        (thead
           [
             tr
               [
                 th
                   ~a:
                     [
                       a_class
                         [
                           "px-6 py-2 text-start text-xs font-bold \
                            text-primary-600 uppercase";
                         ];
                     ]
                   [ txt "Packages" ];
               ];
           ])
      (List.map
         (fun (package : Builder_web.duniverse) ->
           tr
             [
               td
                 ~a:
                   [ a_class [ "px-6 py-1 text-sm font-medium text-gray-800" ] ]
                 [ txt (package.name ^ " - " ^ package.value_) ];
             ])
         packages))

let opam_version_table (packages : Builder_web.package_version_diff list) =
  Tyxml_html.(
    table
      ~a:[ a_class [ "table-auto min-w-full divide-y divide-gray-200" ] ]
      ~thead:
        (thead
           [
             tr
               [
                 th
                   ~a:
                     [
                       a_class
                         [
                           "px-6 py-2 text-start text-xs font-bold \
                            text-primary-600 uppercase";
                         ];
                     ]
                   [ txt "Opam Package" ];
                 th
                   ~a:
                     [
                       a_class
                         [
                           "px-6 py-2 text-start text-xs font-bold \
                            text-primary-600 uppercase";
                         ];
                     ]
                   [ txt "Current Build" ];
                 th
                   ~a:
                     [
                       a_class
                         [
                           "px-6 py-2 text-start text-xs font-bold \
                            text-primary-600 uppercase";
                         ];
                     ]
                   [ txt "Latest Build" ];
               ];
           ])
      (List.map
         (fun (package : Builder_web.package_version_diff) ->
           tr
             [
               td
                 ~a:
                   [ a_class [ "px-6 py-1 text-sm font-medium text-gray-800" ] ]
                 [ txt package.name ];
               td
                 ~a:
                   [
                     a_class
                       [ "px-6 py-1 text-sm font-semibold text-secondary-500" ];
                   ]
                 [ txt package.version_left ];
               td
                 ~a:
                   [
                     a_class
                       [ "px-6 py-1 text-sm font-semibold text-primary-500" ];
                   ]
                 [ txt package.version_right ];
             ])
         packages))

let opam_diff_table (diffs : Builder_web.o_diff list) =
  Tyxml_html.(
    table
      ~a:[ a_class [ "table-auto min-w-full divide-y divide-gray-200" ] ]
      ~thead:
        (thead
           [
             tr
               [
                 th
                   ~a:
                     [
                       a_class
                         [
                           "px-6 py-2 text-start text-xs font-bold \
                            text-primary-600 uppercase";
                         ];
                     ]
                   [ txt "Package" ];
               ];
           ])
      (List.map
         (fun (diff : Builder_web.o_diff) ->
           tr
             ~a:[ a_class [ "border" ] ]
             [
               td
                 ~a:
                   [
                     a_class [ "px-6 py-1 text-sm font-semibold text-gray-800" ];
                   ]
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

let unikernel_update_layout ~unikernel_name unikernel current_time
    (build_comparison : Builder_web.compare) =
  let u_name, data = unikernel in
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
                                        data.Vmm_core.Unikernel.started);
                              ];
                          ];
                        p
                          ~a:[ a_class [ "text-sm" ] ]
                          [ txt (Ohex.encode data.digest) ];
                      ];
                    (if build_comparison.right.main_binary then
                       Modal_dialog.modal_dialog
                         ~modal_title:"Unikernel Configuration"
                         ~button_content:(txt "Update to Latest")
                         ~content:
                           (arg_modal ~unikernel_name
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
                              [ txt "Current Build" ];
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
        (if build_comparison.right.main_binary then
           Modal_dialog.modal_dialog ~modal_title:"Unikernel Configuration"
             ~button_content:(txt "Update to Latest")
             ~content:
               (arg_modal ~unikernel_name
                  ~to_be_updated_unikernel:build_comparison.right
                  ~currently_running_unikernel:build_comparison.left unikernel)
             ()
         else
           p
             ~a:[ a_class [ "text-secondary-500 font-semibold" ] ]
             [ txt "Can't update. No binary in latest build." ]);
      ])
