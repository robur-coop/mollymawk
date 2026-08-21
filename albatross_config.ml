let delimit_string str =
  if String.length str <= 20 then str else String.sub str 0 20 ^ "..."

let albatross_table_row ((_, albatross) : Vmm_core.Name.Label.t * Albatross.t) =
  let configuration = albatross.configuration in
  let ip = Ipaddr.to_string configuration.server_ip in
  let port = string_of_int configuration.server_port in
  let certificate = X509.Certificate.encode_pem configuration.certificate in
  let private_key = X509.Private_key.encode_pem configuration.private_key in
  let last_update = Utils.TimeHelper.string_of_ptime configuration.updated_at in
  let name = Configuration.name_to_str configuration.name in
  let error_buttons =
    Tyxml_html.(
      div
        [
          p
            ~a:[ a_class [ "text-secondary-500 font-semibold" ] ]
            [ txt (Albatross.Status.to_string albatross.status) ];
          div
            ~a:[ a_class [ "my-2 space-x-1 flex" ] ]
            [
              a
                ~a:
                  [
                    a_href ("/admin/albatross/errors?instance=" ^ name);
                    a_class
                      [
                        "bg-secondary-500 text-gray-100 font-semibold py-2 \
                         px-2 rounded-md hover:bg-secondary-800";
                      ];
                  ]
                [ txt "View Errors" ];
              Utils.button_component
                ~attribs:
                  [
                    a_id ("retry-btn-" ^ name);
                    a_onclick ("retryConnectingAlbatross(`" ^ name ^ "`)");
                  ]
                ~content:(txt "Retry connecting") ~btn_type:`Primary_full ();
            ];
        ])
  in
  Tyxml_html.(
    tr
      [
        td
          ~a:
            [
              a_class
                [
                  "px-6 py-4 whitespace-nowrap text-sm font-medium \
                   text-gray-800";
                ];
            ]
          [
            txt name;
            (match albatross.status with
            | Online ->
                p
                  ~a:[ a_class [ "text-primary-500 font-semibold" ] ]
                  [ txt (Albatross.Status.to_string albatross.status) ]
            | Degraded _ -> error_buttons);
          ];
        td
          ~a:
            [
              a_class
                [
                  "px-6 py-4 whitespace-nowrap text-sm font-medium \
                   text-gray-800";
                ];
            ]
          [
            txt (delimit_string certificate);
            p [ txt (delimit_string private_key) ];
          ];
        td
          ~a:
            [
              a_class
                [
                  "px-6 py-4 whitespace-nowrap text-sm font-medium \
                   text-gray-800";
                ];
            ]
          [ txt (ip ^ ":" ^ port) ];
        td
          ~a:
            [
              a_class
                [
                  "px-6 py-4 whitespace-nowrap text-sm font-medium \
                   text-gray-800";
                ];
            ]
          [ txt last_update ];
        td
          ~a:
            [
              a_class
                [
                  "px-6 py-4 whitespace-nowrap text-sm font-medium \
                   text-gray-800 flex space-x-4";
                ];
            ]
          [
            Utils.button_component
              ~attribs:
                [
                  a_onclick
                    ("openConfigForm(`" ^ name ^ "`,`" ^ ip ^ "`,`" ^ port
                   ^ "`,`" ^ certificate ^ "`,`" ^ private_key ^ "`)");
                ]
              ~content:(i ~a:[ a_class [ "fa-solid fa-pen" ] ] [])
              ~btn_type:`Primary_outlined ();
            Utils.button_component
              ~attribs:
                [
                  a_id
                    ("delete-config-btn-"
                    ^ Configuration.name_to_str configuration.name);
                  a_onclick
                    ("deleteAlbatrossConfig(`"
                    ^ Configuration.name_to_str configuration.name
                    ^ "`)");
                ]
              ~content:(i ~a:[ a_class [ "fa-solid fa-trash" ] ] [])
              ~btn_type:`Danger_full ();
          ];
      ])

let albatross_config_layout albatross_instances =
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        section
          ~a:
            [
              a_id "config-form";
              a_class [ "my-5 hidden" ];
              a_aria "labelledby" [ "modal-title" ];
              a_role [ "dialog" ];
              a_aria "modal" [ "true" ];
            ]
          [
            p ~a:[ a_id "form-alert"; a_class [ "my-4 hidden" ] ] [];
            input ~a:[ a_input_type `Hidden; a_id "config-name-edit" ] ();
            div
              ~a:[ a_class [ "bg-gray-50 px-4 pb-4 pt-5 sm:p-6 sm:pb-4 my-4" ] ]
              [
                div
                  [
                    label
                      ~a:
                        [
                          a_class [ "block text-sm font-medium" ];
                          a_label_for "name";
                        ]
                      [ txt "Albatross Instance Name*" ];
                    input
                      ~a:
                        [
                          a_autocomplete `Off;
                          a_input_type `Text;
                          a_name "albatross-name";
                          a_id "albatross-name";
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
              ];
            div
              ~a:[ a_class [ "bg-gray-50 px-4 pb-4 pt-5 sm:p-6 sm:pb-4 my-4" ] ]
              [
                div
                  [
                    label
                      ~a:
                        [
                          a_class [ "block text-sm font-medium" ];
                          a_label_for "ip";
                        ]
                      [ txt "Server IP*" ];
                    input
                      ~a:
                        [
                          a_autocomplete `Off;
                          a_input_type `Text;
                          a_name "server-ip";
                          a_id "server-ip";
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
              ];
            div
              ~a:[ a_class [ "bg-gray-50 px-4 pb-4 pt-5 sm:p-6 sm:pb-4 my-4" ] ]
              [
                div
                  [
                    label
                      ~a:
                        [
                          a_class [ "block text-sm font-medium" ];
                          a_label_for "port";
                        ]
                      [ txt "Server Port*" ];
                    input
                      ~a:
                        [
                          a_autocomplete `Off;
                          a_input_type `Number;
                          a_name "server-port";
                          a_id "server-port";
                          a_maxlength 4;
                          a_minlength 4;
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
              ];
            div
              ~a:[ a_class [ "bg-gray-50 px-4 pb-4 pt-5 sm:p-6 sm:pb-4 my-4" ] ]
              [
                div
                  [
                    label
                      ~a:
                        [
                          a_class [ "block text-sm font-medium" ];
                          a_label_for "certificate";
                        ]
                      [ txt "Certificate*" ];
                    textarea
                      ~a:
                        [
                          a_rows 7;
                          a_name "certificate";
                          a_id "certificate";
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
                      (txt "");
                  ];
              ];
            div
              ~a:[ a_class [ "bg-gray-50 px-4 pb-4 pt-5 sm:p-6 sm:pb-4 my-4" ] ]
              [
                div
                  [
                    label
                      ~a:
                        [
                          a_class [ "block text-sm font-medium" ];
                          a_label_for "private-key";
                        ]
                      [ txt "Private Key*" ];
                    textarea
                      ~a:
                        [
                          a_rows 4;
                          a_name "private-key";
                          a_id "private-key";
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
                      (txt "");
                  ];
                div
                  ~a:[ a_class [ "mx-auto my-4 flex justify-center" ] ]
                  [
                    Utils.button_component
                      ~attribs:
                        [
                          a_onclick "saveAlbatrossConfig()";
                          a_id "config-button";
                        ]
                      ~content:(txt "") ~btn_type:`Primary_full ();
                  ];
              ];
          ];
        div
          ~a:[ a_id "config-body"; a_class [ "flex flex-col block" ] ]
          [
            div
              ~a:[ a_id "add-config" ]
              [
                Utils.button_component
                  ~attribs:[ a_onclick "openConfigForm('','','','','')" ]
                  ~content:
                    (span
                       [
                         i ~a:[ a_class [ "fa-solid fa-plus mr-2" ] ] [];
                         txt "Add Albatross Instance";
                       ])
                  ~btn_type:`Primary_outlined ();
              ];
            div
              ~a:[ a_class [ "-m-1.5 overflow-x-auto" ] ]
              [
                div
                  ~a:
                    [ a_class [ "p-1.5 min-w-full inline-block align-middle" ] ]
                  [
                    div
                      ~a:[ a_class [ "overflow-hidden" ] ]
                      [
                        table
                          ~a:
                            [
                              a_id "config-table";
                              a_class
                                [
                                  "table-auto min-w-full divide-y \
                                   divide-gray-200";
                                ];
                            ]
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
                                               "px-6 py-3 text-start text-xs \
                                                font-bold text-primary-600 \
                                                uppercase";
                                             ];
                                         ]
                                       [ txt "Name" ];
                                     th
                                       ~a:
                                         [
                                           a_class
                                             [
                                               "px-6 py-3 text-start text-xs \
                                                font-bold text-primary-600 \
                                                uppercase";
                                             ];
                                         ]
                                       [ txt "Certificate/Key" ];
                                     th
                                       ~a:
                                         [
                                           a_class
                                             [
                                               "px-6 py-3 text-start text-xs \
                                                font-bold text-primary-600 \
                                                uppercase";
                                             ];
                                         ]
                                       [ txt "IP" ];
                                     th
                                       ~a:
                                         [
                                           a_class
                                             [
                                               "px-6 py-3 text-start text-xs \
                                                font-bold text-primary-600 \
                                                uppercase";
                                             ];
                                         ]
                                       [ txt "Last update" ];
                                     th
                                       ~a:
                                         [
                                           a_class
                                             [
                                               "px-6 py-3 text-start text-xs \
                                                font-bold text-primary-600 \
                                                uppercase";
                                             ];
                                         ]
                                       [ txt "Action" ];
                                   ];
                               ])
                          (List.map albatross_table_row
                             (Albatross.Albatross_map.bindings
                                albatross_instances));
                      ];
                  ];
              ];
          ];
      ])
