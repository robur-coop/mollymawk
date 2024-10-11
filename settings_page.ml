let settings_layout (configuration : Configuration.t) csrf =
  let ip = Ipaddr.to_string configuration.server_ip in
  let port = string_of_int configuration.server_port in
  let certificate = X509.Certificate.encode_pem configuration.certificate in
  let private_key = X509.Private_key.encode_pem configuration.private_key in
  let last_update = Utils.TimeHelper.string_of_ptime configuration.updated_at in
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        div
          ~a:[ a_class [ "px-3 flex justify-between items-center" ] ]
          [
            Utils.csrf_form_input csrf;
            div
              [
                p
                  ~a:[ a_class [ "font-bold text-gray-700" ] ]
                  [ txt "Configuration" ];
              ];
            div
              ~a:[ a_id "add-config" ]
              [
                button
                  ~a:
                    [
                      a_onclick "openConfigForm('','','','')";
                      a_class
                        [
                          "inline-flex items-center gap-x-2 text-sm \
                           font-semibold rounded-lg border border-1 py-1 px-2 \
                           border-primary-400 text-primary-600 \
                           hover:text-primary-500 focus:outline-none \
                           focus:text-primary-800 disabled:opacity-50 \
                           disabled:pointer-events-none";
                        ];
                    ]
                  [ i ~a:[ a_class [ "fa-solid fa-plus" ] ] [] ];
              ];
          ];
        hr ~a:[ a_class [ "border border-primary-500 my-5" ] ] ();
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
                    button
                      ~a:
                        [
                          a_onclick "saveConfig()";
                          a_id "config-button";
                          a_class
                            [
                              "py-3 px-3 rounded bg-primary-500 \
                               hover:bg-primary-800 text-gray-50 font-semibold";
                            ];
                        ]
                      [ txt "" ];
                  ];
              ];
          ];
        div
          ~a:[ a_id "config-body"; a_class [ "flex flex-col block" ] ]
          [
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
                                       [ txt "Certificate" ];
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
                                       [ txt "Private Key" ];
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
                                       [ txt "Port" ];
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
                          [
                            tr
                              [
                                td
                                  ~a:
                                    [
                                      a_class
                                        [
                                          "px-6 py-4 whitespace-nowrap text-sm \
                                           font-medium text-gray-800";
                                        ];
                                    ]
                                  [ txt (String.sub certificate 10 30 ^ "...") ];
                                td
                                  ~a:
                                    [
                                      a_class
                                        [
                                          "px-6 py-4 whitespace-nowrap text-sm \
                                           font-medium text-gray-800";
                                        ];
                                    ]
                                  [ txt (String.sub private_key 10 30 ^ "...") ];
                                td
                                  ~a:
                                    [
                                      a_class
                                        [
                                          "px-6 py-4 whitespace-nowrap text-sm \
                                           font-medium text-gray-800";
                                        ];
                                    ]
                                  [ txt ip ];
                                td
                                  ~a:
                                    [
                                      a_class
                                        [
                                          "px-6 py-4 whitespace-nowrap text-sm \
                                           font-medium text-gray-800";
                                        ];
                                    ]
                                  [ txt port ];
                                td
                                  ~a:
                                    [
                                      a_class
                                        [
                                          "px-6 py-4 whitespace-nowrap text-sm \
                                           font-medium text-gray-800";
                                        ];
                                    ]
                                  [ txt last_update ];
                                td
                                  ~a:
                                    [
                                      a_class
                                        [
                                          "px-6 py-4 whitespace-nowrap text-sm \
                                           font-medium text-gray-800";
                                        ];
                                    ]
                                  [
                                    button
                                      ~a:
                                        [
                                          a_onclick
                                            ("openConfigForm('" ^ ip ^ "','"
                                           ^ port ^ "','"
                                           ^ String.escaped certificate ^ "','"
                                           ^ String.escaped private_key ^ "')");
                                          a_class
                                            [
                                              "inline-flex items-center \
                                               gap-x-2 text-sm font-semibold \
                                               rounded-lg border border-1 py-1 \
                                               px-2 border-primary-400 \
                                               text-primary-600 \
                                               hover:text-primary-500 \
                                               focus:outline-none \
                                               focus:text-primary-800 \
                                               disabled:opacity-50 \
                                               disabled:pointer-events-none";
                                            ];
                                        ]
                                      [
                                        i
                                          ~a:[ a_class [ "fa-solid fa-pen" ] ]
                                          [];
                                      ];
                                  ];
                              ];
                          ];
                      ];
                  ];
              ];
          ];
      ])
