let tokens_index_layout tokens current_time =
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        div
          ~a:[ a_class [ "px-3 flex justify-between items-center" ] ]
          [
            div
              [
                p
                  ~a:[ a_class [ "font-bold text-gray-700" ] ]
                  [
                    txt ("API Keys (" ^ string_of_int (List.length tokens) ^ ")");
                  ];
              ];
            div
              [
                Modal_dialog.modal_dialog ~modal_title:"Create API Key"
                  ~button_content:(txt "New API Key")
                  ~content:Tokens_ui.create_token ();
              ];
          ];
        hr ~a:[ a_class [ "border border-primary-500 my-5" ] ] ();
        div
          [
            input
              ~a:
                [
                  a_onkeyup "filterData()";
                  a_placeholder "search";
                  a_id "searchQuery";
                  a_name "searchQuery";
                  a_input_type `Text;
                  a_class
                    [
                      "rounded py-2 px-3 border border-primary-200 \
                       focus:border-primary-500 outline-0";
                    ];
                ]
              ();
          ];
        div
          ~a:[ a_class [ "flex flex-col" ] ]
          [
            div
              ~a:[ a_class [ "-m-1.5 overflow-x-auto" ] ]
              [
                div
                  ~a:
                    [ a_class [ "p-1.5 min-w-full inline-block align-middle" ] ]
                  [
                    div
                      ~a:
                        [
                          Unsafe.string_attrib "x-data" "sort_data()";
                          a_class [ "overflow-hidden" ];
                        ]
                      [
                        table
                          ~a:
                            [
                              a_id "data-table";
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
                                           Unsafe.string_attrib "x-on:click"
                                             "sortByColumn";
                                           a_class
                                             [
                                               "px-6 py-3 text-start text-xs \
                                                font-bold text-primary-600 \
                                                uppercase cursor-pointer \
                                                select-none";
                                             ];
                                         ]
                                       [
                                         txt "Name";
                                         span
                                           ~a:[ a_class [ "px-2" ] ]
                                           [
                                             i
                                               ~a:
                                                 [
                                                   a_class
                                                     [ "fa-solid fa-sort" ];
                                                 ]
                                               [];
                                           ];
                                       ];
                                     th
                                       ~a:
                                         [
                                           Unsafe.string_attrib "x-on:click"
                                             "sortByColumn";
                                           a_class
                                             [
                                               "px-6 py-3 text-start text-xs \
                                                font-bold text-primary-600 \
                                                uppercase cursor-pointer \
                                                select-none";
                                             ];
                                         ]
                                       [ txt "Type" ];
                                     th
                                       ~a:
                                         [
                                           Unsafe.string_attrib "x-on:click"
                                             "sortByColumn";
                                           a_class
                                             [
                                               "px-6 py-3 text-start text-xs \
                                                font-bold text-primary-600 \
                                                uppercase cursor-pointer \
                                                select-none";
                                             ];
                                         ]
                                       [
                                         txt "Key";
                                         span
                                           ~a:[ a_class [ "px-2" ] ]
                                           [
                                             i
                                               ~a:
                                                 [
                                                   a_class
                                                     [ "fa-solid fa-sort" ];
                                                 ]
                                               [];
                                           ];
                                       ];
                                     th
                                       ~a:
                                         [
                                           Unsafe.string_attrib "x-on:click"
                                             "sortByColumn";
                                           a_class
                                             [
                                               "px-6 py-3 text-start text-xs \
                                                font-bold text-primary-600 \
                                                uppercase cursor-pointer \
                                                select-none";
                                             ];
                                         ]
                                       [
                                         txt "Created";
                                         span
                                           ~a:[ a_class [ "px-2" ] ]
                                           [
                                             i
                                               ~a:
                                                 [
                                                   a_class
                                                     [ "fa-solid fa-sort" ];
                                                 ]
                                               [];
                                           ];
                                       ];
                                     th
                                       ~a:
                                         [
                                           Unsafe.string_attrib "x-on:click"
                                             "sortByColumn";
                                           a_class
                                             [
                                               "px-6 py-3 text-start text-xs \
                                                font-bold text-primary-600 \
                                                uppercase cursor-pointer \
                                                select-none";
                                             ];
                                         ]
                                       [
                                         txt "Expires";
                                         span
                                           ~a:[ a_class [ "px-2" ] ]
                                           [
                                             i
                                               ~a:
                                                 [
                                                   a_class
                                                     [ "fa-solid fa-sort" ];
                                                 ]
                                               [];
                                           ];
                                       ];
                                     th
                                       ~a:
                                         [
                                           Unsafe.string_attrib "x-on:click"
                                             "sortByColumn";
                                           a_class
                                             [
                                               "px-6 py-3 text-start text-xs \
                                                font-bold text-primary-600 \
                                                uppercase cursor-pointer \
                                                select-none";
                                             ];
                                         ]
                                       [
                                         txt "Last used";
                                         span
                                           ~a:[ a_class [ "px-2" ] ]
                                           [
                                             i
                                               ~a:
                                                 [
                                                   a_class
                                                     [ "fa-solid fa-sort" ];
                                                 ]
                                               [];
                                           ];
                                       ];
                                     th
                                       ~a:
                                         [
                                           a_class
                                             [
                                               "px-6 py-3 text-start text-xs \
                                                font-bold text-primary-600 \
                                                uppercase cursor-pointer \
                                                select-none";
                                             ];
                                         ]
                                       [ txt "Action" ];
                                   ];
                               ])
                          (List.map
                             (fun (token : User_model.token) ->
                               tr
                                 ~a:[ a_class [ "border-b border-gray-200" ] ]
                                 [
                                   td
                                     ~a:
                                       [
                                         a_class
                                           [
                                             "px-6 py-4 whitespace-nowrap \
                                              text-sm font-medium \
                                              text-gray-800";
                                           ];
                                       ]
                                     [ txt token.name ];
                                   td
                                     ~a:
                                       [
                                         a_class
                                           [
                                             "px-6 py-4 whitespace-nowrap \
                                              text-sm font-medium \
                                              text-gray-800";
                                           ];
                                       ]
                                     [ txt token.token_type ];
                                   td
                                     ~a:
                                       [
                                         a_class
                                           [
                                             "px-6 py-4 whitespace-nowrap \
                                              text-sm font-medium \
                                              text-gray-800";
                                           ];
                                       ]
                                     [ txt token.value ];
                                   td
                                     ~a:
                                       [
                                         a_class
                                           [
                                             "px-6 py-4 whitespace-nowrap \
                                              text-sm font-medium \
                                              text-gray-800";
                                           ];
                                       ]
                                     [
                                       txt
                                         (Utils.TimeHelper.string_of_ptime
                                            token.created_at);
                                     ];
                                   td
                                     ~a:
                                       [
                                         a_class
                                           [
                                             "px-6 py-4 whitespace-nowrap \
                                              text-sm font-medium \
                                              text-gray-800";
                                           ];
                                       ]
                                     [ txt (string_of_int token.expires_in) ];
                                   td
                                     ~a:
                                       [
                                         a_class
                                           [
                                             "px-6 py-4 whitespace-nowrap \
                                              text-sm font-medium \
                                              text-gray-800";
                                           ];
                                       ]
                                     [
                                       txt
                                         (Utils.TimeHelper.time_ago
                                            ~current_time
                                            ~check_time:token.last_access);
                                     ];
                                   td
                                     ~a:
                                       [
                                         a_class
                                           [
                                             "px-6 py-4 whitespace-nowrap \
                                              text-sm font-medium \
                                              text-gray-800f flex \
                                              justify-between gap-4";
                                           ];
                                       ]
                                     [
                                       Modal_dialog.modal_dialog
                                         ~modal_title:"Edit API Key"
                                         ~button_content:(txt "Edit Key")
                                         ~button_type:`Primary_outlined
                                         ~content:(Tokens_ui.edit_token token)
                                         ();
                                       Utils.button_component
                                         ~attribs:
                                           [
                                             a_id
                                               ("delete-token-button-"
                                              ^ token.value);
                                             a_onclick
                                               ("deleteToken('" ^ token.value
                                              ^ "')");
                                           ]
                                         ~extra_css:"w-full"
                                         ~content:(txt "Delete")
                                         ~btn_type:`Danger_outlined ();
                                     ];
                                 ])
                             tokens);
                      ];
                  ];
              ];
          ];
      ])
