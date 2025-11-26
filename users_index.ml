let users_index_layout (users : User_model.user list) current_time =
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
                  [ txt ("Users (" ^ string_of_int (List.length users) ^ ")") ];
              ];
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
          ];
        hr ~a:[ a_class [ "border border-primary-500 my-5" ] ] ();
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
                                       [
                                         txt "Email";
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
                                         txt "Last Modified";
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
                                                uppercase";
                                             ];
                                         ]
                                       [ txt "Action" ];
                                   ];
                               ])
                          (List.map
                             (fun (user : User_model.user) ->
                               tr
                                 ~a:[ a_class [ "border-b border-gray-200" ] ]
                                 [
                                   td
                                     ~a:
                                       [
                                         a_class
                                           [
                                             "px-6 py-4 whitespace-nowrap \
                                              text-sm font-medium uppercase \
                                              text-gray-800";
                                           ];
                                       ]
                                     [
                                       div
                                         [
                                           div
                                             ~a:
                                               [
                                                 a_class
                                                   [
                                                     "flex justify-start \
                                                      space-x-1 items-center";
                                                   ];
                                               ]
                                             [
                                               p
                                                 [
                                                   txt
                                                     (Configuration.name_to_str
                                                        user.name);
                                                 ];
                                               (match user.active with
                                               | true ->
                                                   i
                                                     ~a:
                                                       [
                                                         a_class
                                                           [
                                                             "text-primary-500 \
                                                              fa-solid \
                                                              fa-check";
                                                           ];
                                                       ]
                                                     []
                                               | false ->
                                                   i
                                                     ~a:
                                                       [
                                                         a_class
                                                           [
                                                             "text-secondary-500 \
                                                              fa-solid fa-x";
                                                           ];
                                                       ]
                                                     []);
                                             ];
                                           (if user.super_user then
                                              i
                                                ~a:
                                                  [
                                                    a_class
                                                      [
                                                        "text-primary-500 \
                                                         lowercase text-sm";
                                                      ];
                                                  ]
                                                [ txt "admin" ]
                                            else p []);
                                         ];
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
                                     [
                                       div
                                         ~a:
                                           [
                                             a_class
                                               [
                                                 "flex-col justify-start \
                                                  space-x-1 items-center";
                                               ];
                                           ]
                                         [
                                           p [ txt user.email ];
                                           (match user.email_verified with
                                           | Some _ ->
                                               i
                                                 ~a:
                                                   [
                                                     a_class
                                                       [ "text-primary-500" ];
                                                   ]
                                                 [ txt "verified" ]
                                           | None ->
                                               i
                                                 ~a:
                                                   [
                                                     a_class
                                                       [ "text-secondary-500" ];
                                                   ]
                                                 [ txt "not verified" ]);
                                         ];
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
                                     [
                                       txt
                                         (Utils.TimeHelper.time_ago
                                            ~current_time
                                            ~check_time:user.created_at);
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
                                     [
                                       txt
                                         (Utils.TimeHelper.time_ago
                                            ~current_time
                                            ~check_time:user.updated_at);
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
                                     [
                                       a
                                         ~a:
                                           [
                                             a_href
                                               ("/admin/user?uuid=" ^ user.uuid);
                                             a_class
                                               [
                                                 "border border-primary-500 \
                                                  hover:bg-primary-700 px-2 \
                                                  py-1 text-primary-800 \
                                                  hover:text-primary-50 \
                                                  rounded";
                                               ];
                                           ]
                                         [ txt "View" ];
                                     ];
                                 ])
                             users);
                      ];
                  ];
              ];
          ];
      ])
