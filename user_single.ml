let user_single_layout (user : User_model.user) unikernels policies current_time
    =
  Tyxml_html.(
    section
      ~a:[ a_class [ "p-4 bg-gray-50 my-1" ] ]
      [
        p
          ~a:[ a_class [ "text-3xl font-semibold uppercase" ] ]
          [ txt user.name ];
        section
          ~a:[ a_class [ "my-5" ] ]
          [
            ul
              ~a:
                [
                  a_class
                    [
                      "mb-5 flex list-none flex-row flex-wrap border-b-0 \
                       bg-primary-50";
                    ];
                  a_role [ "tablist" ];
                ]
              [
                li
                  ~a:
                    [
                      a_role [ "presentation" ];
                      a_class
                        [ "border hover:bg-primary-700 hover:text-gray-50" ];
                    ]
                  [
                    a
                      ~a:
                        [
                          a_href "#account";
                          a_class
                            [
                              "my-2 block border-x-0 border-b-2 border-t-0 \
                               border-transparent px-7 pb-3.5 pt-4 text-xs \
                               font-medium uppercase leading-tight \
                               text-neutral-500 hover:isolate \
                               hover:border-transparent hover:bg-neutral-100 \
                               focus:isolate focus:border-transparent tab-link \
                               active";
                            ];
                          a_role [ "tab" ];
                          a_aria "controls" [ "tabs-account" ];
                          a_aria "selected" [ "true" ];
                        ]
                      [ txt "Account" ];
                  ];
                li
                  ~a:
                    [
                      a_role [ "presentation" ];
                      a_class
                        [ "border hover:bg-primary-700 hover:text-gray-50" ];
                    ]
                  [
                    a
                      ~a:
                        [
                          a_href "#unikernels";
                          a_class
                            [
                              "my-2 block border-x-0 border-b-2 border-t-0 \
                               border-transparent px-7 pb-3.5 pt-4 text-xs \
                               font-medium uppercase leading-tight \
                               text-neutral-500 hover:isolate \
                               hover:border-transparent hover:bg-neutral-100 \
                               focus:isolate focus:border-transparent tab-link \
                               active";
                            ];
                          a_role [ "tab" ];
                          a_aria "controls" [ "tabs-unikernels" ];
                          a_aria "selected" [ "false" ];
                        ]
                      [ txt "Unikernels" ];
                  ];
                li
                  ~a:
                    [
                      a_role [ "presentation" ];
                      a_class
                        [ "border hover:bg-primary-700 hover:text-gray-50" ];
                    ]
                  [
                    a
                      ~a:
                        [
                          a_href "#settings";
                          a_class
                            [
                              "my-2 block border-x-0 border-b-2 border-t-0 \
                               border-transparent px-7 pb-3.5 pt-4 text-xs \
                               font-medium uppercase leading-tight \
                               text-neutral-500 hover:isolate \
                               hover:border-transparent hover:bg-neutral-100 \
                               focus:isolate focus:border-transparent tab-link \
                               active";
                            ];
                          a_role [ "tab" ];
                          a_aria "controls" [ "tabs-settings" ];
                          a_aria "selected" [ "false" ];
                        ]
                      [ txt "Settings" ];
                  ];
              ];
          ];
        section
          ~a:[ a_class [ "tab-content" ] ]
          [
            div
              ~a:[ a_id "account"; a_class [ "max-w-3xl tab-pane active" ] ]
              [
                h2
                  ~a:[ a_class [ "font-bold uppercase text-xl" ] ]
                  [ txt "Account" ];
                div
                  ~a:[ a_class [ "flex-col justify-center text-center" ] ]
                  [
                    i ~a:[ a_class [ "fa-solid fa-circle-user text-7xl" ] ] [];
                    p
                      ~a:[ a_class [ "text-3xl font-semibold uppercase" ] ]
                      [ txt user.name ];
                  ];
                div
                  ~a:[ a_class [ "grid grid-cols-2 my-4" ] ]
                  [
                    label [ txt "Email" ];
                    p
                      ~a:[ a_class [ "rounded border py-2 px-4 w-full" ] ]
                      [
                        txt user.email;
                        (match user.email_verified with
                        | Some _ptime ->
                            span
                              [
                                i ~a:[ a_class [ "fa-solid fa-check" ] ] [];
                                i ~a:[ a_class [ "text-xs" ] ] [];
                              ]
                        | None ->
                            i
                              ~a:
                                [
                                  a_class [ "fa-solid fa-x text-secondary-500" ];
                                ]
                              []);
                      ];
                    label [ txt "Created at" ];
                    p
                      ~a:[ a_class [ "rounded border py-2 px-4 w-full" ] ]
                      [ txt (Utils.TimeHelper.string_of_ptime user.created_at) ];
                    label [ txt "Last Update" ];
                    p
                      ~a:[ a_class [ "rounded border py-2 px-4 w-full" ] ]
                      [ txt (Utils.TimeHelper.string_of_ptime user.updated_at) ];
                    label [ txt "Email verified at" ];
                    p
                      ~a:[ a_class [ "rounded border py-2 px-4 w-full" ] ]
                      [
                        txt
                          (match user.email_verified with
                          | None -> "not verified"
                          | Some t -> Utils.TimeHelper.string_of_ptime t);
                      ];
                    div
                      ~a:[ a_class [ "flex justify-center space-x-4 my-4" ] ]
                      [
                        (if user.active then
                           button
                             ~a:
                               [
                                 a_onclick
                                   ("toggleUserActiveStatus('" ^ user.uuid
                                  ^ "')");
                                 a_class
                                   [
                                     "px-3 py-2 rounded bg-secondary-500 \
                                      text-secondary-50 hover:bg-secondary-700 \
                                      font-semibold";
                                   ];
                               ]
                             [ txt "Deactivate" ]
                         else
                           button
                             ~a:
                               [
                                 a_onclick
                                   ("toggleUserActiveStatus('" ^ user.uuid
                                  ^ "')");
                                 a_class
                                   [
                                     "px-3 py-2 rounded bg-primary-500 \
                                      text-primary-50 hover:bg-primary-700 \
                                      font-semibold";
                                   ];
                               ]
                             [ txt "Activate" ]);
                        (if user.super_user then
                           button
                             ~a:
                               [
                                 a_onclick
                                   ("toggleUserAdminStatus('" ^ user.uuid ^ "')");
                                 a_class
                                   [
                                     "px-3 py-2 rounded bg-secondary-500 \
                                      text-secondary-50 hover:bg-secondary-700 \
                                      font-semibold";
                                   ];
                               ]
                             [ txt "Remove Admin" ]
                         else
                           button
                             ~a:
                               [
                                 a_onclick
                                   ("toggleUserAdminStatus('" ^ user.uuid ^ "')");
                                 a_class
                                   [
                                     "px-3 py-2 rounded bg-primary-500 \
                                      text-primary-50 hover:bg-primary-700 \
                                      font-semibold";
                                   ];
                               ]
                             [ txt "Make Admin" ]);
                      ];
                  ];
              ];
            section
              ~a:[ a_id "unikernels"; a_class [ "my-5 tab-pane hidden" ] ]
              [
                h2
                  ~a:[ a_class [ "font-bold uppercase text-xl" ] ]
                  [ txt "Unikernels" ];
                Unikernel_index.unikernel_index_layout unikernels current_time;
              ];
            section
              ~a:[ a_id "settings"; a_class [ "my-5 tab-pane hidden" ] ]
              [
                table
                  ~a:
                    [
                      a_class
                        [ "table-auto min-w-full divide-y divide-gray-200" ];
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
                                       "px-6 py-2 text-start text-xs font-bold \
                                        text-primary-600 uppercase";
                                     ];
                                 ]
                               [ txt "Allowed VMs" ];
                             th
                               ~a:
                                 [
                                   a_class
                                     [
                                       "px-6 py-2 text-start text-xs font-bold \
                                        text-primary-600 uppercase";
                                     ];
                                 ]
                               [ txt "Allowed Memory" ];
                             th
                               ~a:
                                 [
                                   a_class
                                     [
                                       "px-6 py-2 text-start text-xs font-bold \
                                        text-primary-600 uppercase";
                                     ];
                                 ]
                               [ txt "Allowed Storage" ];
                             th
                               ~a:
                                 [
                                   a_class
                                     [
                                       "px-6 py-2 text-start text-xs font-bold \
                                        text-primary-600 uppercase";
                                     ];
                                 ]
                               [ txt "CPU IDs" ];
                             th
                               ~a:
                                 [
                                   a_class
                                     [
                                       "px-6 py-2 text-start text-xs font-bold \
                                        text-primary-600 uppercase";
                                     ];
                                 ]
                               [ txt "Network Bridges" ];
                             th
                               ~a:
                                 [
                                   a_class
                                     [
                                       "px-6 py-2 text-start text-xs font-bold \
                                        text-primary-600 uppercase";
                                     ];
                                 ]
                               [ txt "Action" ];
                           ];
                       ])
                  (List.map
                     (fun (_hd, policy) ->
                       tr
                         [
                           td
                             ~a:
                               [
                                 a_class
                                   [
                                     "px-6 py-1 whitespace-nowrap text-sm \
                                      font-medium text-gray-800";
                                   ];
                               ]
                             [ txt (string_of_int policy.Vmm_core.Policy.vms) ];
                           td
                             ~a:
                               [
                                 a_class
                                   [
                                     "px-6 py-1 whitespace-normal text-sm \
                                      font-medium text-gray-800";
                                   ];
                               ]
                             [ txt (string_of_int policy.memory ^ " MB") ];
                           td
                             ~a:
                               [
                                 a_class
                                   [
                                     "px-6 py-1 whitespace-normal text-sm \
                                      font-medium text-gray-800";
                                   ];
                               ]
                             [
                               txt
                                 (string_of_int
                                    (Option.value policy.block ~default:0)
                                 ^ " MB");
                             ];
                           td
                             ~a:
                               [
                                 a_class
                                   [
                                     "px-6 py-1 whitespace-normal text-sm \
                                      font-medium text-gray-800";
                                   ];
                               ]
                             [
                               txt
                                 (String.concat ", "
                                    (List.map string_of_int
                                       (Vmm_core.IS.elements policy.cpuids)));
                             ];
                           td
                             ~a:
                               [
                                 a_class
                                   [
                                     "px-6 py-1 whitespace-normal text-sm \
                                      font-medium text-gray-800";
                                   ];
                               ]
                             [
                               txt
                                 (String.concat ", "
                                    (List.map string_of_uri
                                       (Vmm_core.String_set.elements
                                          policy.bridges)));
                             ];
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
                               a
                                 ~a:
                                   [
                                     a_href "";
                                     a_class
                                       [
                                         "border border-primary-500 \
                                          hover:bg-primary-700 px-2 py-1 \
                                          text-primary-800 \
                                          hover:text-primary-50 rounded";
                                       ];
                                   ]
                                 [ txt "Edit" ];
                             ];
                         ])
                     policies);
              ];
          ];
      ])
