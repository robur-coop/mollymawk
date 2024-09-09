let user_single_layout (user : User_model.user) unikernels current_time =
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        section
          ~a:[ a_class [ "flex-col justify-center" ] ]
          [
            div
              ~a:[ a_class [ "text-center" ] ]
              [
                i ~a:[ a_class [ "fa-solid fa-circle-user text-7xl" ] ] [];
                p
                  ~a:[ a_class [ "text-3xl font-semibold uppercase" ] ]
                  [ txt user.name ];
                div
                  ~a:
                    [
                      a_class
                        [ "flex justify-center justify-items-center space-x-1" ];
                    ]
                  [
                    p ~a:[ a_class [ "text-lg" ] ] [ txt user.email ];
                    (match user.email_verified with
                    | Some ptime ->
                        span
                          [
                            i ~a:[ a_class [ "fa-solid fa-check" ] ] [];
                            i
                              ~a:[ a_class [ "text-xs" ] ]
                              [
                                txt
                                  (Utils.TimeHelper.time_ago current_time ptime);
                              ];
                          ]
                    | None ->
                        i
                          ~a:[ a_class [ "fa-solid fa-x text-secondary-500" ] ]
                          []);
                  ];
              ];
            div
              ~a:[ a_class [ "flex justify-center space-x-4 my-4" ] ]
              [
                (if user.active then
                   button
                     ~a:
                       [
                         a_onclick ("toggleUserStatus('" ^ user.uuid ^ "')");
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
                         a_onclick ("toggleUserStatus('" ^ user.uuid ^ "')");
                         a_class
                           [
                             "px-3 py-2 rounded bg-primary-500 text-primary-50 \
                              hover:bg-primary-700 font-semibold";
                           ];
                       ]
                     [ txt "Activate" ]);
                (if user.super_user then
                   button
                     ~a:
                       [
                         a_onclick ("toggleUserStatus('" ^ user.uuid ^ "')");
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
                         a_onclick ("toggleUserStatus('" ^ user.uuid ^ "')");
                         a_class
                           [
                             "px-3 py-2 rounded bg-primary-500 text-primary-50 \
                              hover:bg-primary-700 font-semibold";
                           ];
                       ]
                     [ txt "Make Admin" ]);
              ];
          ];
        section
          ~a:[ a_class [ "my-4" ] ]
          [ Unikernel_index.unikernel_index_layout unikernels current_time ];
      ])
