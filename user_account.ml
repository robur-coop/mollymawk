let user_account_layout ~csrf (user : User_model.user)
    (active_cookie : User_model.cookie) current_time =
  Tyxml_html.(
    section
      ~a:[ a_class [ "p-4 bg-gray-50 my-1" ] ]
      [
        p
          ~a:[ a_class [ "text-3xl font-semibold uppercase" ] ]
          [ txt (user.name ^ " - Account") ];
        Utils.csrf_form_input csrf;
        section
          ~a:[ a_class [ "my-5" ] ]
          [
            div
              ~a:[ a_class [ "grid grid-cols-2 my-4" ] ]
              [
                div
                  [
                    h2
                      ~a:[ a_class [ "font-semibold text-xl" ] ]
                      [ txt "Profile Information" ];
                  ];
                div
                  [
                    div
                      [
                        p ~a:[ a_class [ "text-sm my-2" ] ] [ txt "Name" ];
                        input
                          ~a:
                            [
                              a_class
                                [
                                  "w-full py-3 px-2 rounded disabled \
                                   bg-gray-100";
                                ];
                              a_disabled ();
                              a_input_type `Text;
                              a_value user.name;
                            ]
                          ();
                      ];
                    div
                      [
                        p
                          ~a:[ a_class [ "text-sm my-2" ] ]
                          [
                            txt "Email";
                            (match user.email_verified with
                            | Some _ ->
                                span
                                  ~a:[ a_class [ "text-primary-500" ] ]
                                  [ txt " verified" ]
                            | None ->
                                span
                                  ~a:[ a_class [ "text-secondary-500" ] ]
                                  [ txt " not verified" ]);
                          ];
                        input
                          ~a:
                            [
                              a_class
                                [
                                  "w-full py-3 px-2 rounded disabled \
                                   bg-gray-100";
                                ];
                              a_disabled ();
                              a_input_type `Text;
                              a_value user.email;
                            ]
                          ();
                      ];
                  ];
              ];
            hr ();
            div
              ~a:[ a_class [ "grid grid-cols-2 my-4" ] ]
              [
                div
                  [
                    h2
                      ~a:[ a_class [ "font-semibold text-xl" ] ]
                      [ txt "Update Password" ];
                  ];
                div
                  [
                    p ~a:[ a_id "form-alert"; a_class [ "my-4" ] ] [];
                    div
                      [
                        p
                          ~a:[ a_class [ "text-sm my-2" ] ]
                          [ txt "Current Password" ];
                        input
                          ~a:
                            [
                              a_input_type `Password;
                              a_name "current_password";
                              a_id "current-password";
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
                    div
                      [
                        p
                          ~a:[ a_class [ "text-sm my-2" ] ]
                          [ txt "New Password" ];
                        input
                          ~a:
                            [
                              a_input_type `Password;
                              a_name "new_password";
                              a_id "new-password";
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
                    div
                      [
                        p
                          ~a:[ a_class [ "text-sm my-2" ] ]
                          [ txt "confirm Password" ];
                        input
                          ~a:
                            [
                              a_input_type `Password;
                              a_name "confirm_password";
                              a_id "confirm-password";
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
                    div
                      ~a:[ a_class [ "my-4 w-1/2 text-center" ] ]
                      [
                        button
                          ~a:
                            [
                              a_id "password-button";
                              a_onclick "updatePassword()";
                              a_class
                                [
                                  "py-3 rounded bg-primary-500 \
                                   hover:bg-primary-800 w-full text-gray-50 \
                                   font-semibold";
                                ];
                            ]
                          [ txt "Save" ];
                      ];
                  ];
              ];
            hr ();
            div
              ~a:[ a_class [ "grid grid-cols-2 my-4" ] ]
              [
                div
                  [
                    h2
                      ~a:[ a_class [ "font-semibold text-xl" ] ]
                      [ txt "Browser Sessions" ];
                  ];
                div
                  [
                    div
                      [
                        p
                          ~a:[ a_class [ "my-2" ] ]
                          [
                            txt
                              "If necessary, you may logout of all of your \
                               other browser sessions across all of your \
                               devices. Some of your recent sessions are \
                               listed below; however, this list may not be \
                               exhaustive. If you feel your account has been \
                               compromised, you should also update your \
                               password.";
                          ];
                      ];
                    div
                      (List.map
                         (fun (cookie : User_model.cookie) ->
                           div
                             ~a:[ a_class [ "flex items-center my-4" ] ]
                             [
                               div
                                 [
                                   i
                                     ~a:
                                       [
                                         a_class
                                           [ "fa-solid fa-desktop text-5xl" ];
                                       ]
                                     [];
                                 ];
                               div
                                 ~a:[ a_class [ "ml-3" ] ]
                                 [
                                   (* TODO: Parse the user-agent string to extract information like OS, browser etc*)
                                   p
                                     ~a:[ a_class [ "text-gray-600" ] ]
                                     [
                                       txt
                                         (match cookie.user_agent with
                                         | Some agent -> agent
                                         | None -> "User-Agent unavailable");
                                     ];
                                   p
                                     ~a:[ a_class [ "text-sm text-gray-600" ] ]
                                     [
                                       txt "Last active: ";
                                       (if
                                          String.equal cookie.value
                                            active_cookie.value
                                        then
                                          span
                                            [
                                              span [ txt "now" ];
                                              span
                                                ~a:
                                                  [
                                                    a_class
                                                      [
                                                        "text-primary-500 \
                                                         font-semibold";
                                                      ];
                                                  ]
                                                [ txt " This device" ];
                                            ]
                                        else
                                          span
                                            [
                                              txt
                                                (Utils.TimeHelper.time_ago
                                                   ~current_time
                                                   ~check_time:
                                                     cookie.last_access);
                                            ]);
                                     ];
                                   p
                                     ~a:[ a_class [ "text-sm text-gray-600" ] ]
                                     [
                                       txt "First use: ";
                                       span
                                         [
                                           txt
                                             (Utils.TimeHelper.time_ago
                                                ~current_time
                                                ~check_time:cookie.created_at);
                                         ];
                                     ];
                                   p
                                     ~a:[ a_class [ "text-sm text-gray-600" ] ]
                                     [
                                       (match
                                          Ptime.add_span cookie.created_at
                                            (Ptime.Span.of_int_s
                                               cookie.expires_in)
                                        with
                                       | Some ptime ->
                                           span
                                             [
                                               txt
                                                 ("Expires on "
                                                 ^ Utils.TimeHelper
                                                   .string_of_ptime ptime);
                                             ]
                                       | None ->
                                           span
                                             [ txt "Expiry time not available" ]);
                                     ];
                                 ];
                             ])
                         (List.filter
                            (fun (cookie : User_model.cookie) ->
                              String.equal cookie.name User_model.session_cookie)
                            user.cookies));
                    div
                      ~a:[ a_class [ "my-4 w-1/2 text-center" ] ]
                      [
                        button
                          ~a:
                            [
                              a_id "session-button";
                              a_onclick "closeSessions()";
                              a_class
                                [
                                  "py-3 rounded bg-secondary-500 \
                                   hover:bg-secondary-800 w-full text-gray-50 \
                                   font-semibold";
                                ];
                            ]
                          [ txt "Logout all other sessions" ];
                      ];
                  ];
              ];
          ];
      ])
