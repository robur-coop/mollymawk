let user_account_layout (user : Models.User.user) sessions ~active_cookie_value
    current_time =
  let user_name = Models.Configuration.name_to_str user.name in
  Tyxml_html.(
    section
      ~a:[ a_class [ "p-4 bg-gray-50 my-1" ] ]
      [
        p
          ~a:[ a_class [ "text-3xl font-semibold uppercase" ] ]
          [ txt (user_name ^ " - Account") ];
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
                    Ui.form_input ~label_text:"Name"
                      ~attribs:[ a_value user_name; a_disabled () ]
                      "user_name";
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
                        Ui.raw_input
                          ~attribs:
                            [
                              a_value (Emile.to_string user.email);
                              a_disabled ();
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
                form
                  ~a:
                    [
                      Unsafe.string_attrib "hx-post" "/account/password/update";
                      Unsafe.string_attrib "hx-ext" "form-json";
                      Unsafe.string_attrib "hx-include" "#molly-csrf";
                      Unsafe.string_attrib "hx-swap" "none";
                      Unsafe.string_attrib "hx-on::after-request"
                        "window.parseXhrResponse(event.detail.xhr)";
                    ]
                  [
                    Ui.form_input ~label_text:"Current Password"
                      ~attribs:[ a_input_type `Password; a_required () ]
                      "current_password";
                    Ui.form_input ~label_text:"New Password"
                      ~attribs:[ a_input_type `Password; a_required () ]
                      "new_password";
                    Ui.form_input ~label_text:"confirm Password"
                      ~attribs:[ a_input_type `Password; a_required () ]
                      "confirm_password";
                    div
                      ~a:[ a_class [ "my-4 w-1/2 text-center" ] ]
                      [
                        Ui.button_component
                          ~attribs:
                            [ a_id "password-button"; a_button_type `Submit ]
                          ~content:(txt "Save") ~btn_type:`Primary_full ();
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
                      (Utils.SM.fold
                         (fun _ (cookie : Models.User.cookie) acc ->
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
                                            active_cookie_value
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
                                   (if
                                      not
                                        (String.equal cookie.value
                                           active_cookie_value)
                                    then
                                      div
                                        [
                                          Ui.button_component
                                            ~attribs:
                                              [
                                                a_id
                                                  ("session-button-"
                                                 ^ cookie.value);
                                                Unsafe.string_attrib "hx-post"
                                                  "/api/account/session/close";
                                                Unsafe.string_attrib "hx-ext"
                                                  "form-json";
                                                Unsafe.string_attrib
                                                  "hx-include" "#molly-csrf";
                                                Unsafe.string_attrib "hx-vals"
                                                  (Fmt.str
                                                     {|{"session_value": "%s"}|}
                                                     cookie.value);
                                                Unsafe.string_attrib "hx-swap"
                                                  "none";
                                                Unsafe.string_attrib
                                                  "hx-on::after-request"
                                                  "window.parseXhrResponse(event.detail.xhr)";
                                              ]
                                            ~content:(txt "Close session")
                                            ~btn_type:`Danger_outlined ();
                                        ]
                                    else div []);
                                 ];
                             ]
                           :: acc)
                         sessions []);
                    div
                      ~a:[ a_class [ "my-4 w-1/2 text-center" ] ]
                      [
                        Ui.button_component
                          ~attribs:
                            [
                              a_id "session-button";
                              Unsafe.string_attrib "hx-post"
                                "/api/account/sessions/close";
                              Unsafe.string_attrib "hx-ext" "form-json";
                              Unsafe.string_attrib "hx-include" "#molly-csrf";
                              Unsafe.string_attrib "hx-swap" "none";
                              Unsafe.string_attrib "hx-on::after-request"
                                "window.parseXhrResponse(event.detail.xhr)";
                            ]
                          ~content:(txt "Logout all other sessions")
                          ~btn_type:`Danger_full ();
                      ];
                  ];
              ];
          ];
      ])
