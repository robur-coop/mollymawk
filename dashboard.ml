open Tyxml

let dashboard_layout (user : User_model.user) ~icon
    ?(page_title = "Dashboard | Mollymawk") ?message ~content () =
  let page =
    Html.(
      html
        (Header_layout.header ~page_title ~icon ())
        (body
           [
             section
               ~a:
                 [
                   a_class
                     [
                       "py-1 bg-gradient-to-r from-primary-950 via-primary-900 \
                        to-primary-950";
                     ];
                 ]
               [
                 div
                   ~a:
                     [
                       a_class
                         [
                           "mx-auto max-w-7xl grid grid-cols-5 justify-between \
                            items-center";
                         ];
                     ]
                   [
                     div
                       ~a:
                         [
                           a_class
                             [
                               "col-span-1 flex items-center justify-self-start";
                             ];
                         ]
                       [
                         img ~src:"/images/robur.png" ~alt:"Robur"
                           ~a:[ a_class [ "md:w-16 w-14" ] ]
                           ();
                         span
                           ~a:[ a_class [ "font-bold text-gray-50" ] ]
                           [ txt "Mollymawk" ];
                       ];
                     div
                       ~a:[ a_class [ "col-span-4 justify-self-end" ] ]
                       [
                         div
                           ~a:
                             [
                               a_class
                                 [
                                   "flex space-x-5 font-semibold text-gray-200";
                                 ];
                             ]
                           [
                             a
                               ~a:
                                 [
                                   a_href "/dashboard";
                                   a_class
                                     [
                                       "flex space-x-1 items-center \
                                        cursor-pointer hover:text-primary-500";
                                     ];
                                 ]
                               [ span [ txt "Dashboard" ] ];
                             div
                               ~a:
                                 [
                                   a_class
                                     [
                                       "flex space-x-5 font-semibold \
                                        text-gray-200";
                                     ];
                                 ]
                               [
                                 a
                                   ~a:
                                     [
                                       a_href "/documentation";
                                       a_class
                                         [
                                           "flex space-x-1 items-center \
                                            cursor-pointer \
                                            hover:text-primary-500";
                                         ];
                                     ]
                                   [ span [ txt "Documentation" ] ];
                               ];
                             div
                               ~a:
                                 [
                                   a_class
                                     [
                                       "flex space-x-5 font-semibold \
                                        text-gray-200";
                                     ];
                                 ]
                               [
                                 a
                                   ~a:
                                     [
                                       a_href "https://mirageos.org";
                                       a_class
                                         [
                                           "flex space-x-1 items-center \
                                            cursor-pointer \
                                            hover:text-primary-500";
                                         ];
                                     ]
                                   [ span [ txt "MirageOS" ] ];
                               ];
                             div
                               ~a:
                                 [
                                   a_class
                                     [
                                       "flex space-x-5 font-semibold \
                                        text-gray-200";
                                     ];
                                 ]
                               [
                                 a
                                   ~a:
                                     [
                                       a_href "/account";
                                       a_class
                                         [
                                           "flex space-x-1 items-center \
                                            cursor-pointer uppercase \
                                            hover:text-primary-500";
                                         ];
                                     ]
                                   [ span [ txt user.name ] ];
                               ];
                           ];
                       ];
                   ];
               ];
             Utils.display_banner message;
             section
               ~a:[ a_class [ "grid grid-cols-12 bg-gray-100 min-h-screen" ] ]
               [
                 section
                   ~a:[ a_class [ "col-span-2 px-4 py-6 w-full mx-auto" ] ]
                   [
                     div
                       ~a:
                         [
                           a_id "alert-container";
                           a_class
                             [
                               "absolute top-1/4 rounded-md right-4 z-50 w-fit \
                                space-y-2 p-4 shadow border text-wrap hidden";
                             ];
                         ]
                       [];
                     div
                       ~a:[ a_class [ "w-full my-2" ] ]
                       [
                         a
                           ~a:
                             [
                               a_href "/unikernel/deploy";
                               a_class
                                 [
                                   "cursor-pointer bg-primary-500 \
                                    hover:bg-primary-700 rounded-md \
                                    text-gray-50 px-2 py-2 font-semibold \
                                    w-full flex space-x-2 items-center \
                                    justify-center";
                                 ];
                             ]
                           [
                             i ~a:[ a_class [ "fa-solid fa-cube text-sm" ] ] [];
                             span
                               ~a:[ a_class [ "text-sm" ] ]
                               [ txt "Deploy a unikernel" ];
                           ];
                       ];
                     div
                       ~a:[ a_class [ "w-full my-6" ] ]
                       [
                         a
                           ~a:
                             [
                               a_href "/projects";
                               a_class
                                 [
                                   "hover:bg-gray-200 hover:text-primary-400 \
                                    font-semibold hover:font-bold \
                                    cursor-pointer rounded p-2 w-full flex \
                                    items-center space-x-1";
                                 ];
                             ]
                           [
                             i
                               ~a:
                                 [
                                   a_class
                                     [
                                       "fa-solid fa-diagram-project text-sm \
                                        text-primary-500";
                                     ];
                                 ]
                               [];
                             span [ txt "Projects" ];
                           ];
                         a
                           ~a:
                             [
                               a_href "/dashboard";
                               a_class
                                 [
                                   "hover:bg-gray-200 hover:text-primary-400 \
                                    font-semibold hover:font-bold \
                                    cursor-pointer rounded p-2 w-full flex \
                                    items-center space-x-1";
                                 ];
                             ]
                           [
                             i
                               ~a:
                                 [
                                   a_class
                                     [
                                       "fa-solid fa-cube text-sm \
                                        text-primary-500";
                                     ];
                                 ]
                               [];
                             span [ txt "Unikernels" ];
                           ];
                         a
                           ~a:
                             [
                               a_href "/volumes";
                               a_class
                                 [
                                   "hover:bg-gray-200 hover:text-primary-400 \
                                    font-semibold hover:font-bold \
                                    cursor-pointer rounded p-2 w-full flex \
                                    items-center space-x-1";
                                 ];
                             ]
                           [
                             i
                               ~a:
                                 [
                                   a_class
                                     [
                                       "fa-solid fa-database text-primary-500 \
                                        text-sm";
                                     ];
                                 ]
                               [];
                             span [ txt "Volumes" ];
                           ];
                         hr ~a:[ a_class [ "my-4" ] ] ();
                         a
                           ~a:
                             [
                               a_href "/billing";
                               a_class
                                 [
                                   "hover:bg-gray-200 hover:text-primary-400 \
                                    font-semibold hover:font-bold \
                                    cursor-pointer rounded p-2 w-full flex \
                                    items-center space-x-1";
                                 ];
                             ]
                           [
                             i
                               ~a:
                                 [
                                   a_class
                                     [
                                       "fa-solid fa-money-bills \
                                        text-primary-500 text-sm";
                                     ];
                                 ]
                               [];
                             span [ txt "Billing" ];
                           ];
                         a
                           ~a:
                             [
                               a_href "/tokens";
                               a_class
                                 [
                                   "hover:bg-gray-200 hover:text-primary-400 \
                                    font-semibold hover:font-bold \
                                    cursor-pointer rounded p-2 w-full flex \
                                    items-center space-x-1";
                                 ];
                             ]
                           [
                             i
                               ~a:
                                 [
                                   a_class
                                     [
                                       "fa-solid fa-key text-primary-500 \
                                        text-sm";
                                     ];
                                 ]
                               [];
                             span [ txt "Tokens" ];
                           ];
                         a
                           ~a:
                             [
                               a_href "/usage";
                               a_class
                                 [
                                   "hover:bg-gray-200 hover:text-primary-400 \
                                    font-semibold hover:font-bold \
                                    cursor-pointer rounded p-2 w-full flex \
                                    items-center space-x-1";
                                 ];
                             ]
                           [
                             i
                               ~a:
                                 [
                                   a_class
                                     [
                                       "fa-solid fa-chart-pie text-primary-500 \
                                        text-sm";
                                     ];
                                 ]
                               [];
                             span [ txt "Usage" ];
                           ];
                         a
                           ~a:
                             [
                               a_href "/activity";
                               a_class
                                 [
                                   "hover:bg-gray-200 hover:text-primary-400 \
                                    font-semibold hover:font-bold \
                                    cursor-pointer rounded p-2 w-full flex \
                                    items-center space-x-1";
                                 ];
                             ]
                           [
                             i
                               ~a:
                                 [
                                   a_class
                                     [
                                       "fa-solid fa-file-lines \
                                        text-primary-500 text-sm";
                                     ];
                                 ]
                               [];
                             span [ txt "Activity" ];
                           ];
                         a
                           ~a:
                             [
                               a_href "/account";
                               a_class
                                 [
                                   "hover:bg-gray-200 hover:text-primary-400 \
                                    font-semibold hover:font-bold \
                                    cursor-pointer rounded p-2 w-full flex \
                                    items-center space-x-1";
                                 ];
                             ]
                           [
                             i
                               ~a:
                                 [
                                   a_class
                                     [
                                       "fa-solid fa-user text-primary-500 \
                                        text-sm";
                                     ];
                                 ]
                               [];
                             span [ txt "My Account" ];
                           ];
                         hr ~a:[ a_class [ "my-4" ] ] ();
                         a
                           ~a:
                             [
                               a_href "/status";
                               a_class
                                 [
                                   "hover:bg-gray-200 hover:text-primary-400 \
                                    font-semibold hover:font-bold \
                                    cursor-pointer rounded p-2 w-full flex \
                                    items-center space-x-1";
                                 ];
                             ]
                           [
                             i
                               ~a:
                                 [
                                   a_class
                                     [
                                       "fa-solid fa-record-vinyl \
                                        text-primary-500 text-sm";
                                     ];
                                 ]
                               [];
                             span [ txt "Status" ];
                           ];
                         a
                           ~a:
                             [
                               a_href "/metrics";
                               a_class
                                 [
                                   "hover:bg-gray-200 hover:text-primary-400 \
                                    font-semibold hover:font-bold \
                                    cursor-pointer rounded p-2 w-full flex \
                                    items-center space-x-1";
                                 ];
                             ]
                           [
                             i
                               ~a:
                                 [
                                   a_class
                                     [
                                       "fa-solid fa-chart-line \
                                        text-primary-500 text-sm";
                                     ];
                                 ]
                               [];
                             span [ txt "Metrics" ];
                           ];
                         a
                           ~a:
                             [
                               a_href "/documentation";
                               a_class
                                 [
                                   "hover:bg-gray-200 hover:text-primary-400 \
                                    font-semibold hover:font-bold \
                                    cursor-pointer rounded p-2 w-full flex \
                                    items-center space-x-1";
                                 ];
                             ]
                           [
                             i
                               ~a:
                                 [
                                   a_class
                                     [
                                       "fa-solid fa-leaf text-primary-500 \
                                        text-sm";
                                     ];
                                 ]
                               [];
                             span [ txt "Documentation" ];
                           ];
                         a
                           ~a:
                             [
                               a_href "/shop";
                               a_class
                                 [
                                   "hover:bg-gray-200 hover:text-primary-400 \
                                    font-semibold hover:font-bold \
                                    cursor-pointer rounded p-2 w-full flex \
                                    items-center space-x-1";
                                 ];
                             ]
                           [
                             i
                               ~a:
                                 [
                                   a_class
                                     [
                                       "fa-solid fa-shop text-primary-500 \
                                        text-sm";
                                     ];
                                 ]
                               [];
                             span [ txt "Marketplace" ];
                           ];
                         (if user.super_user then
                            div
                              [
                                hr ~a:[ a_class [ "my-4" ] ] ();
                                a
                                  ~a:
                                    [
                                      a_href "/admin/users";
                                      a_class
                                        [
                                          "hover:bg-gray-200 \
                                           hover:text-primary-400 \
                                           font-semibold hover:font-bold \
                                           cursor-pointer rounded p-2 w-full \
                                           flex items-center space-x-1";
                                        ];
                                    ]
                                  [
                                    i
                                      ~a:
                                        [
                                          a_class
                                            [
                                              "fa-solid fa-users \
                                               text-primary-500 text-sm";
                                            ];
                                        ]
                                      [];
                                    span [ txt "Users" ];
                                  ];
                                a
                                  ~a:
                                    [
                                      a_href "/admin/settings";
                                      a_class
                                        [
                                          "hover:bg-gray-200 \
                                           hover:text-primary-400 \
                                           font-semibold hover:font-bold \
                                           cursor-pointer rounded p-2 w-full \
                                           flex items-center space-x-1";
                                        ];
                                    ]
                                  [
                                    i
                                      ~a:
                                        [
                                          a_class
                                            [
                                              "fa-solid fa-gears \
                                               text-primary-500 text-sm";
                                            ];
                                        ]
                                      [];
                                    span [ txt "Settings" ];
                                  ];
                              ]
                          else div []);
                       ];
                   ];
                 section
                   ~a:
                     [
                       a_class
                         [ "col-span-10 px-4 py-6 bg-gray-50 shadow-md my-6" ];
                     ]
                   [ content ];
               ];
             Footer_layout.footer;
           ]))
  in
  Format.asprintf "%a" (Html.pp ()) page
