open Tyxml

let top_nav_link ~href label_text =
  Html.(
    a
      ~a:
        [
          a_href href;
          a_class
            [
              "flex space-x-1 items-center cursor-pointer \
               hover:text-primary-500";
            ];
        ]
      [ span [ txt label_text ] ])

let sidebar_link ~href ~icon label_text =
  Html.(
    a
      ~a:
        [
          a_href href;
          a_class
            [
              "hover:bg-gray-200 hover:text-primary-400 font-semibold \
               hover:font-bold cursor-pointer rounded p-2 w-full flex \
               items-center space-x-1";
            ];
        ]
      [
        i ~a:[ a_class [ "fa-solid " ^ icon ^ " text-primary-500 text-sm" ] ] [];
        span [ txt label_text ];
      ])

let dashboard_layout ~csrf (user : Models.User.user)
    ?(page_title = "Dashboard | Mollymawk") ?message ~content () =
  Html.(
    html
      (Header_layout.header ~page_title ())
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
               Ui.csrf_input csrf;
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
                           [ "col-span-1 flex items-center justify-self-start" ];
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
                               [ "flex space-x-5 font-semibold text-gray-200" ];
                           ]
                         [
                           top_nav_link ~href:"/dashboard" "Dashboard";
                           top_nav_link ~href:"/documentation" "Documentation";
                           top_nav_link ~href:"https://mirageos.org" "MirageOS";
                           top_nav_link ~href:"/account"
                             (String.uppercase_ascii
                                (Models.Configuration.name_to_str user.name));
                         ];
                     ];
                 ];
             ];
           Ui.display_banner message;
           section
             ~a:[ a_class [ "grid grid-cols-12 bg-gray-100 min-h-screen" ] ]
             [
               section
                 ~a:[ a_class [ "col-span-2 px-4 py-6 w-full mx-auto" ] ]
                 [
                   Ui.alert_element;
                   div
                     ~a:[ a_class [ "w-full my-2" ] ]
                     [
                       Ui.link_button ~href:"/unikernel/deploy"
                         ~content:
                           (span
                              ~a:[ a_class [ "flex space-x-2 items-center" ] ]
                              [
                                i
                                  ~a:[ a_class [ "fa-solid fa-cube text-sm" ] ]
                                  [];
                                span
                                  ~a:[ a_class [ "text-sm" ] ]
                                  [ txt "Deploy a unikernel" ];
                              ])
                         ~btn_type:`Primary_full ();
                     ];
                   div
                     ~a:[ a_class [ "w-full my-6" ] ]
                     [
                       sidebar_link ~href:"/dashboard" ~icon:"fa-cube"
                         "Unikernels";
                       sidebar_link ~href:"/blocks" ~icon:"fa-database"
                         "Block Devices";
                       hr ~a:[ a_class [ "my-4" ] ] ();
                       sidebar_link ~href:"/usage" ~icon:"fa-chart-pie" "Usage";
                       sidebar_link ~href:"/account" ~icon:"fa-user"
                         "My Account";
                       hr ~a:[ a_class [ "my-4" ] ] ();
                       sidebar_link
                         ~href:
                           "https://robur-coop.github.io/mollymawk-handbook/index.html"
                         ~icon:"fa-book" "Documentation";
                       (if user.super_user then
                          div
                            [
                              hr ~a:[ a_class [ "my-4" ] ] ();
                              sidebar_link ~href:"/admin/users" ~icon:"fa-users"
                                "Users";
                              sidebar_link ~href:"/admin/settings/albatross"
                                ~icon:"fa-gears" "Settings";
                            ]
                        else div []);
                       Ui.button_component
                         ~attribs:
                           [
                             a_id "logout-button";
                             Unsafe.string_attrib "hx-post" "/logout";
                             Unsafe.string_attrib "hx-ext" "form-json";
                             Unsafe.string_attrib "hx-include" "#molly-csrf";
                             Unsafe.string_attrib "hx-swap" "none";
                             Unsafe.string_attrib "hx-on::after-request"
                               "window.location.reload()";
                           ]
                         ~content:(txt "Logout") ~btn_type:`Danger_full ();
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
