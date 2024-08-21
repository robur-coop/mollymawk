open Tyxml

let guest_layout ?(page_title = "Dashboard | Mollymawk") ~icon ~content () =
  let page =
    Html.(
      html
        (Header_layout.header ~page_title ~icon ())
        (body
           [
             section
               [
                 section
                   ~a:
                     [
                       a_class
                         [
                           "py-1  bg-gradient-to-r from-primary-950 \
                            via-primary-900 to-primary-950 px-4";
                         ];
                     ]
                   [
                     div
                       ~a:
                         [
                           a_class
                             [
                               "mx-auto flex justify-between items-center \
                                max-w-7xl";
                             ];
                         ]
                       [
                         a
                           ~a:[ a_href "/"; a_class [ "flex items-center" ] ]
                           [
                             img
                               ~a:[ a_class [ "md:w-16 w-14" ] ]
                               ~src:"/images/robur.png" ~alt:"Robur.coop" ();
                             span
                               ~a:[ a_class [ "font-bold text-gray-50" ] ]
                               [ txt "Mollymawk" ];
                           ];
                         div
                           ~a:
                             [
                               a_class
                                 [
                                   "flex space-x-5 font-semibold text-gray-200";
                                 ];
                             ]
                           [
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
                                   [
                                     i
                                       ~a:
                                         [
                                           a_class
                                             [
                                               "fa-solid fa-leaf \
                                                text-primary-500 text-sm";
                                             ];
                                         ]
                                       [];
                                     span [ txt "Documentation" ];
                                   ];
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
                                       a_href "https://mirage.io";
                                       a_class
                                         [
                                           "flex space-x-1 items-center \
                                            cursor-pointer \
                                            hover:text-primary-500";
                                         ];
                                     ]
                                   [
                                     i
                                       ~a:
                                         [
                                           a_class
                                             [
                                               "fa-solid fa-leaf \
                                                text-primary-500 text-sm";
                                             ];
                                         ]
                                       [];
                                     span [ txt "MirageOS" ];
                                   ];
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
                                       a_href "https://robur.coop";
                                       a_class
                                         [
                                           "flex space-x-1 items-center \
                                            cursor-pointer \
                                            hover:text-primary-500";
                                         ];
                                     ]
                                   [
                                     i
                                       ~a:
                                         [
                                           a_class
                                             [
                                               "fa-solid fa-leaf \
                                                text-primary-500 text-sm";
                                             ];
                                         ]
                                       [];
                                     span [ txt "Robur" ];
                                   ];
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
                                       a_href "/pricing";
                                       a_class
                                         [
                                           "flex space-x-1 items-center \
                                            cursor-pointer \
                                            hover:text-primary-500";
                                         ];
                                     ]
                                   [
                                     i
                                       ~a:
                                         [
                                           a_class
                                             [
                                               "fa-solid fa-leaf \
                                                text-primary-500 text-sm";
                                             ];
                                         ]
                                       [];
                                     span [ txt "Pricing" ];
                                   ];
                               ];
                           ];
                         div
                           ~a:
                             [
                               a_class
                                 [
                                   "flex space-x-5 font-semibold text-gray-200 \
                                    border-y-secondary-300";
                                 ];
                             ]
                           [
                             a
                               ~a:
                                 [
                                   a_href "/sign-in";
                                   a_class
                                     [
                                       "flex space-x-1 items-center \
                                        cursor-pointer hover:text-primary-500 \
                                        border border-primary-500 px-3 py-2 \
                                        rounded-xl";
                                     ];
                                 ]
                               [ txt "Sign In" ];
                             a
                               ~a:
                                 [
                                   a_href "/sign-up";
                                   a_class
                                     [
                                       "flex space-x-1 items-center \
                                        cursor-pointer hover:text-primary-50 \
                                        bg-primary-500 px-3 py-2 rounded-xl";
                                     ];
                                 ]
                               [ txt "Get Started" ];
                           ];
                       ];
                   ];
               ];
             section
               ~a:
                 [
                   a_class [ "col-span-10 px-4 py-6 bg-gray-50 shadow-md my-6" ];
                 ]
               [ content ];
             Footer_layout.footer;
           ]))
  in
  Format.asprintf "%a" (Html.pp ()) page
