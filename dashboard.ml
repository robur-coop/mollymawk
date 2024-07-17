open Tyxml

let dashboard_layout ~icon
    ?(message =
      "To start deploying unikernels you'll need to add a payment method.")
    ~content () =
  let page =
    Html.(
      html
        (Header_layout.header ~page_title:"Dashboard | Mollymawk" ~icon ())
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
                               [
                                 i
                                   ~a:
                                     [
                                       a_class
                                         [
                                           "fa-solid fa-grip text-primary-500 \
                                            text-sm";
                                         ];
                                     ]
                                   [];
                                 span [ txt "Dashboard" ];
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
                                       a_href "/account";
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
                                               "fa-solid fa-user \
                                                text-primary-500 text-sm";
                                             ];
                                         ]
                                       [];
                                     span [ txt "Account" ];
                                   ];
                               ];
                           ];
                       ];
                   ];
               ];
             section
               ~a:
                 [
                   a_class
                     [
                       "w-full bg-primary-200 py-4 text-center text-gray-900 \
                        border border-primary-400 font-semibold";
                     ];
                   a_id "banner-message";
                 ]
               [ p [ txt message ] ];
             section
               ~a:[ a_class [ "grid grid-cols-12 bg-gray-100 min-h-screen" ] ]
               [
                 section
                   ~a:[ a_class [ "col-span-2 px-4 py-6 w-full mx-auto" ] ]
                   [
                     div
                       ~a:[ a_class [ "w-full my-2" ] ]
                       [
                         a
                           ~a:
                             [
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
                               a_href "/unikernels";
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
                             span [ txt "Documentaion" ];
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
                       ];
                   ];
                 section
                   ~a:
                     [
                       a_class
                         [ "col-span-7 px-4 py-6 bg-gray-50 shadow-md my-6" ];
                     ]
                   [ content ];
                 section
                   ~a:[ a_class [ "col-span-3 px-4 py-6" ] ]
                   [
                     div
                       ~a:
                         [
                           a_class
                             [
                               "bg-primary-200 px-4 py-5 rounded-md shadow-md \
                                text-gray-900 text-sm";
                             ];
                           a_id "alert-message";
                         ]
                       [
                         span
                           ~a:[ a_class [ "font-bold" ] ]
                           [ txt "A success alert could be here" ];
                       ];
                     div
                       ~a:
                         [
                           a_class
                             [
                               "bg-secondary-200 px-4 py-5 rounded-md \
                                shadow-md text-gray-900 text-sm";
                             ];
                           a_id "alert-message";
                         ]
                       [
                         span
                           ~a:[ a_class [ "font-bold" ] ]
                           [ txt "An error alert could be here" ];
                       ];
                     div
                       ~a:
                         [
                           a_class
                             [
                               "bg-gray-50 px-4 py-5 rounded-md shadow-md \
                                text-gray-500 text-sm mt-10";
                             ];
                         ]
                       [
                         div
                           ~a:[ a_class [ "mb-4" ] ]
                           [
                             span
                               ~a:[ a_class [ "font-bold mb-4" ] ]
                               [ txt "Credits" ];
                             p
                               ~a:[ a_class [ "text-3xl font-semibold" ] ]
                               [ txt "€0.00" ];
                           ];
                         hr ();
                         div
                           ~a:[ a_class [ "mb-4" ] ]
                           [
                             span
                               ~a:[ a_class [ "font-semibold mb-4 text-sm" ] ]
                               [ txt "Current month so far" ];
                             p
                               ~a:
                                 [
                                   a_class [ "font-semibold text-primary-400" ];
                                 ]
                               [ txt "-" ];
                           ];
                         hr ();
                         div
                           ~a:[ a_class [ "mb-4" ] ]
                           [
                             span
                               ~a:[ a_class [ "font-semibold mb-4 text-sm" ] ]
                               [ txt "Last invoice" ];
                             p
                               ~a:
                                 [
                                   a_class [ "font-semibold text-primary-400" ];
                                 ]
                               [ txt "-" ];
                           ];
                       ];
                   ];
               ];
             Footer_layout.footer;
           ]))
  in
  Format.asprintf "%a" (Html.pp ()) page
