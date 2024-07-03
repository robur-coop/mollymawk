open Tyxml

let index_page ~icon =
  let page =
    Html.(
      html
        (Header_layout.header ~page_title:"Home | Mollymawk" ~icon ())
        (body
           [
             section
               [
                 section
                   ~a:[ a_class [ "max-w-7xl mx-auto" ] ]
                   [
                     h1
                       ~a:
                         [
                           a_class
                             [
                               "font-bold text-3xl text-center text-primary-950";
                             ];
                           a_id "molly-text";
                         ]
                       [ txt "Mollymawk" ];
                     div
                       ~a:
                         [
                           a_id "alert-container";
                           a_class [ "fixed top-4 right-4 z-50 p-3" ];
                         ]
                       [];
                     p
                       ~a:
                         [
                           a_class
                             [
                               "font-semibold text-md text-primary-900 \
                                text-center";
                             ];
                           a_id "molly-desc";
                         ]
                       [
                         txt
                           "A MirageOS unikernel to....oh just have some \
                            Molly...";
                       ];
                     div
                       ~a:[ a_class [ "flex justify-end" ] ]
                       [
                         button
                           ~a:
                             [
                               a_id "add-btn";
                               a_class
                                 [
                                   "py-3 px-2 rounded bg-primary-500 \
                                    hover:bg-primary-800 text-gray-50 \
                                    font-semibold";
                                 ];
                             ]
                           [ txt "Deploy Unikernel" ];
                       ];
                     p
                       ~a:
                         [
                           a_id "no-molly";
                           a_class
                             [
                               "font-semibold text-sm text-primary-50 \
                                text-center hidden";
                             ];
                         ]
                       [
                         txt
                           "Nothing yet..you gotta deploy some unikernels. \
                            Come on chop chop";
                       ];
                     section
                       ~a:
                         [
                           a_id "unikernel-container";
                           a_class [ "my-5 hidden grid grid-cols-2 h-screen" ];
                         ]
                       [
                         div ~a:[ a_id "info-container" ] [];
                         div
                           ~a:
                             [
                               a_id "console-container"; a_class [ "h-screen" ];
                             ]
                           [
                             p
                               ~a:
                                 [
                                   a_class
                                     [ "text-xl font-semibold text-blue-700" ];
                                 ]
                               [ txt "Console Output" ];
                           ];
                       ];
                     p ~a:[ a_id "deploy-unikernel" ] [];
                     p
                       ~a:[ a_id "uni-name"; a_class [ "hidden" ] ]
                       [ txt "foo" ];
                     table
                       ~a:
                         [
                           a_id "unikernel-info";
                           a_class
                             [
                               "items-center bg-transparent w-full \
                                border-collapse my-5";
                             ];
                         ]
                       [];
                     section
                       ~a:[ a_class [ "my-5" ]; a_id "create-unikernel" ]
                       [];
                   ];
               ];
             Footer_layout.footer;
           ]))
  in
  Format.asprintf "%a" (Html.pp ()) page
