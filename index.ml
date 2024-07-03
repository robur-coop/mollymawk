open Tyxml

let index_page =
  let home =
    Html.(
      html
        (head
           (title (txt "Mollymawk"))
           [
             meta ~a:[ a_charset "UTF-8" ] ();
             meta
               ~a:
                 [
                   a_name "viewport";
                   a_content "width=device-width, initial-scale=1.0";
                 ]
               ();
             link ~rel:[ `Stylesheet ]
               ~href:"https://unpkg.com/aos@2.3.1/dist/aos.css" ();
             script ~a:[ a_src "https://cdn.tailwindcss.com" ] (txt "");
             script ~a:[ a_src "main.js" ] (txt "");
             script
               ~a:[ a_src "https://kit.fontawesome.com/d1697f2fa9.js" ]
               (txt "");
           ])
        (body
           ~a:[ a_class [ "bg-gray-900 px-10 py-10 max-w-7xl mx-auto" ] ]
           [
             section
               [
                 h1
                   ~a:
                     [
                       a_class
                         [ "font-bold text-3xl text-center text-gray-100" ];
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
                         [ "font-semibold text-md text-gray-200 text-center" ];
                       a_id "molly-desc";
                     ]
                   [
                     txt "A MirageOS unikernel to....oh just have some Molly...";
                   ];
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
                           "py-1 px-3 rounded bg-blue-500 text-white \
                            hover:bg-blue-700";
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
                       "font-semibold text-sm text-gray-200 text-center hidden";
                     ];
                 ]
               [
                 txt
                   "Nothing yet..you gotta deploy some unikernels. Come on \
                    chop chop";
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
                   ~a:[ a_id "console-container"; a_class [ "h-screen" ] ]
                   [
                     p
                       ~a:[ a_class [ "text-xl font-semibold text-blue-700" ] ]
                       [ txt "Console Output" ];
                   ];
               ];
             p ~a:[ a_id "deploy-unikernel" ] [];
             p ~a:[ a_id "uni-name"; a_class [ "hidden" ] ] [ txt "foo" ];
             table
               ~a:
                 [
                   a_id "unikernel-info";
                   a_class
                     [
                       "items-center bg-transparent w-full border-collapse my-5";
                     ];
                 ]
               [];
             section ~a:[ a_class [ "my-5" ]; a_id "create-unikernel" ] [];
             footer
               [
                 div
                   ~a:[ a_class [ "text-white text-center my-10" ] ]
                   [ p [ txt "© 2024 Mollymawk - Powered by Albatross" ] ];
               ];
           ]))
  in
  Format.asprintf "%a" (Html.pp ()) home
