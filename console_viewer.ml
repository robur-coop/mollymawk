open Tyxml

let console_viewer_layout instance_name unikernel_name =
  let page =
    Html.(
      html
        (head
           (title (txt (Fmt.str "Console: %s" unikernel_name)))
           [
             meta ~a:[ a_charset "UTF-8" ] ();
             meta
               ~a:
                 [
                   a_name "viewport";
                   a_content "width=device-width, initial-scale=1.0";
                 ]
               ();
             script
               ~a:[ a_src "https://unpkg.com/aos@2.3.1/dist/aos.js" ]
               (txt "");
             script ~a:[ a_src "/main.js" ] (txt "");
           ])
        (body
           [
             textarea
               ~a:
                 [
                   a_id "console-output";
                   a_readonly ();
                   a_disabled ();
                   a_style
                     "width: 100%; height: 100vh; box-sizing: border-box; \
                      resize: none; margin: 0; padding: 10px;";
                   a_class
                     [
                       "bg-transparent border-0 overflow-auto \
                        whitespace-pre-wrap font-mono";
                     ];
                 ]
               (txt "");
             footer [];
           ]))
  in
  Format.asprintf "%a" (Html.pp ()) page
