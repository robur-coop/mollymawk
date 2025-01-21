open Tyxml

let header ?(page_title = "Mollymawk") ~icon () =
  Html.(
    head
      (title (txt page_title))
      [
        meta ~a:[ a_charset "UTF-8" ] ();
        meta
          ~a:
            [
              a_name "viewport";
              a_content "width=device-width, initial-scale=1.0";
            ]
          ();
        script ~a:[ a_src "/main.js" ] (txt "");
        link ~rel:[ `Stylesheet ]
          ~href:"https://unpkg.com/aos@2.3.1/dist/aos.css" ();
        link ~rel:[ `Stylesheet ] ~href:"/style.css" ();
        script ~a:[ a_src "https://kit.fontawesome.com/d1697f2fa9.js" ] (txt "");
        link ~rel:[ `Stylesheet ]
          ~href:"https://unpkg.com/aos@2.3.1/dist/aos.css" ();
        (*aos is animate-on-scroll, adds bouncy effects to html elements*)
        script ~a:[ a_src "https://unpkg.com/aos@2.3.1/dist/aos.js" ] (txt "");
        link ~rel:[ `Icon ] ~href:icon ();
        (*https://alpinejs.dev/ is a lightweight js library and we use it for multiselect form elements*)
        script
          ~a:
            [
              a_defer ();
              a_src
                "https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js";
            ]
          (txt "");
        (* chart.js is used for chart elements like pie charts. We use this to visualize usage data of policies*)
        script ~a:[ a_src "https://cdn.jsdelivr.net/npm/chart.js" ] (txt "");
        (* used in update_unikernel.ml to display diffs in better visuals *)
        script
          ~a:
            [
              a_src
                "https://cdn.jsdelivr.net/npm/diff2html/bundles/js/diff2html-ui.min.js";
            ]
          (txt "");
        link ~rel:[ `Stylesheet ]
          ~href:
            "https://cdn.jsdelivr.net/npm/diff2html/bundles/css/diff2html.min.css"
          ();
      ])
