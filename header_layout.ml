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
        script ~a:[ a_src "https://unpkg.com/aos@2.3.1/dist/aos.js" ] (txt "");
        link ~rel:[ `Icon ] ~href:icon ();
        script
          ~a:
            [
              a_defer ();
              a_src
                "https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js";
            ]
          (txt "");
      ])
