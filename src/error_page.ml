let error_layout (error : Models.Response.t) =
  Tyxml_html.(
    section
      ~a:[ a_class [ "text-center" ] ]
      [
        i ~a:[ a_class [ "fa-solid fa-triangle-exclamation text-5xl" ] ] [];
        div
          ~a:[ a_class [ "text-center" ] ]
          [
            p
              ~a:[ a_class [ "uppercase font-bold text-xl" ] ]
              [ txt error.title ];
          ];
        p ~a:[ a_class [ "text-xl my-6" ] ] [ txt error.data ];
      ])
