let error_layout (error : Utils.Status.t) =
  Tyxml_html.(
    section
      ~a:[ a_class [ "text-center" ] ]
      [
        i ~a:[ a_class [ "fa-solid fa-triangle-exclamation text-5xl" ] ] [];
        div
          ~a:[ a_class [ "text-center" ] ]
          [
            p
              ~a:[ a_class [ "text-5xl text-secondary-500 font-semibold" ] ]
              [ txt ("Error " ^ string_of_int error.code) ];
            p
              ~a:[ a_class [ "uppercase font-bold text-xl" ] ]
              [ txt error.title ];
          ];
        p
          ~a:[ a_class [ "text-xl my-6" ] ]
          [ txt (Utils.Json.to_string error.data) ];
      ])
