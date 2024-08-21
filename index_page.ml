let index_page =
  Tyxml_html.(
    section
      ~a:[ a_class [ "text-center" ] ]
      [ p ~a:[ a_class [ "uppercase font-bold text-5xl" ] ] [ txt "Welcome" ] ])
