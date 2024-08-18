let settings_layout =
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        div
          ~a:[ a_class [ "px-3 flex justify-between items-center" ] ]
          [
            div
              [
                p
                  ~a:[ a_class [ "font-bold text-gray-700" ] ]
                  [ txt "Configuration" ];
              ];
          ];
        hr ~a:[ a_class [ "border border-primary-500 my-5" ] ] ();
        div
          ~a:[ a_class [ "flex flex-col" ] ]
          [
            div
              ~a:[ a_class [ "-m-1.5 overflow-x-auto" ] ]
              [
                div
                  ~a:
                    [ a_class [ "p-1.5 min-w-full inline-block align-middle" ] ]
                  [ div ~a:[ a_class [ "overflow-hidden" ] ] [] ];
              ];
          ];
      ])
