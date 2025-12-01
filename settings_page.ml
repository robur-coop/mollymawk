type settings_tab = Albatross | Email

let settings_layout ~active_tab content =
  let tab_style is_active =
    if is_active then
      "px-6 py-2.5 text-sm font-medium text-primary-50 bg-primary-500 \
       rounded-t-lg border-b-0"
    else
      "px-6 py-2.5 text-sm font-medium text-primary-600 bg-gray-100 \
       hover:bg-primary-50 rounded-t-lg transition-colors duration-200"
  in

  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        div
          ~a:
            [
              a_class
                [
                  "flex items-center space-x-1 border-b-2 border-primary-500 \
                   mb-6";
                ];
            ]
          [
            a
              ~a:
                [
                  a_href "/admin/settings/albatross";
                  a_class [ tab_style (active_tab = Albatross) ];
                ]
              [
                i ~a:[ a_class [ "fa-solid fa-server mr-2" ] ] [];
                txt "Albatross";
              ];
            a
              ~a:
                [
                  a_href "/admin/settings/email";
                  a_class [ tab_style (active_tab = Email) ];
                ]
              [
                i ~a:[ a_class [ "fa-solid fa-envelope mr-2" ] ] []; txt "Email";
              ];
          ];
        div [ content ];
      ])
