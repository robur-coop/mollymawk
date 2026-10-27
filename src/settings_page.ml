type settings_tab = Albatross | Email

let settings_layout ~active_tab content =
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        Ui.tab_bar
          [
            ( "/admin/settings/albatross",
              "fa-solid fa-server",
              "Albatross",
              active_tab = Albatross );
            ( "/admin/settings/email",
              "fa-solid fa-envelope",
              "Email",
              active_tab = Email );
          ];
        div [ content ];
      ])
