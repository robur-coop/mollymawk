let email_config_layout (current_config : Models.Email.t option) =
  let ip_val, port_val, from_val, base_url_val =
    match current_config with
    | Some c ->
        ( Ipaddr.to_string c.server,
          string_of_int c.port,
          Emile.to_string c.from_email,
          c.base_url )
    | None -> ("", "", "", "")
  in
  Tyxml_html.(
    section
      ~a:[ a_class [ "p-4 bg-white rounded-lg shadow-sm" ] ]
      [
        h2
          ~a:[ a_class [ "text-lg font-bold text-gray-800 mb-4 px-4" ] ]
          [ txt "Email Server Configuration" ];
        form
          ~a:
            [
              a_class [ "space-y-4 px-4" ];
              Unsafe.string_attrib "hx-ext" "form-json";
              Unsafe.string_attrib "hx-include" "#molly-csrf";
              Unsafe.string_attrib "hx-swap" "none";
              Unsafe.string_attrib "hx-on::config-request"
                "if (!event.detail.parameters['to_email']) delete \
                 event.detail.parameters['to_email'];";
              Unsafe.string_attrib "hx-on::after-request"
                "window.parseXhrResponse(event.detail.xhr)";
            ]
          [
            Ui.form_input ~label_text:"Server IP address"
              ~attribs:[ a_value ip_val; a_required () ]
              "server";
            Ui.form_input ~label_text:"Server Port"
              ~attribs:[ a_input_type `Number; a_value port_val; a_required () ]
              "port";
            Ui.form_input ~label_text:"From"
              ~attribs:[ a_input_type `Email; a_value from_val; a_required () ]
              "from_email";
            Ui.form_input ~label_text:"Base URL"
              ~attribs:[ a_value base_url_val ]
              "base_url";
            Ui.form_input ~label_text:"Test Email Address (optional)"
              ~attribs:[ a_value "" ]
              "to_email";
            div
              ~a:
                [
                  a_class [ "my-6 flex mx-4 justify-center px-4" ];
                  a_style "gap: 1rem";
                ]
              [
                Ui.button_component
                  ~attribs:
                    [
                      a_id "test-email-btn";
                      a_button_type `Button;
                      Unsafe.string_attrib "hx-post"
                        "/api/admin/settings/email/test";
                    ]
                  ~content:(txt "Send Test Email") ~btn_type:`Primary_outlined
                  ();
                Ui.button_component
                  ~attribs:
                    [
                      a_id "update-email-config-btn";
                      a_button_type `Submit;
                      Unsafe.string_attrib "hx-post"
                        "/api/admin/settings/email/update";
                    ]
                  ~content:(txt "Update Configuration")
                  ~btn_type:`Primary_full ();
              ];
          ];
      ])
