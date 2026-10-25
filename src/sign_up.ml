let register_page csrf =
  Tyxml_html.(
    div
      ~a:[ a_class [ "w-full max-w-lg mx-auto" ] ]
      [
        Utils.Ui.alert_element;
        Utils.Ui.heading "Sign up for an Account";
        form
          ~a:
            [
              a_class [ "space-y-6 mt-8 shadow-md p-6" ];
              Unsafe.string_attrib "hx-ext" "response-targets";
              a_enctype "multipart/form-data";
              Unsafe.string_attrib "hx-post" "/api/register";
              Unsafe.string_attrib "hx-include" "#molly-csrf";
              Unsafe.string_attrib "hx-target-200" "#success-alert";
              Unsafe.string_attrib "hx-target" "#error-alert";
            ]
          [
            Utils.Ui.form_alerts;
            Utils.Ui.csrf_input csrf;
            Utils.Ui.form_input "name";
            Utils.Ui.form_input "email";
            Utils.Ui.form_input ~input_type:`Password "password";
            Utils.Ui.form_link ~href:"/sign-in" ~label_:"Have an account?" ();
            div
              [
                Utils.Ui.button_component
                  ~attribs:[ a_id "register-button"; a_button_type `Submit ]
                  ~content:(txt "Create Account") ~btn_type:`Primary_full ();
              ];
          ];
      ])
