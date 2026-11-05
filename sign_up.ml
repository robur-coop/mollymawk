let register_page csrf =
  Tyxml_html.(
    div
      ~a:[ a_class [ "w-full max-w-lg mx-auto" ] ]
      [
        Ui.alert_element;
        Ui.heading "Sign up for an Account";
        form
          ~a:
            [
              a_class [ "space-y-6 mt-8 shadow-md p-6" ];
              Unsafe.string_attrib "hx-ext" "form-json";
              Unsafe.string_attrib "hx-post" "/api/register";
              Unsafe.string_attrib "hx-swap" "none";
              Unsafe.string_attrib "hx-on::after-request"
                "window.parseXhrResponse(event.detail.xhr, '/dashboard')";
            ]
          [
            Ui.csrf_input csrf;
            Ui.form_input "name";
            Ui.form_input "email";
            Ui.form_input ~attribs:[ a_input_type `Password ] "password";
            Ui.form_link ~href:"/sign-in" ~label_:"Have an account?" ();
            div
              [
                Ui.button_component
                  ~attribs:[ a_id "register-button"; a_button_type `Submit ]
                  ~content:(txt "Create Account") ~btn_type:`Primary_full ();
              ];
          ];
      ])
