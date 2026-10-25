let login_page csrf =
  Tyxml_html.(
    div
      ~a:[ a_class [ "w-full max-w-lg mx-auto" ] ]
      [
        Utils.Ui.alert_element;
        Utils.Ui.heading "Sign in to your Account";
        form
          ~a:
            [
              a_class [ "space-y-6 mt-8 shadow-md p-6" ];
              Unsafe.string_attrib "hx-ext" "response-targets";
              a_enctype "multipart/form-data";
              Unsafe.string_attrib "hx-post" "/api/login";
              Unsafe.string_attrib "hx-include" "#molly-csrf";
              Unsafe.string_attrib "hx-target-200" "#success-alert";
              Unsafe.string_attrib "hx-target" "#error-alert";
            ]
          [
            Utils.Ui.form_alerts;
            Utils.Ui.csrf_input csrf;
            Utils.Ui.form_input "email";
            Utils.Ui.form_input ~input_type:`Password "password";
            div
              ~a:[ a_class [ "flex items-center justify-between" ] ]
              [
                Utils.Ui.form_link ~href:"/forgot-password"
                  ~label_:"Forgot Your Password?" ();
                Utils.Ui.form_link ~href:"/sign-up" ~label_:"Need an account?"
                  ();
              ];
            div
              [
                Utils.Ui.button_component
                  ~attribs:[ a_id "login-button"; a_button_type `Submit ]
                  ~content:(txt "Sign in") ~btn_type:`Primary_full ();
              ];
          ];
      ])
