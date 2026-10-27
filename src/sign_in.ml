let login_page csrf =
  Tyxml_html.(
    div
      ~a:[ a_class [ "w-full max-w-lg mx-auto" ] ]
      [
        Ui.alert_element;
        Ui.heading "Sign in to your Account";
        form
          ~a:
            [
              a_class [ "space-y-6 mt-8 shadow-md p-6" ];
              Unsafe.string_attrib "hx-ext" "form-json";
              Unsafe.string_attrib "hx-post" "/api/login";
              Unsafe.string_attrib "hx-include" "#molly-csrf";
              Unsafe.string_attrib "hx-swap" "none";
              Unsafe.string_attrib "hx-on::after-request"
                "window.parseXhrResponse(event.detail.xhr, new \
                 URLSearchParams(window.location.search).get('redirect') || \
                 '/dashboard')";
            ]
          [
            p ~a:[ a_id "form-alert"; a_class [ "my-4 hidden" ] ] [];
            Ui.csrf_input csrf;
            Ui.form_input "email";
            Ui.form_input ~attribs:[ a_input_type `Password ] "password";
            div
              ~a:[ a_class [ "flex items-center justify-between" ] ]
              [
                Ui.form_link ~href:"/forgot-password"
                  ~label_:"Forgot Your Password?" ();
                Ui.form_link ~href:"/sign-up" ~label_:"Need an account?" ();
              ];
            div
              [
                Ui.button_component
                  ~attribs:[ a_id "login-button"; a_button_type `Submit ]
                  ~content:(txt "Sign in") ~btn_type:`Primary_full ();
              ];
          ];
      ])
