let create_token =
  Tyxml_html.(
    section
      ~a:[ a_id "token-create"; a_class [ "w-full mx-auto" ] ]
      [
        div
          ~a:[ a_class [ "my-6" ] ]
          [
            p ~a:[ a_id "tokens-form-alert"; a_class [ "my-4 hidden" ] ] [];
            label
              ~a:[ a_class [ "block text-sm font-medium" ]; a_label_for "name" ]
              [ txt "Name" ];
            input
              ~a:
                [
                  a_autocomplete `On;
                  a_input_type `Text;
                  a_name "token_name";
                  a_id "token_name";
                  a_class
                    [
                      "ring-primary-100 mt-1.5 transition appearance-none \
                       block w-full px-3 py-3 rounded-xl shadow-sm border \
                       hover:border-primary-200\n\
                      \                                           \
                       focus:border-primary-300 bg-primary-50 bg-opacity-0 \
                       hover:bg-opacity-50 focus:bg-opacity-50 \
                       ring-primary-200 focus:ring-primary-200\n\
                      \                                           \
                       focus:ring-[1px] focus:outline-none";
                    ];
                ]
              ();
          ];
        div
          ~a:[ a_class [ "my-6" ] ]
          [
            label
              ~a:
                [
                  a_class [ "block text-sm font-medium" ];
                  a_label_for "validity";
                ]
              [ txt "Valid for" ];
            select
              ~a:
                [
                  a_name "token_expiry";
                  a_id "token_expiry";
                  a_class
                    [
                      "ring-primary-100 mt-1.5 transition block w-full px-3 \
                       py-3 rounded-xl shadow-sm border \
                       hover:border-primary-200\n\
                      \                                           \
                       focus:border-primary-300 bg-primary-50 bg-opacity-0 \
                       hover:bg-opacity-50 focus:bg-opacity-50 \
                       ring-primary-200 focus:ring-primary-200\n\
                      \                                           \
                       focus:ring-[1px] focus:outline-none";
                    ];
                ]
              [
                option ~a:[ a_value "2419200" ] (txt "1 month");
                option ~a:[ a_value "7257600" ] (txt "3 months");
                option ~a:[ a_value "14515200" ] (txt "6 months");
                option ~a:[ a_value "29030400" ] (txt "1 year");
                option ~a:[ a_value "58060800" ] (txt "2 years");
              ];
          ];
        div
          ~a:[ a_class [ "my-6" ] ]
          [
            Utils.button_component
              ~attribs:[ a_id "create-token-button"; a_onclick "createToken()" ]
              ~extra_css:"w-full" ~content:(txt "Create Token")
              ~btn_type:`Primary_full ();
          ];
      ])

let edit_token (token : User_model.token) =
  Tyxml_html.(
    section
      ~a:[ a_id "token-update"; a_class [ "w-full mx-auto" ] ]
      [
        div
          ~a:[ a_class [ "my-6" ] ]
          [
            p ~a:[ a_id "tokens-form-alert"; a_class [ "my-4 hidden" ] ] [];
            label
              ~a:[ a_class [ "block text-sm font-medium" ]; a_label_for "name" ]
              [ txt "Name" ];
            input
              ~a:
                [
                  a_autocomplete `On;
                  a_input_type `Text;
                  a_name "token_name";
                  a_id "token_name";
                  a_value token.name;
                  a_class
                    [
                      "ring-primary-100 mt-1.5 transition appearance-none \
                       block w-full px-3 py-3 rounded-xl shadow-sm border \
                       hover:border-primary-200\n\
                      \                                           \
                       focus:border-primary-300 bg-primary-50 bg-opacity-0 \
                       hover:bg-opacity-50 focus:bg-opacity-50 \
                       ring-primary-200 focus:ring-primary-200\n\
                      \                                           \
                       focus:ring-[1px] focus:outline-none";
                    ];
                ]
              ();
          ];
        div
          ~a:[ a_class [ "my-6" ] ]
          [
            label
              ~a:
                [
                  a_class [ "block text-sm font-medium" ];
                  a_label_for "validity";
                ]
              [ txt "Valid for" ];
            select
              ~a:
                [
                  a_name "token_expiry";
                  a_id "token_expiry";
                  a_class
                    [
                      "ring-primary-100 mt-1.5 transition block w-full px-3 \
                       py-3 rounded-xl shadow-sm border \
                       hover:border-primary-200\n\
                      \                                           \
                       focus:border-primary-300 bg-primary-50 bg-opacity-0 \
                       hover:bg-opacity-50 focus:bg-opacity-50 \
                       ring-primary-200 focus:ring-primary-200\n\
                      \                                           \
                       focus:ring-[1px] focus:outline-none";
                    ];
                ]
              [
                option
                  ~a:
                    ([ a_value "2419200" ]
                    @
                    if token.expires_in = 2419200 then [ a_selected () ] else []
                    )
                  (txt "1 month");
                option
                  ~a:
                    ([ a_value "7257600" ]
                    @
                    if token.expires_in = 7257600 then [ a_selected () ] else []
                    )
                  (txt "3 months");
                option
                  ~a:
                    ([ a_value "14515200" ]
                    @
                    if token.expires_in = 14515200 then [ a_selected () ]
                    else [])
                  (txt "6 months");
                option
                  ~a:
                    ([ a_value "29030400" ]
                    @
                    if token.expires_in = 29030400 then [ a_selected () ]
                    else [])
                  (txt "1 year");
                option
                  ~a:
                    ([ a_value "58060800" ]
                    @
                    if token.expires_in = 58060800 then [ a_selected () ]
                    else [])
                  (txt "2 years");
              ];
          ];
        div
          ~a:[ a_class [ "my-6" ] ]
          [
            Utils.button_component
              ~attribs:
                [
                  a_id "update-token-button";
                  a_onclick ("updateToken('" ^ token.value ^ "')");
                ]
              ~extra_css:"w-full" ~content:(txt "Update Token")
              ~btn_type:`Primary_full ();
          ];
      ])
