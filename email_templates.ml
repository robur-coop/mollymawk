let handbook_link = "https://robur-coop.github.io/mollymawk-handbook/index.html"

let link_button ~href ~text =
  let base_style =
    "display:inline-block; background-color:rgb(54 156 140); color:#ffffff; \
     font-weight:700; text-decoration:none; padding:12px 16px; \
     border-radius:8px; text-align:center;"
  in
  Tyxml_html.(a ~a:[ a_href href; a_style base_style ] [ txt text ])

let paragraph content =
  let base_style = "font-size:16px; margin:0 0 16px 0; line-height:1.6;" in
  Tyxml_html.(p ~a:[ a_style base_style ] content)

let header1 text =
  let base_style =
    "font-size:28px; font-weight:700; text-align:center; margin:0 0 24px 0;"
  in
  Tyxml_html.(h1 ~a:[ a_style base_style ] [ Tyxml_html.txt text ])

let email_footer () =
  Tyxml_html.(
    div
      [
        p
          [
            txt "Need help with Mollymawk? Read our handbook: ";
            a ~a:[ a_href handbook_link ] [ txt "Mollymawk Handbook" ];
          ];
        p
          [
            txt "Check out our blog: ";
            a ~a:[ a_href "https://blog.robur.coop" ] [ txt "Robur's Blog" ];
          ];
        p
          [
            txt "Check out our website: ";
            a ~a:[ a_href "https://robur.coop" ] [ txt "Robur's Website" ];
          ];
        p
          [
            txt "For any other inquiries, contact us at ";
            a ~a:[ a_href "team@robur.coop" ] [ txt "team@robur.coop" ];
          ];
        br ();
        p [ txt "© Robur.coop" ];
      ])

let welcome_email (user : User_model.user) verification_link
    (email_config : Utils.Email.t) =
  let verification_link =
    Fmt.str "%s/%s" email_config.mollymawk_domain verification_link
  in
  let page =
    Tyxml_html.(
      html
        (head
           (title (txt "Welcome to Mollymawk"))
           [ meta ~a:[ a_charset "UTF-8" ] () ])
        (body
           ~a:
             [
               a_style
                 "margin:0; padding:0; background-color:#f9fafb; \
                  font-family:Arial, Helvetica, sans-serif; color:#1f2937;";
             ]
           [
             div
               ~a:
                 [
                   a_style
                     "max-width:600px; margin:0 auto; \
                      background-color:#ffffff; padding:24px;";
                 ]
               [
                 header1 "Welcome to Mollymawk";
                 paragraph
                   [
                     txt "Hello ";
                     span
                       ~a:[ a_style "font-weight:700;" ]
                       [ txt (Configuration.name_to_str user.name) ];
                     txt ",";
                   ];
                 paragraph
                   [
                     txt
                       "Thank you for signing up with Mollymawk. Your account \
                        has been created successfully. An administrator will \
                        activate your account shortly. Once activated, you \
                        will be able to log in and start deploying unikernels. \
                        Please use the link below to verify your email \
                        address:";
                   ];
                 br ();
                 link_button ~href:verification_link
                   ~text:"Verify email address";
                 br ();
                 paragraph
                   [ txt "This verification link will expire in 1 hour." ];
                 br ();
                 paragraph
                   [
                     txt
                       "If the button doesn't work, copy and paste this link \
                        into your browser:";
                   ];
                 a
                   ~a:[ a_href verification_link; a_style "color:#007bff;" ]
                   [ txt verification_link ];
                 br ();
                 paragraph
                   [
                     txt
                       "You will also receive email notifications for \
                        important events, including unikernel updates.";
                   ];
                 br ();
                 email_footer ();
               ];
           ]))
  in
  Format.asprintf "%a\n" (Tyxml_html.pp ()) page

let verification_email (user : User_model.user) verification_link
    (email_config : Utils.Email.t) =
  let verification_link =
    Fmt.str "%s/%s" email_config.mollymawk_domain verification_link
  in
  let page =
    Tyxml_html.(
      html
        (head
           (title (txt "Verify your email address"))
           [ meta ~a:[ a_charset "UTF-8" ] () ])
        (body
           ~a:
             [
               a_style
                 "margin:0; padding:0; background-color:#f9fafb; \
                  font-family:Arial, Helvetica, sans-serif; color:#1f2937;";
             ]
           [
             div
               ~a:
                 [
                   a_style
                     "max-width:600px; margin:0 auto; \
                      background-color:#ffffff; padding:24px;";
                 ]
               [
                 header1 "Verify your email address";
                 paragraph
                   [
                     txt "Hello ";
                     span
                       ~a:[ a_style "font-weight:700;" ]
                       [ txt (Configuration.name_to_str user.name) ];
                     txt ",";
                   ];
                 paragraph
                   [
                     txt
                       "We received a request to verify this email address for \
                        your Mollymawk account. Please confirm your email by \
                        clicking the link below:";
                   ];
                 br ();
                 link_button ~href:verification_link
                   ~text:"Verify email address";
                 br ();
                 paragraph
                   [ txt "This verification link will expire in 1 hour." ];
                 br ();
                 paragraph
                   [
                     txt
                       "If the button doesn't work, copy and paste this link \
                        into your browser:";
                   ];
                 a ~a:[ a_href verification_link ] [ txt verification_link ];
                 paragraph
                   [
                     txt
                       "If you didn't request this email, you can safely \
                        ignore it; no changes will be made to your account.";
                   ];
                 br ();
                 email_footer ();
               ];
           ]))
  in
  Format.asprintf "%a\n" (Tyxml_html.pp ()) page

let updated_unikernels (updates : Update_flow.user_unikernel_available_updates)
    (email_config : Utils.Email.t) =
  let updates_table () =
    Tyxml_html.(
      let cell_style =
        "border:1px solid #d1d5db; padding:8px 12px; text-align:left;"
      in
      let header_cell_style =
        "border:1px solid #d1d5db; padding:10px 12px; \
         background-color:#f9fafb; font-weight:700; text-align:left;"
      in

      let header =
        thead
          [
            tr
              [
                th ~a:[ a_style header_cell_style ] [ txt "Instance" ];
                th ~a:[ a_style header_cell_style ] [ txt "Unikernel" ];
                th ~a:[ a_style header_cell_style ] [ txt "Action" ];
              ];
          ]
      in

      let rows =
        updates.available_updates
        |> List.concat_map (fun (instance, unikernels) ->
            unikernels
            |> List.map (fun (name, _info, _comparison) ->
                let unikernel_name =
                  Option.value ~default:"no name"
                    (Option.map Vmm_core.Name.Label.to_string
                       (Vmm_core.Name.name name))
                in
                tr
                  [
                    td
                      ~a:[ a_style cell_style ]
                      [ txt (Configuration.name_to_str instance) ];
                    td ~a:[ a_style cell_style ] [ txt unikernel_name ];
                    td
                      ~a:[ a_style cell_style ]
                      [
                        a
                          ~a:
                            [
                              a_href
                                (Fmt.str
                                   "%s/unikernel/update/compare-changes?instance=%s&unikernel=%s"
                                   email_config.mollymawk_domain
                                   (Configuration.name_to_str instance)
                                   unikernel_name);
                              a_target "_blank";
                            ]
                          [ txt "View changes" ];
                      ];
                  ]))
      in

      table
        ~a:
          [
            a_style
              "width:100%; border-collapse:collapse; \
               font-family:Arial,sans-serif; font-size:14px;";
          ]
        ~thead:header rows)
  in
  let page =
    Tyxml_html.(
      html
        (head
           (title (txt "Mollymawk Unikernel Updates Available"))
           [ meta ~a:[ a_charset "UTF-8" ] () ])
        (body
           ~a:
             [
               a_style
                 "margin:0; padding:0; background-color:#f9fafb; \
                  font-family:Arial, Helvetica, sans-serif; color:#1f2937;";
             ]
           [
             div
               ~a:
                 [
                   a_style
                     "max-width:600px; margin:0 auto; \
                      background-color:#ffffff; padding:24px;";
                 ]
               [
                 header1 "Unikernel Updates Available";
                 paragraph
                   [
                     txt "Hello ";
                     span
                       ~a:[ a_style "font-weight:700;" ]
                       [ txt (Configuration.name_to_str updates.user.name) ];
                     txt ",";
                   ];
                 paragraph
                   [ txt "The following unikernels have updates available:" ];
                 updates_table ();
                 br ();
                 email_footer ();
               ];
           ]))
  in
  Format.asprintf "%a\n" (Tyxml_html.pp ()) page
