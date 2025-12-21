let handbook_link = "https://robur-coop.github.io/mollymawk-handbook/index.html"

let link_button ~href ~text =
  let base_style =
    "display:inline-block; background-color:rgb(54 156 140); color:#ffffff; \
     font-weight:700; text-decoration:none; padding:12px 16px; \
     border-radius:8px;"
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

let welcome_email (user : User_model.user) verification_link =
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
               [ header1 "Welcome to Mollymawk" ];
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
                   "Thank you for signing up for Mollymawk. Your account has \
                    been created successfully. An administrator will activate \
                    your account shortly. Once activated, you will be able to \
                    log in and start deploying unikernels. Please use the link \
                    below to verify your email address:";
               ];
             link_button ~href:verification_link ~text:"Verify email address";
             paragraph
               [
                 txt
                   "You will also receive email notifications for important \
                    events, including unikernel updates.";
               ];
             paragraph
               [
                 txt
                   "To get started and learn more about using Mollymawk, \
                    please refer to our handbook: ";
                 link_button ~href:handbook_link ~text:"Mollymawk Handbook.";
               ];
             hr
               ~a:
                 [
                   a_style
                     "border:none; border-top:1px solid #e5e7eb; margin:32px 0;";
                 ]
               ();
             paragraph
               [
                 txt
                   "If you have any questions or run into issues, feel free to \
                    reply to this email. We’re happy to help.";
               ];
             paragraph [ txt "Robur.coop" ];
           ]))
  in
  Format.asprintf "%a\n" (Tyxml_html.pp ()) page

let verification_email (user : User_model.user) verification_link =
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
                 paragraph
                   [
                     link_button ~href:verification_link
                       ~text:"Verify email address";
                   ];
                 paragraph
                   [
                     txt
                       "If the button doesn't work, copy and paste this link \
                        into your browser:";
                   ];
                 paragraph
                   [
                     link_button ~href:verification_link ~text:verification_link;
                   ];
                 paragraph
                   [
                     txt
                       "If you didn't request this email, you can safely \
                        ignore it; no changes will be made to your account.";
                   ];
                 hr
                   ~a:
                     [
                       a_style
                         "border:none; border-top:1px solid #e5e7eb; \
                          margin:28px 0;";
                     ]
                   ();
                 paragraph
                   [
                     txt "Need help getting started? Read our handbook: ";
                     link_button ~href:handbook_link ~text:"Mollymawk Handbook";
                     txt ".";
                   ];
                 paragraph [ txt "Robur.coop" ];
               ];
           ]))
  in
  Format.asprintf "%a\n" (Tyxml_html.pp ()) page
