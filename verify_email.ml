open Tyxml

let verify_page ~csrf ~icon (user : User_model.user) =
  let page =
    Html.(
      html
        (Header_layout.header ~page_title:"Verify Email" ~icon ())
        (body
           [
             section
               ~a:[ a_class [ "max-w-7xl relative px-5 mx-auto" ] ]
               [
                 div
                   ~a:[ a_class [ "absolute" ] ]
                   [
                     img
                       ~a:[ a_class [ "md:w-20 w-14" ] ]
                       ~src:"/images/robur.png" ~alt:"Robur.coop" ();
                   ];
               ];
             main
               [
                 section
                   ~a:[ a_class [ "my-5 text-center" ] ]
                   [
                     div
                       ~a:[ a_class [ "flex justify-center" ] ]
                       [
                         div
                           ~a:
                             [
                               a_class
                                 [
                                   "rounded-full bg-primary-200 w-32 h-32 flex \
                                    justify-center items-center";
                                 ];
                             ]
                           [
                             i
                               ~a:
                                 [
                                   a_class
                                     [
                                       "fa-solid fa-envelope text-7xl \
                                        text-primary-500";
                                     ];
                                 ]
                               [];
                           ];
                       ];
                     (match user.email_verified with
                     | Some _ ->
                         div
                           [
                             div
                               ~a:[ a_class [ "my-5" ] ]
                               [
                                 Utils.csrf_form_input csrf;
                                 h1
                                   ~a:[ a_class [ "text-3xl font-bold" ] ]
                                   [ txt "Email Verified!" ];
                                 p
                                   [
                                     txt
                                       "Your email is already verified. You \
                                        can proceed to your dashboard.";
                                   ];
                               ];
                             div
                               ~a:[ a_class [ "my-10" ] ]
                               [
                                 a
                                   ~a:
                                     [
                                       a_href "/dashboard";
                                       a_class
                                         [
                                           "py-3 px-2 bg-primary-500 rounded \
                                            text-gray-100 font-semibold \
                                            cursor-pointer";
                                         ];
                                     ]
                                   [ txt "Go to Dashboard" ];
                               ];
                           ]
                     | None ->
                         div
                           [
                             div
                               ~a:[ a_class [ "my-5" ] ]
                               [
                                 Utils.csrf_form_input csrf;
                                 h1
                                   ~a:[ a_class [ "text-3xl font-bold" ] ]
                                   [ txt "Please verify your email" ];
                                 p
                                   [
                                     txt
                                       "You're almost there! We sent an email \
                                        to";
                                   ];
                                 p
                                   ~a:[ a_class [ "font-bold text-xl" ] ]
                                   [ txt (Emile.to_string user.email) ];
                               ];
                             div
                               ~a:[ a_class [ "my-3" ] ]
                               [
                                 p
                                   [
                                     txt
                                       "Just click on the link in that email \
                                        to complete your signup.";
                                   ];
                                 p
                                   [
                                     txt "If you don't see it, you may need to";
                                     span
                                       ~a:[ a_class [ "font-bold" ] ]
                                       [ txt " check your spam " ];
                                     txt "folder.";
                                   ];
                               ];
                             div
                               ~a:[ a_class [ "my-3" ] ]
                               [
                                 p
                                   [
                                     txt
                                       "Still can't find the email? No problem.";
                                   ];
                               ];
                             div
                               ~a:[ a_class [ "my-10" ] ]
                               [
                                 a
                                   ~a:
                                     [
                                       a_href "/verify-email";
                                       a_class
                                         [
                                           "py-3 px-2 bg-primary-500 rounded \
                                            text-gray-100 font-semibold \
                                            cursor-pointer";
                                         ];
                                     ]
                                   [ txt "Resend Verification Email" ];
                               ];
                           ]);
                   ];
               ];
             Footer_layout.footer;
             Tyxml.Html.Unsafe.data "";
           ]))
  in
  Format.asprintf "%a" (Html.pp ()) page
