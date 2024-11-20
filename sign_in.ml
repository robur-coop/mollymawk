open Tyxml

let login_page ~icon () =
  let page =
    Html.(
      html
        (Header_layout.header ~page_title:"Sign in | Mollymawk" ~icon ())
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
               ~a:
                 [
                   a_class
                     [
                       "w-full grid md:grid-cols-3 grid-cols-1 text-gray-700 \
                        md:h-screen";
                     ];
                 ]
               [
                 section
                   ~a:
                     [
                       a_class
                         [
                           "h-full flex justify-center items-center col-span-2 \
                            md:py-10 py-5";
                         ];
                     ]
                   [
                     div
                       ~a:
                         [
                           a_id "alert-container";
                           a_class
                             [
                               "absolute top-1/4 rounded-md right-4 z-50 w-fit \
                                space-y-2 p-4 shadow text-wrap hidden";
                             ];
                         ]
                       [];
                     div
                       ~a:[ a_class [ "w-full max-w-lg mt-16 pb-16 mx-auto" ] ]
                       [
                         h1
                           ~a:
                             [
                               a_class
                                 [
                                   "font-semibold text-2xl md:text-3xl mb-8 \
                                    text-primary-800 p-6";
                                 ];
                             ]
                           [ txt "Sign in to Your Account" ];
                         div
                           ~a:[ a_class [ "space-y-6 mt-8 shadow-md p-6" ] ]
                           [
                             p ~a:[ a_id "form-alert" ] [];
                             div
                               [
                                 label
                                   ~a:
                                     [
                                       a_class [ "block text-sm font-medium" ];
                                       a_label_for "email";
                                     ]
                                   [ txt "Email Address*" ];
                                 input
                                   ~a:
                                     [
                                       a_autocomplete `On;
                                       a_input_type `Text;
                                       a_name "email";
                                       a_id "email";
                                       a_class
                                         [
                                           "ring-primary-100 mt-1.5 transition \
                                            appearance-none block w-full px-3 \
                                            py-3 rounded-xl shadow-sm border \
                                            hover:border-primary-200\n\
                                           \                                           \
                                            focus:border-primary-300 \
                                            bg-primary-50 bg-opacity-0 \
                                            hover:bg-opacity-50 \
                                            focus:bg-opacity-50 \
                                            ring-primary-200 \
                                            focus:ring-primary-200\n\
                                           \                                           \
                                            focus:ring-[1px] \
                                            focus:outline-none";
                                         ];
                                     ]
                                   ();
                               ];
                             div
                               [
                                 label
                                   ~a:
                                     [
                                       a_class [ "block text-sm font-medium" ];
                                       a_label_for "password";
                                     ]
                                   [ txt "Password*" ];
                                 input
                                   ~a:
                                     [
                                       a_input_type `Password;
                                       a_name "password";
                                       a_id "password";
                                       a_class
                                         [
                                           "ring-primary-100 mt-1.5 transition \
                                            appearance-none block w-full px-3 \
                                            py-3 rounded-xl shadow-sm border \
                                            hover:border-primary-200\n\
                                           \                                           \
                                            focus:border-primary-300 \
                                            bg-primary-50 bg-opacity-0 \
                                            hover:bg-opacity-50 \
                                            focus:bg-opacity-50 \
                                            ring-primary-200 \
                                            focus:ring-primary-200\n\
                                           \                                           \
                                            focus:ring-[1px] \
                                            focus:outline-none";
                                         ];
                                     ]
                                   ();
                                 p ~a:[ a_id "password-alert" ] [];
                               ];
                             div
                               ~a:
                                 [
                                   a_class
                                     [
                                       "flex items-center justify-between \
                                        text-sm font-medium text-primary-500";
                                     ];
                                 ]
                               [
                                 a
                                   ~a:
                                     [
                                       a_class
                                         [
                                           "hover:text-primary-800 \
                                            transition-colors cursor-pointer";
                                         ];
                                     ]
                                   [ txt "Forgot Your Password?" ];
                                 a
                                   ~a:
                                     [
                                       a_href "/sign-up";
                                       a_class
                                         [
                                           "hover:text-primary-800 \
                                            transition-colors cursor-pointer";
                                         ];
                                     ]
                                   [ txt "Need an account?" ];
                               ];
                             div
                               [
                                 Utils.button_component
                                   ~attribs:[ a_id "login-button" ]
                                   ~content:(txt "Sign In")
                                   ~btn_type:`Primary_full ();
                               ];
                           ];
                       ];
                   ];
                 aside
                   ~a:
                     [
                       a_class
                         [ "relative h-full p-16 col-span-1 md:block hidden" ];
                     ]
                   [
                     img ~src:"/images/molly_bird.jpeg"
                       ~alt:"Mollymawk delivering unikernels"
                       ~a:
                         [
                           a_class
                             [
                               "absolute inset-1 max-w-none w-full h-full \
                                object-cover";
                             ];
                         ]
                       ();
                   ];
               ];
             Footer_layout.footer;
             Tyxml.Html.Unsafe.data
               "<script>\n\
               \                  const loginButton = \
                document.getElementById('login-button')\n\
               \                  loginButton.addEventListener('click', async \
                function() {\n\
               \                    const email = \
                document.getElementById('email').value\n\
               \                    const password = \
                document.getElementById('password').value\n\
               \                    let form_alert = \
                document.getElementById('form-alert')\n\
               \                    let password_alert = \
                document.getElementById('password-alert')\n\
               \                    form_alert.classList.add('hidden')\n\
               \                    password_alert.classList.add('hidden')\n\
               \                    if (!email || !password) {\n\
               \                      form_alert.classList.remove('hidden')\n\
               \                      \
                form_alert.classList.add('text-secondary-500', 'block')\n\
               \                      form_alert.textContent = 'All fields are \
                required'\n\
               \                      return;\n\
               \                    }\n\
               \                    if (password.length < 8) {\n\
               \                      password_alert.classList.remove('hidden')\n\
               \                      \
                password_alert.classList.add('text-secondary-500', 'block')\n\
               \                      password_alert.textContent = 'Password \
                must be at least 8 characters long.'\n\
               \                      return;\n\
               \                    }\n\
               \                    try {\n\
               \                      const response = await \
                fetch('/api/login', {\n\
               \                                           method: 'POST',\n\
               \                                           headers: {\n\
               \                                            'Content-Type': \
                'application/json',\n\
               \                                           },\n\
               \                                           body: \
                JSON.stringify({email, password })\n\
               \                                        })\n\
               \                      const data = await response.json();\n\
               \                      if (data.status === 200) {\n\
               \                        window.location.replace('/dashboard')\n\
               \                      } else {\n\
               \                        form_alert.classList.remove('hidden')\n\
               \                        \
                form_alert.classList.add('text-secondary-500', 'block')\n\
               \                        form_alert.textContent = data.data\n\
               \                      }\n\
               \                    } catch (error) {\n\
               \                          form_alert.classList.remove('hidden')\n\
               \                          \
                form_alert.classList.add('text-secondary-500', 'block')\n\
               \                          form_alert.textContent = error\n\
               \                          return;\n\
               \                      }})\n\
               \                </script>";
           ]))
  in
  Format.asprintf "%a" (Html.pp ()) page
