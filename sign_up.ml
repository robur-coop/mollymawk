open Tyxml

let register_page ~csrf ~icon =
  let page =
    Html.(
      html
        (Header_layout.header ~page_title:"Sign up | Mollymawk" ~icon ())
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
                     Utils.csrf_form_input csrf;
                     div
                       ~a:[ a_class [ "w-full max-w-lg mt-16 pb-16 mx-auto" ] ]
                       [
                         div
                           ~a:
                             [
                               a_id "alert-container";
                               a_class
                                 [
                                   "absolute top-1/4 rounded-md right-4 z-50 \
                                    w-fit space-y-2 p-4 shadow text-wrap \
                                    hidden";
                                 ];
                             ]
                           [];
                         h1
                           ~a:
                             [
                               a_class
                                 [
                                   "font-semibold text-2xl md:text-3xl mb-8 \
                                    text-primary-800 p-6";
                                 ];
                             ]
                           [ txt "Sign up for an Account" ];
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
                                       a_label_for "name";
                                     ]
                                   [ txt "Name*" ];
                                 input
                                   ~a:
                                     [
                                       a_autocomplete `On;
                                       a_autofocus ();
                                       a_input_type `Text;
                                       a_name "name";
                                       a_id "name";
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
                                 p ~a:[ a_id "name-alert" ] [];
                               ];
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
                                       a_href "/sign-in";
                                       a_class
                                         [
                                           "hover:text-primary-800 \
                                            transition-colors cursor-pointer";
                                         ];
                                     ]
                                   [ txt "Have an account?" ];
                               ];
                             div
                               [
                                 button
                                   ~a:
                                     [
                                       a_id "register-button";
                                       a_class
                                         [
                                           "py-3 rounded bg-primary-500 \
                                            hover:bg-primary-800 w-full \
                                            text-gray-50 font-semibold";
                                         ];
                                     ]
                                   [ txt "Create Account" ];
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
               "\n\
               \              <script>\n\
               \                const registerButton = \
                document.getElementById('register-button')\n\
               \                registerButton.addEventListener('click', async \
                function() {\n\
                const form_csrf = document.getElementById('molly-csrf').value\n\
               \               const name = \
                document.getElementById('name').value\n\
               \                    const email = \
                document.getElementById('email').value\n\
               \                    const password = \
                document.getElementById('password').value\n\
               \                    let form_alert = \
                document.getElementById('form-alert')\n\
               \                    let name_alert = \
                document.getElementById('name-alert')\n\
               \                    let password_alert = \
                document.getElementById('password-alert')\n\n\
               \                    form_alert.classList.add('hidden')\n\
               \                    name_alert.classList.add('hidden')\n\
               \                    password_alert.classList.add('hidden')\n\n\
               \                   if (!name || !email || !password) {\n\
               \                        form_alert.classList.remove('hidden')\n\
               \                        \
                form_alert.classList.add('text-secondary-500', 'block')\n\
               \                        form_alert.textContent = 'All fields \
                are required'\n\
               \                        return;\n\
               \                    }\n\n\
               \                    if (name.length < 4) {\n\
               \                        name_alert.classList.remove('hidden')\n\
               \                        \
                name_alert.classList.add('text-secondary-500', 'block')\n\
               \                        name_alert.textContent = 'Name must be \
                at least 4 characters.'\n\
               \                        return;\n\
               \                    }\n\n\
               \                    if (password.length < 8) {\n\
               \                        \
                password_alert.classList.remove('hidden')\n\
               \                        \
                password_alert.classList.add('text-secondary-500', 'block')\n\
               \                        password_alert.textContent = 'Password \
                must be at least 8 characters long.'\n\
               \                        return;\n\
               \                    }\n\n\
               \                    try {\n\
               \                        const response = await fetch \
                ('/api/register', {\n\
               \                            method: 'POST',\n\
               \                            headers: {\n\
               \                                'Content-Type': \
                'application/json',\n\
               \                            },\n\
               \                            body: JSON.stringify({ name, \
                email, password, form_csrf })\n\
               \                        })\n\
               \                        const data = await response.json();\n\
               \                        if (data.status === 200) {\n\
               \                           postAlert('bg-primary-300', \
                'Account created. Waiting for activation by an administrator.')\n\
               \                          setTimeout(function () \
                {window.location.replace('/dashboard')}, 3000);\n\
               \                        } else {\n\
               \                            \
                form_alert.classList.remove('hidden')\n\
               \                            \
                form_alert.classList.add('text-secondary-500', 'block')\n\
               \                            form_alert.textContent = data.data\n\
               \                        }\n\
               \                    } catch (error) {\n\
               \                        form_alert.classList.remove('hidden')\n\
               \                        \
                form_alert.classList.add('text-secondary-500', 'block')\n\
               \                        form_alert.textContent = error\n\
               \                    }\n\
               \                })\n\
               \            </script>";
           ]))
  in
  Format.asprintf "%a" (Html.pp ()) page
