open Tyxml

let register_page =
  let page =
    Html.(
      html
        (Header_layout.header ~page_title:"Sign up | Mollymawk" ())

        (body
            [
              section ~a:[a_class["max-w-7xl relative px-5 mx-auto"]] [
                div ~a:[a_class["absolute"]] [
                  img ~a:[a_class["md:w-20 w-14"]] ~src:"/images/robur.png" ~alt:"Robur.coop" ()
                ]
              ]
             ; main ~a:[a_class["w-full grid grid-cols-3 text-gray-700 md:h-screen"]]
                [section ~a:[a_class["h-full flex justify-center items-center col-span-2 md:py-10 py-5"]] [
                  div ~a:[a_class["w-full max-w-lg mt-16 pb-16 mx-auto"]] [
                    h1 ~a:[a_class["font-semibold text-2xl md:text-3xl mb-8 text-primary-800 p-6"]] [txt "Sign up for an Account"]
                  ; div ~a:[a_class["space-y-6 mt-8 shadow-md p-6"]][
                      div [
                        label ~a:[a_class["block text-sm font-medium"]; a_label_for "name"][txt "Name"]
                      ; input ~a:[
                                  a_autocomplete `On;
                                  a_autofocus ();
                                  a_input_type `Text;
                                  a_name "name";
                                  a_id "name";
                                  a_class["ring-primary-100 mt-1.5 transition appearance-none block w-full px-3 py-3 rounded-xl shadow-sm border hover:border-primary-200
                                           focus:border-primary-300 bg-primary-50 bg-opacity-0 hover:bg-opacity-50 focus:bg-opacity-50 ring-primary-200 focus:ring-primary-200
                                           focus:ring-[1px] focus:outline-none"]] ()
                      ]
                     ; div [
                        label ~a:[a_class["block text-sm font-medium"]; a_label_for "email"][txt "Email Address"]
                      ; input ~a:[
                                  a_autocomplete `On;
                                  a_input_type `Text;
                                  a_name "email";
                                  a_id "email";
                                  a_class["ring-primary-100 mt-1.5 transition appearance-none block w-full px-3 py-3 rounded-xl shadow-sm border hover:border-primary-200
                                           focus:border-primary-300 bg-primary-50 bg-opacity-0 hover:bg-opacity-50 focus:bg-opacity-50 ring-primary-200 focus:ring-primary-200
                                           focus:ring-[1px] focus:outline-none"]] ()
                      ]
                      ; div [
                        label ~a:[a_class["block text-sm font-medium"]; a_label_for "password"][txt "Password"]
                      ; input ~a:[
                                  a_input_type `Password;
                                  a_name "password";
                                  a_id "password";
                                  a_class["ring-primary-100 mt-1.5 transition appearance-none block w-full px-3 py-3 rounded-xl shadow-sm border hover:border-primary-200
                                           focus:border-primary-300 bg-primary-50 bg-opacity-0 hover:bg-opacity-50 focus:bg-opacity-50 ring-primary-200 focus:ring-primary-200
                                           focus:ring-[1px] focus:outline-none"]] ()
                      ]
                    ; div ~a:[a_class["flex items-center justify-between text-sm font-medium text-primary-500"]][
                        a ~a:[a_class["hover:text-primary-800 transition-colors cursor-pointer"]][txt "Forgot Your Password?"]
                      ;  a ~a:[a_class["hover:text-primary-800 transition-colors cursor-pointer"]][txt "Have an account?"]
                    ]
                    ; div [
                        button ~a:[a_class["py-3 rounded bg-primary-500 hover:bg-primary-800 w-full text-gray-50 font-semibold"]][txt "Create Account"]
                    ]
                  ]
                  ]
                ]
                ; aside ~a:[a_class["relative h-full p-16 col-span-1"]] [
                  img ~src:"/images/molly_bird.jpeg" ~alt:"Mollymawk delivering unikernels" ~a:[a_class["absolute inset-1 max-w-none w-full h-full object-cover"]]()
                ]]
            ; Footer_layout.footer
           ]))
    in
    Format.asprintf "%a" (Html.pp()) (page)
