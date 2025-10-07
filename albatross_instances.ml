let select_instance (user : User_model.user) albatross_instances
    (callback : string) =
  Tyxml_html.(
    section
      ~a:[ a_id "instance-form"; a_class [ "p-4 bg-gray-50 my-1" ] ]
      [
        h2
          ~a:[ a_class [ "font-semibold text-2xl" ] ]
          [ txt ("Select an instance for " ^ user.name) ];
        p
          ~a:[ a_class [ "text-gray-600 my-2" ] ]
          [ txt "Please choose the Albatross instance for this operation." ];
        div
          ~a:
            [
              a_class
                [
                  "mt-6 border-t border-b border-gray-200 divide-y \
                   divide-gray-200";
                ];
            ]
          (List.map
             (fun (instance, (pol : Albatross.t)) ->
               let url =
                 a_href
                   (Middleware.construct_instance_redirect_url callback instance)
               in
               match pol.error with
               | Some err ->
                   a
                     ~a:
                       [
                         url;
                         a_class
                           [
                             "block";
                             "p-4";
                             "transition";
                             "duration-150";
                             "ease-in-out";
                             "bg-gray-100";
                             "cursor-not-allowed";
                             "pointer-events-none";
                           ];
                         Unsafe.string_attrib "aria-disabled" "true";
                         a_tabindex (-1);
                       ]
                     [
                       div
                         ~a:
                           [
                             a_class
                               [ "flex"; "items-center"; "justify-between" ];
                           ]
                         [
                           div
                             [
                               p
                                 ~a:
                                   [
                                     a_class
                                       [ "font-medium"; "text-secondary-500" ];
                                   ]
                                 [
                                   txt (Configuration.name_to_str instance);
                                   br ();
                                   small
                                     [
                                       txt
                                         ("Can't access this albatross \
                                           instance: " ^ err);
                                     ];
                                 ];
                             ];
                           div
                             [
                               i
                                 ~a:
                                   [
                                     a_class
                                       [
                                         "fa-solid";
                                         "fa-chevron-right";
                                         "text-gray-400";
                                       ];
                                   ]
                                 [];
                             ];
                         ];
                     ]
               | None ->
                   a ~a:[ url ]
                     [
                       div
                         ~a:
                           [
                             a_class
                               [
                                 "flex";
                                 "items-center";
                                 "justify-between";
                                 "block";
                                 "p-4";
                                 "transition";
                                 "duration-150";
                                 "ease-in-out";
                               ];
                           ]
                         [
                           div
                             [
                               p
                                 ~a:
                                   [
                                     a_class
                                       [ "font-medium"; "text-primary-500" ];
                                   ]
                                 [ txt (Configuration.name_to_str instance) ];
                             ];
                           div
                             [
                               i
                                 ~a:
                                   [
                                     a_class
                                       [
                                         "fa-solid";
                                         "fa-chevron-right";
                                         "text-gray-400";
                                       ];
                                   ]
                                 [];
                             ];
                         ];
                     ])
             albatross_instances);
      ])
