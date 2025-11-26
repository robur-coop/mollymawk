let select_instance (user : User_model.user) albatross_instances
    (callback : string) =
  Tyxml_html.(
    section
      ~a:[ a_id "instance-form"; a_class [ "p-4 bg-gray-50 my-1" ] ]
      [
        p ~a:[ a_id "form-alert"; a_class [ "my-4 hidden" ] ] [];
        h2
          ~a:[ a_class [ "font-semibold text-2xl" ] ]
          [
            txt ("Select an instance for " ^ Configuration.name_to_str user.name);
          ];
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
               let name = Configuration.name_to_str instance in
               match pol.status with
               | Online ->
                   a
                     ~a:
                       [
                         a_href
                           (Middleware.construct_instance_redirect_url callback
                              instance);
                       ]
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
                                 "hover:bg-primary-700";
                                 "hover:text-primary-50";
                                 "cursor-pointer";
                               ];
                           ]
                         [
                           div
                             [
                               p ~a:[ a_class [ "font-bold" ] ] [ txt name ];
                               p
                                 ~a:
                                   [
                                     a_class
                                       [ "text-primary-500 font-semibold" ];
                                   ]
                                 [ txt (Albatross.Status.to_string pol.status) ];
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
               | _ ->
                   div
                     ~a:[ a_class [ "my-2 p-4" ] ]
                     [
                       div
                         [
                           p ~a:[ a_class [ "font-bold" ] ] [ txt name ];
                           p
                             ~a:
                               [
                                 a_class [ "text-secondary-500 font-semibold" ];
                               ]
                             [ txt (Albatross.Status.to_string pol.status) ];
                         ];
                       Utils.button_component
                         ~attribs:
                           [
                             a_id ("retry-btn-" ^ name);
                             a_onclick
                               ("retryConnectingAlbatross(`" ^ name ^ "`)");
                           ]
                         ~content:(txt "Retry connecting")
                         ~btn_type:`Danger_outlined ();
                     ])
             albatross_instances);
      ])
