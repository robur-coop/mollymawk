let construct_instance_redirect_url callback instance_name =
  let separator = if String.contains callback '?' then "&" else "?" in
  Fmt.str "%s%sinstance=%s" callback separator
    (Models.Configuration.name_to_str instance_name)

let select_instance (user : Models.User.user) albatross_instances
    (callback : string) =
  Tyxml_html.(
    section
      ~a:[ a_id "instance-form"; a_class [ "p-4 bg-gray-50 my-1" ] ]
      [
        p ~a:[ a_id "form-alert"; a_class [ "my-4 hidden" ] ] [];
        h2
          ~a:[ a_class [ "font-semibold text-2xl" ] ]
          [
            txt
              ("Select an instance for "
              ^ Models.Configuration.name_to_str user.name);
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
          (Utils.LM.fold
             (fun instance (pol : Albatross_client.t) acc ->
               let name = Models.Configuration.name_to_str instance in
               let row =
                 match pol.status with
                 | Online ->
                     a
                       ~a:
                         [
                           a_href
                             (construct_instance_redirect_url callback instance);
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
                                   [
                                     txt
                                       (Albatross_client.Status.to_string
                                          pol.status);
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
                                   a_class
                                     [ "text-secondary-500 font-semibold" ];
                                 ]
                               [
                                 txt
                                   (Albatross_client.Status.to_string pol.status);
                               ];
                           ];
                         Ui.button_component
                           ~attribs:
                             [
                               a_id ("retry-btn-" ^ name);
                               a_onclick
                                 ("retryConnectingAlbatross(`" ^ name ^ "`)");
                             ]
                           ~content:(txt "Retry connecting")
                           ~btn_type:`Danger_outlined ();
                       ]
               in
               row :: acc)
             albatross_instances []);
      ])
