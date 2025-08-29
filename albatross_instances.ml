let select_instance (user : User_model.user) albatross_instances
    (callback_url : string) =
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
             (fun instance ->
               a
                 ~a:
                   [
                     a_href (Fmt.str "%s?instance=%s" callback_url instance);
                     a_class
                       [
                         "block p-4 transition duration-150 ease-in-out \
                          hover:bg-primary-100";
                       ];
                   ]
                 [
                   div
                     ~a:[ a_class [ "flex items-center justify-between" ] ]
                     [
                       div
                         [
                           p
                             ~a:[ a_class [ "font-medium text-primary-700" ] ]
                             [ txt instance ];
                         ];
                       div
                         [
                           i
                             ~a:
                               [
                                 a_class
                                   [ "fa-solid fa-chevron-right text-gray-400" ];
                               ]
                             [];
                         ];
                     ];
                 ])
             albatross_instances);
      ])
