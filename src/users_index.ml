let users_index_layout (users : Models.User.user Utils.UM.t) current_time =
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        div
          ~a:[ a_class [ "px-3 flex justify-between items-center" ] ]
          [
            div
              [
                p
                  ~a:[ a_class [ "font-bold text-gray-700" ] ]
                  [
                    txt
                      ("Users (" ^ string_of_int (Utils.UM.cardinal users) ^ ")");
                  ];
              ];
            div [ Ui.search_input () ];
          ];
        hr ~a:[ a_class [ "border border-primary-500 my-5" ] ] ();
        Ui.data_table
          ~headers:
            [
              Ui.sortable_th "Name";
              Ui.sortable_th "Email";
              Ui.sortable_th "Created";
              Ui.sortable_th "Last Modified";
              Ui.static_th "Action";
            ]
          (Utils.UM.fold
             (fun _ (user : Models.User.user) acc ->
               tr
                 ~a:[ a_class [ "border-b border-gray-200" ] ]
                 [
                   Ui.td_cell
                     [
                       div
                         ~a:[ a_class [ "uppercase" ] ]
                         [
                           div
                             ~a:
                               [
                                 a_class
                                   [
                                     "flex justify-start space-x-1 items-center";
                                   ];
                               ]
                             [
                               p
                                 [
                                   txt
                                     (Models.Configuration.name_to_str user.name);
                                 ];
                               (match user.active with
                               | true ->
                                   i
                                     ~a:
                                       [
                                         a_class
                                           [
                                             "text-primary-500 fa-solid \
                                              fa-check";
                                           ];
                                       ]
                                     []
                               | false ->
                                   i
                                     ~a:
                                       [
                                         a_class
                                           [
                                             "text-secondary-500 fa-solid fa-x";
                                           ];
                                       ]
                                     []);
                             ];
                           (if user.super_user then
                              i
                                ~a:
                                  [
                                    a_class
                                      [ "text-primary-500 lowercase text-sm" ];
                                  ]
                                [ txt "admin" ]
                            else p []);
                         ];
                     ];
                   Ui.td_cell
                     [
                       div
                         ~a:
                           [
                             a_class
                               [
                                 "flex-col justify-start space-x-1 items-center";
                               ];
                           ]
                         [
                           p [ txt (Emile.to_string user.email) ];
                           (match user.email_verified with
                           | Some _ ->
                               i
                                 ~a:[ a_class [ "text-primary-500" ] ]
                                 [ txt "verified" ]
                           | None ->
                               i
                                 ~a:[ a_class [ "text-secondary-500" ] ]
                                 [ txt "not verified" ]);
                         ];
                     ];
                   Ui.td_txt
                     (Utils.TimeHelper.time_ago ~current_time
                        ~check_time:user.created_at);
                   Ui.td_txt
                     (Utils.TimeHelper.time_ago ~current_time
                        ~check_time:user.updated_at);
                   Ui.td_cell
                     [
                       Ui.link_button
                         ~href:
                           ("/admin/user/profile?uuid="
                          ^ Uuidm.to_string user.uuid)
                         ~content:(txt "View") ~btn_type:`Primary_outlined ();
                     ];
                 ]
               :: acc)
             users []);
      ])
