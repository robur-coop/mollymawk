type users_tab = Profile | Unikernels | Policy

let tab_style is_active =
  if is_active then
    "px-6 py-2.5 text-sm font-medium text-primary-50 bg-primary-500 \
     rounded-t-lg border-b-0"
  else
    "px-6 py-2.5 text-sm font-medium text-primary-600 bg-gray-100 \
     hover:bg-primary-50 rounded-t-lg transition-colors duration-200"

let policy_row ?(error = "") instance_name policy (user : User_model.user) =
  Tyxml_html.(
    tr
      [
        td
          ~a:
            [
              a_class
                [
                  "px-6 py-1 whitespace-nowrap text-sm font-medium \
                   text-gray-800";
                ];
            ]
          [
            txt (Configuration.name_to_str instance_name);
            p ~a:[ a_class [ "text-red-500 text-sm" ] ] [ txt error ];
          ];
        td
          ~a:
            [
              a_class
                [
                  "px-6 py-1 whitespace-nowrap text-sm font-medium \
                   text-gray-800";
                ];
            ]
          [ txt (string_of_int policy.Vmm_core.Policy.unikernels) ];
        td
          ~a:
            [
              a_class
                [
                  "px-6 py-1 whitespace-normal text-sm font-medium \
                   text-gray-800";
                ];
            ]
          [ txt (string_of_int policy.memory ^ " MB") ];
        td
          ~a:
            [
              a_class
                [
                  "px-6 py-1 whitespace-normal text-sm font-medium \
                   text-gray-800";
                ];
            ]
          [ txt (string_of_int (Option.value policy.block ~default:0) ^ " MB") ];
        td
          ~a:
            [
              a_class
                [
                  "px-6 py-1 whitespace-normal text-sm font-medium \
                   text-gray-800";
                ];
            ]
          [
            txt
              (String.concat ", "
                 (List.map string_of_int (Vmm_core.IS.elements policy.cpuids)));
          ];
        td
          ~a:
            [
              a_class
                [
                  "px-6 py-1 whitespace-normal text-sm font-medium \
                   text-gray-800";
                ];
            ]
          [
            txt
              (String.concat ", "
                 (List.map string_of_uri
                    (Vmm_core.String_set.elements policy.bridges)));
          ];
        td
          ~a:
            [
              a_class
                [
                  "px-6 py-4 whitespace-nowrap text-sm font-medium \
                   text-gray-800";
                ];
            ]
          [
            a
              ~a:
                [
                  a_href
                    ("/admin/u/policy/edit?uuid=" ^ user.uuid ^ "&instance="
                    ^ Configuration.name_to_str instance_name);
                  a_class
                    [
                      "border border-primary-500 hover:bg-primary-700 px-2 \
                       py-1 text-primary-800 hover:text-primary-50 rounded";
                    ];
                ]
              [ txt "Edit" ];
          ];
      ])

let user_profile (user : User_model.user) =
  let user_name = Configuration.name_to_str user.name in
  Tyxml_html.(
    section
      ~a:[ a_class [ "p-4 bg-gray-50 my-1" ] ]
      [
        div
          ~a:[ a_id "account"; a_class [ "max-w-3xl" ] ]
          [
            div
              ~a:[ a_class [ "flex-col justify-center text-center" ] ]
              [
                i ~a:[ a_class [ "fa-solid fa-circle-user text-7xl" ] ] [];
                p
                  ~a:[ a_class [ "text-3xl font-semibold uppercase" ] ]
                  [ txt user_name ];
              ];
            div
              ~a:[ a_class [ "grid grid-cols-2 my-4" ] ]
              [
                label [ txt "Email" ];
                p
                  ~a:[ a_class [ "rounded border py-2 px-4 w-full" ] ]
                  [
                    txt (Mrmime.Mailbox.to_string user.email);
                    (match user.email_verified with
                    | Some _ptime ->
                        span
                          [
                            i ~a:[ a_class [ "fa-solid fa-check" ] ] [];
                            i ~a:[ a_class [ "text-xs" ] ] [];
                          ]
                    | None ->
                        i
                          ~a:[ a_class [ "fa-solid fa-x text-secondary-500" ] ]
                          []);
                  ];
                label [ txt "Created at" ];
                p
                  ~a:[ a_class [ "rounded border py-2 px-4 w-full" ] ]
                  [ txt (Utils.TimeHelper.string_of_ptime user.created_at) ];
                label [ txt "Last Update" ];
                p
                  ~a:[ a_class [ "rounded border py-2 px-4 w-full" ] ]
                  [ txt (Utils.TimeHelper.string_of_ptime user.updated_at) ];
                label [ txt "Email verified at" ];
                p
                  ~a:[ a_class [ "rounded border py-2 px-4 w-full" ] ]
                  [
                    txt
                      (match user.email_verified with
                      | None -> "not verified"
                      | Some t -> Utils.TimeHelper.string_of_ptime t);
                  ];
                div
                  ~a:[ a_class [ "flex justify-center space-x-4 my-4" ] ]
                  [
                    Utils.button_component
                      ~attribs:[ a_onclick ("deleteUser('" ^ user.uuid ^ "')") ]
                      ~content:(txt "Delete User") ~btn_type:`Danger_full ();
                    (if user.active then
                       Utils.button_component
                         ~attribs:
                           [
                             a_onclick
                               ("toggleUserActiveStatus('" ^ user.uuid ^ "')");
                           ]
                         ~content:(txt "Deactivate") ~btn_type:`Danger_full ()
                     else
                       Utils.button_component
                         ~attribs:
                           [
                             a_onclick
                               ("toggleUserActiveStatus('" ^ user.uuid ^ "')");
                           ]
                         ~content:(txt "Activate") ~btn_type:`Primary_full ());
                    (if user.super_user then
                       Utils.button_component
                         ~attribs:
                           [
                             a_onclick
                               ("toggleUserAdminStatus('" ^ user.uuid ^ "')");
                           ]
                         ~content:(txt "Remove Admin") ~btn_type:`Danger_full ()
                     else
                       Utils.button_component
                         ~attribs:
                           [
                             a_onclick
                               ("toggleUserAdminStatus('" ^ user.uuid ^ "')");
                           ]
                         ~content:(txt "Make Admin") ~btn_type:`Primary_full ());
                  ];
              ];
          ];
      ])

let user_policy ~empty_policy (user : User_model.user) policies =
  Tyxml_html.(
    section
      ~a:[ a_id "policy-table" ]
      [
        (match policies with
        | [] ->
            a
              ~a:
                [
                  a_href ("/admin/u/policy/edit/" ^ user.uuid);
                  a_class
                    [
                      "border border-primary-500 hover:bg-primary-700 px-2 \
                       py-1 text-primary-800 hover:text-primary-50 rounded";
                    ];
                ]
              [ txt "Add Policy" ]
        | policy ->
            table
              ~a:
                [ a_class [ "table-auto min-w-full divide-y divide-gray-200" ] ]
              ~thead:
                (thead
                   [
                     tr
                       [
                         th
                           ~a:
                             [
                               a_class
                                 [
                                   "px-6 py-2 text-start text-xs font-bold \
                                    text-primary-600 uppercase";
                                 ];
                             ]
                           [ txt "Albatross instance" ];
                         th
                           ~a:
                             [
                               a_class
                                 [
                                   "px-6 py-2 text-start text-xs font-bold \
                                    text-primary-600 uppercase";
                                 ];
                             ]
                           [ txt "Allowed unikernels" ];
                         th
                           ~a:
                             [
                               a_class
                                 [
                                   "px-6 py-2 text-start text-xs font-bold \
                                    text-primary-600 uppercase";
                                 ];
                             ]
                           [ txt "Allowed Memory" ];
                         th
                           ~a:
                             [
                               a_class
                                 [
                                   "px-6 py-2 text-start text-xs font-bold \
                                    text-primary-600 uppercase";
                                 ];
                             ]
                           [ txt "Allowed Storage" ];
                         th
                           ~a:
                             [
                               a_class
                                 [
                                   "px-6 py-2 text-start text-xs font-bold \
                                    text-primary-600 uppercase";
                                 ];
                             ]
                           [ txt "CPU IDs" ];
                         th
                           ~a:
                             [
                               a_class
                                 [
                                   "px-6 py-2 text-start text-xs font-bold \
                                    text-primary-600 uppercase";
                                 ];
                             ]
                           [ txt "Network Bridges" ];
                         th
                           ~a:
                             [
                               a_class
                                 [
                                   "px-6 py-2 text-start text-xs font-bold \
                                    text-primary-600 uppercase";
                                 ];
                             ]
                           [ txt "Action" ];
                       ];
                   ])
              (List.map
                 (fun (instance_name, policy) ->
                   match policy with
                   | Error error ->
                       policy_row instance_name empty_policy user ~error
                   | Ok (Some p) -> policy_row instance_name p user
                   | Ok None -> policy_row instance_name empty_policy user)
                 policy));
      ])

let user_single_layout ~active_tab content uuid =
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        div
          ~a:
            [
              a_class
                [
                  "flex items-center space-x-1 border-b-2 border-primary-500 \
                   mb-6";
                ];
            ]
          [
            a
              ~a:
                [
                  a_href ("/admin/user/profile?uuid=" ^ uuid);
                  a_class [ tab_style (active_tab = Profile) ];
                ]
              [ i ~a:[ a_class [ "fa-solid fa-user mr-2" ] ] []; txt "Account" ];
            a
              ~a:
                [
                  a_href ("/admin/user/unikernels?uuid=" ^ uuid);
                  a_class [ tab_style (active_tab = Unikernels) ];
                ]
              [
                i ~a:[ a_class [ "fa-solid fa-server mr-2" ] ] [];
                txt "Unikernels";
              ];
            a
              ~a:
                [
                  a_href ("/admin/user/policy?uuid=" ^ uuid);
                  a_class [ tab_style (active_tab = Policy) ];
                ]
              [
                i ~a:[ a_class [ "fa-solid fa-gear mr-2" ] ] [];
                txt "Resource Policy";
              ];
          ];
        div ~a:[ a_class [ "my-5" ] ] [ content ];
      ])
