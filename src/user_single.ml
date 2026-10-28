type users_tab = Profile | Unikernels | Policy

let policy_row instance_name policy (user : Models.User.user) =
  Tyxml_html.(
    tr
      [
        Ui.td_txt (Models.Configuration.name_to_str instance_name);
        Ui.td_txt (string_of_int policy.Vmm_core.Policy.unikernels);
        Ui.td_txt (string_of_int policy.memory ^ " MB");
        Ui.td_txt (string_of_int (Option.value policy.block ~default:0) ^ " MB");
        Ui.td_txt
          (String.concat ", "
             (List.map string_of_int (Vmm_core.IS.elements policy.cpuids)));
        Ui.td_txt
          (String.concat ", "
             (List.map string_of_uri
                (Vmm_core.String_set.elements policy.bridges)));
        Ui.td_cell
          [
            Ui.link_button
              ~href:
                ("/admin/u/policy/edit?uuid=" ^ Uuidm.to_string user.uuid
               ^ "&instance="
                ^ Models.Configuration.name_to_str instance_name)
              ~content:(txt "Edit") ~btn_type:`Primary_outlined ();
          ];
      ])

let user_profile (user : Models.User.user) =
  let user_name = Models.Configuration.name_to_str user.name in
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
                    txt (Emile.to_string user.email);
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
                (let uuid_str = Uuidm.to_string user.uuid in
                 let hx_user_attribs ?redirect endpoint =
                   let after_req =
                     match redirect with
                     | Some url ->
                         Fmt.str
                           "window.parseXhrResponse(event.detail.xhr, '%s')" url
                     | None -> "window.parseXhrResponse(event.detail.xhr)"
                   in
                   [
                     Unsafe.string_attrib "hx-post" endpoint;
                     Unsafe.string_attrib "hx-ext" "form-json";
                     Unsafe.string_attrib "hx-include" "#molly-csrf";
                     Unsafe.string_attrib "hx-vals"
                       (Fmt.str {|{"uuid": "%s"}|} uuid_str);
                     Unsafe.string_attrib "hx-swap" "none";
                     Unsafe.string_attrib "hx-on::after-request" after_req;
                   ]
                 in
                 div
                   ~a:[ a_class [ "flex justify-center space-x-4 my-4" ] ]
                   [
                     Ui.button_component
                       ~attribs:
                         (hx_user_attribs ~redirect:"/admin/users"
                            "/api/admin/user/account/delete")
                       ~content:(txt "Delete User") ~btn_type:`Danger_full ();
                     (if user.active then
                        Ui.button_component
                          ~attribs:
                            (hx_user_attribs "/api/admin/user/activate/toggle")
                          ~content:(txt "Deactivate") ~btn_type:`Danger_full ()
                      else
                        Ui.button_component
                          ~attribs:
                            (hx_user_attribs "/api/admin/user/activate/toggle")
                          ~content:(txt "Activate") ~btn_type:`Primary_full ());
                     (if user.super_user then
                        Ui.button_component
                          ~attribs:
                            (hx_user_attribs "/api/admin/user/admin/toggle")
                          ~content:(txt "Remove Admin") ~btn_type:`Danger_full
                          ()
                      else
                        Ui.button_component
                          ~attribs:
                            (hx_user_attribs "/api/admin/user/admin/toggle")
                          ~content:(txt "Make Admin") ~btn_type:`Primary_full ());
                   ]);
              ];
          ];
      ])

let user_policy ~empty_policy (user : Models.User.user) policies =
  Tyxml_html.(
    section
      ~a:[ a_id "policy-table" ]
      [
        (if Utils.LM.is_empty policies then
           Ui.link_button
             ~href:("/admin/u/policy/edit/" ^ Uuidm.to_string user.uuid)
             ~content:(txt "Add Policy") ~btn_type:`Primary_outlined ()
         else
           Ui.simple_table
             ~headers:
               [
                 Ui.static_th "Albatross instance";
                 Ui.static_th "Allowed unikernels";
                 Ui.static_th "Allowed Memory";
                 Ui.static_th "Allowed Storage";
                 Ui.static_th "CPU IDs";
                 Ui.static_th "Network Bridges";
                 Ui.static_th "Action";
               ]
             (Utils.LM.fold
                (fun instance_name policy acc ->
                  let p = Option.value ~default:empty_policy policy in
                  policy_row instance_name p user :: acc)
                policies []));
      ])

let user_single_layout ~active_tab content (uuid : Uuidm.t) =
  let uuid = Uuidm.to_string uuid in
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        Ui.tab_bar
          [
            ( "/admin/user/profile?uuid=" ^ uuid,
              "fa-solid fa-user",
              "Account",
              active_tab = Profile );
            ( "/admin/user/unikernels?uuid=" ^ uuid,
              "fa-solid fa-server",
              "Unikernels",
              active_tab = Unikernels );
            ( "/admin/user/policy?uuid=" ^ uuid,
              "fa-solid fa-gear",
              "Resource Policy",
              active_tab = Policy );
          ];
        div ~a:[ a_class [ "my-5" ] ] [ content ];
      ])
