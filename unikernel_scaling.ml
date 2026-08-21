let display_policy ~instance_name ~unikernel_name ~max_allowed scaling_policy =
  match max_allowed with
  | Error err ->
      Tyxml_html.(
        section
          ~a:[ a_id "scaling-policy-container"; a_class [ "w-full mx-auto" ] ]
          [
            div
              ~a:
                [
                  a_class
                    [
                      "p-4 border rounded-lg border-secondary-400 \
                       text-secondary-500 bg-gray-800 text-center";
                    ];
                ]
              [
                p
                  ~a:[ a_class [ "font-semibold mb-2 text-secondary-500" ] ]
                  [ txt "Unable to retrieve scaling limits." ];
                p ~a:[ a_class [ "text-sm" ] ] [ txt err ];
              ];
          ])
  | Ok max_allowed ->
      Tyxml_html.(
        section
          ~a:[ a_id "scaling-policy-container"; a_class [ "w-full mx-auto" ] ]
          [
            form
              ~a:
                [
                  a_enctype "multipart/form-data";
                  Unsafe.string_attrib "hx-post"
                    (Fmt.str
                       "/api/unikernel/scaling/update?instance=%s&unikernel=%s"
                       instance_name unikernel_name);
                  Unsafe.string_attrib "hx-swap" "innerHTML";
                  Unsafe.string_attrib "hx-include" "#molly-csrf";
                  Unsafe.string_attrib "hx-vals"
                    "js:{max_instances: \
                     parseInt(document.getElementById('max-instances').innerText.trim(), \
                     10)}";
                  Unsafe.string_attrib "hx-target" "#scaling-policy-container";
                ]
              [
                Utils.switch_button
                  ~initial_state:
                    (Option.fold ~none:false
                       ~some:(fun p -> true)
                       scaling_policy)
                  ~switch_id:"should_scale" ~switch_label:"Scale this unikernel"
                  (div
                     [
                       div
                         ~a:[ a_class [ "py-3" ] ]
                         [
                           Utils.increment_or_decrement_ui ~id:"max-instances"
                             ~default_value:
                               (Option.fold ~none:1
                                  ~some:(fun p -> p.User_model.max_instances)
                                  scaling_policy)
                             ~min_value:1 ~max_value:max_allowed
                             ~label':"Maximum number of clones to spawn" ();
                         ];
                     ]);
                div
                  ~a:[ a_class [ "my-6" ] ]
                  [
                    Utils.button_component
                      ~attribs:[ a_id "update-scaling"; a_button_type `Submit ]
                      ~extra_css:"w-full"
                      ~content:(txt "Update scaling policy")
                      ~btn_type:`Primary_full ();
                  ];
              ];
          ])
