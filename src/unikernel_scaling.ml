let display_policy ~instance_name ~unikernel_name ~max_allowed scaling_policy =
  Tyxml_html.(
    section
      ~a:[ a_id "scaling-policy-container"; a_class [ "w-full mx-auto" ] ]
      [
        form
          ~a:
            [
              Unsafe.string_attrib "hx-ext" "form-json";
              Unsafe.string_attrib "hx-post"
                (Fmt.str
                   "/api/unikernel/scaling/update?instance=%s&unikernel=%s"
                   instance_name unikernel_name);
              Unsafe.string_attrib "hx-swap" "none";
              Unsafe.string_attrib "hx-include" "#molly-csrf";
              Unsafe.string_attrib "hx-vals"
                "js:{max_instances: \
                 parseInt(document.getElementById('max-instances').innerText.trim(), \
                 10)}";
              Unsafe.string_attrib "hx-on::after-request"
                "window.parseXhrResponse(event.detail.xhr)";
            ]
          [
            Ui.switch_button
              ~initial_state:
                (Option.fold ~none:false ~some:(fun _p -> true) scaling_policy)
              ~switch_id:"should_scale" ~switch_label:"Scale this unikernel"
              (div
                 [
                   div
                     ~a:[ a_class [ "py-3" ] ]
                     [
                       Ui.increment_or_decrement_ui ~id:"max-instances"
                         ~default_value:
                           (Option.fold ~none:1
                              ~some:(fun p -> p.Models.User.max_instances)
                              scaling_policy)
                         ~min_value:1 ~max_value:max_allowed
                         ~label':"Maximum number of clones to spawn" ();
                     ];
                 ]);
            div
              ~a:[ a_class [ "my-6" ] ]
              [
                Ui.button_component
                  ~attribs:[ a_id "update-scaling"; a_button_type `Submit ]
                  ~content:(txt "Update scaling policy")
                  ~btn_type:`Primary_full ();
              ];
          ];
      ])
