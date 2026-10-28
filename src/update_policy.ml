let update_policy_layout (user : Models.User.user) ~user_policy
    ~unallocated_resources instance_name =
  Tyxml_html.(
    form
      ~a:
        [
          a_id "policy-form";
          Unsafe.string_attrib "hx-post" "/api/admin/u/policy/update";
          Unsafe.string_attrib "hx-ext" "form-json";
          Unsafe.string_attrib "hx-include" "#molly-csrf";
          Unsafe.string_attrib "hx-vals"
            (Fmt.str
               "js:{albatross_instance: '%s', user_uuid: '%s', unikernels: \
                Number(document.getElementById('f_allowed_unikernels').innerText), \
                memory: \
                Number(document.getElementById('f_allowed_memory').innerText), \
                block: \
                Number(document.getElementById('f_allowed_storage').innerText), \
                cpuids: document.getElementById('selectedCPUs').value, \
                bridges: document.getElementById('selectedBridges').value}"
               (Models.Configuration.name_to_str instance_name)
               (Uuidm.to_string user.uuid));
          Unsafe.string_attrib "hx-swap" "none";
          Unsafe.string_attrib "hx-on::after-request"
            (Fmt.str
               "window.parseXhrResponse(event.detail.xhr, \
                '/admin/user/policy?uuid=%s')"
               (Uuidm.to_string user.uuid));
        ]
      [
        h2
          ~a:[ a_class [ "font-semibold text-2xl" ] ]
          [
            txt
              ("Set Policy for "
              ^ Models.Configuration.name_to_str user.name
              ^ "on albatross instance "
              ^ Models.Configuration.name_to_str instance_name);
          ];
        p
          ~a:[ a_id "user_id"; a_class [ "hidden" ] ]
          [ txt (Uuidm.to_string user.uuid) ];
        div
          ~a:[ a_class [ "py-3" ] ]
          [
            Ui.increment_or_decrement_ui ~id:"f_allowed_unikernels"
              ~default_value:user_policy.Vmm_core.Policy.unikernels ~min_value:0
              ~max_value:
                Vmm_core.Policy.(
                  unallocated_resources.unikernels + user_policy.unikernels)
              ~figure_unit:"VMs" ~label':"Allowed Unikernels" ();
          ];
        hr ();
        div
          ~a:[ a_class [ "py-3" ] ]
          [
            Ui.increment_or_decrement_ui ~id:"f_allowed_memory"
              ~default_value:user_policy.Vmm_core.Policy.memory ~step:32
              ~min_value:0
              ~max_value:
                Vmm_core.Policy.(
                  unallocated_resources.memory + user_policy.memory)
              ~figure_unit:"MB" ~label':"Allowed Memory" ();
          ];
        hr ();
        div
          ~a:[ a_class [ "py-3" ] ]
          [
            Ui.increment_or_decrement_ui ~id:"f_allowed_storage"
              ~default_value:
                (Option.value ~default:0 user_policy.Vmm_core.Policy.block)
              ~min_value:0
              ~max_value:
                Vmm_core.Policy.(
                  Option.value ~default:0 unallocated_resources.block
                  + Option.value ~default:0 user_policy.block)
              ~figure_unit:"MB" ~step:32 ~label':"Allowed Storage" ();
          ];
        hr ();
        Ui.alpine_multiselect ~id:"selectedCPUs" ~label_text:"CPU IDs"
          ~button_label:"Assign CPUs" ~option_prefix:"CPU "
          ~selected_json:
            (Utils.cpuid_to_array_string
               (Vmm_core.IS.elements user_policy.cpuids))
          ~options_json:
            (Utils.cpuid_to_array_string
               (Vmm_core.IS.elements unallocated_resources.cpuids))
          ();
        (let bridges_to_array_string set =
           if Vmm_core.String_set.is_empty set then "[]"
           else
             "[\""
             ^ String.concat "\", \"" (Vmm_core.String_set.elements set)
             ^ "\"]"
         in
         Ui.alpine_multiselect ~id:"selectedBridges"
           ~label_text:"Network interfaces" ~button_label:"Assign Bridges"
           ~selected_json:(bridges_to_array_string user_policy.bridges)
           ~options_json:(bridges_to_array_string unallocated_resources.bridges)
           ());
        hr ();
        div
          ~a:[ a_class [ "my-3" ] ]
          [
            Ui.button_component
              ~attribs:[ a_id "set-policy-btn"; a_button_type `Submit ]
              ~content:(txt "Set Policy") ~btn_type:`Primary_full ();
          ];
      ])
