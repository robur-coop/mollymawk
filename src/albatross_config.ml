let delimit_string str =
  if String.length str <= 20 then str else String.sub str 0 20 ^ "..."

let albatross_table_row ((_, albatross) : Vmm_core.Name.Label.t * Albatross_client.t) =
  let configuration = albatross.configuration in
  let ip = Ipaddr.to_string configuration.server_ip in
  let port = string_of_int configuration.server_port in
  let certificate = X509.Certificate.encode_pem configuration.certificate in
  let private_key = X509.Private_key.encode_pem configuration.private_key in
  let last_update = Utils.TimeHelper.string_of_ptime configuration.updated_at in
  let name = Models.Configuration.name_to_str configuration.name in
  let error_buttons =
    Tyxml_html.(
      div
        [
          p
            ~a:[ a_class [ "text-secondary-500 font-semibold" ] ]
            [ txt (Albatross_client.Status.to_string albatross.status) ];
          div
            ~a:[ a_class [ "my-2 space-x-1 flex" ] ]
            [
              Ui.link_button
                ~href:("/admin/albatross/errors?instance=" ^ name)
                ~content:(txt "View Errors") ~btn_type:`Danger_full ();
              Ui.button_component
                ~attribs:
                  [
                    a_id ("retry-btn-" ^ name);
                    Unsafe.string_attrib "hx-get"
                      ("/api/admin/albatross/retry?instance=" ^ name);
                    Unsafe.string_attrib "hx-swap" "none";
                    Unsafe.string_attrib "hx-on::after-request"
                      "window.parseXhrResponse(event.detail.xhr)";
                  ]
                ~content:(txt "Retry connecting") ~btn_type:`Primary_full ();
            ];
        ])
  in
  Tyxml_html.(
    tr
      [
        Ui.td_cell
          [
            txt name;
            (match albatross.status with
            | Online ->
                p
                  ~a:[ a_class [ "text-primary-500 font-semibold" ] ]
                  [ txt (Albatross_client.Status.to_string albatross.status) ]
            | Degraded _ -> error_buttons);
          ];
        Ui.td_cell
          [
            txt (delimit_string certificate);
            p [ txt (delimit_string private_key) ];
          ];
        Ui.td_txt (ip ^ ":" ^ port);
        Ui.td_txt last_update;
        Ui.td_cell
          [
            div
              ~a:[ a_class [ "flex space-x-4" ] ]
              [
                Ui.button_component
                  ~attribs:
                    [
                      a_onclick
                        ("openConfigForm(`" ^ name ^ "`,`" ^ ip ^ "`,`" ^ port
                       ^ "`,`" ^ certificate ^ "`,`" ^ private_key ^ "`)");
                    ]
                  ~content:(i ~a:[ a_class [ "fa-solid fa-pen" ] ] [])
                  ~btn_type:`Primary_outlined ();
                Ui.button_component
                  ~attribs:
                    [
                      a_id
                        ("delete-config-btn-"
                        ^ Models.Configuration.name_to_str configuration.name);
                      Unsafe.string_attrib "hx-post"
                        "/api/admin/settings/albatross/delete";
                      Unsafe.string_attrib "hx-ext" "form-json";
                      Unsafe.string_attrib "hx-include" "#molly-csrf";
                      Unsafe.string_attrib "hx-vals"
                        (Fmt.str {|{"name": "%s"}|}
                           (Models.Configuration.name_to_str configuration.name));
                      Unsafe.string_attrib "hx-swap" "none";
                      Unsafe.string_attrib "hx-on::after-request"
                        "window.parseXhrResponse(event.detail.xhr)";
                    ]
                  ~content:(i ~a:[ a_class [ "fa-solid fa-trash" ] ] [])
                  ~btn_type:`Danger_full ();
              ];
          ];
      ])

let albatross_config_layout albatross_instances =
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        form
          ~a:
            [
              a_id "config-form";
              a_class [ "my-5 hidden" ];
              a_aria "labelledby" [ "modal-title" ];
              a_role [ "dialog" ];
              a_aria "modal" [ "true" ];
              Unsafe.string_attrib "hx-post"
                "/api/admin/settings/albatross/create";
              Unsafe.string_attrib "hx-ext" "form-json";
              Unsafe.string_attrib "hx-include" "#molly-csrf";
              Unsafe.string_attrib "hx-swap" "none";
              Unsafe.string_attrib "hx-on::after-request"
                "window.parseXhrResponse(event.detail.xhr)";
            ]
          [
            p ~a:[ a_id "form-alert"; a_class [ "my-4 hidden" ] ] [];
            Ui.hidden_input ~attribs:[ a_id "config-name-edit" ] ();
            div
              ~a:[ a_class [ "bg-gray-50 px-4 pb-4 pt-5 sm:p-6 sm:pb-4 my-4" ] ]
              [
                Ui.form_input ~label_text:"Albatross_client Instance Name*"
                  ~attribs:[ a_required () ]
                  "name";
              ];
            div
              ~a:[ a_class [ "bg-gray-50 px-4 pb-4 pt-5 sm:p-6 sm:pb-4 my-4" ] ]
              [
                Ui.form_input ~label_text:"Server IP*"
                  ~attribs:[ a_required () ]
                  "server_ip";
              ];
            div
              ~a:[ a_class [ "bg-gray-50 px-4 pb-4 pt-5 sm:p-6 sm:pb-4 my-4" ] ]
              [
                Ui.form_input ~label_text:"Server Port*"
                  ~attribs:
                    [
                      a_input_type `Number;
                      a_required ();
                      a_maxlength 4;
                      a_minlength 4;
                    ]
                  "server_port";
              ];
            div
              ~a:[ a_class [ "bg-gray-50 px-4 pb-4 pt-5 sm:p-6 sm:pb-4 my-4" ] ]
              [
                Ui.form_textarea ~label_text:"Certificate*"
                  ~attribs:[ a_rows 7; a_required () ]
                  "certificate";
              ];
            div
              ~a:[ a_class [ "bg-gray-50 px-4 pb-4 pt-5 sm:p-6 sm:pb-4 my-4" ] ]
              [
                Ui.form_textarea ~label_text:"Private Key*"
                  ~attribs:[ a_rows 4; a_required () ]
                  "private_key";
                div
                  ~a:[ a_class [ "mx-auto my-4 flex justify-center" ] ]
                  [
                    Ui.button_component
                      ~attribs:[ a_button_type `Submit; a_id "config-button" ]
                      ~content:(txt "") ~btn_type:`Primary_full ();
                  ];
              ];
          ];
        div
          ~a:[ a_id "config-body"; a_class [ "flex flex-col block" ] ]
          [
            div
              ~a:[ a_id "add-config" ]
              [
                Ui.button_component
                  ~attribs:[ a_onclick "openConfigForm('','','','','')" ]
                  ~content:
                    (span
                       [
                         i ~a:[ a_class [ "fa-solid fa-plus mr-2" ] ] [];
                         txt "Add Albatross_client Instance";
                       ])
                  ~btn_type:`Primary_outlined ();
              ];
            Ui.data_table ~id:"config-table" ~sortable:false
              ~headers:
                [
                  Ui.static_th "Name";
                  Ui.static_th "Certificate/Key";
                  Ui.static_th "IP";
                  Ui.static_th "Last update";
                  Ui.static_th "Action";
                ]
              (List.map albatross_table_row
                 (Utils.LM.bindings albatross_instances));
          ];
      ])
