let input_css_classes =
  [
    "ring-primary-100 mt-1.5 transition appearance-none block w-full px-3 py-3";
    "rounded-xl shadow-sm border hover:border-primary-200";
    "focus:border-primary-300 bg-primary-50 bg-opacity-0 hover:bg-opacity-50";
    "focus:bg-opacity-50 ring-primary-200 focus:ring-primary-200";
    "focus:ring-[1px] focus:outline-none";
  ]

let render_input ~label_text ~name ~id ~input_type ~value () =
  Tyxml_html.(
    div
      ~a:[ a_class [ "bg-gray-50 px-4 pb-4 pt-5 sm:p-6 sm:pb-4 my-4" ] ]
      [
        div
          [
            label
              ~a:[ a_class [ "block text-sm font-medium" ]; a_label_for id ]
              [ txt label_text ];
            input
              ~a:
                [
                  a_autocomplete `Off;
                  a_input_type input_type;
                  a_name name;
                  a_id id;
                  a_value value;
                  a_class input_css_classes;
                ]
              ();
          ];
      ])

let email_config_layout (current_config : Utils.Email.t option) =
  let ip_val, port_val, sender_val, mollymawk_val =
    match current_config with
    | Some c ->
        ( Ipaddr.to_string c.server,
          string_of_int c.port,
          Emile.to_string c.sender_email,
          c.mollymawk_domain )
    | None -> ("", "", "", "")
  in

  Tyxml_html.(
    section
      ~a:[ a_class [ "p-4 bg-white rounded-lg shadow-sm" ] ]
      [
        h2
          ~a:[ a_class [ "text-lg font-bold text-gray-800 mb-4 px-4" ] ]
          [ txt "Email Server Configuration" ];
        div
          [
            p ~a:[ a_id "form-alert"; a_class [ "my-4 hidden" ] ] [];
            render_input ~label_text:"Server IP address" ~name:"email_ip"
              ~id:"email-ip" ~input_type:`Text ~value:ip_val ();
            render_input ~label_text:"Server Port" ~name:"email_port"
              ~id:"email-port" ~input_type:`Number ~value:port_val ();
            render_input ~label_text:"Sender Email Address" ~name:"email_sender"
              ~id:"email-sender" ~input_type:`Email ~value:sender_val ();
            render_input ~label_text:"Mollymawk Domain Address"
              ~name:"mollymawk_domain" ~id:"mollymawk-domain" ~input_type:`Text
              ~value:mollymawk_val ();
            div
              ~a:[ a_class [ "mx-auto my-6 flex justify-center px-4" ] ]
              [
                Utils.button_component
                  ~attribs:
                    [
                      a_id "update-email-config-btn";
                      a_onclick "updateEmailConfig()";
                    ]
                  ~content:(txt "Update Configuration")
                  ~btn_type:`Primary_full ();
              ];
          ];
      ])
