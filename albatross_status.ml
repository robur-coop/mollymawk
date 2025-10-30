let render_log log = Fmt.str "%a" Albatross.Status.pp_error log

let albatross_status_layout (albatross : Albatross.t) =
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        (match albatross.status with
        | Online ->
            div
              [
                div
                  ~a:[ a_class [ "flex my-3" ] ]
                  [
                    h1
                      ~a:[ a_class [ "font-bold text-xl" ] ]
                      [
                        txt
                          (Configuration.name_to_str
                             albatross.configuration.name);
                      ];
                    div
                      ~a:
                        [
                          a_style
                            "height: 10px; width: 10px; border-radius: 100%; \
                             background-color: #04AA6D;";
                        ]
                      [];
                  ];
                h2
                  [
                    txt "Status: ";
                    span
                      ~a:[ a_class [ "text-primary-500 font-bold" ] ]
                      [ txt "Online" ];
                  ];
                p
                  [
                    txt
                      "This Albatross service is currently online and \
                       operational.";
                  ];
              ]
        | Degraded error ->
            div
              [
                div
                  ~a:[ a_class [ "flex my-3" ] ]
                  [
                    h1
                      ~a:[ a_class [ "font-bold text-xl" ] ]
                      [
                        txt
                          (Configuration.name_to_str
                             albatross.configuration.name);
                      ];
                    div
                      ~a:
                        [
                          a_style
                            "height: 10px; width: 10px; border-radius: 100%; \
                             background-color: #ff4e33;";
                        ]
                      [];
                  ];
                h2
                  [
                    txt "Status: ";
                    span
                      ~a:[ a_class [ "text-secondary-500 font-bold" ] ]
                      [
                        txt
                          (Printf.sprintf "Degraded (%d retries)" error.retries);
                      ];
                  ];
                p ~a:[ a_class [ "mb-2" ] ] [ txt "Logs collected so far:" ];
                code
                  (List.map
                     (fun log -> code [ txt (render_log log) ])
                     error.log);
              ]
        | Offline errors ->
            div
              [
                div
                  ~a:[ a_class [ "flex my-3" ] ]
                  [
                    h1
                      ~a:[ a_class [ "font-bold text-xl" ] ]
                      [
                        txt
                          (Configuration.name_to_str
                             albatross.configuration.name);
                      ];
                    div
                      ~a:
                        [
                          a_style
                            "height: 10px; width: 10px; border-radius: 100%; \
                             background-color: #ff4e33;";
                        ]
                      [];
                  ];
                h2
                  [
                    txt "Status: ";
                    span
                      ~a:[ a_class [ "text-secondary-500 font-bold" ] ]
                      [ txt "Offline" ];
                  ];
                p ~a:[ a_class [ "mb-2" ] ] [ txt "Logs collected so far:" ];
                p
                  ~a:[ a_class [ "divide-y-1" ] ]
                  (List.map
                     (fun error -> code [ txt (render_log error) ])
                     errors);
              ]);
      ])
