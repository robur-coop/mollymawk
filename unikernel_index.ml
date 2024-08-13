open Tyxml

let unikernel_index_layout ?unikernels () =
  Tyxml_html.(
    section
      [
        table
          ~thead:
            (thead
               [
                 tr
                   [
                     th [ txt "Name" ];
                     th [ txt "Type" ];
                     th [ txt "CPU" ];
                     th [ txt "Memory" ];
                   ];
               ])
          [
            tr
              [
                td [ txt "robur" ];
                td [ txt "solo5" ];
                td [ txt "1" ];
                td [ txt "256 MB" ];
                td
                  [
                    a
                      ~a:[ a_href "/unikernel/[uuid]" ]
                      [ button [ txt "View" ] ];
                  ];
              ];
          ];
      ])
