open Tyxml

let unikernel_index_layout unikernels =
  Tyxml_html.(
    section
      [
        table
          ~a:[ a_class [ "table-auto" ] ]
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
          (List.map
             (fun (name, unikernel) ->
               let name =
                 Option.value ~default:"no name" (Vmm_core.Name.name name)
               in
               tr
                 [
                   td [ txt name ];
                   td
                     [
                       txt
                         (match unikernel.Vmm_core.Unikernel.typ with
                         | `Solo5 -> "solo5");
                     ];
                   td [ txt (string_of_int unikernel.cpuid) ];
                   td [ txt (string_of_int unikernel.memory ^ " MB") ];
                   td
                     [
                       a
                         ~a:[ a_href ("/unikernel/info/" ^ name) ]
                         [ button [ txt "View" ] ];
                     ];
                 ])
             unikernels);
      ])
