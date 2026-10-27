let block_index_layout instance_name blocks policy =
  let total_block_used =
    List.fold_left (fun total_size (_, size, _) -> total_size + size) 0 blocks
  in
  let total_free_space =
    match policy with
    | Some policy ->
        Option.value ~default:0 policy.Vmm_core.Policy.block - total_block_used
    | None -> 0
  in
  if blocks = [] then
    Tyxml_html.(
      section
        ~a:
          [
            a_class
              [
                "col-span-7 p-4 bg-gray-50 my-1 flex flex-col items-center \
                 justify-center text-center space-y-6";
              ];
            a_style "min-height: 60vh;";
          ]
        [
          div
            ~a:[ a_class [ "text-gray-400 mt-12" ] ]
            [ i ~a:[ a_class [ "fa-solid fa-hard-drive fa-4x mb-4" ] ] [] ];
          h2
            ~a:[ a_class [ "text-2xl font-bold text-gray-700" ] ]
            [ txt "No Block devices Found" ];
          p
            ~a:[ a_class [ "text-gray-500 max-w-xl text-sm" ] ]
            [
              txt
                "Note: \"Block devices\" refer to persistent storage units. \
                 You haven't provisioned any block devices yet.";
            ];
          div
            ~a:[ a_class [ "mt-4" ] ]
            [
              Modal_dialog.modal_dialog ~modal_title:"Create a Block device"
                ~button_content:(txt "Create your first block device")
                ~button_type:`Primary_full
                ~content:(Block_ui.create_block instance_name total_free_space)
                ();
            ];
        ])
  else
    Tyxml_html.(
      section
        ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
        [
          section
            ~a:[ a_id "block-display"; a_class [ "block" ] ]
            [
              div
                ~a:[ a_class [ "px-3 flex justify-between items-center" ] ]
                [
                  div
                    [
                      p
                        ~a:[ a_class [ "font-bold text-gray-700" ] ]
                        [
                          txt
                            (string_of_int (List.length blocks)
                            ^ " block devices found on instance "
                            ^ Models.Configuration.name_to_str instance_name);
                        ];
                      p
                        ~a:
                          [ a_class [ "text-sm text-gray-500 mt-1 max-w-2xl" ] ]
                        [
                          txt
                            "Note: \"Block devices\" refer to persistent \
                             storage units. The \"Host Device\" block exposes \
                             this storage to a unikernel.";
                        ];
                    ];
                  div
                    [
                      Modal_dialog.modal_dialog
                        ~modal_title:"Create a Block device"
                        ~button_content:(txt "Create block device")
                        ~content:
                          (Block_ui.create_block instance_name total_free_space)
                        ();
                    ];
                ];
              div
                ~a:[ a_class [ "mx-auto text-center" ] ]
                [
                  Ui.pie_chart ~canvas_id:"usageChart" ~dataset_label:"Size"
                    ~labels:
                      [
                        "Free storage ("
                        ^ string_of_int total_free_space
                        ^ "MB)";
                        "Used storage ("
                        ^ string_of_int total_block_used
                        ^ "MB)";
                      ]
                    ~data:[ total_free_space; total_block_used ];
                ];
              div [ Ui.search_input () ];
              hr ~a:[ a_class [ "border border-primary-500 my-5" ] ] ();
              p ~a:[ a_id "form-alert"; a_class [ "my-4 hidden" ] ] [];
              Ui.data_table
                ~headers:
                  [
                    Ui.sortable_th "Host Device";
                    Ui.sortable_th "Size (MB)";
                    Ui.sortable_th "Used";
                    Ui.static_th "Action";
                  ]
                (List.map
                   (fun ((name, size, used) : Vmm_core.Name.t * int * bool) ->
                     let name =
                       Option.value ~default:"no name"
                         (Option.map Vmm_core.Name.Label.to_string
                            (Vmm_core.Name.name name))
                     in
                     tr
                       ~a:[ a_class [ "border-b border-gray-200" ] ]
                       [
                         Ui.td_txt name;
                         Ui.td_txt (string_of_int size ^ "MB");
                         Ui.td_txt (string_of_bool used);
                         Ui.td_cell
                           [
                             div
                               ~a:[ a_class [ "flex space-x-5" ] ]
                               [
                                 Modal_dialog.modal_dialog
                                   ~modal_title:("Upload data to " ^ name)
                                   ~button_content:(txt "Upload")
                                   ~button_type:`Primary_outlined
                                   ~content:
                                     (Block_ui.upload_to_block ~instance_name
                                        ~block_name:name)
                                   ();
                                 Modal_dialog.modal_dialog
                                   ~modal_title:"Download Block device"
                                   ~button_content:(txt "Download")
                                   ~content:
                                     (Block_ui.download_block ~instance_name
                                        ~block_name:name)
                                   ();
                                 Ui.button_component
                                   ~attribs:
                                     [
                                       a_id ("delete-block-button-" ^ name);
                                       Unsafe.string_attrib "hx-post"
                                         "/api/block/delete";
                                       Unsafe.string_attrib "hx-ext" "form-json";
                                       Unsafe.string_attrib "hx-include"
                                         "#molly-csrf";
                                       Unsafe.string_attrib "hx-vals"
                                         (Fmt.str
                                            {|{"block_name": "%s", "albatross_instance": "%s"}|}
                                            name
                                            (Models.Configuration.name_to_str
                                               instance_name));
                                       Unsafe.string_attrib "hx-swap" "none";
                                       Unsafe.string_attrib
                                         "hx-on::after-request"
                                         "window.parseXhrResponse(event.detail.xhr)";
                                     ]
                                   ~content:(txt "Delete")
                                   ~btn_type:`Danger_full ();
                               ];
                           ];
                       ])
                   blocks);
            ];
        ])
