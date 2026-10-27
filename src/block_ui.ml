let create_block instance_name total_free_space =
  Tyxml_html.(
    section
      ~a:[ a_id "block-create"; a_class [ "w-full mx-auto" ] ]
      [
        p ~a:[ a_id "create-block-form-alert"; a_class [ "my-4 hidden" ] ] [];
        Ui.form_input ~label_text:"Block device name" "block_name";
        div
          ~a:[ a_class [ "py-3" ] ]
          [
            Ui.increment_or_decrement_ui ~id:"block_size" ~min_value:0
              ~max_value:total_free_space ~figure_unit:"MB" ~step:32
              ~label':"Block device size" ();
          ];
        Ui.switch_button ~switch_id:"dataToggle"
          ~switch_label:"Dump data to this Block device"
          (div
             [
               Ui.form_input
                 ~label_text:"Select a file to dump to the Block device"
                 ~attribs:[ a_input_type `File ]
                 "block_data";
               div
                 ~a:[ a_class [ "my-4 flex items-center" ] ]
                 [
                   Ui.checkbox_input
                     ~attribs:
                       [ a_id "block_compressed"; a_name "block_compressed" ]
                     ();
                   label
                     ~a:
                       [ a_class [ "text-sm font-medium" ]; a_label_for "data" ]
                     [ txt "Is this file compressed?" ];
                 ];
             ]);
        div
          ~a:[ a_class [ "my-6" ] ]
          [
            Ui.button_component
              ~attribs:
                [
                  a_id "create-block-button";
                  a_onclick
                    ("createBlock('"
                    ^ Models.Configuration.name_to_str instance_name
                    ^ "')");
                ]
              ~content:(txt "Create Block device")
              ~btn_type:`Primary_full ();
          ];
      ])

let download_block ~instance_name ~block_name =
  Tyxml_html.(
    section
      ~a:[ a_id ("block-download-" ^ block_name); a_class [ "w-full mx-auto" ] ]
      [
        div
          [
            label
              ~a:
                [
                  a_class [ "block my-2 font-medium" ]; a_label_for "block name";
                ]
              [ txt block_name ];
            p ~a:[ a_id "download-alert"; a_class [ "my-2 hidden" ] ] [];
            label
              ~a:
                [
                  a_class [ "block text-sm font-medium" ];
                  a_label_for "compression_level";
                ]
              [ txt "Set Compression level" ];
            Ui.increment_or_decrement_ui
              ~id:("compression-level-" ^ block_name)
              ~min_value:0 ~max_value:9 ~label':"Compression level" ();
            Ui.button_component
              ~attribs:
                [
                  a_id ("download-block-button-" ^ block_name);
                  a_onclick
                    ("downloadBlock('" ^ block_name ^ "', '"
                    ^ Models.Configuration.name_to_str instance_name
                    ^ "')");
                ]
              ~content:(txt ("Download " ^ block_name))
              ~btn_type:`Primary_full ();
          ];
      ])

let upload_to_block ~instance_name ~block_name =
  Tyxml_html.(
    section
      ~a:[ a_id ("block-upload-" ^ block_name); a_class [ "w-full mx-auto" ] ]
      [
        Ui.form_input ~label_text:"Select a file to dump to the Block device"
          ~attribs:[ a_input_type `File ]
          ("block_data_upload-" ^ block_name);
        div
          ~a:[ a_class [ "my-4 flex items-center" ] ]
          [
            Ui.checkbox_input
              ~attribs:
                [
                  a_id ("block_compressed-" ^ block_name);
                  a_name ("block_compressed-" ^ block_name);
                ]
              ();
            label
              ~a:[ a_class [ "text-sm font-medium" ]; a_label_for "data" ]
              [ txt "Is this file compressed?" ];
          ];
        div
          ~a:[ a_class [ "my-6" ] ]
          [
            Ui.button_component
              ~attribs:
                [
                  a_id ("upload-block-button-" ^ block_name);
                  a_onclick
                    ("uploadToBlock('" ^ block_name ^ "', '"
                    ^ Models.Configuration.name_to_str instance_name
                    ^ "')");
                ]
              ~content:(txt "Upload data") ~btn_type:`Primary_full ();
          ];
      ])
