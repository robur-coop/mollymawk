let create_volume instance_name total_free_space =
  Tyxml_html.(
    section
      ~a:[ a_id "block-create"; a_class [ "w-full mx-auto" ] ]
      [
        p ~a:[ a_id "create-volume-form-alert"; a_class [ "my-4 hidden" ] ] [];
        div
          ~a:[ a_class [ "my-6" ] ]
          [
            label
              ~a:
                [
                  a_class [ "block text-sm font-medium" ];
                  a_label_for "block name";
                ]
              [ txt "Volume name" ];
            input
              ~a:
                [
                  a_autocomplete `On;
                  a_input_type `Text;
                  a_name "block_name";
                  a_id "block_name";
                  a_class
                    [
                      "ring-primary-100 mt-1.5 transition appearance-none \
                       block w-full px-3 py-3 rounded-xl shadow-sm border \
                       hover:border-primary-200\n\
                      \                                           \
                       focus:border-primary-300 bg-primary-50 bg-opacity-0 \
                       hover:bg-opacity-50 focus:bg-opacity-50 \
                       ring-primary-200 focus:ring-primary-200\n\
                      \                                           \
                       focus:ring-[1px] focus:outline-none";
                    ];
                ]
              ();
          ];
        div
          ~a:[ a_class [ "py-3" ] ]
          [
            Utils.increment_or_decrement_ui ~id:"block_size"
              ~min_value:(if total_free_space > 0 then 1 else 0)
              ~max_value:total_free_space ~figure_unit:"MB"
              ~label':"Volume size";
          ];
        Utils.switch_button ~switch_id:"dataToggle"
          ~switch_label:"Dump data to this volume"
          (div
             [
               div
                 ~a:[ a_class [ "my-4" ] ]
                 [
                   label
                     ~a:
                       [
                         a_class [ "block text-sm font-medium" ];
                         a_label_for "data";
                       ]
                     [ txt "Select a file to dump to the volume" ];
                   input
                     ~a:
                       [
                         a_input_type `File;
                         a_name "block_data";
                         a_id "block_data";
                         a_class
                           [
                             "ring-primary-100 mt-1.5 transition \
                              appearance-none block w-full px-3 py-3 \
                              rounded-xl shadow-sm border \
                              hover:border-primary-200\n\
                             \                                           \
                              focus:border-primary-300 bg-primary-50 \
                              bg-opacity-0 hover:bg-opacity-50 \
                              focus:bg-opacity-50 ring-primary-200 \
                              focus:ring-primary-200\n\
                             \                                           \
                              focus:ring-[1px] focus:outline-none";
                           ];
                       ]
                     ();
                 ];
               div
                 ~a:[ a_class [ "my-4 flex items-center" ] ]
                 [
                   input
                     ~a:
                       [
                         a_input_type `Checkbox;
                         a_name "block_compressed";
                         a_id "block_compressed";
                         a_class [ "accent-primary-500 mr-2 w-6 h-6" ];
                       ]
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
            Utils.button_component
              ~attribs:
                [
                  a_id "create-block-button";
                  a_onclick
                    ("createVolume('"
                    ^ Configuration.name_to_str instance_name
                    ^ "')");
                ]
              ~extra_css:"w-full" ~content:(txt "Create Volume")
              ~btn_type:`Primary_full ();
          ];
      ])

let download_volume ~instance_name ~block_name =
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
            div
              ~a:
                [
                  a_class [ "space-x-5 my-4" ];
                  Unsafe.string_attrib "x-data" "{level : 0}";
                ]
              [
                Utils.button_component
                  ~attribs:
                    [
                      Unsafe.string_attrib "x-on:click"
                        "if (level >= 1) level = level - 1";
                    ]
                  ~content:(i ~a:[ a_class [ "fa-solid fa-minus" ] ] [])
                  ~btn_type:`Danger_outlined ();
                span
                  ~a:
                    [
                      a_id ("compression-level-" ^ block_name);
                      a_contenteditable true;
                      a_class [ "text-4xl border px-4" ];
                      a_user_data "x-on:keydown.enter.prevent" "";
                      Unsafe.string_attrib "x-on:input"
                        "let value =\n\
                        \                                                                  \
                         $event.target.innerText.replace(/[^0-9]/g,'');\n\
                        \                                                                  \
                         $event.target.innerText = value;\n\
                        \                                                                  \
                         level = parseInt(value) || 0;";
                      Unsafe.string_attrib "x-text" "level";
                      Unsafe.string_attrib "x-on:blur"
                        "$event.target.innerText = level;";
                    ]
                  [];
                Utils.button_component
                  ~attribs:
                    [
                      Unsafe.string_attrib "x-on:click"
                        "if (level < 9) level = level + 1";
                    ]
                  ~content:(i ~a:[ a_class [ "fa-solid fa-plus" ] ] [])
                  ~btn_type:`Primary_outlined ();
              ];
            Utils.button_component
              ~attribs:
                [
                  a_id ("download-block-button-" ^ block_name);
                  a_onclick
                    ("downloadVolume('" ^ block_name ^ "', '"
                    ^ Configuration.name_to_str instance_name
                    ^ "')");
                ]
              ~extra_css:"my-4"
              ~content:(txt ("Download " ^ block_name))
              ~btn_type:`Primary_full ();
          ];
      ])

let upload_to_volume ~instance_name ~block_name =
  Tyxml_html.(
    section
      ~a:[ a_id ("block-upload-" ^ block_name); a_class [ "w-full mx-auto" ] ]
      [
        div
          ~a:[ a_class [ "my-4" ] ]
          [
            label
              ~a:[ a_class [ "block text-sm font-medium" ]; a_label_for "data" ]
              [ txt "Select a file to dump to the volume" ];
            input
              ~a:
                [
                  a_input_type `File;
                  a_name ("block_data_upload-" ^ block_name);
                  a_id ("block_data_upload-" ^ block_name);
                  a_class
                    [
                      "ring-primary-100 mt-1.5 transition appearance-none \
                       block w-full px-3 py-3 rounded-xl shadow-sm border \
                       hover:border-primary-200\n\
                      \                                           \
                       focus:border-primary-300 bg-primary-50 bg-opacity-0 \
                       hover:bg-opacity-50 focus:bg-opacity-50 \
                       ring-primary-200 focus:ring-primary-200\n\
                      \                                           \
                       focus:ring-[1px] focus:outline-none";
                    ];
                ]
              ();
          ];
        div
          ~a:[ a_class [ "my-4 flex items-center" ] ]
          [
            input
              ~a:
                [
                  a_input_type `Checkbox;
                  a_name ("block_compressed-" ^ block_name);
                  a_id ("block_compressed-" ^ block_name);
                  a_class [ "accent-primary-500 mr-2 w-6 h-6" ];
                ]
              ();
            label
              ~a:[ a_class [ "text-sm font-medium" ]; a_label_for "data" ]
              [ txt "Is this file compressed?" ];
          ];
        div
          ~a:[ a_class [ "my-6" ] ]
          [
            Utils.button_component
              ~attribs:
                [
                  a_id ("upload-block-button-" ^ block_name);
                  a_onclick
                    ("uploadToVolume('" ^ block_name ^ "', '"
                    ^ Configuration.name_to_str instance_name
                    ^ "')");
                ]
              ~extra_css:"w-full" ~content:(txt "Upload data")
              ~btn_type:`Primary_full ();
          ];
      ])
