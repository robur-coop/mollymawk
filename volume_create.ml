let create_volume total_free_space =
  Tyxml_html.(
    section
      ~a:[ a_id "block-create"; a_class [ "w-full mx-auto" ] ]
      [
        p ~a:[ a_id "form-alert" ] [];
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
          ~a:[ a_class [ "my-6" ] ]
          [
            label
              ~a:
                [
                  a_class [ "block text-sm font-medium" ];
                  a_label_for "block size";
                ]
              [ txt "Volume size" ];
            small
              ~a:[ a_class [ "text-sm" ] ]
              [
                txt
                  ("can assign up to: " ^ string_of_int total_free_space ^ " MB");
              ];
            div
              ~a:
                [
                  a_class [ "space-x-5 my-4" ];
                  Unsafe.string_attrib "x-data" "{count : 50}";
                ]
              [
                Utils.button_component
                  ~attribs:
                    [ Unsafe.string_attrib "x-on:click" "count = count + 50" ]
                  ~content:(i ~a:[ a_class [ "fa-solid fa-plus" ] ] [])
                  ~btn_type:`Primary_outlined ();
                span
                  ~a:
                    [
                      a_id "block_size";
                      a_contenteditable true;
                      a_class [ "text-4xl border px-4" ];
                      a_user_data "x-on:keydown.enter.prevent" "";
                      Unsafe.string_attrib "x-on:input"
                        "let value = \
                         $event.target.innerText.replace(/[^0-9]/g,'');\n\
                        \                                     \
                         $event.target.innerText = value;\n\
                        \                                     count = \
                         parseInt(value) || 1;";
                      Unsafe.string_attrib "x-text" "count";
                      Unsafe.string_attrib "x-on:blur"
                        "$event.target.innerText = count;";
                    ]
                  [];
                span ~a:[ a_class [ "text-4xl" ] ] [ txt "MB" ];
                Utils.button_component
                  ~attribs:
                    [
                      Unsafe.string_attrib "x-on:click"
                        "if (count > 1) count = count - 50";
                    ]
                  ~content:(i ~a:[ a_class [ "fa-solid fa-minus" ] ] [])
                  ~btn_type:`Danger_outlined ();
              ];
          ];
        div
          ~a:
            [
              a_class [ "my-6" ];
              Unsafe.string_attrib "x-data" "{ showUpload : false}";
            ]
          [
            label
              ~a:
                [
                  a_label_for "dataToggle";
                  a_class [ "inline-flex cursor-pointer items-center gap-3" ];
                ]
              [
                input
                  ~a:
                    [
                      a_id "dataToggle";
                      a_input_type `Checkbox;
                      a_class [ "peer sr-only" ];
                      a_role [ "switch" ];
                      Unsafe.string_attrib "x-on:click"
                        "showUpload = !showUpload";
                    ]
                  ();
                span
                  ~a:
                    [
                      a_aria "hidden" [ "true" ];
                      a_class
                        [
                          "relative h-6 w-11 after:h-5 after:w-5 \
                           peer-checked:after:translate-x-5 rounded-full \
                           border border-gray-300 bg-gray-50 after:absolute \
                           after:bottom-0 after:left-[0.0625rem] after:top-0 \
                           after:my-auto after:rounded-full after:bg-gray-600 \
                           after:transition-all after:content-[''] \
                           peer-checked:bg-primary-500 \
                           peer-checked:after:bg-white peer-focus:outline \
                           peer-focus:outline-2 peer-focus:outline-offset-2 \
                           peer-focus:outline-gray-800 \
                           peer-focus:peer-checked:outline-primary-500 \
                           peer-active:outline-offset-0 \
                           peer-disabled:cursor-not-allowed \
                           peer-disabled:opacity-70 dark:border-gray-700 \
                           dark:bg-gray-900 dark:after:bg-gray-300 \
                           dark:peer-checked:bg-primary-500 \
                           dark:peer-checked:after:bg-white \
                           dark:peer-focus:outline-gray-300 \
                           dark:peer-focus:peer-checked:outline-primary-500";
                        ];
                    ]
                  [];
                span
                  ~a:
                    [
                      a_class
                        [
                          "trancking-wide text-sm font-medium text-gray-600 \
                           peer-checked:text-gray-900 \
                           peer-disabled:cursor-not-allowed \n\
                          \                               \
                           dark:peer-checked:text-white";
                        ];
                    ]
                  [ txt "Dumb data to this volume" ];
              ];
            div
              ~a:
                [
                  Unsafe.string_attrib "x-show" "showUpload"; a_class [ "my-4" ];
                ]
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
                        [
                          a_class [ "text-sm font-medium" ]; a_label_for "data";
                        ]
                      [ txt "Is this file compressed?" ];
                  ];
              ];
          ];
        div
          ~a:[ a_class [ "my-6" ] ]
          [
            Utils.button_component
              ~attribs:
                [ a_id "create-block-button"; a_onclick "createVolume()" ]
              ~content:(txt "Create Volume") ~btn_type:`Primary_full ();
          ];
      ])
