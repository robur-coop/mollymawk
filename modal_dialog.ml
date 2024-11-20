let modal_dialog ~modal_title ~button_content ~content =
  Tyxml_html.(
    section
      ~a:[ a_id "modal-dialog"; a_class [ "mx-auto" ] ]
      [
        div
          ~a:[ Unsafe.string_attrib "x-data" "{modalIsOpen: false}" ]
          [
            Utils.button_component ~content:button_content
              ~btn_type:`Primary_full ()
              ~attribs:
                [ Unsafe.string_attrib "x-on:click" "modalIsOpen = true" ];
            div
              ~a:
                [
                  Unsafe.string_attrib "x-cloak" "";
                  Unsafe.string_attrib "x-show" "modalIsOpen";
                  Unsafe.string_attrib "x-transition.opacity.duration.200ms" "";
                  Unsafe.string_attrib "x-trap.inert.noscroll" "modalIsOpen";
                  Unsafe.string_attrib "x-on:keydown.esc.window"
                    "modalIsOpen = false";
                  Unsafe.string_attrib "x-on:click.self" "modalIsOpen = false";
                  a_class
                    [
                      "fixed inset-0 z-30 flex items-end justify-center \
                       bg-black/20 p-4 backdrop-blur-md sm:items-center";
                    ];
                  a_role [ "dialog" ];
                  a_aria "modal" [ "true" ];
                ]
              [
                div
                  ~a:
                    [
                      Unsafe.string_attrib "x-show" "modalIsOpen";
                      Unsafe.string_attrib "x-transition:enter"
                        "transition ease-out duration-200 delay-100 \
                         motion-reduce:transition-opacity";
                      Unsafe.string_attrib "x-transition:enter-start"
                        "opacity-0 scale-50";
                      Unsafe.string_attrib "x-transition:enter-end"
                        "opacity-100 scale-100";
                      a_class
                        [
                          "flex max-w-xl flex-col gap-4 overflow-hidden \
                           rounded-md border border-neutral-300 bg-gray-50";
                        ];
                    ]
                  [
                    div
                      ~a:
                        [
                          a_class
                            [ "flex items-center justify-between border-b p-4" ];
                        ]
                      [
                        h3
                          ~a:[ a_class [ "font-bold text-gray-700" ] ]
                          [ txt modal_title ];
                        i
                          ~a:
                            [
                              a_class [ "fa-solid fa-x text-sm cursor-pointer" ];
                              Unsafe.string_attrib "x-on:click"
                                "modalIsOpen = false";
                            ]
                          [];
                      ];
                    div ~a:[ a_class [ "px-4" ] ] [ content ];
                  ];
              ];
          ];
      ])
