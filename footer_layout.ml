open Tyxml

let footer =
  Html.(
    footer
      ~a:
        [
          a_class
            [
              "relative text-sm text-primary-50 leading-6 pb-8 pt-12 lg:pt-20 \
               text-medium bg-primary-950 bg-gradient-to-br \
               from-primary-900/50 via-primary-900/50 to-primary-950/50";
            ];
        ]
      [
        div
          ~a:
            [
              a_class
                [
                  "container mx-auto px-4 sm:px-6 lg:px-8 pb-12 lg:pb-20 grid \
                   md:grid-cols-5 items-start gap-10 md:gap-16";
                ];
            ]
          [
            a
              ~a:[ a_href "/" ]
              [
                img
                  ~a:[ a_class [ "md:w-24 w-16" ] ]
                  ~src:"/images/robur.png" ~alt:"Robur.coop" ();
              ];
            dl
              [
                dt
                  ~a:
                    [
                      a_class
                        [
                          "font-bold uppercase tracking-wider text-xs \
                           text-white mb-3";
                        ];
                    ]
                  [ txt "Mollymawk" ];
                dd
                  ~a:[ a_class [ "font-medium flex flex-col items-start" ] ]
                  [
                    a
                      ~a:
                        [
                          a_href "/about";
                          a_class [ "hover:text-primary-500 transition-colors" ];
                        ]
                      [ txt "About" ];
                    a
                      ~a:
                        [
                          a_href "/pricing";
                          a_class [ "hover:text-primary-500 transition-colors" ];
                        ]
                      [ txt "Pricing" ];
                    a
                      ~a:
                        [
                          a_href "https://robur.coop";
                          a_class [ "hover:text-primary-500 transition-colors" ];
                        ]
                      [ txt "Robur.coop" ];
                  ];
              ];
            dl
              [
                dt
                  ~a:
                    [
                      a_class
                        [
                          "font-bold uppercase tracking-wider text-xs \
                           text-white mb-3";
                        ];
                    ]
                  [ txt "Resources" ];
                dd
                  ~a:[ a_class [ "font-medium flex flex-col items-start" ] ]
                  [
                    a
                      ~a:
                        [
                          a_href "/";
                          a_class [ "hover:text-primary-500 transition-colors" ];
                        ]
                      [ txt "Docs" ];
                    a
                      ~a:
                        [
                          a_href "https://mirage.io";
                          a_class [ "hover:text-primary-500 transition-colors" ];
                        ]
                      [ txt "Mirage.io" ];
                    a
                      ~a:
                        [
                          a_href "https://ocaml.org";
                          a_class [ "hover:text-primary-500 transition-colors" ];
                        ]
                      [ txt "OCaml.org" ];
                  ];
              ];
            dl
              [
                dt
                  ~a:
                    [
                      a_class
                        [
                          "font-bold uppercase tracking-wider text-xs \
                           text-white mb-3";
                        ];
                    ]
                  [ txt "Contact" ];
                dd
                  ~a:[ a_class [ "font-medium flex flex-col items-start" ] ]
                  [
                    a
                      ~a:
                        [
                          a_href "https://github.com/robur-coop/mollymawk";
                          a_class [ "hover:text-primary-500 transition-colors" ];
                        ]
                      [ txt "GitHub" ];
                    a
                      ~a:
                        [
                          a_href "/";
                          a_class [ "hover:text-primary-500 transition-colors" ];
                        ]
                      [ txt "Matrix.org" ];
                    a
                      ~a:
                        [
                          a_href "/";
                          a_class [ "hover:text-primary-500 transition-colors" ];
                        ]
                      [ txt "Mastodon" ];
                  ];
              ];
            dl
              [
                dt
                  ~a:
                    [
                      a_class
                        [
                          "font-bold uppercase tracking-wider text-xs \
                           text-white mb-3";
                        ];
                    ]
                  [ txt "Legal" ];
                dd
                  ~a:[ a_class [ "font-medium flex flex-col items-start" ] ]
                  [
                    a
                      ~a:
                        [
                          a_href "/";
                          a_class [ "hover:text-primary-500 transition-colors" ];
                        ]
                      [ txt "Security" ];
                    a
                      ~a:
                        [
                          a_href "/";
                          a_class [ "hover:text-primary-500 transition-colors" ];
                        ]
                      [ txt "Privacy policy" ];
                    a
                      ~a:
                        [
                          a_href "/";
                          a_class [ "hover:text-primary-500 transition-colors" ];
                        ]
                      [ txt "Terms of service" ];
                  ];
              ];
          ];
        p
          ~a:[ a_class [ "text-xs text-center mb-0 mt-4" ] ]
          [ txt "Copyright © 2024 Mollymawk - Powered by Albatross" ];
      ])
