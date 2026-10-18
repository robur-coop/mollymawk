let index_page =
  Tyxml_html.(
    main
      ~a:[ a_class [ "w-full text-gray-900" ] ]
      [
        section
          ~a:
            [
              a_class
                [
                  "relative container lg:max-w-7xl mx-auto px-4 sm:px-6 \
                   lg:px-8 py-10 xl:pb-32";
                ];
            ]
          [
            div
              ~a:[ a_class [ "grid grid-cols-2 gap-4" ] ]
              [
                div
                  ~a:[ a_user_data "aos" "fade-up" ]
                  [
                    h1
                      ~a:
                        [
                          a_class
                            [
                              "font-heading text-3xl sm:text-3xl md:text-4xl \
                               lg:text-5xl relative text-white mb-4 font-bold";
                            ];
                        ]
                      [
                        span
                          ~a:[ a_class [ "relative m-auto" ] ]
                          [
                            txt "Deploy ";
                            span
                              ~a:[ a_class [ "text-primary-500" ] ]
                              [ txt "MirageOS" ];
                            br ();
                            txt "Unikernels In Seconds";
                          ];
                      ];
                    p
                      ~a:
                        [
                          a_class
                            [
                              "text-base lg:text-lg xl:text-xl \
                               text-primary-900 tracking-prose mb-9";
                            ];
                        ]
                      [
                        strong [ txt "Zero configuration, " ];
                        txt
                          "robust security and maximum productivity. With \
                           Mollymawk, you can focus more on your product.";
                      ];
                    div
                      ~a:[ a_class [ "flex flex-col items-start" ] ]
                      [
                        a
                          ~a:
                            [
                              a_href "/sign-up";
                              a_class
                                [
                                  "bg-primary-500 text-gray-100 font-semibold \
                                   py-3 px-2 rounded-md text-xl \
                                   hover:bg-primary-800";
                                ];
                            ]
                          [ txt "Deploy A Unikernel" ];
                      ];
                  ];
                div
                  ~a:
                    [
                      a_user_data "aos" "fade-up";
                      a_class
                        [
                          "shadow-md rounded-2xl p-0 bg-cover bg-center h-96 \
                           relative overflow-hidden";
                        ];
                      a_style
                        "background-image: url('/images/molly_bird.jpeg');";
                    ]
                  [];
              ];
            div
              ~a:[ a_class [ "my-10 text-gray-900" ] ]
              [
                h2
                  ~a:[ a_class [ "text-3xl font-bold text-center my-5" ] ]
                  [ txt "Why Mollymawk?" ];
                div
                  ~a:[ a_class [ "grid grid-cols-3 space-x-4 my-10" ] ]
                  [
                    div
                      ~a:
                        [
                          a_class
                            [
                              "shadow-md rounded-2xl p-0 bg-cover bg-center \
                               h-96 relative overflow-hidden";
                            ];
                          a_style
                            "background-image: url('/images/dashboard_1.png');";
                        ]
                      [
                        div
                          ~a:
                            [
                              a_class [ "absolute inset-0" ];
                              a_style
                                "background-image: linear-gradient(transparent \
                                 0%, #1f423e 100%);";
                              a_user_data "aos" "fade-up";
                            ]
                          [
                            div
                              ~a:[ a_class [ "h-full flex" ] ]
                              [
                                div
                                  ~a:
                                    [
                                      a_class
                                        [
                                          "leading-none p-6 rounded-2xl \
                                           mt-auto mb-2 text-4xl font-semibold \
                                           drop-shadow-sm tracking-tight";
                                        ];
                                      a_style "color: rgb(254, 242, 243);";
                                    ]
                                  [
                                    txt "User friendly";
                                    br ();
                                    span
                                      ~a:[ a_class [ "text-secondary-300" ] ]
                                      [ txt "Dashboard" ];
                                    br ();
                                    txt "with all you need.";
                                  ];
                              ];
                          ];
                      ];
                    div
                      ~a:
                        [
                          a_class
                            [
                              "shadow-md rounded-2xl p-0 bg-cover bg-center \
                               h-96 relative overflow-hidden";
                            ];
                          a_style
                            "background-image: url('/images/albatross_1.png');";
                        ]
                      [
                        div
                          ~a:
                            [
                              a_class [ "absolute inset-0" ];
                              a_style
                                "background-image: linear-gradient(transparent \
                                 0%, #882314 100%);";
                              a_user_data "aos" "fade-up";
                            ]
                          [
                            div
                              ~a:[ a_class [ "h-full flex" ] ]
                              [
                                div
                                  ~a:
                                    [
                                      a_class
                                        [
                                          "leading-none p-6 rounded-2xl \
                                           mt-auto mb-2 text-4xl font-semibold \
                                           drop-shadow-sm tracking-tight";
                                        ];
                                      a_style "color: rgb(254, 242, 243);";
                                    ]
                                  [
                                    txt "Powered by";
                                    br ();
                                    span
                                      ~a:[ a_class [ "text-primary-300" ] ]
                                      [ txt "Albatross" ];
                                    br ();
                                    txt "Engine.";
                                  ];
                              ];
                          ];
                      ];
                    div
                      ~a:
                        [
                          a_class
                            [
                              "shadow-md rounded-2xl p-0 bg-cover bg-center \
                               h-96 relative overflow-hidden";
                            ];
                          a_style
                            "background-image: url('/images/mirage_os_1.png');";
                        ]
                      [
                        div
                          ~a:
                            [
                              a_class [ "absolute inset-0" ];
                              a_style
                                "background-image: linear-gradient(transparent \
                                 0%, #1f423e 100%);";
                              a_user_data "aos" "fade-up";
                            ]
                          [
                            div
                              ~a:[ a_class [ "h-full flex" ] ]
                              [
                                div
                                  ~a:
                                    [
                                      a_class
                                        [
                                          "leading-none p-6 rounded-2xl \
                                           mt-auto mb-2 text-4xl font-semibold \
                                           drop-shadow-sm tracking-tight";
                                        ];
                                      a_style "color: rgb(254, 242, 243);";
                                    ]
                                  [
                                    txt "Focus on your";
                                    br ();
                                    span
                                      ~a:[ a_class [ "text-secondary-300" ] ]
                                      [ txt "Product" ];
                                    br ();
                                    txt "not infrasture.";
                                  ];
                              ];
                          ];
                      ];
                  ];
              ];
            div
              ~a:[ a_class [ "my-5" ] ]
              [
                h2
                  ~a:[ a_class [ "text-3xl font-semibold" ] ]
                  [ txt "Users of Mollymawk" ];
                p
                  ~a:[ a_class [ "text-xl mt-4" ] ]
                  [ txt "Learn how Mollymawk fits your use case." ];
                div
                  ~a:[ a_class [ "grid grid-cols-2 space-x-20 my-4 text-xl" ] ]
                  [
                    div
                      ~a:
                        [
                          a_class [ "rounded bg-primary-50 p-10 shadow-md" ];
                          a_user_data "aos" "fade-up";
                        ]
                      [
                        h3
                          ~a:
                            [
                              a_class
                                [ "font-semibold text-3xl my-4 text-gray-900" ];
                            ]
                          [ txt "Unikernel Operators" ];
                        p
                          [
                            txt
                              "Manage and deploy unikernels without needing to \
                               write any code. Explore our marketplace of \
                               ready-built Unikernels and choose from dozens \
                               of high quality projects ranging from Mail \
                               servers to Caldav servers etc.";
                          ];
                        div
                          ~a:[ a_class [ "mt-10" ] ]
                          [
                            a
                              ~a:
                                [
                                  a_href "/blog/unikernel-operators";
                                  a_class
                                    [
                                      "bg-primary-500 hover:bg-primary-800 \
                                       px-2 py-3 text-gray-50 text-xl rounded";
                                    ];
                                ]
                              [ txt "Learn More" ];
                          ];
                      ];
                    div
                      ~a:
                        [
                          a_class [ "rounded bg-primary-50 p-10 shadow-md" ];
                          a_user_data "aos" "fade-up";
                        ]
                      [
                        h3
                          ~a:
                            [
                              a_class
                                [ "font-semibold text-3xl my-4 text-gray-900" ];
                            ]
                          [ txt "Unikernel Developers" ];
                        p
                          [
                            txt
                              "Write and test your unikernels in an \
                               environment tailored specifically for \
                               unikernels. Integrate with your CI/CD pipelines \
                               to automate the deployment process, pushing \
                               your code directly to production.";
                          ];
                        div
                          ~a:[ a_class [ "mt-10" ] ]
                          [
                            a
                              ~a:
                                [
                                  a_href "/blog/unikernel-developers";
                                  a_class
                                    [
                                      "bg-primary-500 hover:bg-primary-800 \
                                       px-2 py-3 text-gray-50 text-xl rounded";
                                    ];
                                ]
                              [ txt "Learn More" ];
                          ];
                      ];
                  ];
              ];
            div
              ~a:
                [
                  a_class
                    [
                      "shadow-md p-10 overflow-hidden text-center \
                       bg-primary-100 rounded-md";
                    ];
                  a_user_data "aos" "fade-up";
                ]
              [
                div
                  ~a:[ a_class [ "container-fluid" ] ]
                  [
                    h2
                      ~a:[ a_class [ "font-bold text-gray-900 text-xl" ] ]
                      [ txt "Reproducible builds" ];
                    p
                      ~a:[ a_class [ "text-lg text-content text-gray-900" ] ]
                      [ txt "Explore dozens of ready-to-go unikernels" ];
                  ];
                div
                  ~a:[ a_class [ "my-5" ] ]
                  [
                    a
                      ~a:
                        [
                          a_href "/robur/shop";
                          a_class
                            [
                              "bg-primary-500 rounded py-3 px-3 text-gray-50 \
                               text-center hover:bg-primary-800";
                            ];
                        ]
                      [ txt "Explore Marketplace" ];
                  ];
              ];
          ];
      ])
