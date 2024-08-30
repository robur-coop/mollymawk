let unikernel_create_layout =
  Tyxml_html.(
    section
      ~a:[ a_class [ "col-span-7 p-4 bg-gray-50 my-1" ] ]
      [
        div
          ~a:[ a_class [ "px-3 flex justify-between items-center" ] ]
          [
            p
              ~a:[ a_class [ "font-bold text-gray-700" ] ]
              [ txt "Deploy a Unikernel" ];
          ];
        hr ();
        div
          ~a:[ a_class [ "space-y-6 mt-8 p-6 max-w-5xl mx-auto" ] ]
          [
            p ~a:[ a_id "form-alert"; a_class [ "my-4 hidden" ] ] [];
            div
              [
                label
                  ~a:[ a_class [ "block text-sm font-medium" ] ]
                  [ txt "Name*" ];
                input
                  ~a:
                    [
                      a_input_type `Text;
                      a_name "name";
                      a_required () ;
                      a_id "unikernel-name";
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
              [
                label
                  ~a:[ a_class [ "block text-sm font-medium" ] ]
                  [ txt "Arguments*" ];
                textarea
                  ~a:
                    [
                      a_rows 4;
                      a_required () ;
                      a_name "arguments";
                      a_id "unikernel-arguments";
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
                  (txt "");
              ];
            div
              [
                label
                  ~a:[ a_class [ "block text-sm font-medium" ] ]
                  [ txt "Unikernel Image Binary*" ];
                input
                  ~a:
                    [
                      a_input_type `File;
                      a_name "binary";
                      a_required () ;
                      a_id "unikernel-binary";
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
              ~a:[ a_class [ "flex justify-items-center mx-auto w-60" ] ]
              [
                button
                  ~a:
                    [
                      a_id "deploy-button";
                      a_onclick "deployUnikernel()";
                      a_class
                        [
                          "py-3 rounded bg-primary-500 hover:bg-primary-800 \
                           w-full text-gray-50 font-semibold";
                        ];
                    ]
                  [ txt "Deploy" ];
              ];
          ];
      ])
