let input_classes =
  "ring-primary-100 mt-1.5 transition appearance-none block w-full px-3 py-3 \
   rounded-xl shadow-sm border hover:border-primary-200 \
   focus:border-primary-300 bg-primary-50 bg-opacity-0 hover:bg-opacity-50 \
   focus:bg-opacity-50 ring-primary-200 focus:ring-primary-200 \
   focus:ring-[1px] focus:outline-none disabled:bg-gray-100"

let select_classes =
  "ring-primary-100 mt-1.5 transition block w-full px-3 py-3 rounded-xl \
   shadow-sm border hover:border-primary-200 focus:border-primary-300 \
   bg-primary-50 bg-opacity-0 hover:bg-opacity-50 focus:bg-opacity-50 \
   ring-primary-200 focus:ring-primary-200 focus:ring-[1px] focus:outline-none"

let hidden_input ?(attribs = []) () =
  Tyxml_html.(input ~a:(a_input_type `Hidden :: attribs) ())

let csrf_input csrf =
  Tyxml_html.(
    hidden_input
      ~attribs:[ a_id "molly-csrf"; a_name "molly_csrf"; a_value csrf ]
      ())

let raw_input ?(attribs = []) () =
  Tyxml_html.(input ~a:(a_class [ input_classes ] :: attribs) ())

let form_input ?label_text ?description ?(attribs = []) field =
  let label_text =
    Option.value ~default:(String.capitalize_ascii field) label_text
  in
  let desc_elt =
    match description with
    | Some d ->
        [
          Tyxml_html.(
            p ~a:[ a_class [ "text-sm text-gray-500 my-1" ] ] [ txt d ]);
        ]
    | None -> []
  in
  Tyxml_html.(
    div
      ([
         label
           ~a:
             [ a_class [ "block text-sm font-medium my-1" ]; a_label_for field ]
           [ txt label_text ];
       ]
      @ desc_elt
      @ [ raw_input ~attribs:(a_id field :: a_name field :: attribs) () ]))

let raw_textarea ?(attribs = []) value =
  Tyxml_html.(textarea ~a:(a_class [ input_classes ] :: attribs) (txt value))

let form_textarea ?label_text ?description ?(attribs = []) ?(value = "") field =
  let label_text =
    Option.value ~default:(String.capitalize_ascii field) label_text
  in
  let desc_elt =
    match description with
    | Some d ->
        [
          Tyxml_html.(
            p ~a:[ a_class [ "text-sm text-gray-500 my-1" ] ] [ txt d ]);
        ]
    | None -> []
  in
  Tyxml_html.(
    div
      ([
         label
           ~a:[ a_class [ "block text-sm font-medium" ]; a_label_for field ]
           [ txt label_text ];
       ]
      @ desc_elt
      @ [ raw_textarea ~attribs:(a_id field :: a_name field :: attribs) value ]
      ))

let raw_select ?(attribs = []) options =
  Tyxml_html.(select ~a:(a_class [ select_classes ] :: attribs) options)

let form_select ?label_text ?description ?(attribs = []) field options =
  let label_text =
    Option.value ~default:(String.capitalize_ascii field) label_text
  in
  let desc_elt =
    match description with
    | Some d ->
        [
          Tyxml_html.(
            p ~a:[ a_class [ "text-sm text-gray-500 my-1" ] ] [ txt d ]);
        ]
    | None -> []
  in
  Tyxml_html.(
    div
      ~a:[ a_class [ "my-4" ] ]
      ([
         label
           ~a:[ a_class [ "block text-sm font-medium" ]; a_label_for field ]
           [ txt label_text ];
       ]
      @ desc_elt
      @ [ raw_select ~attribs:(a_id field :: a_name field :: attribs) options ]
      ))

let checkbox_input ?(attribs = []) () =
  Tyxml_html.(
    input
      ~a:
        (a_input_type `Checkbox
        :: a_class [ "accent-primary-500 mr-2 cursor-pointer" ]
        :: attribs)
      ())

let radio_input ?(attribs = []) () =
  Tyxml_html.(
    input ~a:(a_input_type `Radio :: a_class [ "sr-only" ] :: attribs) ())

let search_input () =
  Tyxml_html.(
    input
      ~a:
        [
          a_onkeyup "filterData()";
          a_placeholder "search";
          a_id "searchQuery";
          a_name "searchQuery";
          a_input_type `Text;
          a_class
            [
              "rounded py-2 px-3 border border-primary-200 \
               focus:border-primary-500 outline-0";
            ];
        ]
      ())

let switch_button ~switch_id ~switch_label ?(initial_state = false) html_content
    =
  let checked_attr =
    if initial_state then [ Tyxml_html.a_checked () ] else []
  in
  Tyxml_html.(
    div
      ~a:
        [
          a_class [ "my-6" ];
          Unsafe.string_attrib "x-data"
            ("{ toggle: " ^ string_of_bool initial_state ^ " }");
        ]
      [
        label
          ~a:
            [
              a_label_for switch_id;
              a_class [ "inline-flex cursor-pointer items-center gap-3" ];
            ]
          [
            input
              ~a:
                ([
                   a_input_type `Checkbox;
                   a_class [ "peer sr-only" ];
                   a_id switch_id;
                   a_name switch_id;
                   a_role [ "switch" ];
                   Unsafe.string_attrib "x-model" "toggle";
                 ]
                @ checked_attr)
              ();
            span
              ~a:
                [
                  a_aria "hidden" [ "true" ];
                  a_class
                    [
                      "relative h-6 w-11 after:h-5 after:w-5 \
                       peer-checked:after:translate-x-5 rounded-full border \
                       border-gray-300 bg-gray-50 after:absolute \
                       after:bottom-0 after:left-[0.0625rem] after:top-0 \
                       after:my-auto after:rounded-full after:bg-gray-600 \
                       after:transition-all after:content-[''] \
                       peer-checked:bg-primary-500 peer-checked:after:bg-white \
                       peer-focus:outline peer-focus:outline-2 \
                       peer-focus:outline-offset-2 peer-focus:outline-gray-800 \
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
                      "tracking-wide text-sm font-medium text-gray-600 \
                       peer-checked:text-gray-900 \
                       peer-disabled:cursor-not-allowed \
                       dark:peer-checked:text-white";
                    ];
                ]
              [ txt switch_label ];
          ];
        div
          ~a:[ Unsafe.string_attrib "x-show" "toggle"; a_class [ "my-4" ] ]
          [ html_content ];
      ])

let btn_classes = function
  | `Primary_full ->
      "py-2 px-2 rounded bg-primary-500 hover:bg-primary-800 text-gray-50 \
       font-semibold"
  | `Primary_outlined ->
      "py-2 px-2 rounded border border-1 border-primary-400 text-primary-600 \
       hover:text-gray-50 focus:outline-none hover:bg-primary-800 \
       font-semibold"
  | `Danger_full ->
      "py-2 px-2 rounded bg-secondary-500 hover:bg-secondary-800 text-gray-50 \
       font-semibold"
  | `Danger_outlined ->
      "py-2 px-2 rounded border border-1 border-secondary-400 \
       text-secondary-600 hover:text-gray-50 focus:outline-none \
       hover:bg-secondary-800 font-semibold"
  | `Custom css -> css

let button_component ~attribs ~content ~btn_type () =
  Tyxml_html.(
    button ~a:(attribs @ [ a_class [ btn_classes btn_type ] ]) [ content ])

let link_button ~href ?(attribs = []) ~content ~btn_type () =
  Tyxml_html.(
    a
      ~a:((a_href href :: attribs) @ [ a_class [ btn_classes btn_type ] ])
      [ content ])

let tab_style is_active =
  if is_active then
    "px-6 py-2.5 text-sm font-medium text-primary-50 bg-primary-500 \
     rounded-t-lg border-b-0"
  else
    "px-6 py-2.5 text-sm font-medium text-primary-600 bg-gray-100 \
     hover:bg-primary-50 rounded-t-lg transition-colors duration-200"

let tab_bar tabs =
  Tyxml_html.(
    div
      ~a:
        [
          a_class
            [ "flex items-center space-x-1 border-b-2 border-primary-500 mb-6" ];
        ]
      (List.map
         (fun (href, icon_css, label_text, is_active) ->
           a
             ~a:[ a_href href; a_class [ tab_style is_active ] ]
             [ i ~a:[ a_class [ icon_css ^ " mr-2" ] ] []; txt label_text ])
         tabs))

let sortable_th col_name =
  Tyxml_html.(
    th
      ~a:
        [
          Unsafe.string_attrib "x-on:click" "sortByColumn";
          Unsafe.string_attrib ":class"
            (Printf.sprintf
               "sortBy === '%s' ? 'border-b-2 border-primary-500 \
                text-primary-800 bg-primary-100' : 'hover:bg-primary-100 \
                text-primary-600'"
               col_name);
          a_class
            [
              "px-6 py-3 text-start text-xs font-bold uppercase cursor-pointer \
               select-none";
            ];
        ]
      [
        span [ txt col_name ];
        span
          ~a:
            [
              a_class [ "px-2" ];
              Unsafe.string_attrib "x-show"
                (Printf.sprintf "sortBy === '%s'" col_name);
            ]
          [
            i
              ~a:
                [
                  Unsafe.string_attrib ":class"
                    "sortAsc ? 'fa-solid fa-sort-up' : 'fa-solid fa-sort-down'";
                ]
              [];
          ];
      ])

let static_th label_text =
  Tyxml_html.(
    th
      ~a:
        [
          a_class
            [
              "px-6 py-3 text-start text-xs font-bold text-primary-600 \
               uppercase select-none";
            ];
        ]
      [ txt label_text ])

let td_cell children =
  Tyxml_html.(
    td
      ~a:
        [
          a_class
            [ "px-6 py-4 text-sm font-medium text-gray-800 whitespace-nowrap" ];
        ]
      children)

let td_txt str = td_cell [ Tyxml_html.txt str ]

let simple_table ?(attribs = []) ~headers rows =
  let css = "data-table table-auto min-w-full divide-y divide-gray-200" in
  Tyxml_html.(
    table ~a:(a_class [ css ] :: attribs) ~thead:(thead [ tr headers ]) rows)

let table_wrapper ?(sortable = true) content =
  let inner_a =
    (if sortable then [ Tyxml_html.Unsafe.string_attrib "x-data" "sort_data()" ]
     else [])
    @ [ Tyxml_html.a_class [ "overflow-hidden" ] ]
  in
  Tyxml_html.(
    div
      ~a:[ a_class [ "flex flex-col" ] ]
      [
        div
          ~a:[ a_class [ "-m-1.5 overflow-x-auto" ] ]
          [
            div
              ~a:[ a_class [ "p-1.5 min-w-full inline-block align-middle" ] ]
              [ div ~a:inner_a [ content ] ];
          ];
      ])

let data_table ?(id = "data-table") ?(sortable = true) ~headers rows =
  table_wrapper ~sortable
    (simple_table ~attribs:[ Tyxml_html.a_id id ] ~headers rows)

let pie_chart ~canvas_id ~dataset_label ~labels ~data =
  let labels_str =
    String.concat "," (List.map (Printf.sprintf "'%s'") labels)
  in
  let data_str = String.concat ", " (List.map string_of_int data) in
  let init_js =
    Printf.sprintf
      "chart = new Chart(document.getElementById('%s').getContext('2d'), { \
       type: 'pie', data: { labels: [%s], datasets: [{ label: '%s', data: \
       [%s], backgroundColor: ['rgb(156, 156, 156)','rgb(54, 156, 140)'], \
       hoverOffset: 4 }] }, options: {} });"
      canvas_id labels_str dataset_label data_str
  in
  Tyxml_html.(
    div
      ~a:
        [
          Unsafe.string_attrib "x-data" "chart: null";
          Unsafe.string_attrib "x-init" init_js;
          a_class [ "flex justify-center items-center" ];
          a_style "position:relative; height:40vh; width:40vh;";
        ]
      [ canvas ~a:[ a_id canvas_id ] [] ])

let bar_chart ~canvas_id ~dataset_label ~labels ~data ~x_title ~y_title =
  let labels_str =
    String.concat ", " (List.map (Printf.sprintf "\"%s\"") labels)
  in
  let data_str = String.concat ", " (List.map (Printf.sprintf "\"%d\"") data) in
  let init_js =
    Printf.sprintf
      "chart = new Chart(document.getElementById('%s').getContext('2d'), { \
       type: 'bar', data: { labels: [%s], datasets: [{ label: '%s', data: \
       [%s], backgroundColor: ['rgb(54, 156, 140)'], hoverOffset: 4 }] }, \
       options: { scales: { x: { title: { display: true, text: '%s' } }, y: { \
       title: { display: true, text: '%s' }, ticks: { stepSize: 1, callback: \
       function(value) { if (Number.isInteger(value)) { return value; } return \
       null; } }, beginAtZero: true } } } });"
      canvas_id labels_str dataset_label data_str x_title y_title
  in
  Tyxml_html.(
    div
      ~a:
        [
          Unsafe.string_attrib "x-data" "chart: null";
          Unsafe.string_attrib "x-init" init_js;
          a_class [ "flex justify-center items-center" ];
        ]
      [
        div
          ~a:
            [
              a_class [ "chart-container" ];
              a_style "position:relative; height:40vh; width:80vh;";
            ]
          [ canvas ~a:[ a_id canvas_id ] [] ];
      ])

let alpine_multiselect ~id ~label_text ~button_label ?(option_prefix = "")
    ~selected_json ~options_json () =
  Tyxml_html.(
    div
      ~a:[ a_class [ "my-3" ] ]
      [
        label ~a:[ a_class [ "block font-medium" ] ] [ txt label_text ];
        div
          ~a:
            [
              Unsafe.string_attrib "x-data"
                (Printf.sprintf "multiselect(%s, %s)" selected_json options_json);
              a_class [ "multiselect border my-4 p-4 rounded" ];
            ]
          [
            div
              ~a:[ a_class [ "selected-items" ] ]
              [
                template
                  ~a:
                    [
                      Unsafe.string_attrib "x-for" "(item, index) in selected";
                      Unsafe.string_attrib ":key" "item";
                    ]
                  [
                    span
                      ~a:
                        [
                          a_class
                            [ "selected-tag rounded bg-primary-100 p-1 ml-1" ];
                        ]
                      [
                        span ~a:[ Unsafe.string_attrib "x-text" "item" ] [];
                        button_component
                          ~attribs:
                            [
                              a_button_type `Button;
                              Unsafe.string_attrib "x-on:click"
                                "removeItem(index)";
                            ]
                          ~content:(txt "x") ~btn_type:`Danger_full ();
                      ];
                  ];
              ];
            div
              ~a:
                [
                  Unsafe.string_attrib "x-on:click" "toggleDropdown";
                  a_class [ "dropdown my-3" ];
                ]
              [
                button_component
                  ~attribs:[ a_button_type `Button ]
                  ~content:
                    (span
                       [
                         span ~a:[ a_class [ "px-2" ] ] [ txt button_label ];
                         i ~a:[ a_class [ "fa-solid fa-caret-down" ] ] [];
                       ])
                  ~btn_type:`Primary_outlined ();
              ];
            div
              ~a:
                [
                  Unsafe.string_attrib "x-show" "isOpen";
                  a_class [ "dropdown-list" ];
                ]
              [
                template
                  ~a:
                    [
                      Unsafe.string_attrib "x-for" "(option, index) in options";
                      Unsafe.string_attrib ":key" "option";
                    ]
                  [
                    p
                      ~a:[ a_class [ "py-2" ] ]
                      ([
                         checkbox_input
                           ~attribs:
                             [
                               Unsafe.string_attrib ":value" "option";
                               Unsafe.string_attrib "x-on:change"
                                 "updateSelection($event, option)";
                               Unsafe.string_attrib ":checked"
                                 "selected.includes(option)";
                             ]
                           ();
                       ]
                      @ (if option_prefix = "" then []
                         else [ span [ txt option_prefix ] ])
                      @ [
                          span ~a:[ Unsafe.string_attrib "x-text" "option" ] [];
                        ]);
                  ];
              ];
            hidden_input
              ~attribs:
                [ a_id id; Unsafe.string_attrib ":value" "selected.join(',')" ]
              ();
          ];
      ])

let dynamic_dropdown_form (items : 'a list) ~(get_label : 'a -> string)
    ~(get_value : 'a -> string) ~(id : string) ?(manual_entry = false) () =
  let alpine_options =
    "["
    ^ String.concat ", "
        (List.map
           (fun item ->
             Printf.sprintf "{ label: '%s', value: '%s' }"
               (String.escaped (get_label item))
               (String.escaped (get_value item)))
           items)
    ^ "]"
  in
  Tyxml_html.(
    div
      ~a:
        [
          Unsafe.string_attrib "x-data"
            ("{ fields: [], options: " ^ alpine_options ^ ", field_id: '" ^ id
           ^ "' }");
          Unsafe.string_attrib "@populate-manifest.window"
            ("if (window.mapFields && $event.detail[field_id]) fields = \
              window.mapFields(field_id, $event.detail, " ^ alpine_options
           ^ "); else if (!$event.detail[field_id]) fields = [];");
        ]
      [
        Unsafe.data "<template x-for='(field, index) in fields' :key='index'>";
        div
          ~a:[ a_class [ "grid grid-cols-2 gap-4 my-2" ] ]
          [
            (* Text input with dynamic ID *)
            div
              ~a:[ a_class [ "flex space-x-2" ] ]
              [
                (* Remove button *)
                Unsafe.data
                  {|
                  <button type="button" @click="fields.splice(index, 1)"
                    class="text-secondary-500 text-2xl hover:text-secondary-700 font-bold">
                    &times;
                  </button>
                |};
                raw_input
                  ~attribs:
                    [
                      a_name "name";
                      a_required ();
                      a_placeholder "Name of this device in your unikernel";
                      Unsafe.string_attrib ":id" "field_id + '-input-' + index";
                      Unsafe.string_attrib "x-model" "field.title";
                    ]
                  ();
              ];
            div
              ~a:[ Unsafe.string_attrib "x-data" "{ isManual: false }" ]
              [
                (if manual_entry then
                   div
                     [
                       label
                         ~a:
                           [
                             a_class
                               [
                                 "inline-flex cursor-pointer items-center gap-3";
                               ];
                           ]
                         [
                           span [ txt "Type host device name manually" ];
                           checkbox_input
                             ~attribs:
                               [
                                 Unsafe.string_attrib ":id"
                                   "field_id + '-toggle-' + index";
                                 a_role [ "switch" ];
                                 Unsafe.string_attrib "x-model" "isManual";
                               ]
                             ();
                         ];
                     ]
                 else div []);
                template
                  ~a:[ Unsafe.string_attrib "x-if" "isManual" ]
                  [
                    div
                      [
                        raw_input
                          ~attribs:
                            [
                              a_name "manual-name";
                              a_placeholder "Name of this device on the host";
                              Unsafe.string_attrib ":id"
                                "field_id + '-select-' + index";
                            ]
                          ();
                      ];
                  ];
                template
                  ~a:[ Unsafe.string_attrib "x-if" "!isManual" ]
                  [
                    div
                      [
                        raw_select
                          ~attribs:
                            [
                              Unsafe.string_attrib ":id"
                                "field_id + '-select-' + index";
                            ]
                          [
                            Unsafe.data
                              {|
                    <template x-for="option in options" :key="option.value">
                      <option :value="option.value" x-text="option.label" :selected="option.value === field.selected"></option>
                    </template>
                  |};
                          ];
                      ];
                  ];
              ];
          ];
        Unsafe.data "</template>";
        (* Add button *)
        button_component
          ~attribs:
            [
              a_button_type `Button;
              Unsafe.string_attrib "@click"
                "fields.push({ selected: '', title: '' })";
            ]
          ~content:(txt "+ Add") ~btn_type:`Primary_full ();
      ])

let form_link ~label_ ~href () =
  Tyxml_html.(
    a
      ~a:
        [
          a_href href;
          a_class
            [
              "hover:text-primary-800 transition-colors my-4 cursor-pointer \
               text-sm font-medium text-primary-500";
            ];
        ]
      [ txt label_ ])

let alert_element =
  Tyxml_html.(
    div
      ~a:
        [
          a_id "alert-container";
          a_class
            [
              "fixed top-1/4 rounded-md right-4 z-50 w-fit space-y-2 p-4 \
               shadow text-wrap hidden";
            ];
        ]
      [])

let heading label_ =
  Tyxml_html.(
    h1
      ~a:
        [
          a_class
            [ "font-semibold text-2xl md:text-3xl mb-8 text-primary-800 p-6" ];
        ]
      [ txt label_ ])

(** A UI with two buttons, one for decrementing a value and the other for
    incrementing the value*)
let increment_or_decrement_ui ~max_value ~min_value ?(step = 1)
    ?(default_value = 0) ?(figure_unit = "") ~id ~label' () =
  let max_value = if max_value <= min_value then min_value else max_value in
  let default_value =
    if default_value <= min_value then min_value
    else if default_value >= max_value then max_value
    else default_value
  in
  Tyxml_html.(
    div
      ~a:
        [
          a_class [ "space-x-5" ];
          Unsafe.string_attrib "x-data"
            ("{count : " ^ string_of_int default_value ^ "}");
        ]
      [
        label
          ~a:[ a_class [ "block font-medium my-2" ] ]
          [ txt (Fmt.str "%s (max %d %s)" label' max_value figure_unit) ];
        button_component
          ~attribs:
            [
              a_button_type `Button;
              Unsafe.string_attrib "x-on:click"
                (Fmt.str "if (count > %d) count = count - %d" min_value step);
            ]
          ~content:(i ~a:[ a_class [ "fa-solid fa-minus" ] ] [])
          ~btn_type:`Danger_outlined ();
        span
          ~a:
            [
              a_id id;
              a_contenteditable `True;
              a_class [ "text-4xl border px-4" ];
              Unsafe.string_attrib "@keydown.enter.prevent" "";
              Unsafe.string_attrib "x-text" "count";
              Unsafe.string_attrib "x-on:blur"
                (Fmt.str
                   "let rawValue = parseInt($el.innerText.replace(/[^0-9-]/g, \
                    '')) || 0; count = Math.max(%d, Math.min(%d, rawValue)); \
                    $el.innerText = count;"
                   min_value max_value);
              Unsafe.string_attrib "x-init" "$el.innerText = count";
            ]
          [];
        span ~a:[ a_class [ "text-4xl" ] ] [ txt figure_unit ];
        button_component
          ~attribs:
            [
              a_button_type `Button;
              Unsafe.string_attrib "x-on:click"
                (Fmt.str "if (count < %d) count = count + %d" max_value step);
            ]
          ~content:(i ~a:[ a_class [ "fa-solid fa-plus" ] ] [])
          ~btn_type:`Primary_outlined ();
      ])

let display_banner = function
  | Some message ->
      Tyxml_html.(
        section
          ~a:
            [
              a_class
                [
                  "w-full bg-primary-200 py-4 text-center text-gray-200 border \
                   border-primary-400 font-semibold flex justify-center px-5 \
                   space-x-5";
                ];
              a_id "banner-message";
            ]
          [
            p [ txt message ];
            button_component
              ~attribs:[ a_id "close-banner-btn"; a_onclick "closeBanner()" ]
              ~content:(i ~a:[ a_class [ "fa-solid fa-x text-sm" ] ] [])
              ~btn_type:`Primary_outlined ();
          ])
  | None -> Tyxml_html.div []
