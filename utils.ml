open Lwt.Infix

let ( let* ) = Result.bind

module Json = struct
  let get key assoc =
    Option.map snd (List.find_opt (fun (k, _) -> String.equal k key) assoc)

  let string_or_none field = function
    | None | Some `Null -> Ok None
    | Some (`String v) -> Ok (Some v)
    | Some json ->
        Error
          (`Msg
             ("invalid json for " ^ field ^ ": " ^ Yojson.Basic.to_string json))
end

module TimeHelper = struct
  let ptime_of_string (t_str : string) : (Ptime.t, [> `Msg of string ]) result =
    match Ptime.of_rfc3339 t_str with
    | Ok (ptime, _, _) -> Ok ptime
    | Error (`RFC3339 (_, err)) ->
        Logs.err (fun m -> m "string_to_ptime: %a" Ptime.pp_rfc3339_error err);
        Error
          (`Msg
             (Format.asprintf "invalid created_at format: %a"
                Ptime.pp_rfc3339_error err))

  let string_of_ptime (t : Ptime.t) : string =
    Ptime.to_rfc3339 ~space:true t ~frac_s:0

  (* calculate the diff between two timestamps *)
  let diff_in_seconds ~current_time ~check_time =
    let span = Ptime.diff current_time check_time in
    match Ptime.Span.to_int_s span with Some s -> s | None -> 0

  (* print timestamp in human-readable form *)
  let print_human_readable ~now ~timestamp =
    let duration =
      diff_in_seconds ~current_time:now ~check_time:timestamp |> Duration.of_sec
    in
    Format.asprintf "%a" Duration.pp duration

  (* parse Ptime.t option to json *)
  let ptime_to_json ptime =
    Option.value ~default:`Null
      (Option.map (fun p -> `String (string_of_ptime p)) ptime)

  (* parse Ptime.t option from a json *)
  let ptime_of_json = function
    | `String s -> (
        match ptime_of_string s with
        | Ok ptime -> Ok (Some ptime)
        | Error _ -> Error (`Msg "invalid date string"))
    | `Null -> Ok None
    | _ -> Error (`Msg "invalid json for Ptime.t option")

  let future_time ptime seconds =
    let created_span = Ptime.to_span ptime in
    let expiry = Ptime.Span.of_int_s seconds in
    let future_span = Ptime.Span.add created_span expiry in
    Ptime.of_span future_span

  let time_ago ~current_time ~check_time =
    let diff = diff_in_seconds ~current_time ~check_time in
    if diff < 60 then Printf.sprintf "%d s ago" diff
    else if diff < 3600 then Printf.sprintf "%d m ago" (diff / 60)
    else if diff < 86400 then Printf.sprintf "%d h ago" (diff / 3600)
    else if diff < 604800 then Printf.sprintf "%d d ago" (diff / 86400)
    else if diff < 2592000 then Printf.sprintf "%d w ago" (diff / 604800)
    else if diff < 31536000 then Printf.sprintf "%d mo ago" (diff / 2419200)
    else Printf.sprintf "%d y ago" (diff / 31536000)
end

module Email = struct
  type config = {
    smtp_host : string;
    smtp_port : int;
    username : string;
    password : string;
  }

  let validate_email email =
    match Emile.of_string email with
    | Ok _ -> true
    | Error s ->
        Logs.err (fun m -> m "Emile-Email-Validation: %a" Emile.pp_error s);
        false

  let generate_verification_link uuid =
    "/auth/verify?token=" ^ Uuidm.to_string uuid
end

module Status = struct
  type t = { code : int; title : string; data : Yojson.Basic.t; success : bool }

  let to_json (status : t) : string =
    `Assoc
      [
        ("status", `Int status.code);
        ("title", `String status.title);
        ("success", `Bool status.success);
        ("data", status.data);
      ]
    |> Yojson.Basic.to_string
end

let csrf_form_input csrf =
  Tyxml_html.(
    input
      ~a:
        [
          a_input_type `Hidden;
          a_id "molly-csrf";
          a_name "molly-csrf-input";
          a_value csrf;
        ]
      ())

let button_component ~attribs ~content ~btn_type ?(extra_css = "") () =
  Tyxml_html.(
    button
      ~a:
        (List.fold_left (fun acc attrib -> acc @ [ attrib ]) [] attribs
        @ [
            (match btn_type with
            | `Primary_full ->
                a_class
                  [
                    "py-2 px-2 rounded bg-primary-500 hover:bg-primary-800 \
                     text-gray-50 font-semibold " ^ extra_css;
                  ]
            | `Primary_outlined ->
                a_class
                  [
                    "py-2 px-2 rounded border border-1 border-primary-400 \
                     text-primary-600 hover:text-gray-50 focus:outline-none \
                     hover:bg-primary-800 font-semibold " ^ extra_css;
                  ]
            | `Danger_full ->
                a_class
                  [
                    "py-2 px-2 rounded bg-secondary-500 hover:bg-secondary-800 \
                     text-gray-50 font-semibold " ^ extra_css;
                  ]
            | `Danger_outlined ->
                a_class
                  [
                    "py-2 px-2 rounded border border-1 border-secondary-400 \
                     text-secondary-600 hover:text-gray-50 focus:outline-none \
                     hover:bg-secondary-800 font-semibold " ^ extra_css;
                  ]);
          ])
      [ content ])

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
              Unsafe.string_attrib "x-on:click"
                (Fmt.str "if (count > %d) count = count - %d" min_value step);
            ]
          ~content:(i ~a:[ a_class [ "fa-solid fa-minus" ] ] [])
          ~btn_type:`Danger_outlined ();
        span
          ~a:
            [
              a_id id;
              a_contenteditable true;
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

let bytes_to_megabytes bytes =
  let megabytes = float_of_int bytes /. (1024.0 *. 1024.0) in
  Printf.sprintf "%.2f MB" megabytes

(*10 minutes for a rollback to be valid*)
let rollback_seconds_limit = 600

let switch_button ~switch_id ~switch_label ?(initial_state = false) html_content
    =
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
                [
                  a_id switch_id;
                  a_input_type `Checkbox;
                  a_class [ "peer sr-only" ];
                  a_role [ "switch" ];
                  Unsafe.string_attrib "x-model" "toggle";
                  (if initial_state then a_checked () else a_alt "");
                ]
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

let dynamic_dropdown_form (items : 'a list) ~(get_label : 'a -> string)
    ~(get_value : 'a -> string) ~(id : string) =
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
        ]
      [
        Unsafe.data "<template x-for='(field, index) in fields' :key='index'>";
        div
          ~a:[ a_class [ "flex items-center space-x-2 my-2" ] ]
          [
            (* Text input with dynamic ID *)
            input
              ~a:
                [
                  a_input_type `Text;
                  a_name "name";
                  a_required ();
                  a_placeholder "Name of this device in your unikernel";
                  a_class
                    [
                      "ring-primary-100 mt-1.5 transition appearance-none \
                       block w-full px-3 py-3 rounded-xl shadow-sm border";
                      "hover:border-primary-200 focus:border-primary-300 \
                       bg-primary-50 bg-opacity-0";
                      "hover:bg-opacity-50 focus:bg-opacity-50 \
                       ring-primary-200 focus:ring-primary-200";
                      "focus:ring-[1px] focus:outline-none";
                    ];
                  Unsafe.string_attrib ":id" "field_id + '-input-' + index";
                  Unsafe.string_attrib "x-model" "field.title";
                ]
              ();
            (* Dropdown select with dynamic ID *)
            select
              ~a:
                [
                  a_class
                    [
                      "ring-primary-100 mt-1.5 transition block w-full px-3 \
                       py-3 rounded-xl shadow-sm border";
                      "hover:border-primary-200 focus:border-primary-300 \
                       bg-primary-50 bg-opacity-0";
                      "hover:bg-opacity-50 focus:bg-opacity-50 \
                       ring-primary-200 focus:ring-primary-200";
                      "focus:ring-[1px] focus:outline-none";
                    ];
                  Unsafe.string_attrib ":id" "field_id + '-select-' + index";
                ]
              [
                Unsafe.data
                  {|
            <template x-for="option in options" :key="option.value">
              <option :value="option.value" x-text="option.label"></option>
            </template>
          |};
              ];
            (* Remove button *)
            Unsafe.data
              {|
                  <button type="button" @click="fields.splice(index, 1)"
                    class="text-secondary-500 hover:text-secondary-700 font-bold">
                    &times;
                  </button>
                |};
          ];
        Unsafe.data "</template>";
        (* Add button *)
        button
          ~a:
            [
              a_button_type `Button;
              Unsafe.string_attrib "@click"
                "fields.push({ selected: '', title: '' })";
              a_class
                [
                  "bg-primary-500 text-gray-100 px-3 py-1 rounded \
                   hover:bg-primary-600";
                ];
            ]
          [ txt "+ Add" ];
      ])

let cpuid_to_array_string lst =
  if lst = [] then "[]"
  else "[\"" ^ String.concat "\", \"" (List.map string_of_int lst) ^ "\"]"

let send_http_request ?(path = "") ~base_url http_client =
  let url = base_url ^ path in
  let body = "" in
  let body_f _ acc chunk = Lwt.return (acc ^ chunk) in
  Http_mirage_client.request http_client ~follow_redirect:true
    ~headers:[ ("Accept", "application/json") ]
    url body_f body
  >>= function
  | Error (`Msg err) -> Lwt.return (Error (`Msg err))
  | Error `Cycle -> Lwt.return (Error (`Msg "returned cycle"))
  | Error `Not_found -> Lwt.return (Error (`Msg "returned not found"))
  | Ok (resp, body) ->
      if Http_mirage_client.Status.is_successful resp.Http_mirage_client.status
      then Lwt.return (Ok body)
      else
        Lwt.return
          (Error
             (`Msg
                ("accessing " ^ url ^ " resulted in an error: "
                ^ Http_mirage_client.Status.to_string resp.status
                ^ " " ^ resp.reason)))

type user_policy_usage = {
  deployed_unikernels : int;
  total_volume_used : int;
  total_free_space : int;
  total_memory_used : int;
  cpu_usage_count : (int * int) list;
  bridge_usage_count : (string * int) list;
}

let deployed_unikernels unikernels = List.length unikernels

let cpu_usage_count policy unikernels =
  let cpuid_count = Hashtbl.create (List.length unikernels) in
  (* count usage of each cpuid in unikernels *)
  List.iter
    (fun (_, unikernel) ->
      let cpuid = unikernel.Vmm_core.Unikernel.cpuid in
      let count =
        Hashtbl.find_opt cpuid_count cpuid |> Option.value ~default:0
      in
      Hashtbl.replace cpuid_count cpuid (count + 1))
    unikernels;
  (* Prepare list of all cpuids from policy *)
  let policy_cpuids = Vmm_core.IS.elements policy.Vmm_core.Policy.cpuids in
  (* Generate the list with all policy.cpuids and their respective counts *)
  List.map
    (fun cpuid ->
      let count =
        Hashtbl.find_opt cpuid_count cpuid |> Option.value ~default:0
      in
      (cpuid, count))
    policy_cpuids

let total_volume_used blocks =
  List.fold_left (fun total_size (_, size, _) -> total_size + size) 0 blocks

let total_free_space policy total_volume_used =
  Option.value ~default:0 policy.Vmm_core.Policy.block - total_volume_used

let total_memory_used unikernels =
  List.fold_left
    (fun total_memory (_, unikernel) ->
      total_memory + unikernel.Vmm_core.Unikernel.memory)
    0 unikernels

let bridge_usage_count policy unikernels =
  let bridge_count =
    Hashtbl.create (Vmm_core.String_set.cardinal policy.Vmm_core.Policy.bridges)
  in
  (* Initialize all policy bridges with a count of 0 *)
  Vmm_core.String_set.iter
    (fun bridge -> Hashtbl.replace bridge_count bridge 0)
    policy.bridges;

  (* Count each bridge usage from unikernel.bridges, but only if it's in policy.bridges *)
  List.iter
    (fun (_, unikernel) ->
      List.iter
        (fun { Vmm_core.Unikernel.host_device; _ } ->
          (* Only count if the bridge is in policy.bridges *)
          if Vmm_core.String_set.mem host_device policy.bridges then
            let count =
              Hashtbl.find_opt bridge_count host_device
              |> Option.value ~default:0
            in
            Hashtbl.replace bridge_count host_device (count + 1))
        unikernel.Vmm_core.Unikernel.bridges)
    unikernels;

  (* Convert the hash table to a sorted list of (bridge_name, count) *)
  let bridge_usage_list =
    Hashtbl.fold
      (fun bridge count acc -> (bridge, count) :: acc)
      bridge_count []
  in
  (* Sort the list by bridge name *)
  List.sort (fun (b1, _) (b2, _) -> String.compare b1 b2) bridge_usage_list

let user_policy_usage policy unikernels blocks : user_policy_usage =
  let deployed_unikernels = deployed_unikernels unikernels in
  let total_volume_used = total_volume_used blocks in
  let total_free_space = total_free_space policy total_volume_used in
  let total_memory_used = total_memory_used unikernels in
  let cpu_usage_count = cpu_usage_count policy unikernels in
  let bridge_usage_count = bridge_usage_count policy unikernels in
  {
    deployed_unikernels;
    total_volume_used;
    total_free_space;
    total_memory_used;
    cpu_usage_count;
    bridge_usage_count;
  }
