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
    "/auth/verify/token=" ^ Uuidm.to_string uuid
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
