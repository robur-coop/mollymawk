open Lwt.Infix

let ( let* ) = Result.bind

module Json = struct
  let get key assoc =
    Option.map snd (List.find_opt (fun (k, _) -> String.equal k key) assoc)
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

module Builder_web = struct
  let base_url = "https://builds.robur.coop"

  type build = {
    job : string;
    uuid : string;
    platform : string;
    start_time : Ptime.t option;
    finish_time : Ptime.t option;
    main_binary : bool;
    main_binary_size : int option;
  }

  type package = { name : string; version : string }

  type package_version_diff = {
    name : string;
    version_left : string;
    version_right : string;
  }

  type o_diff = {
    package_version : string;
    effectively_equal : bool;
    diff : string;
  }

  type duniverse = { name : string; value_ : string }
  type duniverse_detailed_diff = { name : string }

  type package_diff = {
    left_packages : package list;
    right_packages : package list;
    different_versions : package_version_diff list;
  }

  type duniverse_diff = {
    left : duniverse list;
    right : duniverse list;
    detailed_diff : duniverse_detailed_diff list;
  }

  type opam_diff = {
    opam_diff : o_diff list;
    version_diff : package_version_diff list;
    only_in_left : package list;
    only_in_right : package list;
    duniverse_diff : duniverse_diff;
  }

  type compare = {
    left : build;
    right : build;
    env_diff : package_diff;
    package_diff : package_diff;
    opam_diff : opam_diff;
  }

  let list_of_json parse_fn json =
    List.fold_left
      (fun acc item ->
        match (acc, parse_fn item) with
        | Ok lst, Ok parsed -> Ok (parsed :: lst)
        | Error e, _ -> Error e
        | _, Error e -> Error e)
      (Ok []) json
    |> Result.map List.rev

  let duniverse_detailed_diff_of_json = function
    | `Assoc xs -> (
        match Json.(get "name" xs) with
        | Some (`String name) -> Ok { name }
        | _ ->
            Error
              (`Msg
                ("invalid json for duniverse_detailed_diff: "
                ^ Yojson.Basic.to_string (`Assoc xs))))
    | js ->
        Error
          (`Msg
            ("invalid json for builder_web duniverse_detailed_diff_of_json: "
           ^ Yojson.Basic.to_string js))

  let duniverse_of_json = function
    | `Assoc xs -> (
        match Json.(get "name" xs, get "value" xs) with
        | Some (`String name), Some (`String value_) -> Ok { name; value_ }
        | _ ->
            Error
              (`Msg
                ("invalid json for duniverse: "
                ^ Yojson.Basic.to_string (`Assoc xs))))
    | js ->
        Error
          (`Msg
            ("invalid json for duniverse_of_json: " ^ Yojson.Basic.to_string js))

  let duniverse_diff_of_json json =
    match
      Json.(get "left" json, get "right" json, get "detailed_diff" json)
    with
    | ( Some (`List left_json),
        Some (`List right_json),
        Some (`List detailed_diff_json) ) ->
        let* left = list_of_json duniverse_of_json left_json in
        let* right = list_of_json duniverse_of_json right_json in
        let* detailed_diff =
          list_of_json duniverse_detailed_diff_of_json detailed_diff_json
        in
        Ok { left; right; detailed_diff }
    | _ ->
        Error
          (`Msg
            ("invalid json for duniverse_diff_of_json: "
            ^ Yojson.Basic.to_string (`Assoc json)))

  let o_diff_of_json = function
    | `Assoc xs -> (
        match
          Json.
            (get "package_version" xs, get "effectively_equal" xs, get "diff" xs)
        with
        | ( Some (`String package_version),
            Some (`Bool effectively_equal),
            Some (`String diff) ) ->
            Ok { package_version; effectively_equal; diff }
        | _ ->
            Error
              (`Msg
                ("invalid json for o_diff: "
                ^ Yojson.Basic.to_string (`Assoc xs))))
    | js ->
        Error
          (`Msg
            ("invalid json for o_diff list, expected a list: "
           ^ Yojson.Basic.to_string js))

  let package_of_json = function
    | `Assoc xs -> (
        match Json.(get "name" xs, get "version" xs) with
        | Some (`String name), Some (`String version) -> Ok { name; version }
        | _ ->
            Error
              (`Msg
                ("invalid json for builder_web package_of_json: "
                ^ Yojson.Basic.to_string (`Assoc xs))))
    | js ->
        Error
          (`Msg
            ("invalid json for builder_web package_of_json, expected a list: "
           ^ Yojson.Basic.to_string js))

  let package_version_of_json = function
    | `Assoc xs -> (
        match
          Json.(get "name" xs, get "version_left" xs, get "version_right" xs)
        with
        | ( Some (`String name),
            Some (`String version_left),
            Some (`String version_right) ) ->
            Ok { name; version_left; version_right }
        | _ ->
            Error
              (`Msg
                ("invalid json for builder_web package_version_of_json: "
                ^ Yojson.Basic.to_string (`Assoc xs))))
    | js ->
        Error
          (`Msg
            ("invalid json for builder_web package_version_of_json, expected a \
              list: " ^ Yojson.Basic.to_string js))

  let int_or_none field = function
    | None | Some `Null -> Ok None
    | Some (`Int v) -> Ok (Some v)
    | Some json ->
        Error
          (`Msg
            ("invalid json for " ^ field ^ ": " ^ Yojson.Basic.to_string json))

  let build_of_json = function
    | `Assoc xs -> (
        match
          Json.
            ( get "job" xs,
              get "uuid" xs,
              get "platform" xs,
              get "start_time" xs,
              get "finish_time" xs,
              get "main_binary" xs,
              get "main_binary_size" xs )
        with
        | ( Some (`String job),
            Some (`String uuid),
            Some (`String platform),
            Some (`String start_time_str),
            Some (`String finish_time_str),
            Some (`Bool main_binary),
            main_binary_size ) ->
            let start_time =
              match TimeHelper.ptime_of_string start_time_str with
              | Ok ptime -> Some ptime
              | Error _ -> None
            in
            let finish_time =
              match TimeHelper.ptime_of_string finish_time_str with
              | Ok ptime -> Some ptime
              | Error _ -> None
            in
            let* main_binary_size =
              int_or_none "main_binary_size" main_binary_size
            in
            Ok
              {
                job;
                uuid;
                platform;
                start_time;
                finish_time;
                main_binary;
                main_binary_size;
              }
        | _ ->
            Error
              (`Msg
                ("invalid json for builder_web build_of_json: "
                ^ Yojson.Basic.to_string (`Assoc xs))))
    | js ->
        Error
          (`Msg
            ("invalid json for builder_web build_of_json, expected a dict: "
           ^ Yojson.Basic.to_string js))

  let diff_of_json json =
    match
      Json.
        ( get "left_packages" json,
          get "right_packages" json,
          get "different_versions" json )
    with
    | ( Some (`List left_packages_json),
        Some (`List right_packages_json),
        Some (`List different_versions_json) ) ->
        let* left_packages = list_of_json package_of_json left_packages_json in
        let* right_packages =
          list_of_json package_of_json right_packages_json
        in
        let* different_versions =
          list_of_json package_version_of_json different_versions_json
        in
        Ok { left_packages; right_packages; different_versions }
    | _ ->
        Error
          (`Msg
            ("invalid json for builder_web env/package_diff_of_json: "
            ^ Yojson.Basic.to_string (`Assoc json)))

  let opam_diff_of_json json =
    match
      Json.
        ( get "opam_diff" json,
          get "version_diff" json,
          get "only_in_left" json,
          get "only_in_right" json,
          get "duniverse_diff" json )
    with
    | ( Some (`List o_diff_json),
        Some (`List version_diff_json),
        Some (`List only_in_left_json),
        Some (`List only_in_right_json),
        Some (`Assoc duniverse_diff_json) ) ->
        let* opam_diff = list_of_json o_diff_of_json o_diff_json in
        let* version_diff =
          list_of_json package_version_of_json version_diff_json
        in
        let* only_in_left = list_of_json package_of_json only_in_left_json in
        let* only_in_right = list_of_json package_of_json only_in_right_json in
        let* duniverse_diff = duniverse_diff_of_json duniverse_diff_json in
        Ok
          {
            opam_diff;
            version_diff;
            only_in_left;
            only_in_right;
            duniverse_diff;
          }
    | _ ->
        Error
          (`Msg
            ("invalid json for builder_web: "
            ^ Yojson.Basic.to_string (`Assoc json)))

  let compare_of_json = function
    | `Assoc xs -> (
        match
          Json.
            ( get "left" xs,
              get "right" xs,
              get "env_diff" xs,
              get "package_diff" xs,
              get "opam_diff" xs )
        with
        | ( Some (`Assoc left_json),
            Some (`Assoc right_json),
            Some (`Assoc env_diff_json),
            Some (`Assoc package_diff_json),
            Some (`Assoc opam_diff_json) ) ->
            let* left = build_of_json (`Assoc left_json) in
            let* right = build_of_json (`Assoc right_json) in
            let* env_diff = diff_of_json env_diff_json in
            let* package_diff = diff_of_json package_diff_json in
            let* opam_diff = opam_diff_of_json opam_diff_json in
            Ok { left; right; env_diff; package_diff; opam_diff }
        | _ ->
            Error
              (`Msg
                ("invalid json for builder_web diff: "
                ^ Yojson.Basic.to_string (`Assoc xs))))
    | js ->
        Error
          (`Msg
            ("invalid json for builder_web diff, expected a dict: "
           ^ Yojson.Basic.to_string js))

  let send_request url =
    let body = "" in
    let body_f _ acc chunk =
      Lwt.return (acc ^ chunk)
    in
    Http_lwt_client.request ~follow_redirect:true
      ~headers:[ ("Accept", "application/json") ]
      url body_f body
    >>= function
    | Error (`Msg err) -> Lwt.return (Error (`Msg err))
    | Ok (resp, body) -> (
        match resp.Http_lwt_client.status with
        | `OK -> Lwt.return (Ok body)
        | _ ->
            Lwt.return
              (Error
                 (`Msg (Format.asprintf "%a" Http_lwt_client.pp_response resp)))
        )
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
