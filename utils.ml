module Json = struct
  let clean_string s =
    (* Remove backslashes and double quotes from the string *)
    let buffer = Buffer.create (String.length s) in
    String.iter
      (fun c ->
        match c with '\\' -> () | '"' -> () | _ -> Buffer.add_char buffer c)
      s;
    Buffer.contents buffer

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

  let string_of_ptime (t : Ptime.t) : string = Ptime.to_rfc3339 t ~frac_s:0

  (* calculate the diff between two timestamps *)
  let diff_in_seconds t1 t2 =
    let span = Ptime.diff t1 t2 in
    match Ptime.Span.to_int_s span with Some s -> s | None -> 0

  (* print timestamp in human-readable form *)
  let print_human_readable ~now ~timestamp =
    let duration = diff_in_seconds now timestamp |> Duration.of_sec in
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

  let time_ago t1 t2 =
    let diff = diff_in_seconds t1 t2 in
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
    match Emile.of_string (Json.clean_string email) with
    | Ok _ -> true
    | Error s ->
        Logs.err (fun m -> m "Emile-Email-Validation: %a" Emile.pp_error s);
        false

  let generate_verification_link uuid =
    "/auth/verify/token=" ^ Uuidm.to_string uuid
end

module Status = struct
  type t = { code : int; title : string; data : string; success : bool }

  let to_json (status : t) : string =
    `Assoc
      [
        ("status", `Int status.code);
        ("title", `String status.title);
        ("success", `Bool status.success);
        ("data", `String status.data);
      ]
    |> Yojson.Safe.to_string
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
            button
              ~a:[ a_id "close-banner-btn"; a_onclick "closeBanner()" ]
              [ i ~a:[ a_class [ "fa-solid fa-x text-sm" ] ] [] ];
          ])
  | None -> Tyxml_html.div []
