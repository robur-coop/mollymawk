module Json = struct
  let clean_string s =
    (* Remove backslashes and double quotes from the string *)
    let buffer = Buffer.create (String.length s) in
    String.iter
      (fun c ->
        match c with '\\' -> () | '"' -> () | _ -> Buffer.add_char buffer c)
      s;
    Buffer.contents buffer
end

module Email = struct
  let validate_email email =
    match Emile.of_string (Json.clean_string email) with
    | Ok _ -> true
    | Error s ->
        Logs.err (fun m -> m "Emile-Email-Validation: %a" Emile.pp_error s);
        false
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
end
