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

  (* convert seconds to human-readable format *)
  let human_readable_diff seconds =
    if seconds < 60 then Printf.sprintf "%ds ago" seconds
    else if seconds < 3600 then Printf.sprintf "%dm ago" (seconds / 60)
    else if seconds < 86400 then Printf.sprintf "%dh ago" (seconds / 3600)
    else if seconds < 2592000 then Printf.sprintf "%dd ago" (seconds / 86400)
      (* 30 days *)
    else if seconds < 31536000 then
      Printf.sprintf "%dmo ago" (seconds / 2592000) (* 30 days in a month *)
    else Printf.sprintf "%dy ago" (seconds / 31536000)
  (* 365 days in a year *)

  (* print timestamp in human-readable form *)
  let print_human_readable ~now ~timestamp =
    let seconds_diff = diff_in_seconds now timestamp in
    human_readable_diff seconds_diff
end
