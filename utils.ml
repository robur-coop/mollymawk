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
