module Email = struct
  let validate_email email =
    match Emile.of_string email with
    | Ok _ -> true
    | Error _ -> false
end
