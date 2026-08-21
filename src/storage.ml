open Utils.Json

let current_version = 10
(* version history:
   [1 - 8] deprecated.
   9 email configuration is now stored
   10 we now have scaling policies for unikernels, default is no policy for existing unikernels
*)

type t = User_model.user list * Configuration.t list * Utils.Email.t option

let t_to_json ?(version = current_version) users configurations email =
  `Assoc
    [
      ("version", `Int version);
      ("users", `List (List.map User_model.user_to_json users));
      ("configuration", Configuration.to_json configurations);
      ("email", Utils.Email.to_json email);
    ]

let t_of_json json =
  match json with
  | `Assoc xs -> (
      let ( let* ) = Result.bind in
      match
        ( get "version" xs,
          get "users" xs,
          get "configuration" xs,
          get "email" xs )
      with
      | Some (`Int v), Some (`List users), Some configuration, email ->
          let* () =
            if v = current_version then Ok ()
            else if v = 9 then Ok ()
            else
              Error
                (`Msg
                   (Fmt.str
                      "expected version %u, found version %u. note: version [1 \
                       - 8] is now deprecated."
                      current_version v))
          in
          let* users =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* user =
                  if v = 9 then User_model.(user_v9_of_json cookie_of_json) js
                  else User_model.(user_of_json cookie_of_json) js
                in
                Ok (user :: acc))
              (Ok []) users
          in
          let* configurations = Configuration.of_json configuration in
          let* email =
            match email with
            | None -> Ok None
            | Some e -> (
                match Utils.Email.of_json e with
                | Ok email -> Ok (Some email)
                | Error _msg -> Ok None)
          in
          Ok (users, configurations, email)
      | _ -> Error (`Msg "invalid data: no version and users field"))
  | _ -> Error (`Msg "invalid data: not an assoc")

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
