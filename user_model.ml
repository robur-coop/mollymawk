module Rng = Mirage_crypto_rng

type token = {
  token_type : string;
  value : string;
  expires_in : int;
      (* In seconds, so after 1 hour would be 3600 seconds of inactivity *)
}

let clean_string s =
  (* Remove backslashes and double quotes from the string *)
  let buffer = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with '\\' -> () | '"' -> () | _ -> Buffer.add_char buffer c)
    s;
  Buffer.contents buffer

let token_to_json t : Yojson.Basic.t =
  `Assoc
    [
      ("token_type", `String t.token_type);
      ("value", `String t.value);
      ("expires_in", `Int t.expires_in);
    ]

let get key assoc =
  match List.find_opt (fun (k, _) -> String.equal k key) assoc with
  | None -> None
  | Some (_, f) -> Some f

let token_of_json = function
  | `Assoc xs -> (
      match (get "token_type" xs, get "value" xs, get "expires_in" xs) with
      | Some (`String token_type), Some (`String value), Some (`Int expires_in)
        ->
          Ok { token_type; value; expires_in }
      | _ ->
          Error
            (`Msg
              "invalid json for token: requires token_type, value, and \
               expires_in"))
  | _ -> Error (`Msg "invalid json for token: expected assoc")

type user = {
  name : string;
  email : string;
  password : string;
  uuid : string;
  tokens : token list;
}

let user_to_json u : Yojson.Basic.t =
  `Assoc
    [
      ("name", `String u.name);
      ("email", `String u.email);
      ("password", `String u.password);
      ("uuid", `String u.uuid);
      ("tokens", `List (List.map token_to_json u.tokens));
    ]

let user_of_json = function
  | `Assoc xs -> (
      let ( let* ) = Result.bind in
      match
        ( get "name" xs,
          get "email" xs,
          get "password" xs,
          get "uuid" xs,
          get "tokens" xs )
      with
      | ( Some (`String name),
          Some (`String email),
          Some (`String password),
          Some (`String uuid),
          Some (`List tokens) ) ->
          let* tokens =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* token = token_of_json js in
                Ok (token :: acc))
              (Ok []) tokens
          in
          Ok { name; email; password; uuid; tokens }
      | _ -> Error (`Msg "invalid json for user"))
  | _ -> Error (`Msg "invalid json for user")

let hash_password password uuid =
  let hash =
    Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string (uuid ^ "-" ^ password))
  in
  Base64.encode_string (Cstruct.to_string hash)

let generate_uuid () =
  let data = Rng.generate 16 in
  Uuidm.v4 (Cstruct.to_bytes data)

let generate_token ?(expires_in = 3600) () =
  let token = generate_uuid () in
  { token_type = "Bearer"; value = Uuidm.to_string token; expires_in }

let check_if_user_exists email users =
  List.exists (fun user -> user.email = clean_string email) users

let create_user ~name ~email ~password =
  let uuid = Uuidm.to_string (generate_uuid ()) in
  let password = hash_password password uuid in
  {
    name = clean_string name;
    email = clean_string email;
    password;
    uuid;
    tokens = [];
  }

let find_user ~email users =
  List.find_opt (fun user -> user.email = clean_string email) users

let login_user ~email ~password users =
  let user = find_user ~email users in
  match user with
  | None -> Error (`Msg "Invalid email or password")
  | Some u ->
      let pass = hash_password password u.uuid in
      if u.password = pass then Ok true
      else Error (`Msg "Invalid email or password")
(* Invalid email or password is a trick error message to at least prevent malicious users from guessing login details :).*)
