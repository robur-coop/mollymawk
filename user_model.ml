module Rng = Mirage_crypto_rng

type token = {
  token_type : string;
  value : string;
  expires_in : int;
  created_at : Ptime.t;
      (* In seconds, so after 1 hour would be 3600 seconds of inactivity *)
}

type cookie = {
  name : string;
  value : string;
  expires_in : int;
  uuid : string option;
  created_at : Ptime.t;
}

type user = {
  name : string;
  email : string;
  email_verified : Ptime.t option;
  password : string;
  uuid : string;
  tokens : token list;
  cookies : cookie list;
  created_at : Ptime.t;
}

let week = 604800 (* a week = 7 days * 24 hours * 60 minutes * 60 seconds *)

let get key assoc =
  match List.find_opt (fun (k, _) -> String.equal k key) assoc with
  | None -> None
  | Some (_, f) -> Some f

let cookie_to_json (cookie : cookie) : Yojson.Basic.t =
  `Assoc
    [
      ("name", `String cookie.name);
      ( "created_at",
        `String (Utils.TimeHelper.string_of_ptime cookie.created_at) );
      ("value", `String cookie.value);
      ("expires_in", `Int cookie.expires_in);
      ( "uuid",
        match cookie.uuid with Some uuid -> `String uuid | None -> `Null );
    ]

let cookie_of_json = function
  | `Assoc xs -> (
      match
        ( get "name" xs,
          get "value" xs,
          get "expires_in" xs,
          get "uuid" xs,
          get "created_at" xs )
      with
      | ( Some (`String name),
          Some (`String value),
          Some (`Int expires_in),
          Some (`String uuid),
          Some (`String created_at_str) ) ->
          let created_at =
            match Utils.TimeHelper.ptime_of_string created_at_str with
            | Ok ptime -> Some ptime
            | Error _ -> None
          in
          Ok
            {
              name;
              value;
              expires_in;
              uuid = Some uuid;
              created_at = Option.get created_at;
            }
      | ( Some (`String name),
          Some (`String value),
          Some (`Int expires_in),
          None,
          Some (`String created_at_str) ) ->
          let created_at =
            match Utils.TimeHelper.ptime_of_string created_at_str with
            | Ok ptime -> Some ptime
            | Error _ -> None
          in
          Ok
            {
              name;
              value;
              expires_in;
              uuid = None;
              created_at = Option.get created_at;
            }
      | _ -> Error (`Msg "invalid json for cookie"))
  | _ -> Error (`Msg "invalid json for cookie")

let cookie_to_string cookie = Yojson.Basic.to_string (cookie_to_json cookie)

let cookie_of_string (s : string) : cookie option =
  try
    let json = Yojson.Basic.from_string s in
    let open Yojson.Basic.Util in
    let name = json |> member "name" |> to_string in
    let value = json |> member "value" |> to_string in
    let expires_in = json |> member "expires_in" |> to_int in
    let uuid =
      match json |> member "uuid" with `String uuid -> Some uuid | _ -> None
    in
    let created_at =
      match
        json |> member "created_at" |> to_string
        |> Utils.TimeHelper.ptime_of_string
      with
      | Ok ptime -> Some ptime
      | Error _ -> None
    in
    Some { name; value; expires_in; uuid; created_at = Option.get created_at }
  with _ -> None

let token_to_json t : Yojson.Basic.t =
  `Assoc
    [
      ("token_type", `String t.token_type);
      ("value", `String t.value);
      ("expires_in", `Int t.expires_in);
      ("created_at", `String (Utils.TimeHelper.string_of_ptime t.created_at));
    ]

let token_of_json = function
  | `Assoc xs -> (
      match
        ( get "token_type" xs,
          get "value" xs,
          get "expires_in" xs,
          get "created_at" xs )
      with
      | ( Some (`String token_type),
          Some (`String value),
          Some (`Int expires_in),
          Some (`String created_at_str) ) ->
          let created_at =
            match Utils.TimeHelper.ptime_of_string created_at_str with
            | Ok ptime -> Some ptime
            | Error _ -> None
          in
          Ok
            {
              token_type;
              value;
              expires_in;
              created_at = Option.get created_at;
            }
      | _ ->
          Error
            (`Msg
              "invalid json for token: requires token_type, value, and \
               expires_in"))
  | _ -> Error (`Msg "invalid json for token: expected assoc")

let user_to_json (u : user) : Yojson.Basic.t =
  `Assoc
    [
      ("name", `String u.name);
      ("email", `String u.email);
      ("email_verified", Utils.TimeHelper.ptime_to_json u.email_verified);
      ("password", `String u.password);
      ("uuid", `String u.uuid);
      ("tokens", `List (List.map token_to_json u.tokens));
      ("cookies", `List (List.map cookie_to_json u.cookies));
      ("created_at", `String (Utils.TimeHelper.string_of_ptime u.created_at));
    ]

let user_of_json = function
  | `Assoc xs -> (
      let ( let* ) = Result.bind in
      match
        ( get "name" xs,
          get "email" xs,
          get "email_verified" xs,
          get "password" xs,
          get "uuid" xs,
          get "tokens" xs,
          get "cookies" xs,
          get "created_at" xs )
      with
      | ( Some (`String name),
          Some (`String email),
          Some email_verified,
          Some (`String password),
          Some (`String uuid),
          Some (`List tokens),
          Some (`List cookies),
          Some (`String created_at_str) ) ->
          let created_at =
            match Utils.TimeHelper.ptime_of_string created_at_str with
            | Ok ptime -> Some ptime
            | Error _ -> None
          in
          let* email_verified = Utils.TimeHelper.ptime_of_json email_verified in
          let* tokens =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* token = token_of_json js in
                Ok (token :: acc))
              (Ok []) tokens
          in
          let* cookies =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* cookie = cookie_of_json js in
                Ok (cookie :: acc))
              (Ok []) cookies
          in
          Ok
            {
              name;
              email;
              email_verified;
              password;
              uuid;
              tokens;
              cookies;
              created_at = Option.get created_at;
            }
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

let generate_token ?(expires_in = 3600) ~created_at () =
  let token = generate_uuid () in
  {
    token_type = "Bearer";
    value = Uuidm.to_string token;
    expires_in;
    created_at;
  }

let generate_cookie name uuid ?(expires_in = 3600) created_at () =
  let id = generate_uuid () in
  {
    name;
    value = Base64.encode_string (Uuidm.to_string id);
    expires_in;
    uuid = Some uuid;
    created_at;
  }

let create_user_session_map (users : user list) : (string * user) list =
  List.map
    (fun user ->
      let cookie =
        List.find
          (fun (cookie : cookie) -> String.equal cookie.name "molly_session")
          user.cookies
      in
      (cookie.value, user))
    users

let create_user_uuid_map (users : user list) : (string * user) list =
  List.map (fun user -> (user.uuid, user)) users

let find_user_by_key (uuid : string) (user_map : (string * user) list) :
    user option =
  List.assoc_opt uuid user_map

let create_user name email password created_at =
  let uuid = Uuidm.to_string (generate_uuid ()) in
  let password = hash_password password uuid in
  let auth_token = generate_token ~created_at () in
  let session =
    generate_cookie "molly_session" ~expires_in:week uuid created_at ()
  in
  (* auth sessions should expire after a week (24hrs * 7days * 60mins * 60secs) *)
  {
    name = Utils.Json.clean_string name;
    email = Utils.Json.clean_string email;
    email_verified = None;
    password;
    uuid;
    tokens = [ auth_token ];
    cookies = [ session ];
    created_at;
  }

let check_if_user_exists email users =
  List.find_opt (fun user -> user.email = Utils.Json.clean_string email) users

let update_user user ?name ?email ?email_verified ?password ?tokens ?cookies ()
    =
  {
    user with
    name = (match name with Some name -> name | _ -> user.name);
    email = (match email with Some email -> email | _ -> user.email);
    email_verified =
      (match email_verified with
      | Some email_verified -> email_verified
      | _ -> user.email_verified);
    password =
      (match password with Some password -> password | _ -> user.password);
    tokens = (match tokens with Some tokens -> tokens | _ -> user.tokens);
    cookies = (match cookies with Some cookies -> cookies | _ -> user.cookies);
  }

let update_cookies (cookies : cookie list) (cookie : cookie) : cookie list =
  List.map
    (fun (c : cookie) ->
      match c.name = cookie.name with true -> cookie | false -> c)
    cookies

let is_valid_cookie (cookie : cookie) now =
  Utils.TimeHelper.diff_in_seconds cookie.created_at now < cookie.expires_in

let is_email_verified user = Option.is_some user.email_verified

let user_auth_cookie_from_user (user : user) =
  List.find_opt
    (fun (cookie : cookie) -> String.equal cookie.name "molly_session")
    user.cookies

let login_user email password users now =
  let user = check_if_user_exists email users in
  match user with
  | None -> Error (`Msg "This account does not exist.")
  | Some u -> (
      let pass = hash_password password u.uuid in
      match String.equal u.password pass with
      | true ->
          let new_session =
            generate_cookie "molly_session" ~expires_in:week u.uuid now ()
          in
          let cookies = update_cookies u.cookies new_session in
          let updated_user = update_user u ~cookies () in
          Ok updated_user
      | false -> Error (`Msg "Invalid email or password."))
(* Invalid email or password is a trick error message to at least prevent malicious users from guessing login details :).*)
