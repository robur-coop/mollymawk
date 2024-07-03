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
