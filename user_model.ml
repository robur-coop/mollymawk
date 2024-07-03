module Rng = Mirage_crypto_rng

type token = {
  token_type: string;
  access_token: string;
  expires_in: int; (* After 1 hour (3660 seconds) of inactivity *)
  expires_on: int;
  refresh_token: string
}

type user = {
  name: string;
  email: string;
  password: string;
  uuid: string option;
  token: token option;
}

let encrypt_password password =
  let salt = Rng.generate 16 in
  Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string (Cstruct.to_string salt ^ "-" ^ password))

let generate_uuid email =
  let email_int = int_of_string email in
  Uuidm.v4_gen (Random.State.make [|email_int|]) ()

let generate_token (user: user) =
  let hash = Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string (user.name ^ "-" ^ user.email)) in
  Base64.encode (Cstruct.to_string hash)
