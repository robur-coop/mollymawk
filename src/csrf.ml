let csrf_expiry = 3600 (* seconds *)
let csrf_field = "molly_csrf"
let mac ~key ~msg = Digestif.SHA256.hmac_string ~key msg
let now () = int_of_float (Ptime.to_float_s (Mkernel.now ()))

(* token = nonce.timestamp.hex(hmac(session_cookie.nonce.timestamp)) *)
let generate_token ~key ~session_cookie =
  let nonce = Ohex.encode (Mirage_crypto_rng.generate 16) in
  let payload = nonce ^ "." ^ string_of_int (now ()) in
  payload ^ "."
  ^ Digestif.SHA256.to_hex (mac ~key ~msg:(session_cookie ^ "." ^ payload))

let verify_token ~key ~session_cookie ~token =
  match String.split_on_char '.' token with
  | [ nonce; ts; sig_hex ] -> (
      match (int_of_string_opt ts, Digestif.SHA256.of_hex_opt sig_hex) with
      | Some issued, Some sig_digest ->
          let expected =
            mac ~key ~msg:(session_cookie ^ "." ^ nonce ^ "." ^ ts)
          in
          let now = now () in
          if not (Digestif.SHA256.equal sig_digest expected) then
            Error `Invalid_signature
          else if now - issued > csrf_expiry || issued > now then Error `Expired
          else Ok ()
      | _ -> Error `Invalid_format)
  | _ -> Error `Invalid_format

let verify ~key ~session_cookie ~token ~submitted_token =
  Eqaf.equal token submitted_token
  && Result.is_ok (verify_token ~key ~session_cookie ~token)
