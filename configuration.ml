open Utils.Json

let current_version = 1

type t = {
  certificate : X509.Certificate.t;
  private_key : X509.Private_key.t;
  server_ip : Ipaddr.t;
  server_port : int;
}

(* this is here to avoid the need for option *)
let empty () =
  let key = X509.Private_key.generate `ED25519 in
  let name = "mollymawk-empty" in
  let cert =
    let dn =
      X509.Distinguished_name.
        [ Relative_distinguished_name.(singleton (CN name)) ]
    in
    let csr = Result.get_ok (X509.Signing_request.create dn key) in
    let valid_from = Ptime.epoch in
    let valid_until =
      Option.get (Ptime.add_span valid_from (Ptime.Span.of_int_s (10 * 60)))
    in
    Result.get_ok
      (X509.Signing_request.sign csr ~valid_from ~valid_until key dn)
  in
  {
    certificate = cert;
    private_key = key;
    server_ip = Ipaddr.(V4 V4.any);
    server_port = 1025;
  }

let to_json t =
  `Assoc
    [
      ("version", `Int current_version);
      ( "certificate",
        `String (Cstruct.to_string (X509.Certificate.encode_pem t.certificate))
      );
      ( "private_key",
        `String (Cstruct.to_string (X509.Private_key.encode_pem t.private_key))
      );
      ("server_ip", `String (Ipaddr.to_string t.server_ip));
      ("server_port", `Int t.server_port);
    ]

let of_json = function
  | `Assoc xs -> (
      match get "version" xs with
      | None -> Error (`Msg "configuration: couldn't find a version")
      | Some (`Int v) ->
          if v = current_version then
            match
              ( get "certificate" xs,
                get "private_key" xs,
                get "server_name" xs,
                get "server_port" xs )
            with
            | ( Some (`String cert),
                Some (`String key),
                Some (`String server_ip),
                Some (`Int server_port) ) ->
                let ( let* ) = Result.bind in
                let* certificate =
                  X509.Certificate.decode_pem (Cstruct.of_string cert)
                in
                let* private_key =
                  X509.Private_key.decode_pem (Cstruct.of_string key)
                in
                let* () =
                  if
                    not
                      (Cstruct.equal
                         (X509.Public_key.fingerprint
                            (X509.Certificate.public_key certificate))
                         (X509.Public_key.fingerprint
                            (X509.Private_key.public private_key)))
                  then Error (`Msg "certificate and private key do not match")
                  else Ok ()
                in
                let* server_ip = Ipaddr.of_string server_ip in
                Ok { certificate; private_key; server_ip; server_port }
            | _ ->
                Error
                  (`Msg
                    (Fmt.str "configuration: unexpected types, got %s"
                       (Yojson.Basic.to_string (`Assoc xs))))
          else
            Error
              (`Msg
                (Fmt.str "configuration: found version %u, expected %u" v
                   current_version))
      | Some _ -> Error (`Msg "configuration: version must be an integer"))
  | _ -> Error (`Msg "configuration: expected a dictionary")
