open Utils.Json

(* TODO: increment version number and handle backwards compatibility *)
let current_version = 2

(* version history:
   1 was initial (fields without name)
   2 added name
*)

type t = {
  name : string;
  certificate : X509.Certificate.t;
  private_key : X509.Private_key.t;
  server_ip : Ipaddr.t;
  server_port : int;
  updated_at : Ptime.t;
}

let to_json t =
  `Assoc
    [
      ("version", `Int current_version);
      ("name", `String t.name);
      ("certificate", `String (X509.Certificate.encode_pem t.certificate));
      ("private_key", `String (X509.Private_key.encode_pem t.private_key));
      ("server_ip", `String (Ipaddr.to_string t.server_ip));
      ("server_port", `Int t.server_port);
      ("updated_at", `String (Utils.TimeHelper.string_of_ptime t.updated_at));
    ]

let of_json_from_http json_dict now =
  match
    ( get "name" json_dict,
      get "certificate" json_dict,
      get "private_key" json_dict,
      get "server_ip" json_dict,
      get "server_port" json_dict )
  with
  | ( Some (`String name),
      Some (`String cert),
      Some (`String key),
      Some (`String server_ip),
      Some (`Int server_port) ) ->
      let ( let* ) = Result.bind in
      let* certificate = X509.Certificate.decode_pem cert in
      let* private_key = X509.Private_key.decode_pem key in
      let* () =
        if
          not
            (String.equal
               (X509.Public_key.fingerprint
                  (X509.Certificate.public_key certificate))
               (X509.Public_key.fingerprint
                  (X509.Private_key.public private_key)))
        then Error (`Msg "certificate and private key do not match")
        else Ok ()
      in
      let* server_ip = Ipaddr.of_string server_ip in
      Ok
        {
          name;
          certificate;
          private_key;
          server_ip;
          server_port;
          updated_at = now;
        }
  | _ ->
      Error
        (`Msg
           (Fmt.str "configuration: unexpected types, got %s"
              (Yojson.Basic.to_string (`Assoc json_dict))))

let of_json_v1 json =
  match json with
  | `Assoc xs -> (
      match get "version" xs with
      | None -> Error (`Msg "configuration: couldn't find a version")
      | Some (`Int v) ->
          if v = current_version then
            match
              ( get "certificate" xs,
                get "private_key" xs,
                get "server_ip" xs,
                get "server_port" xs,
                get "updated_at" xs )
            with
            | ( Some (`String cert),
                Some (`String key),
                Some (`String server_ip),
                Some (`Int server_port),
                Some (`String updated_at) ) ->
                let ( let* ) = Result.bind in
                let* certificate = X509.Certificate.decode_pem cert in
                let* private_key = X509.Private_key.decode_pem key in
                let* () =
                  if
                    not
                      (String.equal
                         (X509.Public_key.fingerprint
                            (X509.Certificate.public_key certificate))
                         (X509.Public_key.fingerprint
                            (X509.Private_key.public private_key)))
                  then Error (`Msg "certificate and private key do not match")
                  else Ok ()
                in
                let* server_ip = Ipaddr.of_string server_ip in
                let* updated_at = Utils.TimeHelper.ptime_of_string updated_at in

                Ok
                  {
                    name = "default";
                    certificate;
                    private_key;
                    server_ip;
                    server_port;
                    updated_at;
                  }
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

let of_json json =
  match json with
  | `Assoc xs -> (
      match get "version" xs with
      | None -> Error (`Msg "configuration: couldn't find a version")
      | Some (`Int v) ->
          if v = current_version then
            match
              ( get "name" xs,
                get "certificate" xs,
                get "private_key" xs,
                get "server_ip" xs,
                get "server_port" xs,
                get "updated_at" xs )
            with
            | ( Some (`String name),
                Some (`String cert),
                Some (`String key),
                Some (`String server_ip),
                Some (`Int server_port),
                Some (`String updated_at) ) ->
                let ( let* ) = Result.bind in
                let* certificate = X509.Certificate.decode_pem cert in
                let* private_key = X509.Private_key.decode_pem key in
                let* () =
                  if
                    not
                      (String.equal
                         (X509.Public_key.fingerprint
                            (X509.Certificate.public_key certificate))
                         (X509.Public_key.fingerprint
                            (X509.Private_key.public private_key)))
                  then Error (`Msg "certificate and private key do not match")
                  else Ok ()
                in
                let* server_ip = Ipaddr.of_string server_ip in
                let* updated_at = Utils.TimeHelper.ptime_of_string updated_at in

                Ok
                  {
                    name;
                    certificate;
                    private_key;
                    server_ip;
                    server_port;
                    updated_at;
                  }
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
