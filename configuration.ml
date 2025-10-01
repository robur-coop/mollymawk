open Utils.Json

type t = {
  name : Vmm_core.Name.t;
  certificate : X509.Certificate.t;
  private_key : X509.Private_key.t;
  server_ip : Ipaddr.t;
  server_port : int;
  updated_at : Ptime.t;
}

let name_to_str name =
  (* this is safe since all constructors check Vmm_core.Name.valid_label *)
  Option.get (Vmm_core.Name.name name)

let name_of_str name =
  if Vmm_core.Name.valid_label name then
    Ok (Vmm_core.Name.create_exn Vmm_core.Name.root_path name)
  else
    Error
      (`Msg
         (Fmt.str
            "invalid 'name' (%S): must be 1–63 characters, use only \
             [A-Za-z0-9.-], and must not start with '-'"
            name))

let to_json t =
  let one_to_json c =
    `Assoc
      [
        ("name", `String (name_to_str c.name));
        ("certificate", `String (X509.Certificate.encode_pem c.certificate));
        ("private_key", `String (X509.Private_key.encode_pem c.private_key));
        ("server_ip", `String (Ipaddr.to_string c.server_ip));
        ("server_port", `Int c.server_port);
        ("updated_at", `String (Utils.TimeHelper.string_of_ptime c.updated_at));
      ]
  in
  `List (List.map one_to_json t)

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
      let* name = name_of_str name in
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
  (* NOTE: this was a single dictionary, and had a version identifier *)
  match json with
  | `Assoc xs -> (
      match get "version" xs with
      | None -> Error (`Msg "configuration: couldn't find a version")
      | Some (`Int v) ->
          if v = 1 then
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
                let* name = name_of_str "default" in
                Ok
                  [
                    {
                      name;
                      certificate;
                      private_key;
                      server_ip;
                      server_port;
                      updated_at;
                    };
                  ]
            | _ ->
                Error
                  (`Msg
                     (Fmt.str "configuration: unexpected types, got %s"
                        (Yojson.Basic.to_string (`Assoc xs))))
          else
            Error
              (`Msg (Fmt.str "configuration: found version %u, expected 1" v))
      | Some _ -> Error (`Msg "configuration: version must be an integer"))
  | _ -> Error (`Msg "configuration: expected a dictionary")

let of_json json =
  let one_of_json = function
    | `Assoc xs -> (
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
            let* name = name_of_str name in
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
                    (Yojson.Basic.to_string (`Assoc xs)))))
    | _ -> Error (`Msg "configuration: expected a dictionary")
  in
  match json with
  | `List cfgs ->
      let ( let* ) = Result.bind in
      List.fold_left
        (fun acc cfg ->
          let* acc = acc in
          let* c = one_of_json cfg in
          Ok (c :: acc))
        (Ok []) cfgs
  | _ -> Error (`Msg "configuration: expected a list")
