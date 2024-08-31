module Make (P : Mirage_clock.PCLOCK) (S : Tcpip.Stack.V4V6) = struct
  module TLS = Tls_mirage.Make (S.TCP)
  module Set = Set.Make (String)
  module Map = Map.Make (String)

  type t = {
    stack : S.t;
    remote : Ipaddr.t * int;
    cert : X509.Certificate.t;
    key : X509.Private_key.t;
    mutable policies : (Vmm_core.Name.t * Vmm_core.Policy.t) list;
    mutable console_readers : Set.t;
    mutable console_output : Yojson.Basic.t list Map.t;
  }

  let key_ids exts pub issuer =
    let open X509 in
    let auth = (Some (Public_key.id issuer), General_name.empty, None) in
    Extension.(
      add Subject_key_id
        (false, Public_key.id pub)
        (add Authority_key_id (false, auth) exts))

  let timestamps validity =
    let now = Ptime.v (P.now_d_ps ()) in
    match
      (* subtracting some seconds here to not require perfectly synchronised
         clocks on client and server *)
      ( Ptime.sub_span now (Ptime.Span.of_int_s (validity / 2)),
        Ptime.add_span now (Ptime.Span.of_int_s validity) )
    with
    | None, _ | _, None -> invalid_arg "span too big - reached end of ptime"
    | Some now, Some exp -> (now, exp)

  let gen_cert t ?domain cmd name =
    let ( let* ) = Result.bind in
    let key_type = `ED25519 in
    let open X509 in
    let* intermediate_key, cert, certs =
      match domain with
      | None -> Ok (t.key, t.cert, [])
      | Some domain -> (
          let* policy =
            match Vmm_core.Name.path_of_string domain with
            | Error (`Msg msg) ->
                Error
                  (Fmt.str "albatross: domain %s is not a path: %s" domain msg)
            | Ok path ->
                Ok
                  (List.assoc_opt
                     (Vmm_core.Name.create_of_path path)
                     t.policies)
          in
          let intermediate_key = Private_key.generate key_type in
          let v =
            Option.map
              (fun p -> Vmm_asn.to_cert_extension (`Policy_cmd (`Policy_add p)))
              policy
          in
          let exts =
            let ku =
              [
                `Key_cert_sign;
                `CRL_sign;
                `Digital_signature;
                `Content_commitment;
              ]
            in
            let extensions =
              match v with
              | None -> Extension.empty
              | Some p ->
                  Extension.singleton (Unsupported Vmm_asn.oid) (false, p)
            in
            Extension.(
              add Basic_constraints
                (true, (true, None))
                (add Key_usage (true, ku) extensions))
          in
          match
            let name =
              [
                Distinguished_name.(
                  Relative_distinguished_name.singleton (CN domain));
              ]
            in
            let extensions = Signing_request.Ext.(singleton Extensions exts) in
            Signing_request.create name ~extensions intermediate_key
          with
          | Error (`Msg msg) ->
              Error
                (Fmt.str "albatross: failed to create CA signing request: %s"
                   msg)
          | Ok csr -> (
              let valid_from, valid_until = timestamps 300 in
              let extensions =
                let capub = X509.Private_key.public t.key in
                key_ids exts Signing_request.((info csr).public_key) capub
              in
              let issuer = Certificate.subject t.cert in
              match
                Signing_request.sign csr ~valid_from ~valid_until ~extensions
                  t.key issuer
              with
              | Error e ->
                  Error
                    (Fmt.str "albatross: failed to sign CA signing request: %a"
                       X509.Validation.pp_signature_error e)
              | Ok mycert -> Ok (intermediate_key, mycert, [ mycert ])))
    in
    let tmpkey = Private_key.generate key_type in
    let extensions =
      let v = Vmm_asn.to_cert_extension cmd in
      Extension.(
        add Key_usage
          (true, [ `Digital_signature; `Key_encipherment ])
          (add Basic_constraints
             (true, (false, None))
             (add Ext_key_usage (true, [ `Client_auth ])
                (singleton (Unsupported Vmm_asn.oid) (false, v)))))
    in
    match
      let name =
        [ Distinguished_name.(Relative_distinguished_name.singleton (CN name)) ]
      in
      let extensions = Signing_request.Ext.(singleton Extensions extensions) in
      Signing_request.create name ~extensions tmpkey
    with
    | Error (`Msg msg) ->
        Error (Fmt.str "albatross: failed to create signing request: %s" msg)
    | Ok csr -> (
        let valid_from, valid_until = timestamps 300 in
        let extensions =
          let capub = X509.Private_key.public intermediate_key in
          key_ids extensions Signing_request.((info csr).public_key) capub
        in
        let issuer = Certificate.subject cert in
        match
          Signing_request.sign csr ~valid_from ~valid_until ~extensions
            intermediate_key issuer
        with
        | Error e ->
            Error
              (Fmt.str "albatross: failed to sign signing request %a"
                 X509.Validation.pp_signature_error e)
        | Ok mycert -> Ok (`Single (mycert :: certs, tmpkey)))

  let decode data =
    match Vmm_asn.wire_of_cstruct data with
    | Error (`Msg msg) ->
        Error (Fmt.str "albatross: failed to decode data %s" msg)
    | Ok (hdr, res) as w ->
        if not Vmm_commands.(is_current hdr.version) then
          Logs.warn (fun m ->
              m "version mismatch, received %a current %a"
                Vmm_commands.pp_version hdr.Vmm_commands.version
                Vmm_commands.pp_version Vmm_commands.current);
        Logs.debug (fun m ->
            m "albatross returned: %a"
              (Vmm_commands.pp_wire ~verbose:true)
              (hdr, res));
        w

  let decode_reply data =
    if Cstruct.length data >= 4 then
      let len = Int32.to_int (Cstruct.BE.get_uint32 data 0) in
      if Cstruct.length data >= 4 + len then (
        if Cstruct.length data > 4 + len then
          Logs.warn (fun m ->
              m "received %d trailing bytes" (Cstruct.length data - len - 4));
        decode (Cstruct.sub data 4 len))
      else
        Error
          (Fmt.str "reply too short: received %u bytes, expected %u bytes"
             (Cstruct.length data) (len + 4))
    else
      Error
        (Fmt.str "buffer too short (%u bytes), need at least 4 bytes"
           (Cstruct.length data))

  let split_many data =
    let rec split acc data =
      if Cstruct.length data >= 4 then
        let len = Int32.to_int (Cstruct.BE.get_uint32 data 0) in
        if Cstruct.length data >= 4 + len then
          split (Cstruct.sub data 4 len :: acc) (Cstruct.shift data (len + 4))
        else (
          Logs.warn (fun m ->
              m "buffer too small: %u bytes, requires %u bytes"
                (Cstruct.length data - 4)
                len);
          acc)
      else if Cstruct.length data = 0 then acc
      else (
        Logs.warn (fun m ->
            m "buffer too small: %u bytes leftover" (Cstruct.length data));
        acc)
    in
    split [] data |> List.rev

  let rec continue_reading t name flow =
    let open Lwt.Infix in
    TLS.read flow >>= function
    | Ok (`Data d) ->
        let bufs = split_many d in
        List.iter
          (fun d ->
            match decode d with
            | Error s ->
                Logs.err (fun m ->
                    m "albatross stop reading console %s: error %s" name s)
            | Ok (_, `Data (`Console_data (ts, data))) ->
                let d = Albatross_json.console_data_to_json (ts, data) in
                t.console_output <-
                  Map.update name
                    (function
                      | None -> Some [ d ]
                      | Some xs ->
                          let xs =
                            if List.length xs > 20 then List.tl xs else xs
                          in
                          Some (xs @ [ d ]))
                    t.console_output
            | _ ->
                Logs.warn (fun m ->
                    m "unexpected reply, expected console output"))
          bufs;
        continue_reading t name flow
    | Ok `Eof ->
        Logs.info (fun m ->
            m "received eof from albatross while reading console %s" name);
        t.console_readers <- Set.remove name t.console_readers;
        TLS.close flow
    | Error e ->
        Logs.err (fun m ->
            m "received error from albatross while reading console %s: %a" name
              TLS.pp_error e);
        t.console_readers <- Set.remove name t.console_readers;
        TLS.close flow

  let raw_query t ?(name = ".") certificates cmd =
    let open Lwt.Infix in
    S.TCP.create_connection (S.tcp t.stack) t.remote >>= function
    | Error e ->
        Lwt.return
          (Error
             (Fmt.str "albatross connection failure %a: %a"
                Fmt.(pair ~sep:(any ":") Ipaddr.pp int)
                t.remote S.TCP.pp_error e))
    | Ok flow -> (
        let tls_config =
          let authenticator =
            X509.Authenticator.chain_of_trust
              ~time:(fun () -> Some (Ptime.v (P.now_d_ps ())))
              [ t.cert ]
          in
          Tls.Config.client ~authenticator ~certificates ()
        in
        TLS.client_of_flow tls_config flow >>= function
        | Error e ->
            S.TCP.close flow >|= fun () ->
            Error
              (Fmt.str "error establishing TLS handshake %a: %a"
                 Fmt.(pair ~sep:(any ":") Ipaddr.pp int)
                 t.remote TLS.pp_write_error e)
        | Ok tls_flow -> (
            TLS.read tls_flow >>= fun r ->
            match r with
            | Ok (`Data d) ->
                (match snd (Vmm_commands.endpoint cmd) with
                | `End -> TLS.close tls_flow
                | `Read ->
                    t.console_readers <- Set.add name t.console_readers;
                    Lwt.async (fun () -> continue_reading t name tls_flow);
                    Lwt.return_unit)
                >|= fun () ->
                Result.map (fun reply -> Some reply) (decode_reply d)
            | Ok `Eof -> TLS.close tls_flow >|= fun () -> Ok None
            | Error e ->
                TLS.close tls_flow >|= fun () ->
                Error
                  (Fmt.str "received error while reading %a: %a"
                     Fmt.(pair ~sep:(any ":") Ipaddr.pp int)
                     t.remote TLS.pp_error e)))

  let init stack server ?(port = 1025) cert key =
    let open Lwt.Infix in
    let t =
      {
        stack;
        remote = (server, port);
        cert;
        key;
        policies = [];
        console_readers = Set.empty;
        console_output = Map.empty;
      }
    in
    let cmd = `Policy_cmd `Policy_info in
    match gen_cert t cmd "." with
    | Error s -> invalid_arg s
    | Ok certificates -> (
        raw_query t certificates cmd >|= function
        | Ok (Some (_hdr, `Success (`Policies ps))) ->
            t.policies <- ps;
            t
        | Ok (Some w) ->
            Logs.err (fun m ->
                m "unexpected reply %a" (Vmm_commands.pp_wire ~verbose:false) w);
            t
        | Ok None -> t
        | Error str ->
            Logs.err (fun m -> m "couldn't query policies: %s" str);
            t)

  let query t ~domain ?(name = ".") cmd =
    match cmd with
    | `Console_cmd (`Console_subscribe _) when Set.mem name t.console_readers ->
        Lwt.return (Ok None)
    | _ -> (
        match gen_cert t ~domain cmd name with
        | Error str -> Lwt.return (Error str)
        | Ok certificates -> raw_query t ~name certificates cmd)
end
