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

  let gen_cert cert key ?bits ?(key_type = `ED25519) cmd name =
    let open X509 in
    let tmpkey = Private_key.generate ?bits key_type in
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
        Logs.err (fun m -> m "failed to construct signing request: %s" msg);
        Error ()
    | Ok csr -> (
        let valid_from, valid_until = timestamps 300 in
        let extensions =
          let capub = X509.Private_key.public key in
          key_ids extensions Signing_request.((info csr).public_key) capub
        in
        let issuer = Certificate.subject cert in
        match
          Signing_request.sign csr ~valid_from ~valid_until ~extensions key
            issuer
        with
        | Error e ->
            Logs.err (fun m ->
                m "failed to sign CSR: %a" X509.Validation.pp_signature_error e);
            Error ()
        | Ok mycert -> Ok (`Single ([ mycert; cert ], tmpkey)))

  let decode data =
    match Vmm_asn.wire_of_cstruct data with
    | Error (`Msg msg) ->
        Logs.err (fun m -> m "error %s while decoding data" msg);
        Error ()
    | Ok (hdr, _) as w ->
        if not Vmm_commands.(is_current hdr.version) then
          Logs.warn (fun m ->
              m "version mismatch, received %a current %a"
                Vmm_commands.pp_version hdr.Vmm_commands.version
                Vmm_commands.pp_version Vmm_commands.current);
        w

  let decode_reply data =
    if Cstruct.length data >= 4 then
      let len = Int32.to_int (Cstruct.BE.get_uint32 data 0) in
      if Cstruct.length data >= 4 + len then (
        if Cstruct.length data > 4 + len then
          Logs.warn (fun m ->
              m "received %d trailing bytes" (Cstruct.length data - len - 4));
        decode (Cstruct.sub data 4 len))
      else (
        Logs.err (fun m ->
            m "buffer too short (%d bytes), need %d + 4 bytes"
              (Cstruct.length data) len);
        Error ())
    else (
      Logs.err (fun m ->
          m "buffer too short (%d bytes), need at least 4 bytes"
            (Cstruct.length data));
      Error ())

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
            | Error () -> ()
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
        t.console_readers <- Set.remove name t.console_readers;
        TLS.close flow
    | Error e ->
        t.console_readers <- Set.remove name t.console_readers;
        TLS.close flow >|= fun () ->
        Logs.err (fun m -> m "received error while reading: %a" TLS.pp_error e)

  let query t ?domain:_ ?(name = ".") cmd =
    let open Lwt.Infix in
    match cmd with
    | `Console_cmd (`Console_subscribe _) when Set.mem name t.console_readers ->
        Lwt.return (Ok None)
    | _ -> (
        S.TCP.create_connection (S.tcp t.stack) t.remote >>= function
        | Error e ->
            Logs.err (fun m ->
                m "error connecting to albatross: %a" S.TCP.pp_error e);
            Lwt.return (Error ())
        | Ok flow -> (
            match gen_cert t.cert t.key cmd name with
            | Error () -> S.TCP.close flow >|= fun () -> Error ()
            | Ok certificates -> (
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
                    Logs.err (fun m ->
                        m "error establishing TLS handshake: %a"
                          TLS.pp_write_error e);
                    S.TCP.close flow >|= fun () -> Error ()
                | Ok tls_flow -> (
                    TLS.read tls_flow >>= fun r ->
                    match r with
                    | Ok (`Data d) ->
                        (match snd (Vmm_commands.endpoint cmd) with
                        | `End -> TLS.close tls_flow
                        | `Read ->
                            t.console_readers <- Set.add name t.console_readers;
                            Lwt.async (fun () ->
                                continue_reading t name tls_flow);
                            Lwt.return_unit)
                        >|= fun () ->
                        Result.map (fun reply -> Some reply) (decode_reply d)
                    | Ok `Eof -> TLS.close tls_flow >|= fun () -> Ok None
                    | Error e ->
                        TLS.close tls_flow >|= fun () ->
                        Logs.err (fun m ->
                            m "received error while reading: %a" TLS.pp_error e);
                        Error ()))))

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
    query t (`Policy_cmd `Policy_info) >|= function
    | Ok (Some (_hdr, `Success (`Policies ps))) ->
        t.policies <- ps;
        t
    | _ -> t
end
