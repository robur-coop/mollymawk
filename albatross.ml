module Make (P : Mirage_clock.PCLOCK) (S : Tcpip.Stack.V4V6) = struct
  module TLS = Tls_mirage.Make (S.TCP)

  module Name = struct
    type t = Vmm_core.Name.t

    let compare a b =
      String.compare (Vmm_core.Name.to_string a) (Vmm_core.Name.to_string b)
  end

  module Set = Set.Make (Name)
  module Map = Map.Make (Name)

  type t = {
    stack : S.t;
    remote : Ipaddr.t * int;
    cert : X509.Certificate.t;
    key : X509.Private_key.t;
    mutable policies : (Vmm_core.Name.t * Vmm_core.Policy.t) list;
    mutable console_readers : Set.t;
    mutable console_output : Yojson.Basic.t list Map.t;
  }

  let policy t domain =
    match Vmm_core.Name.path_of_string domain with
    | Error (`Msg msg) ->
        Error (Fmt.str "albatross: domain %s is not a path: %s" domain msg)
    | Ok path ->
        Ok (List.assoc_opt (Vmm_core.Name.create_of_path path) t.policies)

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

  let key_cert ?(key_type = `ED25519) ~is_ca ?cmd key name issuer =
    let open X509 in
    let exts =
      let ext =
        Option.map Vmm_asn.to_cert_extension cmd
        |> Option.fold ~none:Extension.empty ~some:(fun p ->
               Extension.singleton (Unsupported Vmm_asn.oid) (false, p))
      in
      let ku, bc =
        if is_ca then
          ( [ `Key_cert_sign; `Digital_signature; `Content_commitment ],
            (true, None) )
        else ([ `Digital_signature; `Key_encipherment ], (false, None))
      in
      Extension.(
        add Basic_constraints (true, bc) (add Key_usage (true, ku) ext))
    in
    let tmp_key = Private_key.generate key_type in
    match
      let name =
        [ Distinguished_name.(Relative_distinguished_name.singleton (CN name)) ]
      in
      let extensions = Signing_request.Ext.(singleton Extensions exts) in
      Signing_request.create name ~extensions tmp_key
    with
    | Error (`Msg msg) ->
        Error
          (Fmt.str "albatross: failed to create %ssigning request: %s"
             (if is_ca then "CA " else "")
             msg)
    | Ok csr -> (
        let valid_from, valid_until = timestamps 300 in
        let extensions =
          let pub = X509.Private_key.public key in
          key_ids exts Signing_request.((info csr).public_key) pub
        in
        match
          Signing_request.sign csr ~valid_from ~valid_until ~extensions key
            issuer
        with
        | Error e ->
            Error
              (Fmt.str "albatross: failed to sign %ssigning request: %a"
                 (if is_ca then "CA " else "")
                 X509.Validation.pp_signature_error e)
        | Ok mycert -> Ok (tmp_key, mycert))

  let gen_cert t ?domain cmd name =
    let ( let* ) = Result.bind in
    let* ikey, cert, certs =
      match domain with
      | None -> Ok (t.key, t.cert, [])
      | Some domain ->
          let* policy = policy t domain in
          let policy =
            Option.value
              ~default:
                Vmm_core.(
                  Policy.
                    {
                      vms = 0;
                      cpuids = IS.empty;
                      memory = 0;
                      block = None;
                      bridges = String_set.empty;
                    })
              policy
          in
          let cmd = `Policy_cmd (`Policy_add policy) in
          let* key, cert =
            key_cert ~is_ca:true ~cmd t.key domain
              (X509.Certificate.subject t.cert)
          in
          Ok (key, cert, [ cert ])
    in
    let* key, cert =
      key_cert ~is_ca:false ~cmd ikey name (X509.Certificate.subject cert)
    in
    Ok (`Single (cert :: certs, key))

  let decode data =
    match Vmm_asn.wire_of_str data with
    | Error (`Msg msg) ->
        Error (Fmt.str "albatross: failed to decode data %s" msg)
    | Ok (hdr, res) as w ->
        if not Vmm_commands.(is_current hdr.version) then
          Logs.warn (fun m ->
              m "albatross version mismatch, received %a current %a"
                Vmm_commands.pp_version hdr.Vmm_commands.version
                Vmm_commands.pp_version Vmm_commands.current);
        Logs.debug (fun m ->
            m "albatross returned: %a"
              (Vmm_commands.pp_wire ~verbose:true)
              (hdr, res));
        w

  let decode_reply data =
    if String.length data >= 4 then
      let len = Int32.to_int (String.get_int32_be data 0) in
      if String.length data >= 4 + len then (
        if String.length data > 4 + len then
          Logs.warn (fun m ->
              m "received %d trailing bytes" (String.length data - len - 4));
        decode (String.sub data 4 len))
      else
        Error
          (Fmt.str "albatross short reply: received %u bytes, expected %u bytes"
             (String.length data) (len + 4))
    else
      Error
        (Fmt.str "albatross short buffer (%u bytes), need at least 4 bytes"
           (String.length data))

  let split_many data =
    let rec split acc data off =
      if String.length data - off >= 4 then
        let len = Int32.to_int (String.get_int32_be data off) in
        if String.length data - off >= 4 + len then
          split (String.sub data (4 + off) len :: acc) data (off + len + 4)
        else (
          Logs.warn (fun m ->
              m "albatross buffer too small: %u bytes, requires %u bytes"
                (String.length data - 4 - off)
                len);
          acc)
      else if String.length data = off then acc
      else (
        Logs.warn (fun m ->
            m "albatross buffer too small: %u bytes leftover"
              (String.length data - off));
        acc)
    in
    split [] data 0 |> List.rev

  let rec continue_reading t name flow =
    let open Lwt.Infix in
    TLS.read flow >>= function
    | Ok (`Data d) ->
        let str = Cstruct.to_string d in
        let bufs = split_many str in
        List.iter
          (fun d ->
            match decode d with
            | Error s ->
                Logs.err (fun m ->
                    m "albatross stop reading console %a: error %s"
                      Vmm_core.Name.pp name s)
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
            | Ok w ->
                Logs.warn (fun m ->
                    m "albatross unexpected reply, need console output, got %a"
                      (Vmm_commands.pp_wire ~verbose:false)
                      w))
          bufs;
        continue_reading t name flow
    | Ok `Eof ->
        Logs.info (fun m ->
            m "albatross received eof while reading console %a" Vmm_core.Name.pp
              name);
        t.console_readers <- Set.remove name t.console_readers;
        TLS.close flow
    | Error e ->
        Logs.err (fun m ->
            m "albatross received error while reading console %a: %a"
              Vmm_core.Name.pp name TLS.pp_error e);
        t.console_readers <- Set.remove name t.console_readers;
        TLS.close flow

  let raw_query t ?(name = Vmm_core.Name.root) certificates cmd =
    let open Lwt.Infix in
    S.TCP.create_connection (S.tcp t.stack) t.remote >>= function
    | Error e ->
        Lwt.return
          (Error
             (Fmt.str "albatross connection failure %a while quering %a %a: %a"
                Fmt.(pair ~sep:(any ":") Ipaddr.pp int)
                t.remote Vmm_core.Name.pp name
                (Vmm_commands.pp ~verbose:false)
                cmd S.TCP.pp_error e))
    | Ok flow -> (
        match
          let authenticator =
            X509.Authenticator.chain_of_trust
              ~time:(fun () -> Some (Ptime.v (P.now_d_ps ())))
              [ t.cert ]
          in
          Tls.Config.client ~authenticator ~certificates ()
        with
        | Error (`Msg msg) ->
            Lwt.return
              (Error (Fmt.str "albatross setting up TLS config: %s" msg))
        | Ok tls_config -> (
            TLS.client_of_flow tls_config flow >>= function
            | Error e ->
                S.TCP.close flow >|= fun () ->
                Error
                  (Fmt.str
                     "albatross establishing TLS to %a while querying %a %a: %a"
                     Fmt.(pair ~sep:(any ":") Ipaddr.pp int)
                     t.remote Vmm_core.Name.pp name
                     (Vmm_commands.pp ~verbose:false)
                     cmd TLS.pp_write_error e)
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
                    >|= fun () -> decode_reply (Cstruct.to_string d)
                | Ok `Eof ->
                    TLS.close tls_flow >|= fun () ->
                    Error
                      (Fmt.str "eof from albatross %a querying %a %a"
                         Fmt.(pair ~sep:(any ":") Ipaddr.pp int)
                         t.remote Vmm_core.Name.pp name
                         (Vmm_commands.pp ~verbose:false)
                         cmd)
                | Error e ->
                    TLS.close tls_flow >|= fun () ->
                    Error
                      (Fmt.str
                         "albatross received error reading from %a querying %a \
                          %a: %a"
                         Fmt.(pair ~sep:(any ":") Ipaddr.pp int)
                         t.remote Vmm_core.Name.pp name
                         (Vmm_commands.pp ~verbose:false)
                         cmd TLS.pp_error e))))

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
        | Ok (_hdr, `Success (`Policies ps)) ->
            t.policies <- ps;
            t
        | Ok w ->
            Logs.err (fun m ->
                m "albatross expected success policies, got reply %a"
                  (Vmm_commands.pp_wire ~verbose:false)
                  w);
            t
        | Error str ->
            Logs.err (fun m -> m "albatross: error querying policies: %s" str);
            t)

  let query t ~domain ?(name = ".") cmd =
    match
      Result.bind (Vmm_core.Name.path_of_string domain) (fun domain ->
          Vmm_core.Name.create domain name)
    with
    | Error (`Msg msg) -> Lwt.return (Error msg)
    | Ok vmm_name -> (
        match cmd with
        | `Console_cmd (`Console_subscribe _)
          when Set.mem vmm_name t.console_readers ->
            let hdr =
              Vmm_commands.{ version = current; sequence = 0L; name = vmm_name }
            in
            Lwt.return (Ok (hdr, `Success `Empty))
        | _ -> (
            match gen_cert t ~domain cmd name with
            | Error str -> Lwt.return (Error str)
            | Ok certificates -> raw_query t ~name:vmm_name certificates cmd))
end
