open Lwt.Infix

let pp_msg ppf (`Msg msg) =
  Fmt.pf ppf "%s" msg

let err_to_exit pp = function
  | Ok x -> x
  | Error e ->
    Logs.err (fun m -> m "received error %a" pp e);
    exit Mirage_runtime.argument_error

module K = struct
  open Cmdliner
  let albatross_server =
    let doc = Arg.info ~doc:"albatross server IP" ["albatross-server"] in
    Arg.(value & (opt Mirage_runtime_network.Arg.ip_address (Ipaddr.of_string_exn "192.168.1.3") doc))

  let port =
    let doc = Arg.info ~doc:"server port" ["port"] in
    Arg.(value & (opt int 1025 doc))
end

module Main (R : Mirage_random.S) (P : Mirage_clock.PCLOCK) (M : Mirage_clock.MCLOCK) (T : Mirage_time.S) (S : Tcpip.Stack.V4V6) (KV : Mirage_kv.RO) (KV_JS : Mirage_kv.RO) = struct
  module Paf = Paf_mirage.Make(S.TCP)

  let retrieve_credentials data =
    (KV.get data (Mirage_kv.Key.v "key.pem") >|=
     err_to_exit KV.pp_error >|= fun key ->
     err_to_exit pp_msg (X509.Private_key.decode_pem (Cstruct.of_string key))) >>= fun key ->
    (KV.get data (Mirage_kv.Key.v "cert.pem") >|=
     err_to_exit KV.pp_error >|= fun cert ->
     err_to_exit pp_msg (X509.Certificate.decode_pem_multiple (Cstruct.of_string cert))) >>= fun certs ->
    (KV.get data (Mirage_kv.Key.v "server.pem") >|=
     err_to_exit KV.pp_error >|= fun server ->
     err_to_exit pp_msg (X509.Certificate.decode_pem (Cstruct.of_string server))) >|= fun server ->
    let cert, certs = match certs with
      | hd :: tl -> hd, tl
      | [] -> Logs.err (fun m -> m "no certificate found"); exit Mirage_runtime.argument_error
    in
    if not
        (Cstruct.equal
           (X509.Public_key.fingerprint (X509.Certificate.public_key cert))
           (X509.Public_key.fingerprint (X509.Private_key.public key)))
    then begin
      Logs.err (fun m -> m "certificate and private key do not match");
      exit Mirage_runtime.argument_error
    end;
    (key, cert, certs, server)

  let js_contents js =
    let file = KV_JS.get js (Mirage_kv.Key.v "main.js") in
    let result =
      file >|= fun content ->
      match content with
      | Error _e -> "JS file could not be loaded"
      | Ok js -> js
    in
    Lwt_main.run result

  let create_html_form js =
    let file = KV_JS.get js (Mirage_kv.Key.v "create_unikernel.html") in
    let result =
      file >|= fun content ->
      match content with
      | Error _e -> "Form could not be loaded"
      | Ok html -> html
    in
    Lwt_main.run result


  module TLS = Tls_mirage.Make(S.TCP)

  let key_ids exts pub issuer =
    let open X509 in
    let auth = (Some (Public_key.id issuer), General_name.empty, None) in
    Extension.(add Subject_key_id (false, (Public_key.id pub))
                 (add Authority_key_id (false, auth) exts))

  let timestamps validity =
    let now = Ptime.v (P.now_d_ps ()) in
    match
      (* subtracting some seconds here to not require perfectly synchronised
         clocks on client and server *)
      Ptime.sub_span now (Ptime.Span.of_int_s (validity / 2)),
      Ptime.add_span now (Ptime.Span.of_int_s validity)
    with
    | None, _ | _, None -> invalid_arg "span too big - reached end of ptime"
    | Some now, Some exp -> (now, exp)

  let gen_cert cert ?(certs = []) key ?bits ?(key_type = `ED25519) cmd name =
    let open X509 in
    let tmpkey = Private_key.generate ?bits key_type in
    let extensions =
      let v = Vmm_asn.to_cert_extension cmd in
      Extension.(add Key_usage (true, [ `Digital_signature ; `Key_encipherment ])
                   (add Basic_constraints (true, (false, None))
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
    | Error `Msg msg ->
      Logs.err (fun m -> m "failed to construct signing request: %s" msg);
      Error ()
    | Ok csr ->
      let valid_from, valid_until = timestamps 300 in
      let extensions =
        let capub = X509.Private_key.public key in
        key_ids extensions Signing_request.((info csr).public_key) capub
      in
      let issuer = Certificate.subject cert in
      match Signing_request.sign csr ~valid_from ~valid_until ~extensions key issuer with
      | Error e ->
        Logs.err (fun m -> m "failed to sign CSR: %a" X509.Validation.pp_signature_error e);
        Error ()
      | Ok mycert ->
        Ok (`Single (mycert :: cert :: certs, tmpkey))

  let query_albatross stack (key, cert, certs, server_cert) remote ?(name = ".") cmd =
    S.TCP.create_connection (S.tcp stack) remote >>= function
    | Error e ->
      Logs.err (fun m -> m "error connecting to albatross: %a" S.TCP.pp_error e);
      Lwt.return (Error ())
    | Ok flow ->
      match gen_cert cert ~certs key cmd name with
      | Error () ->
        S.TCP.close flow >|= fun () ->
        Error ()
      | Ok certificates ->
        let tls_config =
          let authenticator =
            X509.Authenticator.chain_of_trust
              ~time:(fun () -> Some (Ptime.v (P.now_d_ps ())))
              [server_cert]
          in
          Tls.Config.client ~authenticator ~certificates ()
        in
        TLS.client_of_flow tls_config flow >>= function
        | Error e ->
          Logs.err (fun m -> m "error establishing TLS handshake: %a"
                       TLS.pp_write_error e);
          S.TCP.close flow >|= fun () ->
          Error ()
        | Ok tls_flow ->
          TLS.read tls_flow >>= fun r ->
          TLS.close tls_flow >|= fun () ->
          match r with
          | Ok `Data d -> Ok (Some d)
          | Ok `Eof -> Ok None
          | Error e ->
            Logs.err (fun m -> m "received error while reading: %a"
                         TLS.pp_error e);
            Error ()

  let decode_reply data =
    if Cstruct.length data >= 4 then
      let len = Int32.to_int (Cstruct.BE.get_uint32 data 0) in
      if Cstruct.length data >= 4 + len then
        match Vmm_asn.wire_of_cstruct (Cstruct.sub data 4 len) with
        | Error (`Msg msg) ->
          Logs.err (fun m -> m "error %s while decoding data" msg) ;
          Error ()
        | (Ok (hdr, _)) as w ->
          if Cstruct.length data > 4 + len then
            Logs.warn (fun m -> m "received %d trailing bytes"
                          (Cstruct.length data - len - 4));
          if not Vmm_commands.(is_current hdr.version) then
            Logs.warn (fun m -> m "version mismatch, received %a current %a"
                          Vmm_commands.pp_version hdr.Vmm_commands.version
                          Vmm_commands.pp_version Vmm_commands.current);
          w
      else
        begin
          Logs.err (fun m -> m "buffer too short (%d bytes), need %d + 4 bytes"
                       (Cstruct.length data) len);
          Error ()
        end
    else begin
      Logs.err (fun m -> m "buffer too short (%d bytes), need at least 4 bytes"
                   (Cstruct.length data));
      Error ()
    end

  module Map = Map.Make (String)

  let to_map ~assoc m =
    let open Multipart_form in
    let rec go (map, rest) = function
      | Leaf { header; body } -> (
          let filename =
            Option.bind
              (Header.content_disposition header)
              Content_disposition.filename in
          match
            Option.bind
              (Header.content_disposition header)
              Content_disposition.name
          with
          | Some name -> (Map.add name (filename, List.assoc body assoc) map, rest)
          | None -> (map, (body, (filename, List.assoc body assoc)) :: rest))
      | Multipart { body; _ } ->
        let fold acc = function Some elt -> go acc elt | None -> acc in
        List.fold_left fold (map, rest) body in
    go (Map.empty, []) m

  let request_handler stack credentials remote js_file html (_ipaddr, _port) reqd =
    Lwt.async (fun () ->
        let reply ?(content_type="text/plain") data =
          let headers = Httpaf.Headers.of_list [
              "content-length", string_of_int (String.length data) ;
              "content-type", content_type ;
            ]
          in
          let resp = Httpaf.Response.create ~headers `OK in
          Httpaf.Reqd.respond_with_string reqd resp data
        in
        let reply_json json =
          reply ~content_type:"application/json" (Yojson.Basic.to_string json)
        in
        let path =
          Uri.(pct_decode (path (of_string (Httpaf.Reqd.request reqd).Httpaf.Request.target)))
        in
        match path with
        | "/" ->
          Lwt.return (reply ~content_type:"text/html" Index.index_page)
        | "/unikernel-info" ->
          (query_albatross stack credentials remote ((* `Block_cmd `Block_info *) (* `Policy_cmd `Policy_info *) `Unikernel_cmd `Unikernel_info) >|= function
          | Error () -> reply "error while querying albatross"
          | Ok None -> reply "got none"
          | Ok Some data ->
            (match decode_reply data with
            | Error () -> reply "couldn't decode albatross' reply"
            | Ok (hdr, res) ->
              Logs.info (fun m -> m "albatross returned: %a"
                            (Vmm_commands.pp_wire ~verbose:true) (hdr, res));
              reply_json (Albatross_json.res res)))
        | path when String.(length path >= 16 && sub path 0 16 = "/unikernel/info/") ->
          let unikernel_name = String.sub path 17 (String.length path - 17) in
          print_endline unikernel_name;
          (query_albatross stack credentials remote (`Unikernel_cmd `Unikernel_info) ~name:unikernel_name >|= function
          | Error () -> reply "error while querying albatross"
          | Ok None -> reply "got none"
          | Ok Some data ->
            (match decode_reply data with
            | Error () -> reply "couldn't decode albatross' reply"
            | Ok (hdr, res) ->
              Logs.info (fun m -> m "albatross returned: %a"
                            (Vmm_commands.pp_wire ~verbose:true) (hdr, res));
                            reply_json (Albatross_json.res res)))
        | path when String.(length path >= 16 && sub path 0 16 = "/unikernel/info/") ->
          let unikernel_name = String.sub path 17 (String.length path - 17) in
          (query_albatross stack credentials remote (`Unikernel_cmd `Unikernel_info) ~name:unikernel_name >|= function
          | Error () -> reply "error while querying albatross"
          | Ok None -> reply "got none"
          | Ok Some data ->
            (match decode_reply data with
            | Error () -> reply "couldn't decode albatross' reply"
            | Ok (hdr, res) ->
              Logs.info (fun m -> m "albatross returned: %a"
                            (Vmm_commands.pp_wire ~verbose:true) (hdr, res));
                            reply_json (Albatross_json.res res)))
        | "/main.js" ->
          Lwt.return (reply ~content_type:"text/plain" js_file)
        | "/unikernel/create" ->
          Lwt.return (reply ~content_type:"text/html" html)
        | path when String.(length path >= 20 && sub path 0 20 = "/unikernel/shutdown/") ->
          let unikernel_name = String.sub path 21 (String.length path - 21) in
            (query_albatross stack credentials remote (`Unikernel_cmd `Unikernel_destroy) ~name:unikernel_name >|= function
            | Error () -> reply "error while querying albatross"
            | Ok None -> reply "got none"
            | Ok Some data ->
              (match decode_reply data with
              | Error () -> reply "couldn't decode albatross' reply"
              | Ok (hdr, res) ->
                Logs.info (fun m -> m "albatross returned: %a"
                              (Vmm_commands.pp_wire ~verbose:true) (hdr, res));
                reply_json (Albatross_json.res res)))
        | "/unikernel/deploy" ->
          let response_body = Httpaf.Reqd.request_body reqd in
          let finished, notify_finished = Lwt.wait () in
          let wakeup v =
            Lwt.wakeup_later notify_finished v
          in
          let on_eof data () = wakeup data in
          let f acc s = acc ^ s in
          let rec on_read on_eof acc bs ~off ~len =
            let str = Bigstringaf.substring ~off ~len bs in
            let acc =
              acc >>= fun acc ->
              Lwt.return (f acc str)
            in
            Httpaf.Body.schedule_read response_body
              ~on_read:(on_read on_eof acc)
              ~on_eof:(on_eof acc)
          in
          let f_init = Lwt.return "" in
          Httpaf.Body.schedule_read response_body
            ~on_read:(on_read on_eof f_init)
            ~on_eof:(on_eof f_init);
          finished >>= fun data ->
          data >>= fun data ->
          let content_type = Httpaf.(Headers.get_exn (Reqd.request reqd).Request.headers "content-type") in
          let ct = Multipart_form.Content_type.of_string (content_type ^ "\r\n") in
          (match ct with
           | Error `Msg msg ->
             Logs.warn (fun m -> m "couldn't content-type: %s" msg);
             Lwt.return (reply "couldn't content-type")
           | Ok ct ->
             match Multipart_form.of_string_to_list data ct with
             | Error `Msg msg ->
             Logs.warn (fun m -> m "couldn't multipart: %s" msg);
               Lwt.return (reply ("couldn't multipart: " ^ msg))
             | Ok (m, assoc) ->
               let m, r = to_map ~assoc m in
               match Map.find_opt "arguments" m,
                     Map.find_opt "name" m,
                     Map.find_opt "binary" m
               with
               | Some (_, args), Some (_, name), Some (_, binary) ->
                 Logs.info (fun m -> m "args %s" args);
                 (match Albatross_json.config_of_json args with
                  | Ok cfg ->
                    let config = { cfg with image = Cstruct.of_string binary } in
                    (query_albatross stack credentials remote ~name (`Unikernel_cmd (`Unikernel_create config)) >|= function
                      | Error () ->
                        Logs.warn (fun m -> m "error querying albatross");
                        reply "error while querying albatross"
                      | Ok None ->
                        Logs.warn (fun m -> m "got none");
                        reply "got none"
                      | Ok Some data ->
                        (match decode_reply data with
                         | Error () ->
                           Logs.warn (fun m -> m "couldn't decode albatross reply");
                           reply "couldn't decode albatross' reply"
                         | Ok (hdr, res) ->
                           Logs.info (fun m -> m "albatross returned: %a"
                                         (Vmm_commands.pp_wire ~verbose:true) (hdr, res));
                           reply_json (Albatross_json.res res)))
                  | Error `Msg msg ->
                    Logs.warn (fun m -> m "couldn't decode data %s" msg);
                    Lwt.return (reply ("couldn't decode data (of_json): " ^ msg)))
               | _ ->
                 Logs.warn (fun m -> m "couldn't find fields");
                 Lwt.return (reply "couldn't find fields"))
        | _ ->
          Lwt.return (reply ~content_type:"text/plain" "Error 404: this endpoint doesn't exist"))

  let pp_error ppf = function
    | #Httpaf.Status.t as code -> Httpaf.Status.pp_hum ppf code
    | `Exn exn -> Fmt.pf ppf "exception %s" (Printexc.to_string exn)

  let error_handler _dst ?request err _ =
    Logs.err (fun m -> m "error %a while processing request %a"
                 pp_error err
                 Fmt.(option ~none:(any "unknown") Httpaf.Request.pp_hum) request)


  let start _ _ _ _ stack data js host port =
    let js_file = js_contents js in
    let html = create_html_form js in
    retrieve_credentials data >>= fun credentials ->
    let remote = host, port in
    let port = 8080 in
    Logs.info (fun m -> m "Initialise an HTTP server (no HTTPS) on http://127.0.0.1:%u/" port) ;
    let request_handler _flow = request_handler stack credentials remote js_file html in
    Paf.init ~port:8080 (S.tcp stack) >>= fun service ->
    let http = Paf.http_service ~error_handler request_handler in
    let (`Initialized th) = Paf.serve http service in
    th
end
