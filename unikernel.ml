open Lwt.Infix

type images = { molly_img : string; robur_img : string }

let pp_msg ppf (`Msg msg) = Fmt.pf ppf "%s" msg

let err_to_exit pp = function
  | Ok x -> x
  | Error e ->
      Logs.err (fun m -> m "received error %a" pp e);
      exit Mirage_runtime.argument_error

module K = struct
  open Cmdliner

  let albatross_server =
    let doc = Arg.info ~doc:"albatross server IP" [ "albatross-server" ] in
    Arg.(
      value
      & opt Mirage_runtime_network.Arg.ip_address
          (Ipaddr.of_string_exn "192.168.1.3")
          doc)

  let port =
    let doc = Arg.info ~doc:"server port" [ "port" ] in
    Arg.(value & opt int 1025 doc)
end

module Main
    (R : Mirage_random.S)
    (P : Mirage_clock.PCLOCK)
    (M : Mirage_clock.MCLOCK)
    (T : Mirage_time.S)
    (S : Tcpip.Stack.V4V6)
    (KV : Mirage_kv.RO)
    (KV_ASSETS : Mirage_kv.RO)
    (BLOCK : Mirage_block.S) =
struct
  module Paf = Paf_mirage.Make (S.TCP)

  let retrieve_credentials data =
    ( KV.get data (Mirage_kv.Key.v "key.pem") >|= err_to_exit KV.pp_error
    >|= fun key ->
      err_to_exit pp_msg (X509.Private_key.decode_pem (Cstruct.of_string key))
    )
    >>= fun key ->
    ( KV.get data (Mirage_kv.Key.v "cert.pem") >|= err_to_exit KV.pp_error
    >|= fun cert ->
      err_to_exit pp_msg
        (X509.Certificate.decode_pem_multiple (Cstruct.of_string cert)) )
    >>= fun certs ->
    ( KV.get data (Mirage_kv.Key.v "server.pem") >|= err_to_exit KV.pp_error
    >|= fun server ->
      err_to_exit pp_msg
        (X509.Certificate.decode_pem (Cstruct.of_string server)) )
    >|= fun server ->
    let cert, certs =
      match certs with
      | hd :: tl -> (hd, tl)
      | [] ->
          Logs.err (fun m -> m "no certificate found");
          exit Mirage_runtime.argument_error
    in
    if
      not
        (Cstruct.equal
           (X509.Public_key.fingerprint (X509.Certificate.public_key cert))
           (X509.Public_key.fingerprint (X509.Private_key.public key)))
    then (
      Logs.err (fun m -> m "certificate and private key do not match");
      exit Mirage_runtime.argument_error);
    (key, cert, certs, server)

  let js_contents assets =
    KV_ASSETS.get assets (Mirage_kv.Key.v "main.js") >|= function
    | Error _e -> invalid_arg "JS file could not be loaded"
    | Ok js -> js

  let css_contents assets =
    KV_ASSETS.get assets (Mirage_kv.Key.v "style.css") >|= function
    | Error _e -> invalid_arg "CSS file could not be loaded"
    | Ok css -> css

  let read_image assets key =
    KV_ASSETS.get assets (Mirage_kv.Key.v key) >|= function
    | Error _e -> invalid_arg "Image could not be loaded"
    | Ok img -> img

  let images assets =
    let molly_img = read_image assets "molly_bird.jpeg" in
    let robur_img = read_image assets "robur.png" in
    Lwt.both molly_img robur_img >|= fun (molly, robur) ->
    { molly_img = molly; robur_img = robur }

  let create_html_form assets =
    KV_ASSETS.get assets (Mirage_kv.Key.v "create_unikernel.html") >|= function
    | Error _e -> invalid_arg "Form could not be loaded"
    | Ok html -> html

  module Store = Storage.Make (BLOCK)
  module TLS = Tls_mirage.Make (S.TCP)

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

  let gen_cert cert ?(certs = []) key ?bits ?(key_type = `ED25519) cmd name =
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
        | Ok mycert -> Ok (`Single (mycert :: cert :: certs, tmpkey)))

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

  module Map = Map.Make (String)

  let console_output : Yojson.Basic.t list Map.t ref = ref Map.empty

  module Set = Set.Make (String)

  let active_console_readers = ref Set.empty

  let rec continue_reading name flow =
    TLS.read flow >>= function
    | Ok (`Data d) ->
        let bufs = split_many d in
        List.iter
          (fun d ->
            match decode d with
            | Error () -> ()
            | Ok (_, `Data (`Console_data (ts, data))) ->
                let d = Albatross_json.console_data_to_json (ts, data) in
                console_output :=
                  Map.update name
                    (function
                      | None -> Some [ d ]
                      | Some xs ->
                          let xs =
                            if List.length xs > 20 then List.tl xs else xs
                          in
                          Some (xs @ [ d ]))
                    !console_output
            | _ ->
                Logs.warn (fun m ->
                    m "unexpected reply, expected console output"))
          bufs;
        continue_reading name flow
    | Ok `Eof ->
        active_console_readers := Set.remove name !active_console_readers;
        TLS.close flow
    | Error e ->
        active_console_readers := Set.remove name !active_console_readers;
        TLS.close flow >|= fun () ->
        Logs.err (fun m -> m "received error while reading: %a" TLS.pp_error e)

  let query_albatross stack (key, cert, certs, server_cert) remote ?(name = ".")
      cmd =
    match cmd with
    | `Console_cmd (`Console_subscribe _)
      when Set.mem name !active_console_readers ->
        Lwt.return (Ok None)
    | _ -> (
        S.TCP.create_connection (S.tcp stack) remote >>= function
        | Error e ->
            Logs.err (fun m ->
                m "error connecting to albatross: %a" S.TCP.pp_error e);
            Lwt.return (Error ())
        | Ok flow -> (
            match gen_cert cert ~certs key cmd name with
            | Error () -> S.TCP.close flow >|= fun () -> Error ()
            | Ok certificates -> (
                let tls_config =
                  let authenticator =
                    X509.Authenticator.chain_of_trust
                      ~time:(fun () -> Some (Ptime.v (P.now_d_ps ())))
                      [ server_cert ]
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
                            active_console_readers :=
                              Set.add name !active_console_readers;
                            Lwt.async (fun () -> continue_reading name tls_flow);
                            Lwt.return_unit)
                        >|= fun () -> Ok (Some d)
                    | Ok `Eof -> TLS.close tls_flow >|= fun () -> Ok None
                    | Error e ->
                        TLS.close tls_flow >|= fun () ->
                        Logs.err (fun m ->
                            m "received error while reading: %a" TLS.pp_error e);
                        Error ()))))

  let to_map ~assoc m =
    let open Multipart_form in
    let rec go (map, rest) = function
      | Leaf { header; body } -> (
          let filename =
            Option.bind
              (Header.content_disposition header)
              Content_disposition.filename
          in
          match
            Option.bind
              (Header.content_disposition header)
              Content_disposition.name
          with
          | Some name ->
              (Map.add name (filename, List.assoc body assoc) map, rest)
          | None -> (map, (body, (filename, List.assoc body assoc)) :: rest))
      | Multipart { body; _ } ->
          let fold acc = function Some elt -> go acc elt | None -> acc in
          List.fold_left fold (map, rest) body
    in
    go (Map.empty, []) m

  let request_handler stack credentials remote js_file css_file imgs html store
      (_ipaddr, _port) reqd =
    Lwt.async (fun () ->
        let reply ?(content_type = "text/plain") data =
          let headers =
            Httpaf.Headers.of_list
              [
                ("content-length", string_of_int (String.length data));
                ("content-type", content_type);
              ]
          in
          let resp = Httpaf.Response.create ~headers `OK in
          Httpaf.Reqd.respond_with_string reqd resp data
        in
        let reply_json json =
          reply ~content_type:"application/json" (Yojson.Basic.to_string json)
        in
        let path =
          Uri.(
            pct_decode
              (path
                 (of_string (Httpaf.Reqd.request reqd).Httpaf.Request.target)))
        in
        match path with
        | "/" ->
            Lwt.return
              (reply ~content_type:"text/html"
                 (Index.index_page ~icon:"/images/robur.png"))
        | "/unikernel-info" -> (
            query_albatross stack credentials remote
              ((* `Block_cmd `Block_info *)
               (* `Policy_cmd `Policy_info *)
                `Unikernel_cmd
                `Unikernel_info)
            >|= function
            | Error () -> reply "error while querying albatross"
            | Ok None -> reply "got none"
            | Ok (Some data) -> (
                match decode_reply data with
                | Error () -> reply "couldn't decode albatross' reply"
                | Ok (hdr, res) ->
                    Logs.info (fun m ->
                        m "albatross returned: %a"
                          (Vmm_commands.pp_wire ~verbose:true)
                          (hdr, res));
                    reply_json (Albatross_json.res res)))
        | path
          when String.(length path >= 16 && sub path 0 16 = "/unikernel/info/")
          -> (
            let unikernel_name = String.sub path 17 (String.length path - 17) in
            print_endline unikernel_name;
            query_albatross stack credentials remote
              (`Unikernel_cmd `Unikernel_info) ~name:unikernel_name
            >|= function
            | Error () -> reply "error while querying albatross"
            | Ok None -> reply "got none"
            | Ok (Some data) -> (
                match decode_reply data with
                | Error () -> reply "couldn't decode albatross' reply"
                | Ok (hdr, res) ->
                    Logs.info (fun m ->
                        m "albatross returned: %a"
                          (Vmm_commands.pp_wire ~verbose:true)
                          (hdr, res));
                    reply_json (Albatross_json.res res)))
        | path
          when String.(length path >= 16 && sub path 0 16 = "/unikernel/info/")
          -> (
            let unikernel_name = String.sub path 17 (String.length path - 17) in
            query_albatross stack credentials remote
              (`Unikernel_cmd `Unikernel_info) ~name:unikernel_name
            >|= function
            | Error () -> reply "error while querying albatross"
            | Ok None -> reply "got none"
            | Ok (Some data) -> (
                match decode_reply data with
                | Error () -> reply "couldn't decode albatross' reply"
                | Ok (hdr, res) ->
                    Logs.info (fun m ->
                        m "albatross returned: %a"
                          (Vmm_commands.pp_wire ~verbose:true)
                          (hdr, res));
                    reply_json (Albatross_json.res res)))
        | "/main.js" -> Lwt.return (reply ~content_type:"text/plain" js_file)
        | "/images/molly_bird.jpeg" ->
            Lwt.return (reply ~content_type:"image/jpeg" imgs.molly_img)
        | "/images/robur.png" ->
            Lwt.return (reply ~content_type:"image/png" imgs.robur_img)
        | "/style.css" -> Lwt.return (reply ~content_type:"text/css" css_file)
        | "/sign-up" ->
            Lwt.return
              (reply ~content_type:"text/html"
                 (Sign_up.register_page ~icon:"/images/robur.png" ()))
        | "/api/register" -> (
            let request = Httpaf.Reqd.request reqd in
            match request.meth with
            | `POST -> (
                let request_body = Httpaf.Reqd.request_body reqd in
                let finished, notify_finished = Lwt.wait () in
                let wakeup v = Lwt.wakeup_later notify_finished v in
                let on_eof data () = wakeup data in
                let f acc s = acc ^ s in
                let rec on_read on_eof acc bs ~off ~len =
                  let str = Bigstringaf.substring ~off ~len bs in
                  let acc = acc >>= fun acc -> Lwt.return (f acc str) in
                  Httpaf.Body.schedule_read request_body
                    ~on_read:(on_read on_eof acc) ~on_eof:(on_eof acc)
                in
                let f_init = Lwt.return "" in
                Httpaf.Body.schedule_read request_body
                  ~on_read:(on_read on_eof f_init) ~on_eof:(on_eof f_init);
                finished >>= fun data ->
                data >>= fun data ->
                let json =
                  try Ok (Yojson.Basic.from_string data)
                  with Yojson.Json_error s -> Error (`Msg s)
                in
                match json with
                | Error (`Msg s) ->
                    Logs.warn (fun m -> m "Failed to parse JSON: %s" s);
                    let res =
                      "{\"status\": 400, \"message\": \"Bad request body\"}"
                    in
                    Lwt.return (reply ~content_type:"application/json" res)
                | Ok json -> (
                    let validate_email_re =
                      Re.Perl.re "[a-zA-Z0-9.$_!]+@[a-zA-Z0-9]+\\.[a-z]{2,3}"
                      |> Re.compile
                    in
                    let validate_user_input name email password =
                      if name = "" || email = "" || password = "" then
                        Error "All fields must be filled."
                      else if String.length name < 4 then
                        Error "Name must be at least 3 characters long."
                      else if not (Re.execp validate_email_re email) then
                        Error "Invalid email address."
                      else if String.length password < 8 then
                        Error "Password must be at least 8 characters long."
                      else Ok "Validation passed."
                    in
                    let name =
                      json
                      |> Yojson.Basic.Util.member "name"
                      |> Yojson.Basic.to_string
                    in
                    let email =
                      json
                      |> Yojson.Basic.Util.member "email"
                      |> Yojson.Basic.to_string
                    in
                    let password =
                      json
                      |> Yojson.Basic.Util.member "password"
                      |> Yojson.Basic.to_string
                    in
                    let validate_user =
                      validate_user_input name email password
                    in
                    match validate_user with
                    | Error s ->
                        let res =
                          "{\"status\": 400, \"success\": false, \"message\": \
                           \"" ^ s ^ "\"}"
                        in
                        Lwt.return (reply ~content_type:"application/json" res)
                    | Ok _ -> (
                        let user =
                          User_model.create_user ~name ~email ~password
                        in
                        Store.add_user !store user >>= function
                        | Ok store' ->
                            store := store';
                            let res =
                              "{\"status\": 200, \"success\": true, \
                               \"message\": {\"user\": "
                              ^ Yojson.Basic.to_string
                                  (User_model.user_to_json user)
                              ^ "}}"
                            in
                            Lwt.return
                              (reply ~content_type:"application/json" res)
                        | Error (`Msg msg) ->
                   let res =
                  "{\"status\": 400, \"success\": false, \"message\": \"Something went wrong. Wait a few seconds and try again.\"}"
                in
                Lwt.return (reply ~content_type:"application/json" res))))
            | _ ->
                let res =
                  "{\"status\": 400, \"success\": false, \"message\": \"Bad \
                   request method\"}"
                in
                Lwt.return (reply ~content_type:"application/json" res))
        | "/dashboard" ->
            Lwt.return
              (reply ~content_type:"text/html"
                 "This is going to be the Dashboard :-)")
        | "/unikernel/create" ->
            Lwt.return (reply ~content_type:"text/html" html)
        | path
          when String.(
                 length path >= 20 && sub path 0 20 = "/unikernel/shutdown/")
          -> (
            let unikernel_name = String.sub path 21 (String.length path - 21) in
            query_albatross stack credentials remote
              (`Unikernel_cmd `Unikernel_destroy) ~name:unikernel_name
            >|= function
            | Error () -> reply "error while querying albatross"
            | Ok None -> reply "got none"
            | Ok (Some data) -> (
                match decode_reply data with
                | Error () -> reply "couldn't decode albatross' reply"
                | Ok (hdr, res) ->
                    Logs.info (fun m ->
                        m "albatross returned: %a"
                          (Vmm_commands.pp_wire ~verbose:true)
                          (hdr, res));
                    reply_json (Albatross_json.res res)))
        | path
          when String.(
                 length path >= 19 && sub path 0 19 = "/unikernel/console/") ->
            let unikernel_name = String.sub path 20 (String.length path - 20) in
            (query_albatross stack credentials remote
               (`Console_cmd (`Console_subscribe (`Count 10)))
               ~name:unikernel_name
             >|= function
             | Error () -> ()
             | Ok None -> ()
             | Ok (Some data) -> (
                 match decode_reply data with
                 | Error () -> ()
                 | Ok (hdr, res) ->
                     Logs.info (fun m ->
                         m "albatross returned: %a"
                           (Vmm_commands.pp_wire ~verbose:true)
                           (hdr, res))))
            >>= fun () ->
            let data =
              Option.value ~default:[]
                (Map.find_opt unikernel_name !console_output)
            in
            reply_json (`List data);
            Lwt.return_unit
        | "/unikernel/deploy" -> (
            let response_body = Httpaf.Reqd.request_body reqd in
            let finished, notify_finished = Lwt.wait () in
            let wakeup v = Lwt.wakeup_later notify_finished v in
            let on_eof data () = wakeup data in
            let f acc s = acc ^ s in
            let rec on_read on_eof acc bs ~off ~len =
              let str = Bigstringaf.substring ~off ~len bs in
              let acc = acc >>= fun acc -> Lwt.return (f acc str) in
              Httpaf.Body.schedule_read response_body
                ~on_read:(on_read on_eof acc) ~on_eof:(on_eof acc)
            in
            let f_init = Lwt.return "" in
            Httpaf.Body.schedule_read response_body
              ~on_read:(on_read on_eof f_init) ~on_eof:(on_eof f_init);
            finished >>= fun data ->
            data >>= fun data ->
            let content_type =
              Httpaf.(
                Headers.get_exn (Reqd.request reqd).Request.headers
                  "content-type")
            in
            let ct =
              Multipart_form.Content_type.of_string (content_type ^ "\r\n")
            in
            match ct with
            | Error (`Msg msg) ->
                Logs.warn (fun m -> m "couldn't content-type: %s" msg);
                Lwt.return (reply "couldn't content-type")
            | Ok ct -> (
                match Multipart_form.of_string_to_list data ct with
                | Error (`Msg msg) ->
                    Logs.warn (fun m -> m "couldn't multipart: %s" msg);
                    Lwt.return (reply ("couldn't multipart: " ^ msg))
                | Ok (m, assoc) -> (
                    let m, _r = to_map ~assoc m in
                    match
                      ( Map.find_opt "arguments" m,
                        Map.find_opt "name" m,
                        Map.find_opt "binary" m )
                    with
                    | Some (_, args), Some (_, name), Some (_, binary) -> (
                        Logs.info (fun m -> m "args %s" args);
                        match Albatross_json.config_of_json args with
                        | Ok cfg -> (
                            let config =
                              { cfg with image = Cstruct.of_string binary }
                            in
                            query_albatross stack credentials remote ~name
                              (`Unikernel_cmd (`Unikernel_create config))
                            >|= function
                            | Error () ->
                                Logs.warn (fun m ->
                                    m "error querying albatross");
                                reply "error while querying albatross"
                            | Ok None ->
                                Logs.warn (fun m -> m "got none");
                                reply "got none"
                            | Ok (Some data) -> (
                                match decode_reply data with
                                | Error () ->
                                    Logs.warn (fun m ->
                                        m "couldn't decode albatross reply");
                                    reply "couldn't decode albatross' reply"
                                | Ok (hdr, res) ->
                                    Logs.info (fun m ->
                                        m "albatross returned: %a"
                                          (Vmm_commands.pp_wire ~verbose:true)
                                          (hdr, res));
                                    reply_json (Albatross_json.res res)))
                        | Error (`Msg msg) ->
                            Logs.warn (fun m -> m "couldn't decode data %s" msg);
                            Lwt.return
                              (reply ("couldn't decode data (of_json): " ^ msg))
                        )
                    | _ ->
                        Logs.warn (fun m -> m "couldn't find fields");
                        Lwt.return (reply "couldn't find fields"))))
        | _ ->
            Lwt.return
              (reply ~content_type:"text/plain"
                 "Error 404: this endpoint doesn't exist"))

  let pp_error ppf = function
    | #Httpaf.Status.t as code -> Httpaf.Status.pp_hum ppf code
    | `Exn exn -> Fmt.pf ppf "exception %s" (Printexc.to_string exn)

  let error_handler _dst ?request err _ =
    Logs.err (fun m ->
        m "error %a while processing request %a" pp_error err
          Fmt.(option ~none:(any "unknown") Httpaf.Request.pp_hum)
          request)

  let start _ _ _ _ stack data assets storage host port =
    js_contents assets >>= fun js_file ->
    css_contents assets >>= fun css_file ->
    images assets >>= fun imgs ->
    create_html_form assets >>= fun html ->
    retrieve_credentials data >>= fun credentials ->
    Store.Stored_data.connect storage >>= fun stored_data ->
    Store.read_data stored_data >>= function
    | Error (`Msg msg) -> failwith msg
    | Ok data ->
        let store = ref data in
        let remote = (host, port) in
        let port = 8080 in
        Logs.info (fun m ->
            m "Initialise an HTTP server (no HTTPS) on http://127.0.0.1:%u/"
              port);
        let request_handler _flow =
          request_handler stack credentials remote js_file css_file imgs html
            store
        in
        Paf.init ~port:8080 (S.tcp stack) >>= fun service ->
        let http = Paf.http_service ~error_handler request_handler in
        let (`Initialized th) = Paf.serve http service in
        th
end
