let ( let* ) = Result.bind

type checks = [ `HTTP of string | `DNS of string * [ `host ] Domain_name.t ]

let timeout = 5 (* 5 seconds timeout for one check *)

let check_dns ~udp ~happy_eyeballs dns_server domain_name =
  match Ipaddr.of_string dns_server with
  | Error (`Msg err) ->
      Logs.err (fun m ->
          m "dns-liveliness-check: Invalid IP %s: %s" dns_server err);
      Error (`Msg ("Invalid DNS server IP: " ^ dns_server))
  | Ok ip ->
      let nameservers = (`Udp, [ `Plaintext (ip, 53) ]) in
      let stack = Mnet_dns.Transport.stack udp happy_eyeballs in
      let dns = Mnet_dns.create ~nameservers ~timeout:5_000_000_000L stack in
      let daemon = Mnet_dns.transport dns in
      Fun.protect
        ~finally:(fun () -> Mnet_dns.Transport.kill daemon)
        (fun () ->
          match Mnet_dns.gethostbyname dns domain_name with
          | Error (`Msg err) ->
              Logs.err (fun m ->
                  m "dns-liveliness-check: Error response of %s with error: %s"
                    (Domain_name.to_string domain_name)
                    err);
              Error
                (`Msg
                   (Domain_name.to_string domain_name
                   ^ " :an error occurred while performing a liveliness check \
                      on the DNS endpoint with error: " ^ err))
          | Ok _response -> Ok ())

let check_http ~happy_eyeballs base_url =
  match Builder_web.send_http_request ~base_url ~happy_eyeballs () with
  | Error (`Msg err) ->
      Logs.err (fun m ->
          m "http-liveliness-check: Error response of %s with error: %s"
            base_url err);
      Error
        (`Msg
           (base_url
          ^ " :an error occurred while performing a liveliness check on the \
             http endpoint with error: " ^ err))
  | Error `Not_found ->
      Logs.err (fun m ->
          m "http-liveliness-check: Endpoint %s returned 404 Not Found" base_url);
      Error (`Msg (base_url ^ ": endpoint returned 404 Not Found"))
  | Ok _response -> Ok ()

let check_type ?(timeout = timeout) ~udp ~happy_eyeballs typ =
  let prm_timeout =
    Miou.async @@ fun () ->
    Mkernel.sleep (timeout * 1_000_000_000);
    `Timeout
  in
  let prm_check =
    Miou.async @@ fun () ->
    match typ with
    | `HTTP address -> `Result (check_http ~happy_eyeballs address)
    | `DNS (address, domain_name) ->
        `Result (check_dns ~udp ~happy_eyeballs address domain_name)
  in
  match Miou.await_first [ prm_timeout; prm_check ] with
  | Ok res -> res
  | Error exn -> `Result (Error (`Msg (Printexc.to_string exn)))

let perform_checks ~udp ~happy_eyeballs checks =
  let failed =
    List.fold_left
      (fun failed check ->
        match check_type ~udp ~happy_eyeballs check with
        | `Result (Ok ()) -> failed
        | `Result (Error (`Msg err)) ->
            let failed_desc =
              match check with
              | `HTTP url -> "HTTP (" ^ url ^ "): " ^ err
              | `DNS (address, domain_name) ->
                  "DNS ("
                  ^ Domain_name.to_string domain_name
                  ^ ", " ^ address ^ "): " ^ err
            in
            failed_desc :: failed
        | `Timeout ->
            let failed_desc =
              match check with
              | `HTTP url -> "HTTP (" ^ url ^ "): timeout"
              | `DNS (address, domain_name) ->
                  "DNS ("
                  ^ Domain_name.to_string domain_name
                  ^ ", " ^ address ^ "): timeout"
            in
            failed_desc :: failed)
      [] checks
  in
  match failed with
  | [] -> Ok ()
  | _ ->
      let err_msg =
        "Liveliness checks failed for: " ^ String.concat ", " (List.rev failed)
      in
      Error (`Msg err_msg)

let parse_domain_name name =
  match Domain_name.of_string name with
  | Ok name -> (
      match Domain_name.host name with
      | Ok host -> Ok host
      | Error err -> Error err)
  | Error (`Msg err) ->
      Logs.err (fun m ->
          m "liveliness-checks-failed while parsing the host name with error %s"
            err);
      Error (`Msg err)

let prepare_liveliness_parameters ~http_liveliness_address
    ~(dns_liveliness : Models.Unikernel.dns_liveliness option) =
  let http_check =
    match http_liveliness_address with
    | Some addr -> Ok (Some (`HTTP addr))
    | None -> Ok None
  in
  let dns_check =
    match dns_liveliness with
    | Some { dns_address = Some address; dns_name = Some domain_name } -> (
        match parse_domain_name domain_name with
        | Ok name -> Ok (Some (`DNS (address, name)))
        | Error (`Msg err) -> Error (`Msg ("Invalid DNS name: " ^ err)))
    | _ -> Ok None
  in
  let* validated_checks =
    match (http_check, dns_check) with
    | Ok None, Ok None -> Ok []
    | Ok (Some http), Ok None -> Ok [ http ]
    | Ok None, Ok (Some dns) -> Ok [ dns ]
    | Ok (Some http), Ok (Some dns) -> Ok [ http; dns ]
    | Error (`Msg e), _ -> Error (`Msg e)
    | _, Error (`Msg e) -> Error (`Msg e)
  in
  Ok validated_checks

let liveliness_checks ~http_liveliness_address ~dns_liveliness ~udp
    ~happy_eyeballs =
  match
    prepare_liveliness_parameters ~http_liveliness_address ~dns_liveliness
  with
  | Error (`Msg msg) -> Error (`Msg msg)
  | Ok checks -> (
      match perform_checks ~udp ~happy_eyeballs checks with
      | Ok () ->
          Logs.info (fun m -> m "All liveliness checks passed.");
          Ok ()
      | Error (`Msg err) ->
          Logs.err (fun m -> m "liveliness-checks-failed with error %s" err);
          Error (`Msg err))

let interval_liveliness_checks ~unikernel_name ~http_liveliness_address
    ~dns_liveliness ~udp ~happy_eyeballs =
  let unikernel_name = Models.Configuration.name_to_str unikernel_name in
  match
    prepare_liveliness_parameters ~http_liveliness_address ~dns_liveliness
  with
  | Ok checks ->
      let rec aux_check failed_checks delay_intervals =
        match delay_intervals with
        | [] -> (
            match failed_checks with
            | [] -> Ok ()
            | _ ->
                let err_msg =
                  "Liveliness checks failed with error(s): "
                  ^ String.concat ", " (List.rev failed_checks)
                in
                Logs.info (fun m ->
                    m "liveliness-checks for %s: %s" unikernel_name err_msg);
                Error (`Msg err_msg))
        | delay :: rest -> (
            Mkernel.sleep (delay * 1_000_000_000);
            Logs.info (fun m ->
                m "Performing liveliness check of %s after %d second delay."
                  unikernel_name delay);
            match perform_checks ~udp ~happy_eyeballs checks with
            | Error (`Msg err) -> aux_check (err :: failed_checks) rest
            | Ok () -> Ok ())
      in
      aux_check [] [ 1; 1; 1; 2; 3; 3; 4; 5 ]
  | Error (`Msg err) -> Error (`Msg err)
