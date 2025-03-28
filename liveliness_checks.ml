open Lwt.Infix

let ( let* ) = Result.bind

type checks = [ `HTTP of string | `DNS of string * [ `host ] Domain_name.t ]

let timeout = 5 (* 5 seconds timeout for one check *)

module Make (S : Tcpip.Stack.V4V6) = struct
  module HE = Happy_eyeballs_mirage.Make (S)
  module Dns = Dns_client_mirage.Make (S) (HE)

  let check_dns stack dns_server domain_name =
    HE.connect_device stack >>= fun he ->
    Dns.connect
      ~nameservers:[ "tcp:" ^ dns_server; "udp:" ^ dns_server ]
      (stack, he)
    >>= fun dns_client ->
    Dns.gethostbyname dns_client domain_name >|= function
    | Error (`Msg err) ->
        Logs.err (fun m ->
            m "dns-liveliness-check: Error response of %s with error: %s"
              (Domain_name.to_string domain_name)
              err);
        Error
          (`Msg
             (Domain_name.to_string domain_name
             ^ " :an error occured while performing a liveliness check on the \
                DNS endpoint with error: " ^ err))
    | Ok _response -> Ok ()

  let check_http http_client base_url =
    Utils.send_http_request http_client ~base_url >|= function
    | Error (`Msg err) ->
        Logs.err (fun m ->
            m "http-liveliness-check: Error response of %s with error: %s"
              base_url err);

        Error
          (`Msg
             (base_url
            ^ " :an error occured while performing a liveliness check on the \
               http endpoint with error: " ^ err))
    | Ok _response -> Ok ()

  let check_type stack http_client typ =
    Lwt.pick
      [
        (Mirage_sleep.ns (Duration.of_sec timeout) >|= fun () -> `Timeout);
        (match typ with
        | `HTTP address -> check_http http_client address >|= fun r -> `Result r
        | `DNS (address, domain_name) ->
            check_dns stack address domain_name >|= fun r -> `Result r);
      ]

  let perform_checks stack http_client checks =
    Lwt_list.fold_left_s
      (fun failed check ->
        check_type stack http_client check >|= function
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
    >|= fun failed ->
    match failed with
    | [] -> Ok ()
    | _ ->
        let err_msg =
          "Liveliness checks failed for: "
          ^ String.concat ", " (List.rev failed)
        in
        Error (`Msg err_msg)

  let parse_domain_name name =
    match Domain_name.of_string name with
    | Ok name -> (
        match Domain_name.host name with
        | Ok host -> Some host
        | Error _ -> None)
    | Error _ -> None

  let dns_liveliness_of_json json =
    match json with
    | `Assoc r -> (
        match Utils.Json.(get "dns_address" r, get "dns_name" r) with
        | Some dns_address, Some dns_name -> Some (dns_address, dns_name)
        | _ -> None)
    | _ -> None

  let dns_liveliness_or_none = function
    | None | Some `Null -> None
    | Some json -> dns_liveliness_of_json json

  let prepare_liveliness_parameters ~http_liveliness_address ~dns_liveliness =
    let* http_liveliness_address =
      Utils.Json.string_or_none "http_liveliness_address"
        http_liveliness_address
    in
    let dns_address, dns_name =
      match dns_liveliness_or_none dns_liveliness with
      | Some (address, name) -> (Some address, Some name)
      | None -> (None, None)
    in
    let* dns_liveliness_address =
      Utils.Json.string_or_none "dns_address" dns_address
    in
    let* dns_liveliness_name = Utils.Json.string_or_none "dns_name" dns_name in
    let validated_checks =
      List.filter_map
        (fun kind ->
          match kind with
          | `HTTP addr -> (
              match addr with Some addr -> Some (`HTTP addr) | None -> None)
          | `DNS (address, domain_name) -> (
              match (address, domain_name) with
              | Some address, Some domain_name -> (
                  match parse_domain_name domain_name with
                  | Some name -> Some (`DNS (address, name))
                  | _ -> None)
              | _ -> None))
        [
          `HTTP http_liveliness_address;
          `DNS (dns_liveliness_address, dns_liveliness_name);
        ]
    in
    Ok validated_checks

  let liveliness_checks ~http_liveliness_address ~dns_liveliness stack
      http_client =
    match
      prepare_liveliness_parameters ~http_liveliness_address ~dns_liveliness
    with
    | Error (`Msg msg) -> Lwt.return (Error (`Msg msg))
    | Ok checks -> (
        perform_checks stack http_client checks >|= function
        | Ok () ->
            Logs.info (fun m -> m "All liveliness checks passed.");
            Ok ()
        | Error (`Msg err) ->
            Logs.err (fun m -> m "liveliness-checks-failed with error %s" err);
            Error (`Msg err))

  let interval_liveliness_checks ~unikernel_name ~http_liveliness_address
      ~dns_liveliness stack http_client =
    match
      prepare_liveliness_parameters ~http_liveliness_address ~dns_liveliness
    with
    | Ok checks ->
        let rec aux_check failed_checks delay_intervals =
          match delay_intervals with
          | [] ->
              (match failed_checks with
              | [] -> Ok ()
              | _ ->
                  let err_msg =
                    "Liveliness checks failed with error(s): "
                    ^ String.concat ", " (List.rev failed_checks)
                  in
                  Logs.info (fun m ->
                      m "liveliness-checks for %s: %s" unikernel_name err_msg);
                  Error (`Msg err_msg))
              |> Lwt.return
          | delay :: rest -> (
              Mirage_sleep.ns (Duration.of_sec delay) >>= fun () ->
              Logs.info (fun m ->
                  m "Performing liveliness check of %s after %d second delay."
                    unikernel_name delay);
              perform_checks stack http_client checks >>= function
              | Error (`Msg err) -> aux_check (err :: failed_checks) rest
              | Ok () -> Ok () |> Lwt.return)
        in
        aux_check [] [ 1; 1; 1; 2; 3; 3; 4; 5 ]
    | Error (`Msg err) -> Error (`Msg err) |> Lwt.return
end
