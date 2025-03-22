open Lwt.Infix

let ( let* ) = Result.bind

type checks = [ `HTTP of string | `TCP of string | `DNS of string ]

let check_http http_client base_url =
  Utils.send_request http_client ~base_url >>= function
  | Error (`Msg err) ->
      Logs.err (fun m ->
          m "http-liveliness-check: Error response of %s with error: %s"
            base_url err);
      Lwt.return
        (Error
           (`Msg
              (base_url
             ^ " :an error occured while performing a liveliness check on the \
                http endpoint with error: " ^ err)))
  | Ok _response -> Ok () |> Lwt.return

let check_type http_client = function
  | `HTTP address -> check_http http_client address
  | `TCP address ->
      Error
        (`Msg (address ^ ": Can't handle TCP liveliness checks at the moment"))
      |> Lwt.return
  | `DNS address ->
      Error
        (`Msg (address ^ ": Can't handle DNS liveliness checks at the moment"))
      |> Lwt.return

let rec perform_checks http_client failed = function
  | [] -> (
      match failed with
      | [] -> Lwt.return (Ok ())
      | _ ->
          let err_msg =
            "Liveliness checks failed for: "
            ^ String.concat ", " (List.rev failed)
          in
          Lwt.return (Error (`Msg err_msg)))
  | check :: rest -> (
      check_type http_client check >>= function
      | Ok _ -> perform_checks http_client failed rest
      | Error (`Msg err) ->
          let failed_desc =
            match check with
            | `HTTP url -> "HTTP (" ^ url ^ "): " ^ err
            | `TCP addr -> "TCP (" ^ addr ^ "): " ^ err
            | `DNS domain -> "DNS (" ^ domain ^ "): " ^ err
          in
          perform_checks http_client (failed_desc :: failed) rest)

let liveliness_checks ~http_liveliness_address ~dns_liveliness_address
    ~tcp_liveliness_address http_client =
  Lwt.return
    (let* http_liveliness_address =
       Utils.Json.string_or_none "http_liveliness_address"
         http_liveliness_address
     in
     let* tcp_liveliness_address =
       Utils.Json.string_or_none "tcp_liveliness_address" tcp_liveliness_address
     in
     let* dns_liveliness_address =
       Utils.Json.string_or_none "dns_liveliness_address" dns_liveliness_address
     in
     let validated_checks =
       List.filter_map
         (fun (constructor, value) ->
           match value with
           | Some addr -> Some (constructor addr)
           | None -> None)
         [
           ((fun addr -> `HTTP addr), http_liveliness_address);
           ((fun addr -> `TCP addr), tcp_liveliness_address);
           ((fun addr -> `DNS addr), dns_liveliness_address);
         ]
     in
     Ok validated_checks)
  >>= function
  | Error (`Msg msg) -> Lwt.return (Error (`Msg msg))
  | Ok checks -> (
      perform_checks http_client [] checks >>= function
      | Ok () ->
          Logs.info (fun m -> m "All liveliness checks passed.");
          Lwt.return (Ok ())
      | Error (`Msg err) ->
          Logs.err (fun m -> m "liveliness-checks-failed with error %s" err);
          Lwt.return (Error (`Msg err)))
