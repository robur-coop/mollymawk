(* This code is adapted from https://github.com/robur-coop/albatross/blob/main/tls/vmm_tls_lwt.ml, originally written by Hannes Mehnert *)

let read_tls_chunk t =
  let rec r_n buf off tot =
    let len = tot - off in
    if len = 0 then Ok ()
    else
      match Mnet_tls.read t ~off ~len buf with
      | 0 ->
          Logs.debug (fun m -> m "TLS: end of file");
          Error `Tls_eof
      | x when Int.equal x len -> Ok ()
      | x when x < len -> r_n buf (off + x) tot
      | _ ->
          Logs.err (fun m -> m "TLS: read too much, shouldn't happen");
          Error `Toomuch
      | exception Mnet_tls.Closed_by_peer ->
          Logs.debug (fun m -> m "TLS: closed by peer");
          Error `Tls_eof
      | exception Mnet_tls.Tls_alert alert ->
          Logs.err (fun m ->
              m "TLS read alert: %s" (Tls.Packet.alert_type_to_string alert));
          Error `Exception
      | exception Mnet_tls.Tls_failure failure ->
          Logs.err (fun m ->
              m "TLS read failure: %s" (Tls.Engine.string_of_failure failure));
          Error `Exception
      | exception exn ->
          Logs.err (fun m ->
              m "TLS read exception: %s" (Printexc.to_string exn));
          Error `Exception
  in
  let buf = Bytes.create 4 in
  match r_n buf 0 4 with
  | Error e -> Error e
  | Ok () ->
      let len = Bytes.get_int32_be buf 0 in
      if len > 0l then
        let b = Bytes.create (Int32.to_int len) in
        match r_n b 0 (Int32.to_int len) with
        | Error e -> Error e
        | Ok () -> Ok (Bytes.unsafe_to_string b)
      else Error `Eof

let read_tls t =
  match read_tls_chunk t with
  | Error _ as e -> e
  | Ok data -> (
      match Vmm_asn.wire_of_str data with
      | Error (`Msg msg) ->
          Logs.err (fun m -> m "error %s while parsing data" msg);
          Error `Exception
      | Ok (hdr, _) as w ->
          if not Vmm_commands.(is_current hdr.version) then
            Logs.warn (fun m ->
                m "version mismatch, received %a current %a"
                  Vmm_commands.pp_version hdr.Vmm_commands.version
                  Vmm_commands.pp_version Vmm_commands.current);
          w)

let write_tls_chunk s data =
  let dlen = Bytes.create 4 in
  Bytes.set_int32_be dlen 0 (Int32.of_int (String.length data));
  let buf = Bytes.unsafe_to_string dlen ^ data in
  try
    Mnet_tls.write s buf;
    Ok ()
  with
  | Mnet_tls.Closed_by_peer ->
      Logs.err (fun m -> m "TLS write: closed by peer");
      Error `Exception
  | Mnet_tls.Tls_alert alert ->
      Logs.err (fun m ->
          m "TLS write alert: %s" (Tls.Packet.alert_type_to_string alert));
      Error `Exception
  | Mnet_tls.Tls_failure failure ->
      Logs.err (fun m ->
          m "TLS write failure: %s" (Tls.Engine.string_of_failure failure));
      Error `Exception
  | exn ->
      Logs.err (fun m -> m "TLS write exception: %s" (Printexc.to_string exn));
      Error `Exception

let write_tls s wire =
  let data = Vmm_asn.wire_to_str wire in
  write_tls_chunk s data

let close tls =
  try Mnet_tls.close tls
  with
  | Mnet_tls.Closed_by_peer | Mnet_tls.Tls_alert _ | Mnet_tls.Tls_failure _ | _
  ->
    ()
