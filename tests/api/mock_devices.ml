open Lwt.Infix

module Mock_KV : Mirage_kv.RO = struct
  type t = unit
  type error = Mirage_kv.error
  type key = Mirage_kv.Key.t

  let pp_error = Mirage_kv.pp_error
  let disconnect _ = Lwt.return_unit
  let exists _ _ = Lwt.return (Ok (Some `Value))
  let get _ _ = Lwt.return (Ok "dummy")
  let list _ _ = Lwt.return (Ok [])
  let size _ _ = Lwt.return (Ok (Optint.Int63.of_int 10))
  let digest _ _ = Lwt.return (Ok "hash")
  let last_modified _ _ = Lwt.return (Ok Ptime.epoch)
  let get_partial _ _ ~offset:_ ~length:_ = Lwt.return (Ok "dummy")
end

module Mock_Block : sig
  include Mirage_block.S

  val create : unit -> t
end = struct
  type t = unit
  type error = Mirage_block.error
  type write_error = Mirage_block.write_error

  let pp_error = Mirage_block.pp_error
  let pp_write_error = Mirage_block.pp_write_error

  let get_info () =
    Lwt.return
      {
        Mirage_block.read_write = true;
        sector_size = 512;
        size_sectors = 4096L;
      }

  let disconnect () = Lwt.return_unit
  let read () _sector_start _bufs = Lwt.return (Ok ())
  let write () _sector_start _bufs = Lwt.return (Ok ())
  let create () = ()
end

module HE = Happy_eyeballs_mirage.Make (Tcpip_stack_socket.V4V6)
module DNS = Dns_client_mirage.Make (Tcpip_stack_socket.V4V6) (HE)
module Mimic_HE = Mimic_happy_eyeballs.Make (Tcpip_stack_socket.V4V6) (HE) (DNS)
module Client = Http_mirage_client.Make (Tcpip_stack_socket.V4V6.TCP) (Mimic_HE)

module App =
  Unikernel.Main (Tcpip_stack_socket.V4V6) (Tcpip_stack_socket.V4V6) (DNS)
    (Mock_KV)
    (Mock_Block)
    (Client)

(* this function query_endpoint is for in-memory http requests *)
let query_endpoint handler raw_request_str =
  let output_buffer = Buffer.create 1024 in
  let finished_promise, notify_finished = Lwt.wait () in
  let request_handler reqd =
    Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            handler reqd >>= fun () ->
            Lwt.wakeup_later notify_finished ();
            Lwt.return_unit)
          (fun exn ->
            Logs.err (fun m ->
                m "Handler exception: %s" (Printexc.to_string exn));
            Lwt.wakeup_later_exn notify_finished exn;
            Lwt.return_unit))
  in
  let conn = H1.Server_connection.create request_handler in
  let bs =
    Bigstringaf.of_string ~off:0
      ~len:(String.length raw_request_str)
      raw_request_str
  in
  let _ =
    H1.Server_connection.read_eof conn bs ~off:0 ~len:(Bigstringaf.length bs)
  in
  finished_promise >>= fun () ->
  let rec drain () =
    match H1.Server_connection.next_write_operation conn with
    | `Write iovecs ->
        List.iter
          (fun { H1.IOVec.buffer; off; len } ->
            let s = Bigstringaf.substring ~off ~len buffer in
            Buffer.add_string output_buffer s;
            H1.Server_connection.report_write_result conn (`Ok len))
          iovecs;
        drain ()
    | `Yield | `Upgrade | `Close _ -> ()
  in
  drain ();
  Lwt.return (Buffer.contents output_buffer)

let init_mock_store () =
  let block = Mock_Block.create () in
  App.Store.connect block >>= function
  | Error (`Msg msg) -> failwith msg
  | Ok store -> Lwt.return store
