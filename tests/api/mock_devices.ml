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
