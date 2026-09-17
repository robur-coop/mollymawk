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