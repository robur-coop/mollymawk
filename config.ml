(* (c) 2022 Hannes Mehnert, all rights reserved *)

open Mirage

let data = crunch "keys"

let albatross_server =
  let doc = Key.Arg.info ~doc:"albatross server IP" ["albatross-server"] in
  Key.(create "albatross-server" Arg.(required ip_address doc))

let port =
  let doc = Key.Arg.info ~doc:"server port" ["port"] in
  Key.(create "port" Arg.(opt int 1025 doc))


let mollymawk =
  let packages =
    [
      package "logs" ;
      package "x509" ;
      package "tls-mirage" ;
      package "albatross" ;
    ]
  and keys = Key.([ v albatross_server ; v port ])
  in
  foreign
    ~keys
    ~packages
    "Unikernel.Main" (random @-> pclock @-> mclock @-> time @-> stackv4v6 @-> kv_ro @-> job)

let () =
  register "mollymawk" [mollymawk $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ generic_stackv4v6 default_network $ data]
