open Mirage

let data = crunch "keys"
let js = crunch "assets"

let mollymawk =
  let packages =
    [
      package "logs" ;
      package "x509" ;
      package "tls-mirage" ;
      package "albatross" ;
      package "yojson";
      package "uri";
      package "tyxml";
      package "multipart_form";
      package "paf" ~sublibs:[ "mirage" ] ~min:"0.5.0" ;
    ]
  and runtime_args =
    [
      runtime_arg ~pos:__POS__ "Unikernel.K.albatross_server";
      runtime_arg ~pos:__POS__ "Unikernel.K.port";
    ]
  in
  main
    ~runtime_args ~packages "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4v6 @-> kv_ro @-> kv_ro @-> job)

let () =
  register "mollymawk"
    [mollymawk $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ generic_stackv4v6 default_network $ data $ js]
