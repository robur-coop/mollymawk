(* mirage >= 4.7.0 & < 4.9.0 *)
open Mirage

let assets = crunch "assets"

let mollymawk =
  let packages =
    [
      package "logs";
      package "x509";
      package "tls-mirage";
      package ~min:"2.3.0" "albatross";
      package "yojson";
      package "uri";
      package "tyxml";
      package "multipart_form";
      package "mirage-crypto-rng";
      package "uuidm";
      package "emile";
      package "sendmail";
      package "paf" ~sublibs:[ "mirage" ] ~min:"0.5.0";
      package "oneffs";
      package "duration";
      package ~min:"0.2.0" "ohex";
      package "http-mirage-client";
      package "solo5-elftool" ~min:"0.4.0";
    ]
  in
  main ~packages "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4v6 @-> kv_ro @-> block
   @-> http_client @-> job)

let block = block_of_file "data"

let http_client =
  let connect _ modname = function
    | [ _pclock; _tcpv4v6; ctx ] ->
        code ~pos:__POS__ {ocaml|%s.connect %s|ocaml} modname ctx
    | _ -> assert false
  in
  impl ~connect "Http_mirage_client.Make"
    (pclock @-> tcpv4v6 @-> mimic @-> Mirage.http_client)

let stack = generic_stackv4v6 default_network
let eyeballs = generic_happy_eyeballs stack
let dns = generic_dns_client stack eyeballs
let tcp = tcpv4v6_of_stackv4v6 stack

let http_client =
  let happy_eyeballs = mimic_happy_eyeballs stack eyeballs dns in
  http_client $ default_posix_clock $ tcp $ happy_eyeballs

let () =
  register "mollymawk"
    [
      mollymawk $ default_random $ default_posix_clock $ default_monotonic_clock
      $ default_time $ stack $ assets $ block $ http_client;
    ]
