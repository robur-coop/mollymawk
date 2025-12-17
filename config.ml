(* mirage >= 4.9.0 & < 4.11.0 *)
open Mirage

let assets = crunch "assets"

let mollymawk =
  let packages =
    [
      package "logs";
      package "x509";
      package "tls-mirage";
      package ~min:"2.6.1" "albatross";
      package "yojson";
      package "uri";
      package "tyxml";
      package "multipart_form";
      package "mirage-crypto-rng";
      package "uuidm";
      package "emile";
      package ~sublibs:[ "emile" ] "colombe" ~min:"0.12.0";
      package "sendmail";
      package "paf" ~sublibs:[ "mirage" ] ~min:"0.5.0";
      package "oneffs";
      package "duration";
      package ~min:"0.2.0" "ohex";
      package "http-mirage-client";
      package "multipart_form-lwt";
      package "sendmail-mirage";
      package "dns-client-mirage";
      package "mrmime";
    ]
  in
  main ~packages "Unikernel.Main"
    (stackv4v6 @-> kv_ro @-> block @-> http_client @-> job)

let block = block_of_file "data"

let http_client =
  let connect _ modname = function
    | [ _tcpv4v6; ctx ] ->
        code ~pos:__POS__ {ocaml|%s.connect %s|ocaml} modname ctx
    | _ -> assert false
  in
  impl ~connect "Http_mirage_client.Make"
    (tcpv4v6 @-> mimic @-> Mirage.http_client)

let stack = generic_stackv4v6 default_network
let eyeballs = generic_happy_eyeballs stack
let dns = generic_dns_client stack eyeballs
let tcp = tcpv4v6_of_stackv4v6 stack

let http_client =
  let happy_eyeballs = mimic_happy_eyeballs stack eyeballs dns in
  http_client $ tcp $ happy_eyeballs

let () =
  register "mollymawk" [ mollymawk $ stack $ assets $ block $ http_client ]
