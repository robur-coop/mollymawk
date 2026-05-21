(* mirage >= 4.9.0 & < 4.12.0 *)
open Mirage

let assets = crunch "assets"

let mollymawk =
  let packages =
    [
      package "logs";
      package "x509";
      package "tls-mirage";
      package ~min:"2.7.0" "albatross";
      package "yojson";
      package "uri";
      package "tyxml";
      package "multipart_form";
      package "mirage-crypto-rng";
      package "uuidm";
      package "emile";
      package ~sublibs:[ "emile" ] ~min:"0.12.0" ~max:"0.13.0" "colombe";
      package "sendmail";
      package ~sublibs:[ "mirage" ] ~min:"0.5.0" "paf";
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
    (stackv4v6 @-> stackv4v6 @-> dns_client @-> kv_ro @-> block @-> http_client
   @-> job @-> job)

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
let dhcp_requests = make_dhcp_requests ()

let management_stack, lease =
  generic_stackv4v6_with_lease ~group:"management" ~dhcp_requests
    (netif "management")

let management_eyeballs = generic_happy_eyeballs management_stack

let management_dns =
  generic_dns_client ~group:"management" management_stack management_eyeballs

let management_domain (dhcp_requests, lease) =
  let () =
    add_dhcp_request dhcp_requests 15
    (* DOMAIN_NAME *)
  in
  let connect _ _ = function
    | [ lease ] ->
        (* TODO: sane default instead of None?! *)
        code ~pos:__POS__
          "Lwt.return @@@@@ match %s with None -> None@ | Some lease -> \
           Dhcp_wire.find_domain_name lease"
          lease
    | _ -> assert false
  in
  impl ~connect ~extra_deps:[ dep lease ] "Dhcp_wire" job

let management_domain_name = management_domain (dhcp_requests, lease)

let http_client =
  let happy_eyeballs = mimic_happy_eyeballs stack eyeballs dns in
  http_client $ tcp $ happy_eyeballs

let () =
  register "mollymawk"
    [
      mollymawk $ stack $ management_stack $ management_dns $ assets $ block
      $ http_client $ management_domain_name;
    ]
