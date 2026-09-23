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

type mock_paf_flow = { flow : unit; mutable no_close : bool }

let make_default_policies ~domain ?(unikernels = 10) ?(memory = 1024) () =
  let path = Vmm_core.Name.Path.of_label domain in
  let name = Vmm_core.Name.make_of_path path in
  let root_policy : Vmm_core.Policy.t =
    {
      unikernels = unikernels * 2;
      cpuids = Vmm_core.IS.empty;
      memory = memory * 2;
      block = None;
      bridges = Vmm_core.String_set.empty;
    }
  in
  let policy : Vmm_core.Policy.t =
    {
      unikernels;
      cpuids = Vmm_core.IS.empty;
      memory;
      block = None;
      bridges = Vmm_core.String_set.empty;
    }
  in
  let trie =
    fst (Vmm_trie.insert Vmm_core.Name.root root_policy Vmm_trie.empty)
  in
  fst (Vmm_trie.insert name policy trie)

let make_app_request_handler ?policies store =
  let js_file = "/* some js */" in
  let css_file = "/* some css */" in
  let grafana_file = "{}" in
  let imgs : Unikernel.images =
    {
      molly_img = "molly";
      robur_img = "robur";
      albatross_img = "albatross";
      mirage_img = "mirage";
      dashboard_img = "dashboard";
    }
  in
  let management_domain = Domain_name.of_string_exn "robur.coop" in
  let success_config = Lwt_main.run Mock_albatross.success_config in
  let failure_config = Lwt_main.run Mock_albatross.failure_config in
  let policies = Option.value ~default:Vmm_trie.empty policies in
  let success_instance : Albatross.t =
    {
      configuration = success_config;
      policies;
      status = Albatross.Status.Online;
    }
  in
  let failure_instance : Albatross.t =
    {
      configuration = failure_config;
      policies;
      status = Albatross.Status.Online;
    }
  in
  let albatross_instances = ref App.Label_map.empty in
  albatross_instances :=
    App.Label_map.empty
    |> App.Label_map.add success_config.name success_instance
    |> App.Label_map.add failure_config.name failure_instance;
  let client_addr = (Ipaddr.of_string_exn "127.0.0.1", 8080) in
  let v4 = Ipaddr.V4.Prefix.global in
  let udp =
    Lwt_main.run
      (Udpv4v6_socket.connect ~ipv4_only:false ~ipv6_only:false v4 None)
  in
  let tcp =
    Lwt_main.run
      (Tcpv4v6_socket.connect ~ipv4_only:false ~ipv6_only:false v4 None)
  in
  let stack = Lwt_main.run (Tcpip_stack_socket.V4V6.connect udp tcp) in
  let happy_eyeballs = HE.create stack in
  let management_happy_eyeballs = happy_eyeballs in
  let http_client = Lwt_main.run (Client.connect Mimic.empty) in
  let flow_type : mock_paf_flow = { flow = (); no_close = true } in
  let flow : App.Paf.TCP.flow = Obj.magic flow_type in
  App.request_handler stack management_happy_eyeballs management_domain
    albatross_instances js_file css_file imgs grafana_file store http_client
    happy_eyeballs flow client_addr

(* this function query_endpoint is for in-memory http requests *)
let query_endpoint handler raw_request_str =
  let output_buffer = Buffer.create 1024 in
  let finished_promise, notify_finished = Lwt.wait () in
  let conn = H1.Server_connection.create handler in
  let bs =
    Bigstringaf.of_string ~off:0
      ~len:(String.length raw_request_str)
      raw_request_str
  in
  let _ =
    H1.Server_connection.read_eof conn bs ~off:0 ~len:(Bigstringaf.length bs)
  in
  let rec drain () =
    match H1.Server_connection.next_write_operation conn with
    | `Write iovecs ->
        List.iter
          (fun { H1.IOVec.buffer; off; len } ->
            let s = Bigstringaf.substring ~off ~len buffer in
            Buffer.add_string output_buffer s;
            H1.Server_connection.report_write_result conn (`Ok len))
          iovecs;
        if
          String.includes ~affix:"application/octet-stream"
            (Buffer.contents output_buffer)
        then drain ()
        else if Buffer.length output_buffer > 0 then (
          if Lwt.is_sleeping finished_promise then
            Lwt.wakeup_later notify_finished ())
        else drain ()
    | `Yield ->
        if
          String.includes ~affix:"t\n\r\n\rEOF\n\r"
            (Buffer.contents output_buffer)
        then (
          if Lwt.is_sleeping finished_promise then
            Lwt.wakeup_later notify_finished ())
        else H1.Server_connection.yield_writer conn (fun () -> drain ())
    | `Close _ | `Upgrade ->
        if Lwt.is_sleeping finished_promise then
          Lwt.wakeup_later notify_finished ()
  in
  drain ();
  finished_promise >>= fun () -> Lwt.return (Buffer.contents output_buffer)

let init_mock_store () =
  let block = Mock_Block.create () in
  App.Store.connect block >>= function
  | Error (`Msg msg) -> failwith msg
  | Ok store -> Lwt.return store

let add_user_csrf store (user : User_model.user) =
  let csrf = Test_utils.make_csrf_cookie user.uuid in
  let user = User_model.update_user user ~cookies:(csrf :: user.cookies) () in
  Storage.update_user store user;
  (user, csrf.value)

let add_user_token ?(name = "test-token") ?(expiry = 86400) store
    (user : User_model.user) =
  let token = Test_utils.make_mock_token ~name ~expiry () in
  let user = User_model.update_user user ~tokens:(token :: user.tokens) () in
  Storage.update_user store user;
  (user, token.value)

let setup_user ?name ?email ?(password = "Password123!") store =
  let name = Option.value ~default:"test" name in
  let email = Option.value ~default:"test@robur.coop" email in
  let handler = make_app_request_handler store in
  let body =
    Fmt.str
      {|{ "name": "%s", "email": "%s", "password": "%s", "form_csrf": "test-csrf" }|}
      name email password
  in
  let req =
    Test_utils.make_post_request ~path:"/api/register" ~body
      ~csrf_token:"test-csrf" ()
  in
  query_endpoint handler req >>= fun _raw_resp ->
  let name_lbl = Test_utils.label_of_string_exn name in
  let user = Option.get (Storage.find_by_name (Storage.users store) name_lbl) in
  let session_cookie = Test_utils.user_session_cookie user in
  let user, csrf_token = add_user_csrf store user in
  Lwt.return (user, session_cookie, csrf_token)

let setup_user_with_token ?name ?email ?password ?(token_name = "test-token")
    ?(expiry = 86400) store =
  setup_user ?name ?email ?password store
  >>= fun (user, _session_cookie, _csrf_token) ->
  let user, token_value = add_user_token ~name:token_name ~expiry store user in
  Lwt.return (user, token_value)
