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

let make_app_request_handler store =
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
  let config = Lwt_main.run Mock_albatross.albatross_config in
  let default_policy =
    Vmm_core.Policy.
      {
        unikernels = 10;
        cpuids = Vmm_core.IS.of_list [ 0; 1; 2; 3 ];
        memory = 1024;
        block = Some 10240;
        bridges = Vmm_core.String_set.of_list [ "service" ];
      }
  in
  let root_path = Vmm_core.Name.root in
  let admin_path =
    Vmm_core.Name.make_of_path
      (Vmm_core.Name.Path.of_label (Test_utils.label_of_string_exn "admin"))
  in
  let user_path =
    Vmm_core.Name.make_of_path
      (Vmm_core.Name.Path.of_label (Test_utils.label_of_string_exn "user"))
  in
  let policies =
    Vmm_trie.empty
    |> Vmm_trie.insert root_path default_policy
    |> fst
    |> Vmm_trie.insert admin_path default_policy
    |> fst
    |> Vmm_trie.insert user_path default_policy
    |> fst
  in
  let instance : Albatross.t =
    { configuration = config; policies; status = Albatross.Status.Online }
  in
  let albatross_instances = ref App.Label_map.empty in
  albatross_instances := App.Label_map.singleton config.name instance;
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
  let flow : App.Paf.TCP.flow = Obj.magic () in
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
        if Buffer.length output_buffer > 0 then (
          if Lwt.is_sleeping finished_promise then
            Lwt.wakeup_later notify_finished ())
        else drain ()
    | `Yield -> H1.Server_connection.yield_writer conn (fun () -> drain ())
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
