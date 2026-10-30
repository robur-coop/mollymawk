module Mkernel = struct
  include Mkernel

  module Block = struct
    include Mkernel.Block

    let sector_size _ = 512
    let length _ = 64 * 1024 * 1024
    let atomic_read _ ~src_off:_ ?dst_off:_ _ = ()
    let atomic_write _ ?src_off:_ ~dst_off:_ _ = ()
  end
end

let rng = Mirage_crypto_rng_unix.use_default ()

(* this dump was gotten from a locally running instance of mollymawk. in store.ml, the function read_disk outputs a string. this is the string that is contained in the value raw_dump below. *)
let raw_dump =
  {|{
    "version": 10,
    "users": [
      {
        "name": "user",
        "email": "user@robur.net",
        "email_verified": null,
        "password": "vn2hllXeTYpW2n9+gPf2Bnal6T2qms8acPQHb+0=",
        "uuid": "1d0a80e3-323b-4b45-9efd-bad03762e87f",
        "tokens": [],
        "cookies": [
          {
            "name": "molly_csrf",
            "created_at": "2026-04-13 19:29:36-00:00",
            "value": "OGZkZjg4NDYtZGQyNS00MjFjLTg5NGQtZjk3OTAwODllMWM3",
            "expires_in": 3600,
            "uuid": "f4c766d0-a9df-4bfd-986e-1b3cdc5a9414",
            "last_access": "2026-04-13 19:29:36-00:00",
            "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
          },
          {
            "name": "molly_session",
            "created_at": "2025-12-28 18:18:24-00:00",
            "value": "MDQwZmNiZmMtOGM3MC00OTM2LTlmMjctOTExMDQwZGM3MDNl",
            "expires_in": 604800,
            "uuid": "1d0a80e3-323b-4b45-9efd-bad03762e87f",
            "last_access": "2025-12-28 18:18:24-00:00",
            "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
          }
        ],
        "created_at": "2026-04-13 19:31:50-00:00",
        "updated_at": "2026-04-13 19:29:58-00:00",
        "email_verification_uuid": "fbf1fd08-0a19-464e-a7bc-e79d1d5f7fc7",
        "active": true,
        "super_user": false,
        "unikernel_updates": [],
        "scaling_policies": []
      },
      {
        "name": "user2",
        "email": "user2@robur.coop",
        "email_verified": "2025-12-28 13:34:10-00:00",
        "password": "WomWAonZcOsQ9ueSRwFbT7nvQAtcsfNFl/o7T66=",
        "uuid": "28538638-0136-43e1-9146-bf7649917a33",
        "tokens": [],
        "cookies": [
          {
            "name": "molly_csrf",
            "created_at": "2025-12-28 13:38:45-00:00",
            "value": "MWIyYWNiNzctMzUxYS00NWU0LTgzZjctOTUxODhjNmM5NGUx",
            "expires_in": 3600,
            "uuid": "aeb59d61-7992-42c6-8f64-91f1cfbfe476",
            "last_access": "2025-12-28 13:38:45-00:00",
            "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
          },
          {
            "name": "molly_session",
            "created_at": "2025-12-28 13:33:06-00:00",
            "value": "MzNjYTM1ZGItYWVkYS00YTBkLThmMTctMDBiNWM3MWY3MDg2",
            "expires_in": 604800,
            "uuid": "28538638-0136-43e1-9146-bf7649917a33",
            "last_access": "2025-12-28 13:33:06-00:00",
            "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
          }
        ],
        "created_at": "2025-12-28 13:34:17-00:00",
        "updated_at": "2025-12-28 13:38:45-00:00",
        "email_verification_uuid": "56245e21-0201-47fd-b7f3-5cbee960c6af",
        "active": true,
        "super_user": false,
        "unikernel_updates": [],
        "scaling_policies": []
      },
      {
        "name": "user3",
        "email": "user3@robur.coop",
        "email_verified": "2025-12-28 12:35:08-00:00",
        "password": "eVHL6CIc5phEoElSOACRkCFZD68vzHAt9StFI=",
        "uuid": "1f42ba42-5458-479b-8095-8450afde8d6b",
        "tokens": [
          {
            "token_type": "Bearer",
            "value": "3cb6bc72-a52d-4f4f-969c-c85465875774",
            "expires_in": 800000,
            "created_at": "2025-12-09 05:12:11-00:00",
            "last_access": "2025-12-09 05:12:11-00:00",
            "name": "updated-gitlab-token",
            "usage_count": 0
          },
          {
            "token_type": "Bearer",
            "value": "6bc27967-30cd-4090-af8f-3997c47c25e1",
            "expires_in": 2419200,
            "created_at": "2025-12-05 04:14:38-00:00",
            "last_access": "2025-12-05 04:14:38-00:00",
            "name": "fobud",
            "usage_count": 68
          }
        ],
        "cookies": [
          {
            "name": "molly_csrf",
            "created_at": "2026-08-25 23:10:15-00:00",
            "value": "NjgzOTI1ZDEtYTgyMy00MDY4LTlkMzgtZWY5YjRjYjNjNzcz",
            "expires_in": 3600,
            "uuid": "c0c0529c-e905-437a-82f9-4ae3be68856a",
            "last_access": "2026-08-25 23:10:15-00:00",
            "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
          },
          {
            "name": "molly_session",
            "created_at": "2026-08-25 23:09:50-00:00",
            "value": "YzhlZmUyOGMtODQ0Mi00ZGNhLWI1NmYtM2FiMDNkMDQ1ODI5",
            "expires_in": 604800,
            "uuid": "1f42ba42-5458-479b-8095-8450afde8d6b",
            "last_access": "2026-08-25 23:09:50-00:00",
            "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
          }
        ],
        "created_at": "2026-07-28 07:20:46-00:00",
        "updated_at": "2026-08-25 23:10:15-00:00",
        "email_verification_uuid": "3e49c0d0-8c1c-4ed7-9763-355d73f62257",
        "active": true,
        "super_user": true,
        "unikernel_updates": [
          {
            "name": "hello",
            "job": "hello",
            "uuid": "6cc1a49d-eb1e-4f46-8471-6452206d1475",
            "config": {
              "typ": "solo5",
              "compressed": false,
              "fail_behaviour": {
                "restart": null,
                "exit_code": [],
                "all_exit_codes": false
              },
              "cpuids": [
                0
              ],
              "memory": 32,
              "block_devices": [],
              "network_interfaces": [],
              "arguments": [],
              "numcpus": 1,
              "linux_boot_partition": null
            },
            "timestamp": "2026-05-01 18:14:22-00:00"
          },
          {
            "name": "hello-4",
            "job": "hello",
            "uuid": "",
            "config": {
              "typ": "solo5",
              "compressed": false,
              "fail_behaviour": {
                "restart": null,
                "exit_code": [],
                "all_exit_codes": false
              },
              "cpuids": [
                0
              ],
              "memory": 1,
              "block_devices": [],
              "network_interfaces": [],
              "arguments": [],
              "numcpus": 1,
              "linux_boot_partition": null
            },
            "timestamp": "2025-12-08 17:58:24-00:00"
          }
        ],
        "scaling_policies": [
          {
            "name": "hello",
            "primary_albatross_instance": "local-server",
            "max_instances": 3
          }
        ]
      }
    ],
    "configuration": [
      {
        "name": "local-server",
        "certificate": "-----BEGIN CERTIFICATE-----\nMIH1MIGooAMCAQICCh4RFZsMHQSV7UMwBQYDK2VwMA0xCzAJBgNVBAMMAmNhMB4X\nDTI1MTIwNTEzMTk0MVoXDTM1MTIwMzEzMTk1MVowDTELMAkGA1UEAwwCY2EwKjAF\nBgMrZXADIQCD6DablGzl/JEQqieuP0B4CWhVlmnLoe5c777q8cOCr6MkMCIwDwYD\nVR0PAQH/BAUDAwfGADAPBgNVHRMBAf8EBTADAQH/MAUGAytlcANBAHNte6yJzB5f\nNkq0L4w7rHhUEIWg1igHciNMXLzJTrxxpm5RxDFmnS3yEhARmMVd2N5h6N/K1200\nZDgtaQRpDQw=\n-----END CERTIFICATE-----\n",
        "private_key": "-----BEGIN PRIVATE KEY-----\nMC4CAQAwBQYDK2VwBCIEIPa9DMmlMckBoVrWvcxB3yq1dQHe9oGhFSLShZA18vvb\n-----END PRIVATE KEY-----\n",
        "server_ip": "10.0.0.1",
        "server_port": 1025,
        "updated_at": "2026-04-15 11:20:27-00:00"
      }
    ],
    "email": {
      "server": "10.0.0.1",
      "port": 25,
      "from_email": "no-reply@robur.coop",
      "base_url": "127.0.0.2"
    }
  }|}

(** Helper exception unwrap *)
let of_string_exn f =
  match f with Ok a -> a | Error (`Msg err) -> failwith err

let json_of_string_exn f =
  match f with Ok a -> a | Error (`Msg err) -> failwith err

let label_of_string_exn label =
  of_string_exn (Vmm_core.Name.Label.of_string label)

let name_of_string_exn name = of_string_exn (Vmm_core.Name.of_string name)
let email_of_string_exn email = of_string_exn (Mrmime.Mailbox.of_string email)

let signing_request_exn ?(cn = "mock-ca") pk =
  let name =
    [
      X509.Distinguished_name.(
        Relative_distinguished_name.singleton (CN (Common_name.v cn)));
    ]
  in
  let key_usage = [ `Digital_signature; `Key_cert_sign ] in
  let basic_constraints = (true, None) in
  let exts =
    X509.Extension.(
      add Basic_constraints (true, basic_constraints)
        (singleton Key_usage (true, key_usage)))
  in
  let extensions = X509.Signing_request.Ext.(singleton Extensions exts) in
  match X509.Signing_request.create name ~extensions pk with
  | Ok c -> c
  | Error (`Msg msg) -> failwith msg
  | Error _ -> failwith "invalid signing request"

let private_key =
  X509.Private_key.generate ~seed:"robur_in_essaouira_2026" `ED25519

let second_private_key =
  X509.Private_key.generate ~seed:"robur_is_great_2026" `ED25519

let certificate_exn ?(cn = "mock-ca") pk =
  let csr = signing_request_exn ~cn pk in
  let name =
    [
      X509.Distinguished_name.(
        Relative_distinguished_name.singleton (CN (Common_name.v cn)));
    ]
  in
  let key_usage = [ `Digital_signature; `Key_cert_sign ] in
  let basic_constraints = (true, None) in
  let exts =
    X509.Extension.(
      add Basic_constraints (true, basic_constraints)
        (singleton Key_usage (true, key_usage)))
  in
  let valid_from = Ptime.epoch in
  let valid_until =
    match Ptime.of_date_time ((2035, 1, 1), ((0, 0, 0), 0)) with
    | Some t -> t
    | None -> failwith "invalid date"
  in
  let pub = X509.Private_key.public pk in
  let extensions =
    let auth = (Some (X509.Public_key.id pub), X509.General_name.empty, None) in
    X509.Extension.(
      add Subject_key_id
        (false, X509.Public_key.id pub)
        (add Authority_key_id (false, auth) exts))
  in
  match
    X509.Signing_request.sign csr ~valid_from ~valid_until ~extensions pk name
  with
  | Ok c -> c
  | Error e -> failwith (Fmt.str "%a" X509.Validation.pp_signature_error e)

(** Alcotest Testables *)
let msg_t : [ `Msg of string ] Alcotest.testable =
  let pp ppf (`Msg s) = Fmt.string ppf s in
  Alcotest.testable pp (fun (`Msg a) (`Msg b) -> String.equal a b)

let pp_storage ppf (users, configuration, email) =
  Fmt.pf ppf "%a" Jsont.Json.pp (Storage.t_to_json users configuration email)

let eq_config_pair (c1 : Models.Configuration.t) (c2 : Models.Configuration.t) =
  let {
    Models.Configuration.name = n1;
    server_ip = si1;
    server_port = sp1;
    updated_at = ua1;
    certificate = cer1;
    private_key = pk1;
  } =
    c1
  and {
    Models.Configuration.name = n2;
    server_ip = si2;
    server_port = sp2;
    updated_at = ua2;
    certificate = cer2;
    private_key = pk2;
  } =
    c2
  in
  Vmm_core.Name.Label.equal n1 n2
  && Ipaddr.compare si1 si2 = 0
  && Int.equal sp1 sp2 && Ptime.equal ua1 ua2
  && String.equal
       (X509.Certificate.fingerprint `SHA256 cer1)
       (X509.Certificate.fingerprint `SHA256 cer2)
  && String.equal
       (X509.Private_key.encode_der pk1)
       (X509.Private_key.encode_der pk2)

let eq_config (config_1 : Models.Configuration.t Utils.LM.t)
    (config_2 : Models.Configuration.t Utils.LM.t) =
  Utils.LM.equal eq_config_pair config_1 config_2

let eq_cookie (c1 : Models.User.cookie) (c2 : Models.User.cookie) =
  String.equal c1.value c2.value
  && Int.equal c1.expires_in c2.expires_in
  && Ptime.equal c1.created_at c2.created_at
  && Ptime.equal c1.last_access c2.last_access
  && Option.equal String.equal c1.user_agent c2.user_agent

let eq_user_pair (u1 : Models.User.user) (u2 : Models.User.user) =
  let {
    Models.User.name = n1;
    email = e1;
    password = p1;
    uuid = id1;
    cookies = c1;
    active = a1;
    super_user = s1;
    updated_at = uat1;
    created_at = cat1;
    email_verified = ev1;
    email_verification_uuid = evu1;
    unikernel_updates = _uk1;
    scaling_policies = _sp1;
  } =
    u1
  and {
    Models.User.name = n2;
    email = e2;
    password = p2;
    uuid = id2;
    cookies = c2;
    active = a2;
    super_user = s2;
    updated_at = uat2;
    created_at = cat2;
    email_verified = ev2;
    email_verification_uuid = evu2;
    unikernel_updates = _uk2;
    scaling_policies = _sp2;
  } =
    u2
  in
  Vmm_core.Name.Label.equal n1 n2
  && Mrmime.Mailbox.equal e1 e2 && String.equal p1 p2 && Uuidm.equal id1 id2
  && Utils.SM.equal eq_cookie c1 c2
  && Bool.equal a1 a2 && Bool.equal s1 s2 && Ptime.equal uat1 uat2
  && Ptime.equal cat1 cat2
  && Option.equal Ptime.equal ev1 ev2
  && Option.equal Uuidm.equal evu1 evu2

let eq_users (users_1 : Models.User.user Utils.UM.t)
    (users_2 : Models.User.user Utils.UM.t) =
  Utils.UM.equal eq_user_pair users_1 users_2

let eq_emails (e1 : Models.Email.t) (e2 : Models.Email.t) =
  let {
    Models.Email.server = s1;
    port = p1;
    base_url = bu1;
    from_email = fe1;
    to_email = te1;
  } =
    e1
  and {
    Models.Email.server = s2;
    port = p2;
    base_url = bu2;
    from_email = fe2;
    to_email = te2;
  } =
    e2
  in
  Ipaddr.compare s1 s2 = 0
  && Int.equal p1 p2 && String.equal bu1 bu2
  && Mrmime.Mailbox.equal fe1 fe2
  && Option.equal Mrmime.Mailbox.equal te1 te2

let eq_storage (u1, c1, e1) (u2, c2, e2) =
  eq_users u1 u2 && eq_config c1 c2 && Option.equal eq_emails e1 e2

let storage_t = Alcotest.testable pp_storage eq_storage

let mock_storage ?(version = 10) ?(users = Utils.UM.empty)
    ?(configuration = Utils.LM.empty) ?(email = None) () =
  Storage.t_to_json ~version users configuration email

let mock_email =
  {
    Models.Email.server = Ipaddr.of_string_exn "10.0.0.1";
    port = 56;
    from_email = email_of_string_exn "test@robur.coop";
    base_url = "robur.coop";
    to_email = None;
  }

let mock_albatross_config =
  let cert = certificate_exn private_key in
  {
    Models.Configuration.name = label_of_string_exn "default";
    certificate = cert;
    private_key;
    server_ip = Ipaddr.of_string_exn "10.0.0.1";
    server_port = 25;
    updated_at = Ptime.epoch;
  }

let csrf_key =
  "2fc872515abe2def1b79259da44ba1d01adbf305e444de885cadf1eb40f7b539"

let session_cookie = "test-session-cookie"

let make_csrf_token ?(session_cookie = session_cookie) () =
  Csrf.generate_token ~key:csrf_key ~session_cookie
    ~now:(int_of_float (Ptime.to_float_s Ptime.epoch))
    ()

let make_mock_user ?(name = "testuser") ?(email = "test@example.com")
    ?(password = "Password123!") ?(active = true) ?(super_user = false) () =
  let name_lbl = label_of_string_exn name in
  let email_box = email_of_string_exn email in
  let now = Ptime.epoch in
  let user, _session_cookie =
    Models.User.create_user ~name:name_lbl ~email:email_box ~password ~active
      ~super_user ~created_at:now ~user_agent:(Some "Alcotest-client")
  in
  user

let cookie_hdr = function
  | "" -> ""
  | s -> Fmt.str "Cookie: molly_session=%s\r\n" s

let make_get_request ~path ?(session_cookie = "") ?csrf_token:_ () =
  let cookie_hdr = cookie_hdr session_cookie in
  Fmt.str
    "GET %s HTTP/1.1\r\n\
     Host: localhost\r\n\
     User-Agent: Alcotest-client\r\n\
     %s\r\n"
    path cookie_hdr

let make_post_request ~path ~body ?csrf_token:_ ?(session_cookie = "") () =
  let cookie_hdr = cookie_hdr session_cookie in
  Fmt.str
    "POST %s HTTP/1.1\r\n\
     Host: localhost\r\n\
     Content-Type: application/json\r\n\
     Content-Length: %d\r\n\
     User-Agent: Alcotest-client\r\n\
     %s\r\n\
     %s"
    path (String.length body) cookie_hdr body

let default_boundary = "----WebKitFormBoundary7MA4YWxkTrZu0gW"

let make_multipart_request ?(boundary = default_boundary) ~parts ?file_part
    ?(session_cookie = "") ?csrf_token:_ path =
  let body_buf = Buffer.create 1024 in
  List.iter
    (fun (key, value) ->
      Buffer.add_string body_buf (Fmt.str "--%s\r\n" boundary);
      Buffer.add_string body_buf
        (Fmt.str "Content-Disposition: form-data; name=\"%s\"\r\n\r\n" key);
      Buffer.add_string body_buf (Fmt.str "%s\r\n" value))
    parts;
  (match file_part with
  | Some (field_name, filename, content_type, content) ->
      Buffer.add_string body_buf (Fmt.str "--%s\r\n" boundary);
      Buffer.add_string body_buf
        (Fmt.str
           "Content-Disposition: form-data; name=\"%s\"; filename=\"%s\"\r\n"
           field_name filename);
      Buffer.add_string body_buf
        (Fmt.str "Content-Type: %s\r\n\r\n" content_type);
      Buffer.add_string body_buf content;
      Buffer.add_string body_buf "\r\n"
  | None -> ());
  Buffer.add_string body_buf (Fmt.str "--%s--\r\n" boundary);
  let body = Buffer.contents body_buf in
  let cookie_hdr = cookie_hdr session_cookie in
  Fmt.str
    "POST %s HTTP/1.1\r\n\
     Host: localhost\r\n\
     Content-Type: multipart/form-data; boundary=%s\r\n\
     Content-Length: %d\r\n\
     User-Agent: Alcotest-client\r\n\
     %s\r\n\
     %s"
    path boundary (String.length body) cookie_hdr body

let is_redirect resp =
  String.starts_with ~prefix:"HTTP/1.1 302" resp
  || String.starts_with ~prefix:"HTTP/1.1 303" resp

let dummy_info (name : Vmm_core.Name.t) =
  let dummy_cfg =
    Vmm_core.Unikernel.
      {
        typ = `Solo5;
        compressed = false;
        image = "";
        fail_behaviour = `Quit;
        add_name = true;
        startup = None;
        cpuids = Vmm_core.IS.singleton 0;
        memory = 32;
        block_devices = [];
        bridges = [];
        argv = None;
        numcpus = 1;
        linux_boot_partition = None;
      }
  in
  let dummy_u =
    Vmm_core.Unikernel.
      {
        config = dummy_cfg;
        cmd = [||];
        pid = 1234;
        taps = [];
        digest = String.make 32 'a';
        started = Utils.now ();
      }
  in
  let info = Vmm_core.Unikernel.info (fun _ -> None) dummy_u in
  (name, info)

let known_unikernel_label = label_of_string_exn "hello"

let success_cfg =
  {
    mock_albatross_config with
    server_ip = Ipaddr.of_string_exn "127.0.0.1";
    server_port = 1025;
  }

let failure_cfg =
  {
    mock_albatross_config with
    name = label_of_string_exn "failing";
    server_ip = Ipaddr.of_string_exn "127.0.0.1";
    server_port = 1026;
  }

let is_failing (config : Models.Configuration.t) =
  String.starts_with ~prefix:"failing"
    (Models.Configuration.name_to_str config.name)
  || config.server_port = failure_cfg.server_port

module Mnet_happy_eyeballs = struct
  include Mnet_happy_eyeballs

  let connect ~kind:_ _ _ _ = Error (`Msg "uninitialized")
end

module Builder_web = struct
  include Builder_web

  let send_http_request ?path:_ ~base_url:_ ~happy_eyeballs:_ () =
    Error (`Msg "Mock happy_eyeballs: no network")

  let fetch_unikernel_jobs _happy_eyeballs = []

  let fetch_unikernel_binary_image _happy_eyeballs ~job:_ ~version:_
      =
    Error (`Msg "Mock happy_eyeballs: no network")
end

module Email_client = struct
  include Email_client

  let send_email ?references:_ _happy_eyeballs _email_config _user_email
      ~subject:_ ~body:_ =
    Error "Connection failed"

  let test_email_configuration _happy_eyeballs _email_settings _recipient =
    Error "Connection failed"
end

module Albatross_client = struct
  include Albatross_client

  let init _tcp (configuration : Models.Configuration.t) =
    let state : t =
      { status = Status.Online; policies = Vmm_trie.empty; configuration }
    in
    if is_failing configuration then Error (state, "albatross failure")
    else Ok state

  let query _tcp t ~domain ?(name = dot_name) ?source:_ cmd =
    let vmm_name =
      Vmm_core.Name.make (Vmm_core.Name.Path.of_label domain) name
    in
    let hdr = Vmm_commands.header vmm_name in
    if is_failing t.configuration then Ok (hdr, `Failure "albatross failure")
    else
      match cmd with
      | `Unikernel_cmd `Unikernel_info ->
          let hello_name =
            Vmm_core.Name.make
              (Vmm_core.Name.Path.of_label domain)
              known_unikernel_label
          in
          Ok (hdr, `Success (`Unikernel_info [ dummy_info hello_name ]))
      | `Unikernel_cmd (`Unikernel_create _) ->
          Ok (hdr, `Success (`String "unikernel created"))
      | `Unikernel_cmd (`Unikernel_force_create _) ->
          Ok (hdr, `Success (`String "unikernel force created"))
      | _ -> Ok (hdr, `Success (`String "ok"))

  let query_console _tcp t ~domain:_ ~name:_ _f =
    if is_failing t.configuration then Error "albatross failure" else Ok ()

  let query_block_dump _tcp t ~domain:_ ~name:_ _compression f =
    if is_failing t.configuration then Error "albatross failure"
    else
      Result.map_error
        (fun () -> "error")
        (f (Some "mock block dump content\n\r\n\rEOF\n\r"))

  let query_unikernel_binary _tcp t ~domain:_ ~name:_ f =
    if is_failing t.configuration then Error "albatross failure"
    else Result.map_error (fun () -> "error") (f "mock binary")

  let query_stats _tcp t _f =
    if is_failing t.configuration then Error "albatross failure" else Ok ()

  let set_policy _tcp t ~domain policy =
    if is_failing t.configuration then Error "albatross failure"
    else
      let name =
        Vmm_core.Name.make_of_path (Vmm_core.Name.Path.of_label domain)
      in
      t.policies <- fst (Vmm_trie.insert name policy t.policies);
      Ok (name, policy)

  let user_blocks_by_instance _tcp _state _user_name = []

  let user_unikernels_by_instance _tcp (state : t) user_name =
    if is_failing state.configuration then []
    else
      let hello_name =
        Vmm_core.Name.make
          (Vmm_core.Name.Path.of_label user_name)
          known_unikernel_label
      in
      [ dummy_info hello_name ]

  let user_deceased_by_instance _tcp (albatross_instances : a_map) _user_name =
    Utils.LM.map (fun _ -> []) albatross_instances

  let user_unikernels tcp (albatross_instances : a_map) user_name =
    Utils.LM.map
      (fun inst -> user_unikernels_by_instance tcp inst user_name)
      albatross_instances

  let user_unikernel _tcp (state : t) ~user_name ~unikernel_name =
    if is_failing state.configuration then Error "albatross failure"
    else if Vmm_core.Name.Label.equal unikernel_name known_unikernel_label then
      let hello_name =
        Vmm_core.Name.make
          (Vmm_core.Name.Path.of_label user_name)
          unikernel_name
      in
      Ok (dummy_info hello_name)
    else Error "Expected one unikernel, but got 0"
end
