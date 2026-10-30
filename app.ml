open Controller

type env = Controller.env = {
  tcp : Mnet.TCP.state;
  udp : Mnet.UDP.state;
  m_tcp : Mnet.TCP.state;
  m_udp : Mnet.UDP.state;
  m_dns : Dns.proto * Mnet_dns_cli.nameserver list;
  block : Mkernel.Block.t;
  csrf_key : string;
}

let rng =
  let fn () =
    Mirage_crypto_rng_mkernel.(initialize (module Mirage_crypto_rng.Fortuna))
  in
  Mkernel.map fn Mkernel.[] |> Mkernel.finally Mirage_crypto_rng_mkernel.kill

let routes () =
  let open Vifu.Route in
  let open Vifu.Type in
  let open Vifu.Uri in
  [
    (* Static routes *)
    get (rel / "style.css" /?? nil)
    --> static_asset_response ~mime:"text/css;charset=utf-8"
          ~content:Assets.style_css ~etag:Etag.style_css;
    get (rel / "main.js" /?? nil)
    --> static_asset_response ~mime:"text/javascript;charset=utf-8"
          ~content:Assets.main_js ~etag:Etag.main_js;
    get (rel / "grafana.json" /?? nil)
    --> static_asset_response ~mime:"application/json; charset=utf-8"
          ~content:Assets.grafana_json ~etag:Etag.grafana_json;
    get (rel / "images" / "robur.png" /?? nil)
    --> static_asset_response ~mime:"image/png" ~content:Assets.robur_png
          ~etag:Etag.robur_png;
    get (rel / "images" / "dashboard_1.png" /?? nil)
    --> static_asset_response ~mime:"image/png" ~content:Assets.dashboard_1_png
          ~etag:Etag.dashboard_1_png;
    get (rel / "images" / "molly_bird.jpeg" /?? nil)
    --> static_asset_response ~mime:"image/jpeg"
          ~content:Assets.mollymawk_bird_jpg ~etag:Etag.mollymawk_bird_jpg;
    get (rel / "images" / "albatross_1.jpg" /?? nil)
    --> static_asset_response ~mime:"image/jpeg" ~content:Assets.albatross_1_jpg
          ~etag:Etag.albatross_1_jpg;
    get (rel / "images" / "mirage_os_1.jpg" /?? nil)
    --> static_asset_response ~mime:"image/jpeg" ~content:Assets.mirage_os_1_jpg
          ~etag:Etag.mirage_os_1_jpg;
    get (rel /?? nil)
    --> page_response
          (Guest_layout.guest_layout
             ~page_title:"Deploy unikernels with ease | Mollymawk"
             Index_page.index_page);
    get (rel / "sign-in" /?? any) --> Auth.sign_in_page;
    get (rel / "sign-up" /?? any) --> Auth.sign_up_page;
    post
      (json_encoding Models.Registration.registration_form)
      (rel / "api" / "register" /?? any)
    --> Auth.register_user;
    post (json_encoding Models.Login.login_form) (rel / "api" / "login" /?? any)
    --> Auth.login_user;
    post
      (json_encoding Models.Close_sessions.close_sessions_form)
      (rel / "logout" /?? any)
    --> Auth.logout_user;
    post
      (json_encoding Models.Password.update_password_form)
      (rel / "account" / "password" / "update" /?? any)
    --> Auth.update_password;
    post
      (json_encoding Models.Close_sessions.close_sessions_form)
      (rel / "api" / "account" / "sessions" / "close" /?? any)
    --> Auth.close_all_sessions;
    post
      (json_encoding Models.Close_session.close_session_form)
      (rel / "api" / "account" / "session" / "close" /?? any)
    --> Auth.close_session;
    (* Email routes *)
    get (rel / "verify-email" /?? any) --> Email.verify_email;
    get (rel / "auth" / "verify" /?? any) --> Email.verify_email_token;
    get (rel / "admin" / "settings" / "email" /?? any) --> Email.email_settings;
    post
      (json_encoding Models.Email.email_form)
      (rel / "api" / "admin" / "settings" / "email" / "update" /?? any)
    --> Email.update_email_configuration;
    post
      (json_encoding Models.Email.email_form)
      (rel / "api" / "admin" / "settings" / "email" / "test" /?? any)
    --> Email.test_email_configuration;
    (* Block routes *)
    get (rel / "blocks" /?? any) --> Block.blocks;
    post
      (json_encoding Models.Block.delete_form)
      (rel / "api" / "block" / "delete" /?? any)
    --> Block.delete_block;
    post (m Models.Block.upload_form) (rel / "api" / "block" / "create" /?? any)
    --> Block.create_block;
    post (m Models.Block.upload_form) (rel / "api" / "block" / "upload" /?? any)
    --> Block.upload_block;
    post
      (json_encoding Models.Block.download_form)
      (rel / "api" / "block" / "download" /?? any)
    --> Block.download_block;
    (* Unikernel routes *)
    get (rel / "dashboard" /?? any) --> Unikernel.dashboard;
    get (rel / "api" / "unikernels" /?? any) --> Unikernel.unikernel_info;
    get (rel / "unikernel" / "info" /?? any) --> Unikernel.unikernel_info_one;
    get (rel / "unikernel" / "console" /?? any)
    --> Unikernel.unikernel_console_viewer;
    get (rel / "unikernel" / "deploy" /?? any) --> Unikernel.deploy_form;
    get (rel / "api" / "unikernel" / "monitoring" / "status" /?? any)
    --> Unikernel.unikernel_monitoring_status;
    post
      (json_encoding Models.Unikernel.monitoring_form)
      (rel / "api" / "unikernel" / "monitoring" / "update" /?? any)
    --> Unikernel.unikernel_monitoring_update;
    post
      (json_encoding Models.Unikernel.scaling_form)
      (rel / "api" / "unikernel" / "scaling" / "update" /?? any)
    --> Unikernel.unikernel_scaling_policy_update;
    post
      (json_encoding Models.Unikernel.destroy_form)
      (rel / "api" / "unikernel" / "destroy" /?? any)
    --> Unikernel.unikernel_destroy;
    post
      (json_encoding Models.Unikernel.restart_form)
      (rel / "api" / "unikernel" / "restart" /?? any)
    --> Unikernel.unikernel_restart;
    get (rel / "api" / "unikernel" / "console" /?? any)
    --> Unikernel.unikernel_console;
    post multipart_form (rel / "api" / "unikernel" / "create" /?? any)
    --> Unikernel.unikernel_create;
    get (rel / "unikernel" / "update" /?? any)
    --> Unikernel.unikernel_prepare_update;
    post
      (json_encoding Models.Unikernel.update_form)
      (rel / "api" / "unikernel" / "update" /?? any)
    --> Unikernel.unikernel_update;
    post
      (json_encoding Models.Unikernel.rollback_form)
      (rel / "api" / "unikernel" / "rollback" /?? any)
    --> Unikernel.unikernel_rollback;
    get (rel / "unikernel" / "update" / "compare-changes" /?? any)
    --> Unikernel.unikernel_prepare_update;
    (* Account routes *)
    get (rel / "account" /?? any) --> Auth.account_page;
    get (rel / "usage" /?? any) --> Auth.account_usage;
    (* Albatross Instance Selection & Admin Config routes *)
    get (rel / "albatross" / "instances" /?? any)
    --> Albatross_admin.instances_redirect;
    get (rel / "select" / "instance" /?? any)
    --> Albatross_admin.choose_instance;
    get (rel / "admin" / "settings" / "albatross" /?? any)
    --> Albatross_admin.albatross_settings;
    get (rel / "admin" / "albatross" / "errors" /?? any)
    --> Albatross_admin.view_albatross_error_logs;
    get (rel / "api" / "admin" / "albatross" / "retry" /?? any)
    --> Albatross_admin.retry_initializing_instance;
    post
      (json_encoding Models.Configuration.config_form)
      (rel / "api" / "admin" / "settings" / "albatross" / "create" /?? any)
    --> Albatross_admin.create_albatross_configuration;
    post
      (json_encoding Models.Configuration.config_form)
      (rel / "api" / "admin" / "settings" / "albatross" / "update" /?? any)
    --> Albatross_admin.update_albatross_configuration;
    post
      (json_encoding Models.Configuration.delete_form)
      (rel / "api" / "admin" / "settings" / "albatross" / "delete" /?? any)
    --> Albatross_admin.delete_albatross_config;
    (* User Administration & Policy Management routes *)
    get (rel / "admin" / "users" /?? any) --> Admin.users;
    get (rel / "admin" / "user" / "profile" /?? any) --> Admin.view_user_profile;
    get (rel / "admin" / "user" / "unikernels" /?? any)
    --> Admin.view_user_unikernels;
    get (rel / "admin" / "user" / "policy" /?? any) --> Admin.view_user_policy;
    get (rel / "admin" / "u" / "policy" / "edit" /?? any) --> Admin.edit_policy;
    post
      (json_encoding Models.Policy.policy_form)
      (rel / "api" / "admin" / "u" / "policy" / "update" /?? any)
    --> Admin.update_policy;
    post
      (json_encoding Models.Admin_user.user_uuid_form)
      (rel / "api" / "admin" / "user" / "activate" / "toggle" /?? any)
    --> Admin.toggle_account_activation;
    post
      (json_encoding Models.Admin_user.user_uuid_form)
      (rel / "api" / "admin" / "user" / "admin" / "toggle" /?? any)
    --> Admin.toggle_admin_activation;
    post
      (json_encoding Models.Admin_user.user_uuid_form)
      (rel / "api" / "admin" / "user" / "account" / "delete" /?? any)
    --> Admin.delete_account;
  ]

let derive_keys secret =
  let prk = Hkdf.extract ~hash:`SHA256 secret in
  ( Hkdf.expand ~hash:`SHA256 ~prk ~info:"cookie" 32,
    Hkdf.expand ~hash:`SHA256 ~prk ~info:"csrf" 32 )

let http =
  let h1 =
    H1.Config.
      {
        default with
        read_buffer_size = 0x4000;
        response_buffer_size = 0x4000;
        response_body_buffer_size = 0x4000;
      }
  in
  `H1 h1

let run stack management_stack m_dns secret _name =
  Mkernel.(run [ rng; stack; management_stack; block "data" ])
  @@ fun _rng (_, tcp, udp) (_, m_tcp, m_udp) block () ->
  let port = 80 in
  let cookie_secret, csrf_key = derive_keys secret in
  let cfg =
    Vifu.Config.v
      ~cookie_key:(Mirage_crypto.AES.GCM.of_secret cookie_secret)
      ~http port
  in
  let devices =
    Vifu.Devices.
      [
        happy_eyeballs;
        management_happy_eyeballs;
        storage;
        albatross;
        Scheduler.device;
      ]
  in
  let env = { tcp; udp; m_tcp; m_udp; m_dns; block; csrf_key } in
  let middlewares = Vifu.Middlewares.[ Session.session ] in
  Vifu.run ~middlewares ~devices ~cfg tcp (routes ()) env
