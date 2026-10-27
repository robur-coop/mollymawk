open Controller

type env = Controller.env = {
  tcp : Mnet.TCP.state;
  udp : Mnet.UDP.state;
  m_tcp : Mnet.TCP.state;
  m_udp : Mnet.UDP.state;
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
  let open Vifu.Uri in
  let open Vifu.Type in
  [
    (* Static routes *)
    get (rel / "style.css" /?? nil)
    --> static_asset_response ~mime:"text/css;charset=utf-8"
          ~content:Assets.style_css;
    get (rel / "main.js" /?? nil)
    --> static_asset_response ~mime:"text/javascript;charset=utf-8"
          ~content:Assets.main_js;
    get (rel / "grafana.json" /?? nil)
    --> static_asset_response ~mime:"application/json; charset=utf-8"
          ~content:Assets.grafana_json;
    get (rel / "images" / "robur.png" /?? nil)
    --> static_asset_response ~mime:"image/png" ~content:Assets.robur_png;
    get (rel / "images" / "dashboard_1.png" /?? nil)
    --> static_asset_response ~mime:"image/png" ~content:Assets.dashboard_1_png;
    get (rel / "images" / "molly_bird.jpeg" /?? nil)
    --> static_asset_response ~mime:"image/jpeg"
          ~content:Assets.mollymawk_bird_by_katelyn_jpg;
    get (rel / "images" / "mollymawk_bird_by_katelyn.jpg" /?? nil)
    --> static_asset_response ~mime:"image/jpeg"
          ~content:Assets.mollymawk_bird_by_katelyn_jpg;
    get (rel / "images" / "albatross_1.png" /?? nil)
    --> static_asset_response ~mime:"image/jpeg"
          ~content:Assets.interconnections_by_guerrillabuzz_jpg;
    get (rel / "images" / "interconnections_by_guerrillabuzz.jpg" /?? nil)
    --> static_asset_response ~mime:"image/jpeg"
          ~content:Assets.interconnections_by_guerrillabuzz_jpg;
    get (rel / "images" / "mirage_os_1.png" /?? nil)
    --> static_asset_response ~mime:"image/jpeg"
          ~content:Assets.server_room_by_yuriy_vertikov_jpg;
    get (rel / "images" / "server_room_by_yuriy-vertikov.jpg" /?? nil)
    --> static_asset_response ~mime:"image/jpeg"
          ~content:Assets.server_room_by_yuriy_vertikov_jpg;
    get (rel /?? nil)
    --> page_response
          (Guest_layout.guest_layout
             ~page_title:"Deploy unikernels with ease | Mollymawk"
             Index_page.index_page);
    get (rel / "sign-in" /?? nil) --> Auth.sign_in_page;
    get (rel / "sign-up" /?? nil) --> Auth.sign_up_page;
    post (m User_model.registration_form) (rel / "api" / "register" /?? nil)
    --> Auth.register_user;
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

let run stack management_stack secret _name =
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
  let env = { tcp; udp; m_tcp; m_udp; block; csrf_key } in
  let middlewares = Vifu.Middlewares.[ Session.session ] in
  Vifu.run ~middlewares ~devices ~cfg tcp (routes ()) env
