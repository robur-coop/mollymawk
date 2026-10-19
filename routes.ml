let respond_with_tyxml_page ty req (_server : Vifu.Server.t) _env =
  let open Vifu.Response.Syntax in
  let* () = Vifu.Response.add ~field:"content-type" "text/html" in
  let* () = Vifu.Response.add ~field:"charset" "utf-8" in
  let* () = Vifu.Response.with_tyxml req ty in
  Vifu.Response.respond `OK

let serve_static ~mime ~content req (_server : Vifu.Server.t) _env =
  let open Vifu.Response.Syntax in
  (* TODO: if-none-match / etag *)
  let* () = Vifu.Response.add ~field:"Content-Type" mime in
  let* () = Vifu.Response.with_string req content in
  Vifu.Response.respond `OK

let routes () =
  let open Vifu.Route in
  let open Vifu.Uri in
  [
    get (rel / "style.css" /?? nil)
    --> serve_static ~mime:"text/css;charset=utf-8" ~content:Assets.style_css;
    get (rel / "main.js" /?? nil)
    --> serve_static ~mime:"text/javascript;charset=utf-8"
          ~content:Assets.main_js;
    get (rel / "grafana.json" /?? nil)
    --> serve_static ~mime:"application/json; charset=utf-8"
          ~content:Assets.grafana_json;
    get (rel / "images" / "robur.png" /?? nil)
    --> serve_static ~mime:"image/png" ~content:Assets.robur_png;
    get (rel / "images" / "dashboard_1.png" /?? nil)
    --> serve_static ~mime:"image/png" ~content:Assets.dashboard_1_png;
    get (rel / "images" / "molly_bird.jpeg" /?? nil)
    --> serve_static ~mime:"image/jpeg"
          ~content:Assets.mollymawk_bird_by_katelyn_jpg;
    get (rel / "images" / "mollymawk_bird_by_katelyn.jpg" /?? nil)
    --> serve_static ~mime:"image/jpeg"
          ~content:Assets.mollymawk_bird_by_katelyn_jpg;
    get (rel / "images" / "albatross_1.png" /?? nil)
    --> serve_static ~mime:"image/jpeg"
          ~content:Assets.interconnections_by_guerrillabuzz_jpg;
    get (rel / "images" / "interconnections_by_guerrillabuzz.jpg" /?? nil)
    --> serve_static ~mime:"image/jpeg"
          ~content:Assets.interconnections_by_guerrillabuzz_jpg;
    get (rel / "images" / "mirage_os_1.png" /?? nil)
    --> serve_static ~mime:"image/jpeg"
          ~content:Assets.server_room_by_yuriy_vertikov_jpg;
    get (rel / "images" / "server_room_by_yuriy-vertikov.jpg" /?? nil)
    --> serve_static ~mime:"image/jpeg"
          ~content:Assets.server_room_by_yuriy_vertikov_jpg;
    get (rel /?? nil)
    --> respond_with_tyxml_page
          (Guest_layout.guest_layout Index_page.index_page);
    get (rel / "sign-in" /?? nil) --> respond_with_tyxml_page Sign_in.login_page;
  ]
