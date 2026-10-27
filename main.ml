open Cmdliner

let name =
  let doc = "The name of the unikernel" in
  let open Arg in
  value & opt string "mollymawk" & info [ "name" ] ~doc ~docv:"NAME"

let secret =
  let validate_key key =
    match Ohex.decode key with
    | exception Invalid_argument _ -> Error "must be hex-encoded"
    | secret when String.length secret < 32 ->
        Error "must be at least 32 bytes (64 hex characters)"
    | secret -> Ok secret
  in
  let pp ppf _ = Format.pp_print_string ppf "<secret>" in
  let secret_conv = Arg.conv' ~docv:"SECRET" (validate_key, pp) in
  let doc =
    "Hex-encoded secret used to sign CSRF tokens and Cookies. Must be at least \
     32 bytes."
  in
  Arg.(
    required
    & opt (some secret_conv) None
    & info [ "secret" ] ~doc ~docv:"SECRET")

let term =
  let open Term in
  const App.run $ Mnet_cli.setup "service"
  $ Mnet_cli.setup "management"
  $ secret $ name

let cmd =
  let info = Cmd.info "mollymawk" in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
