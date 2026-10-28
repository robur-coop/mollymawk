let send_email ?references happy_eyeballs (email_config : Models.Email.t)
    user_email ~subject ~body =
  let email =
    Models.Email.construct_email ?references ~from_email:email_config.from_email
      ~to_email:user_email ~subject ~body ()
  in
  let ip = email_config.server in
  let addr =
    match Ipaddr.to_v4 ip with Some addr -> addr | None -> Ipaddr.V4.any
  in
  let sender = Colombe_emile.to_reverse_path email_config.from_email in
  let recipients = [ Colombe_emile.to_forward_path user_email ] in
  let stream =
    Seq.of_dispenser (Mrmime.Mt.to_stream email)
    |> Flux.Source.seq |> Flux.Stream.from
  in
  let domain = Colombe.Domain.IPv4 addr in
  let res =
    try
      Msendmail.sendmail ~domain ~destination:(`Ips [ ip ])
        ~port:email_config.port happy_eyeballs sender recipients stream
    with exn -> Error (`Msg (Printexc.to_string exn))
  in
  match res with
  | Ok () -> Ok ()
  | Error (`Msg e) ->
      Logs.info (fun m -> m "Email not sent: %s" e);
      Error e
  | Error (#Sendmail_with_starttls.error as e) ->
      let msg =
        Fmt.str "An error occurred while sending email to %s: %a"
          (Emile.to_string user_email)
          Msendmail.pp_error e
      in
      Logs.info (fun m -> m "Email not sent: %s" msg);
      Error msg

let send_welcome_email happy_eyeballs email_config user verification_link =
  let body =
    Email_templates.welcome_email user verification_link email_config
  in
  send_email happy_eyeballs email_config user.Models.User.email
    ~subject:"Welcome to Mollymawk" ~body

let send_verification_email happy_eyeballs email_config user verification_link =
  let body =
    Email_templates.verification_email user verification_link email_config
  in
  send_email happy_eyeballs email_config user.Models.User.email
    ~subject:"Verify your email address" ~body

let send_update_report_email ?references happy_eyeballs email_config report =
  let body = Email_templates.updated_unikernels report email_config in
  send_email ?references happy_eyeballs email_config
    report.Update_flow.user.Models.User.email
    ~subject:"Unikernel updates available" ~body

let test_email_configuration happy_eyeballs email_settings recipient =
  send_email happy_eyeballs email_settings recipient
    ~subject:"Verifying New Email Configuration"
    ~body:"Your email configuration has been verified successfully.\n"
