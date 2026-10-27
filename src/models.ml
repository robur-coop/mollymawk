let csrf_mem map = Jsont.Object.mem "molly_csrf" Jsont.string map

let opt_nullable_mem ?enc ?(enc_omit = Option.is_none) name codec map =
  Jsont.Object.mem ?enc ~enc_omit ~dec_absent:None name (Jsont.option codec) map

let single_field_csrf_form field_name make =
  let open Jsont in
  Object.map make |> Object.mem field_name string |> csrf_mem |> Object.finish

let instance_action_form target_field make =
  let open Jsont in
  Object.map make
  |> Object.mem target_field string
  |> Object.mem "albatross_instance" string
  |> csrf_mem |> Object.finish

module Response = struct
  type 'a t = { title : string; data : 'a }

  let make title data = { title; data }
  let title r = r.title
  let data r = r.data

  let of_data ?(kind = "Response") data_codec =
    Jsont.Object.map ~kind make
    |> Jsont.Object.mem "title" Jsont.string ~enc:title
    |> Jsont.Object.mem "data" data_codec ~enc:data
    |> Jsont.Object.finish
end

module Status = struct
  type t = string Response.t

  let make = Response.make
  let data = Response.data
  let to_json = Response.of_data ~kind:"Status" Jsont.string
end

module Multipart = struct
  type parts = (Vifu.Multipart_form.part * string) Utils.SM.t

  let parse_stream (stream : Vifu.Multipart_form.stream) : parts =
    let open Flux in
    let sink =
      Sink.each ~init:Utils.SM.empty
        ~merge:(fun (part, value) acc ->
          match Vifu.Multipart_form.name part with
          | Some name -> Utils.SM.add name (part, value) acc
          | None -> acc)
        (fun (part, src) ->
          let value = Stream.(into Sink.string (from src)) in
          (part, value))
    in
    Stream.into sink stream

  let find_string name (parts : parts) =
    Option.map snd (Utils.SM.find_opt name parts)
end

module Configuration = struct
  type form_t = {
    name : string;
    certificate : string;
    private_key : string;
    server_ip : string;
    server_port : int;
    molly_csrf : string;
  }

  let config_form =
    let open Jsont in
    let fn name certificate private_key server_ip server_port molly_csrf :
        form_t =
      { name; certificate; private_key; server_ip; server_port; molly_csrf }
    in
    Object.map fn |> Object.mem "name" string
    |> Object.mem "certificate" string
    |> Object.mem "private_key" string
    |> Object.mem "server_ip" string
    |> Object.mem "server_port" int
    |> csrf_mem |> Object.finish

  type delete_t = { name : string; molly_csrf : string }

  let delete_form =
    single_field_csrf_form "name" (fun name molly_csrf -> { name; molly_csrf })

  type t = {
    name : Vmm_core.Name.Label.t;
    certificate : X509.Certificate.t;
    private_key : X509.Private_key.t;
    server_ip : Ipaddr.t;
    server_port : int;
    updated_at : Ptime.t;
  }

  let name_to_str name = Vmm_core.Name.Label.to_string name
  let name_of_str name = Vmm_core.Name.Label.of_string name
  let label_json = Utils.Json.of_string_result ~enc:name_to_str name_of_str

  let certificate_json =
    Utils.Json.of_string_result ~enc:X509.Certificate.encode_pem
      X509.Certificate.decode_pem

  let private_key_json =
    Utils.Json.of_string_result ~enc:X509.Private_key.encode_pem
      X509.Private_key.decode_pem

  let check_cert_key_match certificate private_key =
    if
      String.equal
        (X509.Public_key.fingerprint (X509.Certificate.public_key certificate))
        (X509.Public_key.fingerprint (X509.Private_key.public private_key))
    then Ok ()
    else Error (`Msg "certificate and private key do not match")

  let one_json =
    let open Jsont in
    let make name certificate private_key server_ip server_port updated_at =
      match check_cert_key_match certificate private_key with
      | Error (`Msg msg) -> Error.msg Meta.none msg
      | Ok () ->
          { name; certificate; private_key; server_ip; server_port; updated_at }
    in
    Object.map make
    |> Object.mem "name" label_json ~enc:(fun c -> c.name)
    |> Object.mem "certificate" certificate_json ~enc:(fun c -> c.certificate)
    |> Object.mem "private_key" private_key_json ~enc:(fun c -> c.private_key)
    |> Object.mem "server_ip" Utils.Json.ipaddr_json ~enc:(fun c -> c.server_ip)
    |> Object.mem "server_port" int ~enc:(fun c -> c.server_port)
    |> Object.mem "updated_at" Utils.TimeHelper.ptime_json ~enc:(fun c ->
        c.updated_at)
    |> Object.finish

  let json : t Utils.LM.t Jsont.t =
    Jsont.map
      ~dec:(fun cfgs ->
        List.fold_left
          (fun acc c ->
            if Utils.LM.mem c.name acc then
              Jsont.Error.msg Jsont.Meta.none
                "Duplicated albatross configurations"
            else Utils.LM.add c.name c acc)
          Utils.LM.empty cfgs)
      ~enc:(fun m -> Utils.LM.fold (fun _ c acc -> c :: acc) m [])
      (Jsont.list one_json)

  let to_json (t : t Utils.LM.t) = Utils.Json.encode json t

  let to_configuration (f : form_t) now =
    let ( let* ) = Result.bind in
    let* name = name_of_str f.name in
    let* certificate = X509.Certificate.decode_pem f.certificate in
    let* private_key = X509.Private_key.decode_pem f.private_key in
    let* () = check_cert_key_match certificate private_key in
    let* server_ip = Ipaddr.of_string f.server_ip in
    Ok
      {
        name;
        certificate;
        private_key;
        server_ip;
        server_port = f.server_port;
        updated_at = now;
      }
end

module Email = struct
  type form_t = {
    server : string;
    port : int;
    from_email : string;
    base_url : string;
    to_email : string option;
    molly_csrf : string;
  }

  let email_form =
    let open Jsont in
    let fn server port from_email base_url to_email molly_csrf : form_t =
      { server; port; from_email; base_url; to_email; molly_csrf }
    in
    Object.map fn |> Object.mem "server" string |> Object.mem "port" int
    |> Object.mem "from_email" string
    |> Object.mem "base_url" string
    |> opt_nullable_mem "to_email" string
    |> csrf_mem |> Object.finish

  type t = {
    server : Ipaddr.t;
    port : int;
    from_email : Mrmime.Mailbox.t;
    base_url : string;
    to_email : Mrmime.Mailbox.t option;
  }

  let json =
    let ipaddr_json = Utils.Json.ipaddr_json in
    let mailbox_json = Utils.Json.mailbox_json in
    let open Jsont in
    let make server port from_email base_url to_email =
      { server; port; from_email; base_url; to_email }
    in
    Object.map make
    |> Object.mem "server" ipaddr_json ~enc:(fun c -> c.server)
    |> Object.mem "port" int ~enc:(fun c -> c.port)
    |> Object.mem "from_email" mailbox_json ~enc:(fun c -> c.from_email)
    |> Object.mem "base_url" string ~enc:(fun c -> c.base_url)
    |> opt_nullable_mem "to_email" mailbox_json ~enc:(fun _ -> None)
    |> Object.finish

  let to_json config = Utils.Json.encode (Jsont.option json) config
  let of_json j = Utils.Json.decode json j
  let validate_email email = Mrmime.Mailbox.of_string email

  let generate_verification_link uuid =
    "/auth/verify?token=" ^ Uuidm.to_string uuid

  let subject_of_strings xs =
    List.fold_left
      (fun acc s -> acc @ Mrmime.Unstructured.Craft.[ sp 1; v s ])
      []
      (String.split_on_char ' ' xs)

  let construct_email ?references ~from_email ~to_email ~subject ~body () =
    let open Mrmime in
    let fields =
      Field.
        [
          Field
            ( Field_name.subject,
              Unstructured,
              Unstructured.Craft.(compile (subject_of_strings subject)) );
          Field (Field_name.v "From", Addresses, [ `Mailbox from_email ]);
          Field (Field_name.v "To", Addresses, [ `Mailbox to_email ]);
          Field (Field_name.date, Date, Date.of_ptime ~zone:GMT (Utils.now ()));
          Field
            ( Field_name.content_type,
              Content,
              Content_type.(
                make `Text (Subtype.v `Text "html")
                  (Parameters.singleton (Parameters.k "charset")
                     (Parameters.v "utf-8"))) );
          Field (Field_name.content_encoding, Encoding, `Quoted_printable);
        ]
    in
    let fields =
      match references with
      | None -> fields
      | Some r ->
          Field.Field
            ( Field_name.references,
              Field.Unstructured,
              Unstructured.Craft.(compile [ v ("<" ^ r ^ ">") ]) )
          :: fields
    in
    let header = Header.of_list fields in
    let body_stream =
      let sent = ref false in
      fun () ->
        if !sent then None
        else (
          sent := true;
          Some (body, 0, String.length body))
    in
    let part = Mt.part ~header body_stream in
    Mt.make header Mt.simple part

  let validate (f : form_t) : (t, string) result =
    let ( let* ) = Result.bind in
    let map_err r = Result.map_error (fun (`Msg e) -> e) r in
    let* server = Ipaddr.of_string f.server |> map_err in
    let* from_email = validate_email f.from_email |> map_err in
    let* to_email =
      match f.to_email with
      | None -> Ok None
      | Some email_str ->
          validate_email email_str |> map_err |> Result.map Option.some
    in
    Ok { server; port = f.port; from_email; base_url = f.base_url; to_email }
end

module User = struct
  type cookie = {
    value : string;
    expires_in : int;
    created_at : Ptime.t;
    last_access : Ptime.t;
    user_agent : string option;
  }

  type unikernel_update = {
    name : Vmm_core.Name.Label.t;
    job : string;
    uuid : Uuidm.t;
    config : Vmm_core.Unikernel.config;
    timestamp : Ptime.t;
  }

  type unikernel_scaling_policy = {
    name : Vmm_core.Name.Label.t;
    primary_albatross_instance : Vmm_core.Name.Label.t;
    max_instances : int;
  }

  module Scaling_policy_key = struct
    type t = Vmm_core.Name.Label.t * Vmm_core.Name.Label.t
    (* the unikernel name and the instance name to which the primary unikernel has been deployed on. *)

    let compare (n1, i1) (n2, i2) =
      let c = Vmm_core.Name.Label.compare n1 n2 in
      if c <> 0 then c else Vmm_core.Name.Label.compare i1 i2
  end

  module Scaling_policy_map = Map.Make (Scaling_policy_key)

  type user = {
    name : Vmm_core.Name.Label.t;
    email : Mrmime.Mailbox.t;
    email_verified : Ptime.t option;
    password : string;
    uuid : Uuidm.t;
    created_at : Ptime.t;
    updated_at : Ptime.t;
    email_verification_uuid : Uuidm.t option;
    active : bool;
    super_user : bool;
    unikernel_updates : unikernel_update Utils.LM.t;
    scaling_policies : unikernel_scaling_policy Scaling_policy_map.t;
  }

  let week = 604800 (* a week = 7 days * 24 hours * 60 minutes * 60 seconds *)

  let scaling_policy_json =
    let open Jsont in
    let make name primary_albatross_instance max_instances :
        unikernel_scaling_policy =
      { name; primary_albatross_instance; max_instances }
    in
    Object.map make
    |> Object.mem "name" Configuration.label_json
         ~enc:(fun (p : unikernel_scaling_policy) -> p.name)
    |> Object.mem "primary_albatross_instance" Configuration.label_json
         ~enc:(fun (p : unikernel_scaling_policy) ->
           p.primary_albatross_instance)
    |> Object.mem "max_instances" int
         ~enc:(fun (p : unikernel_scaling_policy) -> p.max_instances)
    |> Object.finish

  let unikernel_update_json =
    let open Jsont in
    let make name job uuid config timestamp : unikernel_update =
      { name; job; uuid; config; timestamp }
    in
    Object.map make
    |> Object.mem "name" Configuration.label_json
         ~enc:(fun (u : unikernel_update) -> u.name)
    |> Object.mem "job" string ~enc:(fun (u : unikernel_update) -> u.job)
    |> Object.mem "uuid" Utils.Json.uuid_json
         ~enc:(fun (u : unikernel_update) -> u.uuid)
    |> Object.mem "config" Albatross_json.config_json
         ~enc:(fun (u : unikernel_update) -> u.config)
    |> Object.mem "timestamp" Utils.TimeHelper.ptime_json
         ~enc:(fun (u : unikernel_update) -> u.timestamp)
    |> Object.finish

  let unikernel_update_to_json u = Utils.Json.encode unikernel_update_json u
  let unikernel_update_of_json j = Utils.Json.decode unikernel_update_json j

  let unikernel_updates_map_json =
    Jsont.map
      ~dec:(fun j_list ->
        List.fold_left
          (fun acc js ->
            match unikernel_update_of_json js with
            | Ok u -> Utils.LM.add u.name u acc
            | Error (`Msg err) ->
                Logs.warn (fun m ->
                    m "discarding malformed unikernel_update: %s" err);
                acc)
          Utils.LM.empty j_list)
      ~enc:(fun m ->
        Utils.LM.fold (fun _ u acc -> unikernel_update_to_json u :: acc) m [])
      (Jsont.list Jsont.json)

  let scaling_policies_map_json =
    Jsont.map
      ~dec:(fun sps ->
        List.fold_left
          (fun acc (sp : unikernel_scaling_policy) ->
            Scaling_policy_map.add
              (sp.name, sp.primary_albatross_instance)
              sp acc)
          Scaling_policy_map.empty sps)
      ~enc:(fun m -> Scaling_policy_map.fold (fun _ sp acc -> sp :: acc) m [])
      (Jsont.list scaling_policy_json)

  let user_json =
    let open Jsont in
    let make name email email_verified password uuid created_at updated_at
        email_verification_uuid active super_user unikernel_updates
        scaling_policies =
      {
        name;
        email;
        email_verified;
        password;
        uuid;
        created_at;
        updated_at;
        email_verification_uuid;
        active;
        super_user;
        unikernel_updates;
        scaling_policies;
      }
    in
    Object.map make
    |> Object.mem "name" Configuration.label_json ~enc:(fun u -> u.name)
    |> Object.mem "email" Utils.Json.mailbox_json ~enc:(fun u -> u.email)
    |> Object.mem "email_verified" (option Utils.TimeHelper.ptime_json)
         ~enc:(fun u -> u.email_verified)
    |> Object.mem "password" string ~enc:(fun u -> u.password)
    |> Object.mem "uuid" Utils.Json.uuid_json ~enc:(fun u -> u.uuid)
    |> Object.mem "created_at" Utils.TimeHelper.ptime_json ~enc:(fun u ->
        u.created_at)
    |> Object.mem "updated_at" Utils.TimeHelper.ptime_json ~enc:(fun u ->
        u.updated_at)
    |> Object.mem "email_verification_uuid" (option Utils.Json.uuid_json)
         ~enc:(fun u -> u.email_verification_uuid)
    |> Object.mem "active" bool ~enc:(fun u -> u.active)
    |> Object.mem "super_user" bool ~enc:(fun u -> u.super_user)
    |> Object.mem "unikernel_updates" unikernel_updates_map_json ~enc:(fun u ->
        u.unikernel_updates)
    |> Object.mem "scaling_policies" scaling_policies_map_json
         ~dec_absent:Scaling_policy_map.empty ~enc:(fun u -> u.scaling_policies)
    |> Object.finish

  let hash_password ~password ~uuid =
    let hash =
      Digestif.SHA256.(
        to_raw_string (digestv_string [ Uuidm.to_string uuid; "-"; password ]))
    in
    Base64.encode_string hash

  let generate_uuid () =
    let data = Mirage_crypto_rng.generate 16 in
    Uuidm.v4 (Bytes.unsafe_of_string data)

  let generate_cookie ?(expires_in = 3600) created_at user_agent () =
    let id = generate_uuid () in
    {
      value = Base64.encode_string (Uuidm.to_string id);
      expires_in;
      created_at;
      last_access = created_at;
      user_agent;
    }

  let create_user ~name ~email ~password ~created_at ~active ~super_user
      ~user_agent =
    let uuid = generate_uuid () in
    let password = hash_password ~password ~uuid in
    let session = generate_cookie ~expires_in:week created_at user_agent () in
    ( {
        name;
        email;
        email_verified = None;
        password;
        uuid;
        created_at;
        updated_at = created_at;
        email_verification_uuid = None;
        active;
        super_user;
        unikernel_updates = Utils.LM.empty;
        scaling_policies = Scaling_policy_map.empty;
      },
      session )

  let update_user user ?name ?email ?email_verified ?password ?updated_at
      ?email_verification_uuid ?active ?super_user ?unikernel_updates
      ?scaling_policies () =
    {
      user with
      name = Option.value ~default:user.name name;
      email = Option.value ~default:user.email email;
      email_verified = Option.value ~default:user.email_verified email_verified;
      password = Option.value ~default:user.password password;
      updated_at = Option.value ~default:user.updated_at updated_at;
      email_verification_uuid =
        Option.value ~default:user.email_verification_uuid
          email_verification_uuid;
      active = Option.value ~default:user.active active;
      super_user = Option.value ~default:user.super_user super_user;
      unikernel_updates =
        Option.value ~default:user.unikernel_updates unikernel_updates;
      scaling_policies =
        Option.value ~default:user.scaling_policies scaling_policies;
    }

  let is_valid_cookie (cookie : cookie) now =
    Utils.TimeHelper.is_within_seconds ~current_time:now
      ~check_time:cookie.created_at cookie.expires_in

  let is_email_verified user = Option.is_some user.email_verified
  let password_validation password = String.length password >= 8

  let verify_email_token u _uuid timestamp =
    match u with
    | None ->
        Logs.err (fun m -> m "email verification: Token couldn't be found.");
        Error (`Msg "No token was found.")
    | Some u ->
        if
          Utils.TimeHelper.is_within_seconds ~current_time:timestamp
            ~check_time:u.updated_at Csrf.csrf_expiry
        then
          Ok
            (update_user u ~email_verified:(Some timestamp)
               ~updated_at:timestamp ~email_verification_uuid:None ())
        else (
          Logs.err (fun m -> m "email verification: This link is expired.");
          Error
            (`Msg
               "This link has expired. Please sign in to get a new \
                verification link."))

  let login_user ~email ~password ~user_agent user now =
    match user with
    | None -> Error (`Msg "Invalid email or password.")
    | Some u ->
        if not u.active then Error (`Msg "This account is not active")
        else
          let pass = hash_password ~password ~uuid:u.uuid in
          if String.equal u.password pass && Mrmime.Mailbox.equal u.email email
          then
            let new_session =
              generate_cookie ~expires_in:week now user_agent ()
            in
            Ok (u, new_session)
          else Error (`Msg "Invalid email or password.")
end

module Registration = struct
  type t = {
    name : string;
    email : string;
    password : string;
    molly_csrf : string;
  }

  let registration_form =
    let open Jsont in
    let fn name email password molly_csrf =
      { name; email; password; molly_csrf }
    in
    Object.map fn |> Object.mem "name" string |> Object.mem "email" string
    |> Object.mem "password" string
    |> csrf_mem |> Object.finish

  let validate_user_input ~name ~email ~password =
    if name = "" || email = "" || password = "" then
      Error "All fields must be filled."
    else if String.length name < 4 then
      Error "Name must be at least 3 characters long."
    else if not (User.password_validation password) then
      Error "Password must be at least 8 characters long."
    else
      match Configuration.name_of_str name with
      | Error (`Msg err) -> Error err
      | Ok name -> (
          match Email.validate_email email with
          | Error (`Msg _err) -> Error "Invalid email address."
          | Ok email -> Ok (name, email))
end

module Login = struct
  type t = { email : string; password : string; molly_csrf : string }

  let login_form =
    let open Jsont in
    let fn email password molly_csrf = { email; password; molly_csrf } in
    Object.map fn |> Object.mem "email" string
    |> Object.mem "password" string
    |> csrf_mem |> Object.finish
end

module Password = struct
  type t = {
    current_password : string;
    new_password : string;
    confirm_password : string;
    molly_csrf : string;
  }

  let update_password_form =
    let open Jsont in
    let fn current_password new_password confirm_password molly_csrf =
      { current_password; new_password; confirm_password; molly_csrf }
    in
    Object.map fn
    |> Object.mem "current_password" string
    |> Object.mem "new_password" string
    |> Object.mem "confirm_password" string
    |> csrf_mem |> Object.finish
end

module Close_sessions = struct
  type t = { molly_csrf : string }

  let close_sessions_form =
    let open Jsont in
    Object.map (fun molly_csrf -> { molly_csrf }) |> csrf_mem |> Object.finish
end

module Close_session = struct
  type t = { session_value : string; molly_csrf : string }

  let close_session_form =
    single_field_csrf_form "session_value" (fun session_value molly_csrf ->
        { session_value; molly_csrf })
end

module Block = struct
  type delete_t = {
    block_name : string;
    albatross_instance : string;
    molly_csrf : string;
  }

  let delete_form =
    instance_action_form "block_name"
      (fun block_name albatross_instance molly_csrf ->
        { block_name; albatross_instance; molly_csrf })

  type download_t = {
    block_name : string;
    albatross_instance : string;
    compression_level : int;
    molly_csrf : string;
  }

  let download_form =
    let open Jsont in
    let fn block_name albatross_instance compression_level molly_csrf =
      { block_name; albatross_instance; compression_level; molly_csrf }
    in
    Object.map fn
    |> Object.mem "block_name" string
    |> Object.mem "albatross_instance" string
    |> Object.mem "compression_level" int
    |> csrf_mem |> Object.finish

  type block_json = {
    block_name : string;
    block_size : int option;
    block_compressed : bool;
  }

  let block_json_form =
    let open Jsont in
    let fn block_name block_size block_compressed =
      { block_name; block_size; block_compressed }
    in
    Object.map fn
    |> Object.mem "block_name" string
    |> Object.opt_mem "block_size" int
    |> Object.mem "block_compressed" bool
    |> Object.finish

  type upload_t = {
    albatross_instance : string;
    molly_csrf : string;
    json_data : string;
    block_data : string;
  }

  let upload_form =
    let open Vifu.Multipart_form in
    let fn albatross_instance molly_csrf json_data block_data =
      { albatross_instance; molly_csrf; json_data; block_data }
    in
    record fn
    |+ field "albatross_instance" string
    |+ field "molly_csrf" string |+ field "json_data" string
    |+ field "block_data" string |> sealr
end

module Unikernel = struct
  type destroy_t = {
    name : string;
    albatross_instance : string;
    molly_csrf : string;
  }

  let destroy_form =
    instance_action_form "name" (fun name albatross_instance molly_csrf ->
        { name; albatross_instance; molly_csrf })

  type restart_t = { arguments : string option; molly_csrf : string }

  let restart_form =
    let open Jsont in
    let fn arguments molly_csrf = { arguments; molly_csrf } in
    Object.map fn
    |> opt_nullable_mem "arguments" string
    |> csrf_mem |> Object.finish

  type rollback_t = {
    unikernel_name : string;
    albatross_instance : string;
    molly_csrf : string;
  }

  let rollback_form =
    instance_action_form "unikernel_name"
      (fun unikernel_name albatross_instance molly_csrf ->
        { unikernel_name; albatross_instance; molly_csrf })

  type dns_liveliness = {
    dns_address : string option;
    dns_name : string option;
  }

  let dns_liveliness_json =
    let open Jsont in
    let fn dns_address dns_name = { dns_address; dns_name } in
    Object.map fn
    |> opt_nullable_mem "dns_address" string
    |> opt_nullable_mem "dns_name" string
    |> Object.finish

  type update_t = {
    albatross_instance : string;
    job : string;
    to_be_updated_unikernel : string;
    currently_running_unikernel : string;
    unikernel_name : string;
    http_liveliness_address : string option;
    dns_liveliness : dns_liveliness option;
    unikernel_arguments : Jsont.json option;
    molly_csrf : string;
  }

  let update_form =
    let open Jsont in
    let fn albatross_instance job to_be_updated_unikernel
        currently_running_unikernel unikernel_name http_liveliness_address
        dns_liveliness unikernel_arguments molly_csrf =
      {
        albatross_instance;
        job;
        to_be_updated_unikernel;
        currently_running_unikernel;
        unikernel_name;
        http_liveliness_address;
        dns_liveliness;
        unikernel_arguments;
        molly_csrf;
      }
    in
    Object.map fn
    |> Object.mem "albatross_instance" string
    |> Object.mem "job" string
    |> Object.mem "to_be_updated_unikernel" string
    |> Object.mem "currently_running_unikernel" string
    |> Object.mem "unikernel_name" string
    |> opt_nullable_mem "http_liveliness_address" string
    |> opt_nullable_mem "dns_liveliness" dns_liveliness_json
    |> Object.opt_mem "unikernel_arguments" json
    |> csrf_mem |> Object.finish

  type create_t = {
    albatross_instance : string;
    unikernel_name : string;
    unikernel_config : string;
    unikernel_force_create : string;
    molly_csrf : string;
    deploy_mode : string;
    builder_job : string option;
    binary : string option;
  }

  let create_of_parts (parts : Multipart.parts) : (create_t, string) result =
    match
      ( Multipart.find_string "albatross_instance" parts,
        Multipart.find_string "unikernel_name" parts,
        Multipart.find_string "unikernel_config" parts,
        Multipart.find_string "unikernel_force_create" parts,
        Multipart.find_string "molly_csrf" parts,
        Multipart.find_string "deploy_mode" parts )
    with
    | ( Some albatross_instance,
        Some unikernel_name,
        Some unikernel_config,
        Some unikernel_force_create,
        Some molly_csrf,
        Some deploy_mode ) ->
        let builder_job = Multipart.find_string "builder_job" parts in
        let binary = Multipart.find_string "binary" parts in
        Ok
          {
            albatross_instance;
            unikernel_name;
            unikernel_config;
            unikernel_force_create;
            molly_csrf;
            deploy_mode;
            builder_job;
            binary;
          }
    | _ -> Error "One or more required fields are missing."

  type scaling_t = {
    should_scale : bool;
    max_instances : int option;
    molly_csrf : string;
  }

  let scaling_form =
    let open Jsont in
    let fn should_scale max_instances molly_csrf =
      {
        should_scale = Option.value ~default:false should_scale;
        max_instances;
        molly_csrf;
      }
    in
    Object.map fn
    |> opt_nullable_mem "should_scale" bool
    |> opt_nullable_mem "max_instances" int
    |> csrf_mem |> Object.finish

  type monitoring_t = {
    command : string option;
    unikernel_name : string option;
    molly_csrf : string;
  }

  let monitoring_form =
    let open Jsont in
    let fn command unikernel_name molly_csrf =
      { command; unikernel_name; molly_csrf }
    in
    Object.map fn
    |> opt_nullable_mem "command" string
    |> opt_nullable_mem "unikernel_name" string
    |> csrf_mem |> Object.skip_unknown |> Object.finish

  type instance_error = { instance : string; error : string }

  let instance_error_json =
    let open Jsont in
    Object.enc_only ()
    |> Object.mem "instance" string ~enc:(fun (e : instance_error) ->
        e.instance)
    |> Object.mem "error" string ~enc:(fun (e : instance_error) -> e.error)
    |> Object.finish

  type info_response = { data : Jsont.json list; errors : instance_error list }

  let info_response_json =
    let open Jsont in
    Object.enc_only ()
    |> Object.mem "data" (list json) ~enc:(fun (r : info_response) -> r.data)
    |> Object.mem "errors" (list instance_error_json)
         ~enc:(fun (r : info_response) -> r.errors)
    |> Object.finish
end

module Policy = struct
  type t = {
    user_uuid : string;
    albatross_instance : string;
    unikernels : int;
    memory : int;
    block : int;
    cpuids : string;
    bridges : string;
    molly_csrf : string;
  }

  let policy_form =
    let open Jsont in
    let fn user_uuid albatross_instance unikernels memory block cpuids bridges
        molly_csrf =
      {
        user_uuid;
        albatross_instance;
        unikernels;
        memory;
        block;
        cpuids;
        bridges;
        molly_csrf;
      }
    in
    Object.map fn
    |> Object.mem "user_uuid" string
    |> Object.mem "albatross_instance" string
    |> Object.mem "unikernels" int
    |> Object.mem "memory" int |> Object.mem "block" int
    |> Object.mem "cpuids" string
    |> Object.mem "bridges" string
    |> csrf_mem |> Object.finish

  let to_policy (t : t) =
    let policy =
      {
        Vmm_core.Policy.unikernels = t.unikernels;
        memory = t.memory;
        block = (if t.block = 0 then None else Some t.block);
        cpuids =
          (let parsed_cpuids =
             List.filter_map
               (fun s ->
                 match int_of_string_opt s with
                 | Some i -> Some i
                 | None ->
                     Logs.warn (fun m -> m "Ignoring invalid CPU id: %s" s);
                     None)
               (String.split_on_char ',' t.cpuids)
           in
           Vmm_core.IS.of_list parsed_cpuids);
        bridges =
          Vmm_core.String_set.of_list (String.split_on_char ',' t.bridges);
      }
    in
    let ( let* ) = Result.bind in
    let* () = Vmm_core.Policy.usable policy in
    Ok policy

  let policy_info_json =
    let open Jsont in
    let name (n, _) = Vmm_core.Name.to_string n in
    let allowed_unikernels (_, (p : Vmm_core.Policy.t)) = p.unikernels in
    let allowed_cpuids (_, (p : Vmm_core.Policy.t)) =
      Vmm_core.IS.elements p.cpuids
    in
    let allowed_memory (_, (p : Vmm_core.Policy.t)) = p.memory in
    let allowed_block_size (_, (p : Vmm_core.Policy.t)) =
      Option.value ~default:0 p.block
    in
    let allowed_bridges (_, (p : Vmm_core.Policy.t)) =
      Vmm_core.String_set.elements p.bridges
    in
    Object.enc_only ~kind:"PolicyInfo" ()
    |> Object.mem "name" string ~enc:name
    |> Object.mem "allowed_unikernels" int ~enc:allowed_unikernels
    |> Object.mem "allowed_cpuids" (list int) ~enc:allowed_cpuids
    |> Object.mem "allowed_memory" int ~enc:allowed_memory
    |> Object.mem "allowed_block_size" int ~enc:allowed_block_size
    |> Object.mem "allowed_bridges" (list string) ~enc:allowed_bridges
    |> Object.finish
end

module Admin_user = struct
  type t = { uuid : string; molly_csrf : string }

  let user_uuid_form =
    single_field_csrf_form "uuid" (fun uuid molly_csrf -> { uuid; molly_csrf })
end
