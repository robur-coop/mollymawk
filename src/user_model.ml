type token = {
  name : string;
  token_type : string;
  value : string;
  expires_in : int;
      (* the number of seconds until this token is invalid, starts counting from created_at*)
  created_at : Ptime.t;
  last_access : Ptime.t;
  usage_count : int;
}

type cookie = {
  name : string;
  value : string;
  expires_in : int;
  uuid : Uuidm.t option;
  created_at : Ptime.t;
  last_access : Ptime.t;
  user_agent : string option;
}

type unikernel_update = {
  name : Vmm_core.Name.Label.t;
  job : string;
  uuid : string;
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
  tokens : token Utils.SM.t;
  cookies : cookie Utils.SM.t;
  created_at : Ptime.t;
  updated_at : Ptime.t;
  email_verification_uuid : Uuidm.t option;
  active : bool;
  super_user : bool;
  unikernel_updates : unikernel_update Utils.LM.t;
  scaling_policies : unikernel_scaling_policy Scaling_policy_map.t;
}

let week = 604800 (* a week = 7 days * 24 hours * 60 minutes * 60 seconds *)
let session_cookie = "molly_session"
let csrf_cookie = "molly_csrf"

let unikernel_update_to_json (u : unikernel_update) : Yojson.Basic.t =
  `Assoc
    [
      ("name", `String (Configuration.name_to_str u.name));
      ("job", `String u.job);
      ("uuid", `String u.uuid);
      ("config", Albatross_json.config_to_json u.config);
      ("timestamp", `String (Utils.TimeHelper.string_of_ptime u.timestamp));
    ]

let scaling_policy_to_json (p : unikernel_scaling_policy) : Yojson.Basic.t =
  `Assoc
    [
      ("name", `String (Configuration.name_to_str p.name));
      ( "primary_albatross_instance",
        `String (Configuration.name_to_str p.primary_albatross_instance) );
      ("max_instances", `Int p.max_instances);
    ]

let ( let* ) = Result.bind

let scaling_policy_of_json = function
  | `Assoc xs -> (
      match
        Utils.Json.
          ( get "name" xs,
            get "primary_albatross_instance" xs,
            get "max_instances" xs )
      with
      | ( Some (`String name),
          Some (`String primary_albatross_instance),
          Some (`Int max_instances) ) ->
          let* name = Configuration.name_of_str name in
          let* primary_albatross_instance =
            Configuration.name_of_str primary_albatross_instance
          in
          Ok { name; primary_albatross_instance; max_instances }
      | _ ->
          Error
            (`Msg
               ("Invalid JSON for scaling policy: requires name, primary \
                 albatross instance, max_instances but got: "
               ^ Utils.Json.to_string (`Assoc xs))))
  | js ->
      Error
        (`Msg
           ("Invalid JSON for scaling policy: expected a dictionary, got: "
          ^ Utils.Json.to_string js))

let unikernel_update_of_json = function
  | `Assoc xs -> (
      match
        ( Utils.Json.get "name" xs,
          Utils.Json.get "job" xs,
          Utils.Json.get "uuid" xs,
          Utils.Json.get "config" xs,
          Utils.Json.get "timestamp" xs )
      with
      | ( Some (`String name),
          Some (`String job),
          Some (`String uuid),
          Some config,
          Some (`String timestamp_str) ) ->
          let* timestamp = Utils.TimeHelper.ptime_of_string timestamp_str in
          let* config =
            Albatross_json.config_of_json (Yojson.Basic.to_string config)
          in
          let* name = Configuration.name_of_str name in
          Ok { name; job; uuid; config; timestamp }
      | _ ->
          Error
            (`Msg
               ("Invalid JSON for unikernel_update: requires name, job, uuid, \
                 config and timestamp but got: "
               ^ Utils.Json.to_string (`Assoc xs))))
  | js ->
      Error
        (`Msg
           ("Invalid JSON for unikernel_update: expected a dictionary, got: "
          ^ Utils.Json.to_string js))

let cookie_to_json (cookie : cookie) =
  `Assoc
    [
      ("name", `String cookie.name);
      ( "created_at",
        `String (Utils.TimeHelper.string_of_ptime cookie.created_at) );
      ("value", `String cookie.value);
      ("expires_in", `Int cookie.expires_in);
      ( "uuid",
        match cookie.uuid with
        | Some uuid -> `String (Uuidm.to_string uuid)
        | None -> `Null );
      ( "last_access",
        `String (Utils.TimeHelper.string_of_ptime cookie.last_access) );
      ( "user_agent",
        match cookie.user_agent with
        | Some agent -> `String agent
        | None -> `Null );
    ]

let cookie_of_json = function
  | `Assoc xs -> (
      match
        Utils.Json.
          ( get "name" xs,
            get "value" xs,
            get "expires_in" xs,
            get "uuid" xs,
            get "created_at" xs,
            get "last_access" xs,
            get "user_agent" xs )
      with
      | ( Some (`String name),
          Some (`String value),
          Some (`Int expires_in),
          uuid,
          Some (`String created_at_str),
          Some (`String last_access_str),
          user_agent ) ->
          let created_at =
            match Utils.TimeHelper.ptime_of_string created_at_str with
            | Ok ptime -> ptime
            | Error (`Msg msg) ->
                Logs.warn (fun m ->
                    m "couldn't parse created_at %s: value %S" msg
                      created_at_str);
                Ptime.epoch
          in
          let last_access =
            match Utils.TimeHelper.ptime_of_string last_access_str with
            | Ok ptime -> ptime
            | Error (`Msg msg) ->
                Logs.warn (fun m ->
                    m "couldn't parse last_access %s: value %S" msg
                      last_access_str);
                created_at
          in
          let* uuid =
            match uuid with
            | None | Some `Null -> Ok None
            | Some (`String s) -> (
                match Uuidm.of_string s with
                | Some u -> Ok (Some u)
                | None -> Error (`Msg ("invalid cookie UUID: " ^ s)))
            | Some js ->
                Error
                  (`Msg
                     ("invalid json for cookie uuid: " ^ Utils.Json.to_string js))
          in
          let* user_agent = Utils.Json.string_or_none "user-agent" user_agent in
          Ok
            {
              name;
              value;
              expires_in;
              uuid;
              created_at;
              last_access;
              user_agent;
            }
      | _ ->
          Error
            (`Msg
               ("invalid json for cookie: " ^ Utils.Json.to_string (`Assoc xs)))
      )
  | js ->
      Error
        (`Msg
           ("invalid json for cookie, expected a dict: "
          ^ Utils.Json.to_string js))

let token_to_json t =
  `Assoc
    [
      ("token_type", `String t.token_type);
      ("value", `String t.value);
      ("expires_in", `Int t.expires_in);
      ("created_at", `String (Utils.TimeHelper.string_of_ptime t.created_at));
      ("last_access", `String (Utils.TimeHelper.string_of_ptime t.last_access));
      ("name", `String t.name);
      ("usage_count", `Int t.usage_count);
    ]

let token_of_json = function
  | `Assoc xs -> (
      match
        Utils.Json.
          ( get "token_type" xs,
            get "value" xs,
            get "expires_in" xs,
            get "created_at" xs,
            get "last_access" xs,
            get "name" xs,
            get "usage_count" xs )
      with
      | ( Some (`String token_type),
          Some (`String value),
          Some (`Int expires_in),
          Some (`String created_at_str),
          Some (`String last_access_str),
          Some (`String name),
          Some (`Int usage_count) ) ->
          let created_at =
            match Utils.TimeHelper.ptime_of_string created_at_str with
            | Ok ptime -> Some ptime
            | Error _ -> None
          in
          let last_access =
            match Utils.TimeHelper.ptime_of_string last_access_str with
            | Ok ptime -> Some ptime
            | Error _ -> None
          in
          Ok
            {
              token_type;
              value;
              expires_in;
              created_at = Option.get created_at;
              last_access = Option.get last_access;
              name;
              usage_count;
            }
      | _ ->
          Error
            (`Msg
               ("invalid json for token: requires token_type, value, and \
                 expires_in: "
               ^ Utils.Json.to_string (`Assoc xs))))
  | js ->
      Error
        (`Msg
           ("invalid json for token: expected a dict: "
          ^ Utils.Json.to_string js))

let user_to_json (u : user) =
  `Assoc
    [
      ("name", `String (Configuration.name_to_str u.name));
      ("email", `String (Emile.to_string u.email));
      ("email_verified", Utils.TimeHelper.ptime_to_json u.email_verified);
      ("password", `String u.password);
      ("uuid", `String (Uuidm.to_string u.uuid));
      ( "tokens",
        `List
          (Utils.SM.fold (fun _ t acc -> token_to_json t :: acc) u.tokens []) );
      ( "cookies",
        `List
          (Utils.SM.fold (fun _ c acc -> cookie_to_json c :: acc) u.cookies [])
      );
      ("created_at", `String (Utils.TimeHelper.string_of_ptime u.created_at));
      ("updated_at", `String (Utils.TimeHelper.string_of_ptime u.updated_at));
      ( "email_verification_uuid",
        match u.email_verification_uuid with
        | None -> `Null
        | Some s -> `String (Uuidm.to_string s) );
      ("active", `Bool u.active);
      ("super_user", `Bool u.super_user);
      ( "unikernel_updates",
        `List
          (Utils.LM.fold
             (fun _ uu acc -> unikernel_update_to_json uu :: acc)
             u.unikernel_updates []) );
      ( "scaling_policies",
        `List
          (Scaling_policy_map.fold
             (fun _ sp acc -> scaling_policy_to_json sp :: acc)
             u.scaling_policies []) );
    ]

let user_v9_of_json cookie_fn = function
  | `Assoc xs -> (
      match
        Utils.Json.
          ( get "name" xs,
            get "email" xs,
            get "email_verified" xs,
            get "password" xs,
            get "uuid" xs,
            get "tokens" xs,
            get "cookies" xs,
            get "created_at" xs,
            get "updated_at" xs,
            get "email_verification_uuid" xs,
            get "active" xs,
            get "super_user" xs,
            get "unikernel_updates" xs )
      with
      | ( Some (`String name),
          Some (`String email),
          Some email_verified,
          Some (`String password),
          Some (`String uuid),
          Some (`List tokens),
          Some (`List cookies),
          Some (`String updated_at_str),
          Some (`String created_at_str),
          Some email_verification_uuid,
          Some (`Bool active),
          Some (`Bool super_user),
          Some (`List unikernel_updates) ) ->
          let* uuid =
            Option.to_result
              ~none:(`Msg ("invalid UUID for user: " ^ uuid))
              (Uuidm.of_string uuid)
          in
          let created_at =
            match Utils.TimeHelper.ptime_of_string created_at_str with
            | Ok ptime -> Some ptime
            | Error _ -> None
          in
          let updated_at =
            match Utils.TimeHelper.ptime_of_string updated_at_str with
            | Ok ptime -> Some ptime
            | Error _ -> None
          in
          let* email_verified = Utils.TimeHelper.ptime_of_json email_verified in
          let* tokens =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* token = token_of_json js in
                Ok (Utils.SM.add token.value token acc))
              (Ok Utils.SM.empty) tokens
          in
          let* cookies =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* cookie = cookie_fn js in
                Ok (Utils.SM.add cookie.value cookie acc))
              (Ok Utils.SM.empty) cookies
          in
          let* email_verification_uuid =
            match email_verification_uuid with
            | `Null -> Ok None
            | `String s ->
                let* uuid =
                  Option.to_result
                    ~none:
                      (`Msg ("invalid UUID for email verification UUID: " ^ s))
                    (Uuidm.of_string s)
                in
                Ok (Some uuid)
            | js ->
                Error
                  (`Msg
                     ("invalid json data for email verification UUID, expected \
                       a string: " ^ Utils.Json.to_string js))
          in
          let* unikernel_updates =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* unikernel_update = unikernel_update_of_json js in
                Ok (Utils.LM.add unikernel_update.name unikernel_update acc))
              (Ok Utils.LM.empty) unikernel_updates
          in
          let* name = Configuration.name_of_str name in
          let* email = Mrmime.Mailbox.of_string email in
          Ok
            {
              name;
              email;
              email_verified;
              password;
              uuid;
              tokens;
              cookies;
              created_at = Option.get created_at;
              updated_at = Option.get updated_at;
              email_verification_uuid;
              active;
              super_user;
              unikernel_updates;
              scaling_policies = Scaling_policy_map.empty;
            }
      | _ ->
          Error
            (`Msg ("invalid json for user: " ^ Utils.Json.to_string (`Assoc xs)))
      )
  | js ->
      Error
        (`Msg
           ("invalid json for user, expected a dict: " ^ Utils.Json.to_string js))

let user_of_json cookie_fn = function
  | `Assoc xs -> (
      match
        Utils.Json.
          ( get "name" xs,
            get "email" xs,
            get "email_verified" xs,
            get "password" xs,
            get "uuid" xs,
            get "tokens" xs,
            get "cookies" xs,
            get "created_at" xs,
            get "updated_at" xs,
            get "email_verification_uuid" xs,
            get "active" xs,
            get "super_user" xs,
            get "unikernel_updates" xs,
            get "scaling_policies" xs )
      with
      | ( Some (`String name),
          Some (`String email),
          Some email_verified,
          Some (`String password),
          Some (`String uuid),
          Some (`List tokens),
          Some (`List cookies),
          Some (`String updated_at_str),
          Some (`String created_at_str),
          Some email_verification_uuid,
          Some (`Bool active),
          Some (`Bool super_user),
          Some (`List unikernel_updates),
          Some (`List scaling_policies) ) ->
          let* uuid =
            Option.to_result
              ~none:(`Msg ("invalid UUID for user: " ^ uuid))
              (Uuidm.of_string uuid)
          in
          let created_at =
            match Utils.TimeHelper.ptime_of_string created_at_str with
            | Ok ptime -> Some ptime
            | Error _ -> None
          in
          let updated_at =
            match Utils.TimeHelper.ptime_of_string updated_at_str with
            | Ok ptime -> Some ptime
            | Error _ -> None
          in
          let* email_verified = Utils.TimeHelper.ptime_of_json email_verified in
          let* tokens =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* token = token_of_json js in
                Ok (Utils.SM.add token.value token acc))
              (Ok Utils.SM.empty) tokens
          in
          let* cookies =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* cookie = cookie_fn js in
                Ok (Utils.SM.add cookie.value cookie acc))
              (Ok Utils.SM.empty) cookies
          in
          let* email_verification_uuid =
            match email_verification_uuid with
            | `Null -> Ok None
            | `String s ->
                let* uuid =
                  Option.to_result
                    ~none:
                      (`Msg ("invalid UUID for email verification UUID: " ^ s))
                    (Uuidm.of_string s)
                in
                Ok (Some uuid)
            | js ->
                Error
                  (`Msg
                     ("invalid json data for email verification UUID, expected \
                       a string: " ^ Utils.Json.to_string js))
          in
          let* unikernel_updates =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* unikernel_update = unikernel_update_of_json js in
                Ok (Utils.LM.add unikernel_update.name unikernel_update acc))
              (Ok Utils.LM.empty) unikernel_updates
          in
          let* name = Configuration.name_of_str name in
          let* email = Mrmime.Mailbox.of_string email in
          let* scaling_policies =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* scaling_policy = scaling_policy_of_json js in
                Ok
                  (Scaling_policy_map.add
                     ( scaling_policy.name,
                       scaling_policy.primary_albatross_instance )
                     scaling_policy acc))
              (Ok Scaling_policy_map.empty) scaling_policies
          in
          Ok
            {
              name;
              email;
              email_verified;
              password;
              uuid;
              tokens;
              cookies;
              created_at = Option.get created_at;
              updated_at = Option.get updated_at;
              email_verification_uuid;
              active;
              super_user;
              unikernel_updates;
              scaling_policies;
            }
      | _ ->
          Error
            (`Msg ("invalid json for user: " ^ Utils.Json.to_string (`Assoc xs)))
      )
  | js ->
      Error
        (`Msg
           ("invalid json for user, expected a dict: " ^ Utils.Json.to_string js))

let hash_password ~password ~uuid =
  let hash =
    Digestif.SHA256.(
      to_raw_string (digestv_string [ Uuidm.to_string uuid; "-"; password ]))
  in
  Base64.encode_string hash

let generate_uuid () =
  let data = Mirage_crypto_rng.generate 16 in
  Uuidm.v4 (Bytes.unsafe_of_string data)

let generate_cookie ~name ~uuid ?(expires_in = 3600) ~created_at ~user_agent ()
    =
  let id = generate_uuid () in
  {
    name;
    value = Base64.encode_string (Uuidm.to_string id);
    expires_in;
    uuid = Some uuid;
    created_at;
    last_access = created_at;
    user_agent;
  }

let generate_token ~name ~expiry ~current_time =
  let value = generate_uuid () in
  {
    name;
    token_type = "Bearer";
    value = Uuidm.to_string value;
    expires_in = expiry;
    created_at = current_time;
    last_access = current_time;
    usage_count = 0;
  }

let create_user ~name ~email ~password ~created_at ~active ~super_user
    ~user_agent =
  let uuid = generate_uuid () in
  let password = hash_password ~password ~uuid in
  let session =
    generate_cookie ~name:session_cookie ~expires_in:week ~uuid ~created_at
      ~user_agent ()
  in
  ( {
      name;
      email;
      email_verified = None;
      password;
      uuid;
      tokens = Utils.SM.empty;
      cookies = Utils.SM.singleton session.value session;
      created_at;
      updated_at = created_at;
      email_verification_uuid = None;
      active;
      super_user;
      unikernel_updates = Utils.LM.empty;
      scaling_policies = Scaling_policy_map.empty;
    },
    session )

let update_user user ?name ?email ?email_verified ?password ?tokens ?cookies
    ?updated_at ?email_verification_uuid ?active ?super_user ?unikernel_updates
    ?scaling_policies () =
  {
    user with
    name = Option.value ~default:user.name name;
    email = Option.value ~default:user.email email;
    email_verified = Option.value ~default:user.email_verified email_verified;
    password = Option.value ~default:user.password password;
    tokens = Option.value ~default:user.tokens tokens;
    cookies = Option.value ~default:user.cookies cookies;
    updated_at = Option.value ~default:user.updated_at updated_at;
    email_verification_uuid =
      Option.value ~default:user.email_verification_uuid email_verification_uuid;
    active = Option.value ~default:user.active active;
    super_user = Option.value ~default:user.super_user super_user;
    unikernel_updates =
      Option.value ~default:user.unikernel_updates unikernel_updates;
    scaling_policies =
      Option.value ~default:user.scaling_policies scaling_policies;
  }

let is_valid_cookie (cookie : cookie) now =
  Utils.TimeHelper.diff_in_seconds ~current_time:now
    ~check_time:cookie.created_at
  < cookie.expires_in

let is_valid_token (token : token) now =
  Utils.TimeHelper.diff_in_seconds ~current_time:now
    ~check_time:token.created_at
  < token.expires_in

let is_email_verified user = Option.is_some user.email_verified
let password_validation password = String.length password >= 8

let verify_email_token u _uuid timestamp =
  match u with
  | None ->
      Logs.err (fun m -> m "email verification: Token couldn't be found.");
      Error (`Msg "No token was found.")
  | Some u -> (
      match
        Utils.TimeHelper.diff_in_seconds ~current_time:timestamp
          ~check_time:u.updated_at
        < 3600
      with
      | true ->
          let updated_user =
            update_user u ~email_verified:(Some timestamp) ~updated_at:timestamp
              ~email_verification_uuid:None ()
          in
          Ok updated_user
      | false ->
          Logs.err (fun m -> m "email verification: This link is expired.");
          Error
            (`Msg
               "This link has expired. Please sign in to get a new \
                verification link."))

let user_session_cookie (user : user) cookie_value =
  match Utils.SM.find_opt cookie_value user.cookies with
  | Some cookie when String.equal cookie.name session_cookie -> Some cookie
  | _ -> None

let user_csrf_token (user : user) cookie_value =
  match Utils.SM.find_opt cookie_value user.cookies with
  | Some cookie when String.equal cookie.name csrf_cookie -> Some cookie
  | _ -> None

let keep_session_cookies user =
  Utils.SM.filter
    (fun _ (cookie : cookie) -> String.equal cookie.name session_cookie)
    user.cookies

let login_user ~email ~password ~user_agent user now =
  match user with
  | None -> Error (`Msg "Invalid email or password.")
  | Some u -> (
      if not u.active then
        (* TODO move to a middleware, provide instructions how to reactive an account *)
        Error (`Msg "This account is not active")
      else
        let pass = hash_password ~password ~uuid:u.uuid in
        match
          String.equal u.password pass && Mrmime.Mailbox.equal u.email email
        with
        | true ->
            let new_session =
              generate_cookie ~name:session_cookie ~expires_in:week ~uuid:u.uuid
                ~created_at:now ~user_agent ()
            in
            let cookies =
              Utils.SM.add new_session.value new_session
                (keep_session_cookies u)
            in
            let updated_user = update_user u ~cookies () in
            Ok (updated_user, new_session)
        | false -> Error (`Msg "Invalid email or password."))
(* Invalid email or password is a trick error message to at least prevent malicious users from guessing login details :).*)
