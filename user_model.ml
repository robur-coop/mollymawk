module Rng = Mirage_crypto_rng

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
  uuid : string option;
  created_at : Ptime.t;
  last_access : Ptime.t;
  user_agent : string option;
}

type unikernel_update = {
  name : string;
  job : string;
  uuid : string;
  config : Vmm_core.Unikernel.config;
  timestamp : Ptime.t;
}

type user = {
  name : Vmm_core.Name.label;
  email : string;
  email_verified : Ptime.t option;
  password : string;
  uuid : string;
  tokens : token list;
  cookies : cookie list;
  created_at : Ptime.t;
  updated_at : Ptime.t;
  email_verification_uuid : Uuidm.t option;
  active : bool;
  super_user : bool;
  unikernel_updates : unikernel_update list;
}

let week = 604800 (* a week = 7 days * 24 hours * 60 minutes * 60 seconds *)
let session_cookie = "molly_session"
let csrf_cookie = "molly_csrf"

let unikernel_update_to_json (u : unikernel_update) : Yojson.Basic.t =
  `Assoc
    [
      ("name", `String u.name);
      ("job", `String u.job);
      ("uuid", `String u.uuid);
      ("config", Albatross_json.config_to_json u.config);
      ("timestamp", `String (Utils.TimeHelper.string_of_ptime u.timestamp));
    ]

let ( let* ) = Result.bind

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
          Ok { name; job; uuid; config; timestamp }
      | _ ->
          Error
            (`Msg
               ("Invalid JSON for unikernel_update: requires name, job, uuid, \
                 config and timestamp but got: "
               ^ Yojson.Basic.to_string (`Assoc xs))))
  | js ->
      Error
        (`Msg
           ("Invalid JSON for unikernel_update: expected a dictionary, got: "
          ^ Yojson.Basic.to_string js))

let cookie_to_json (cookie : cookie) =
  `Assoc
    [
      ("name", `String cookie.name);
      ( "created_at",
        `String (Utils.TimeHelper.string_of_ptime cookie.created_at) );
      ("value", `String cookie.value);
      ("expires_in", `Int cookie.expires_in);
      ( "uuid",
        match cookie.uuid with Some uuid -> `String uuid | None -> `Null );
      ( "last_access",
        `String (Utils.TimeHelper.string_of_ptime cookie.last_access) );
      ( "user_agent",
        match cookie.user_agent with
        | Some agent -> `String agent
        | None -> `Null );
    ]

let cookie_v1_of_json = function
  | `Assoc xs -> (
      match
        Utils.Json.
          ( get "name" xs,
            get "value" xs,
            get "expires_in" xs,
            get "uuid" xs,
            get "created_at" xs )
      with
      | ( Some (`String name),
          Some (`String value),
          Some (`Int expires_in),
          uuid,
          Some (`String created_at_str) ) ->
          let created_at =
            match Utils.TimeHelper.ptime_of_string created_at_str with
            | Ok ptime -> ptime
            | Error (`Msg msg) ->
                Logs.warn (fun m ->
                    m "couldn't parse created_at %s: value %S" msg
                      created_at_str);
                Ptime.epoch
          in
          let* uuid = Utils.Json.string_or_none "uuid" uuid in
          Ok
            {
              name;
              value;
              expires_in;
              uuid;
              created_at;
              last_access = created_at;
              user_agent = None;
            }
      | _ ->
          Error
            (`Msg
               ("invalid json for cookie: " ^ Yojson.Basic.to_string (`Assoc xs)))
      )
  | js ->
      Error
        (`Msg
           ("invalid json for cookie, expected a dict: "
          ^ Yojson.Basic.to_string js))

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
          let* uuid = Utils.Json.string_or_none "uuid" uuid in
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
               ("invalid json for cookie: " ^ Yojson.Basic.to_string (`Assoc xs)))
      )
  | js ->
      Error
        (`Msg
           ("invalid json for cookie, expected a dict: "
          ^ Yojson.Basic.to_string js))

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

let token_v1_of_json = function
  | `Assoc xs -> (
      match
        Utils.Json.
          ( get "token_type" xs,
            get "value" xs,
            get "expires_in" xs,
            get "created_at" xs )
      with
      | ( Some (`String token_type),
          Some (`String value),
          Some (`Int expires_in),
          Some (`String created_at_str) ) ->
          let created_at =
            match Utils.TimeHelper.ptime_of_string created_at_str with
            | Ok ptime -> Some ptime
            | Error _ -> None
          in
          Ok
            {
              token_type;
              value;
              expires_in;
              created_at = Option.get created_at;
              last_access = Option.get created_at;
              name = "";
              usage_count = 0;
            }
      | _ ->
          Error
            (`Msg
               ("invalid json for token: requires token_type, value, and \
                 expires_in: "
               ^ Yojson.Basic.to_string (`Assoc xs))))
  | js ->
      Error
        (`Msg
           ("invalid json for token: expected a dict: "
          ^ Yojson.Basic.to_string js))

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
               ^ Yojson.Basic.to_string (`Assoc xs))))
  | js ->
      Error
        (`Msg
           ("invalid json for token: expected a dict: "
          ^ Yojson.Basic.to_string js))

let user_to_json (u : user) =
  `Assoc
    [
      ("name", `String (Vmm_core.Name.string_of_label u.name));
      ("email", `String u.email);
      ("email_verified", Utils.TimeHelper.ptime_to_json u.email_verified);
      ("password", `String u.password);
      ("uuid", `String u.uuid);
      ("tokens", `List (List.map token_to_json u.tokens));
      ("cookies", `List (List.map cookie_to_json u.cookies));
      ("created_at", `String (Utils.TimeHelper.string_of_ptime u.created_at));
      ("updated_at", `String (Utils.TimeHelper.string_of_ptime u.updated_at));
      ( "email_verification_uuid",
        match u.email_verification_uuid with
        | None -> `Null
        | Some s -> `String (Uuidm.to_string s) );
      ("active", `Bool u.active);
      ("super_user", `Bool u.super_user);
      ( "unikernel_updates",
        `List (List.map unikernel_update_to_json u.unikernel_updates) );
    ]

let user_v1_of_json = function
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
            get "email_verification_uuid" xs )
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
          Some email_verification_uuid ) ->
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
                let* token = token_v1_of_json js in
                Ok (token :: acc))
              (Ok []) tokens
          in
          let* cookies =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* cookie = cookie_v1_of_json js in
                Ok (cookie :: acc))
              (Ok []) cookies
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
                       a string: " ^ Yojson.Basic.to_string js))
          in
          let* name = Vmm_core.Name.label_of_string name in
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
              active = true;
              super_user = true;
              unikernel_updates = [];
            }
      | _ ->
          Error
            (`Msg
               ("invalid json for user: " ^ Yojson.Basic.to_string (`Assoc xs)))
      )
  | js ->
      Error
        (`Msg
           ("invalid json for user, expected a dict: "
          ^ Yojson.Basic.to_string js))

let user_v2_of_json = function
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
            get "active" xs )
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
          Some (`Bool active) ) ->
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
                let* token = token_v1_of_json js in
                Ok (token :: acc))
              (Ok []) tokens
          in
          let* cookies =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* cookie = cookie_v1_of_json js in
                Ok (cookie :: acc))
              (Ok []) cookies
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
                       a string: " ^ Yojson.Basic.to_string js))
          in
          let* name = Vmm_core.Name.label_of_string name in
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
              super_user = true;
              unikernel_updates = [];
            }
      | _ ->
          Error
            (`Msg
               ("invalid json for user: " ^ Yojson.Basic.to_string (`Assoc xs)))
      )
  | js ->
      Error
        (`Msg
           ("invalid json for user, expected a dict: "
          ^ Yojson.Basic.to_string js))

let user_v3_of_json cookie_fn = function
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
            get "super_user" xs )
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
          Some (`Bool super_user) ) ->
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
                let* token = token_v1_of_json js in
                Ok (token :: acc))
              (Ok []) tokens
          in
          let* cookies =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* cookie = cookie_fn js in
                Ok (cookie :: acc))
              (Ok []) cookies
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
                       a string: " ^ Yojson.Basic.to_string js))
          in
          let* name = Vmm_core.Name.label_of_string name in
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
              unikernel_updates = [];
            }
      | _ ->
          Error
            (`Msg
               ("invalid json for user: " ^ Yojson.Basic.to_string (`Assoc xs)))
      )
  | js ->
      Error
        (`Msg
           ("invalid json for user, expected a dict: "
          ^ Yojson.Basic.to_string js))

let user_v4_of_json cookie_fn = function
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
            get "super_user" xs )
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
          Some (`Bool super_user) ) ->
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
                Ok (token :: acc))
              (Ok []) tokens
          in
          let* cookies =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* cookie = cookie_fn js in
                Ok (cookie :: acc))
              (Ok []) cookies
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
                       a string: " ^ Yojson.Basic.to_string js))
          in
          let* name = Vmm_core.Name.label_of_string name in
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
              unikernel_updates = [];
            }
      | _ ->
          Error
            (`Msg
               ("invalid json for user: " ^ Yojson.Basic.to_string (`Assoc xs)))
      )
  | js ->
      Error
        (`Msg
           ("invalid json for user, expected a dict: "
          ^ Yojson.Basic.to_string js))

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
                Ok (token :: acc))
              (Ok []) tokens
          in
          let* cookies =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* cookie = cookie_fn js in
                Ok (cookie :: acc))
              (Ok []) cookies
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
                       a string: " ^ Yojson.Basic.to_string js))
          in
          let* unikernel_updates =
            List.fold_left
              (fun acc js ->
                let* acc = acc in
                let* unikernel_update = unikernel_update_of_json js in
                Ok (unikernel_update :: acc))
              (Ok []) unikernel_updates
          in
          let* name = Vmm_core.Name.label_of_string name in
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
            }
      | _ ->
          Error
            (`Msg
               ("invalid json for user: " ^ Yojson.Basic.to_string (`Assoc xs)))
      )
  | js ->
      Error
        (`Msg
           ("invalid json for user, expected a dict: "
          ^ Yojson.Basic.to_string js))

let hash_password ~password ~uuid =
  let hash =
    Digestif.SHA256.(to_raw_string (digestv_string [ uuid; "-"; password ]))
  in
  Base64.encode_string hash

let generate_uuid () =
  let data = Rng.generate 16 in
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
  let uuid = Uuidm.to_string (generate_uuid ()) in
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
      tokens = [];
      cookies = [ session ];
      created_at;
      updated_at = created_at;
      email_verification_uuid = None;
      active;
      super_user;
      unikernel_updates = [];
    },
    session )

let update_user user ?name ?email ?email_verified ?password ?tokens ?cookies
    ?updated_at ?email_verification_uuid ?active ?super_user ?unikernel_updates
    () =
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
  List.find_opt
    (fun (cookie : cookie) ->
      String.equal cookie.name session_cookie
      && String.equal cookie_value cookie.value)
    user.cookies

let user_csrf_token (user : user) cookie_value =
  List.find_opt
    (fun (cookie : cookie) ->
      String.equal cookie.name csrf_cookie
      && String.equal cookie_value cookie.value)
    user.cookies

let keep_session_cookies user =
  List.filter
    (fun (cookie : cookie) -> String.equal cookie.name session_cookie)
    user.cookies

let login_user ~email ~password ~user_agent user now =
  match user with
  | None -> Error (`Msg "This account does not exist.")
  | Some u -> (
      if not u.active then
        (* TODO move to a middleware, provide instructions how to reactive an account *)
        Error (`Msg "This account is not active")
      else
        let pass = hash_password ~password ~uuid:u.uuid in
        match String.equal u.password pass && String.equal u.email email with
        | true ->
            let new_session =
              generate_cookie ~name:session_cookie ~expires_in:week ~uuid:u.uuid
                ~created_at:now ~user_agent ()
            in
            let cookies = new_session :: keep_session_cookies u in
            let updated_user = update_user u ~cookies () in
            Ok (updated_user, new_session)
        | false -> Error (`Msg "Invalid email or password."))
(* Invalid email or password is a trick error message to at least prevent malicious users from guessing login details :).*)
