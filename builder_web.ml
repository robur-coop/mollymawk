let ( let* ) = Result.bind
let base_url = "https://builds.robur.coop"

type build = {
  job : string;
  uuid : string;
  platform : string;
  start_time : Ptime.t;
  finish_time : Ptime.t;
  main_binary : bool;
  main_binary_size : int option;
}

type package = { name : string; version : string }

type package_version_diff = {
  name : string;
  version_left : string;
  version_right : string;
}

type o_diff = {
  package_version : string;
  effectively_equal : bool;
  diff : string;
}

type duniverse = { name : string; value_ : string }

type package_diff = {
  left_packages : package list;
  right_packages : package list;
  different_versions : package_version_diff list;
}

type duniverse_diff = {
  left : duniverse list;
  right : duniverse list;
  detailed_diff : string list;
}

type opam_diff = {
  opam_diff : o_diff list;
  version_diff : package_version_diff list;
  only_in_left : package list;
  only_in_right : package list;
  duniverse_diff : duniverse_diff;
}

type compare = {
  left : build;
  right : build;
  env_diff : package_diff;
  package_diff : package_diff;
  opam_diff : opam_diff;
}

let list_of_json parse_fn json =
  List.fold_left
    (fun acc item ->
      match (acc, parse_fn item) with
      | Ok lst, Ok parsed -> Ok (parsed :: lst)
      | Error e, _ -> Error e
      | _, Error e -> Error e)
    (Ok []) json
  |> Result.map List.rev

let duniverse_detailed_diff_of_json = function
  | `Assoc xs -> (
      match Utils.Json.(get "name" xs) with
      | Some (`String name) -> Ok name
      | _ ->
          Error
            (`Msg
               ("invalid json for duniverse_detailed_diff: "
               ^ Utils.Json.to_string (`Assoc xs))))
  | js ->
      Error
        (`Msg
           ("invalid json for builder_web duniverse_detailed_diff_of_json: "
          ^ Utils.Json.to_string js))

let duniverse_of_json = function
  | `Assoc xs -> (
      match Utils.Json.(get "name" xs, get "value" xs) with
      | Some (`String name), Some (`String value_) -> Ok { name; value_ }
      | _ ->
          Error
            (`Msg
               ("invalid json for duniverse: "
               ^ Utils.Json.to_string (`Assoc xs))))
  | js ->
      Error
        (`Msg ("invalid json for duniverse_of_json: " ^ Utils.Json.to_string js))

let duniverse_diff_of_json json =
  match
    Utils.Json.(get "left" json, get "right" json, get "detailed_diff" json)
  with
  | ( Some (`List left_json),
      Some (`List right_json),
      Some (`List detailed_diff_json) ) ->
      let* left = list_of_json duniverse_of_json left_json in
      let* right = list_of_json duniverse_of_json right_json in
      let* detailed_diff =
        list_of_json duniverse_detailed_diff_of_json detailed_diff_json
      in
      Ok { left; right; detailed_diff }
  | _ ->
      Error
        (`Msg
           ("invalid json for duniverse_diff_of_json: "
           ^ Utils.Json.to_string (`Assoc json)))

let o_diff_of_json = function
  | `Assoc xs -> (
      match
        Utils.Json.
          (get "package_version" xs, get "effectively_equal" xs, get "diff" xs)
      with
      | ( Some (`String package_version),
          Some (`Bool effectively_equal),
          Some (`String diff) ) ->
          Ok { package_version; effectively_equal; diff }
      | _ ->
          Error
            (`Msg
               ("invalid json for o_diff: " ^ Utils.Json.to_string (`Assoc xs)))
      )
  | js ->
      Error
        (`Msg
           ("invalid json for o_diff list, expected a list: "
          ^ Utils.Json.to_string js))

let package_of_json = function
  | `Assoc xs -> (
      match Utils.Json.(get "name" xs, get "version" xs) with
      | Some (`String name), Some (`String version) -> Ok { name; version }
      | _ ->
          Error
            (`Msg
               ("invalid json for builder_web package_of_json: "
               ^ Utils.Json.to_string (`Assoc xs))))
  | js ->
      Error
        (`Msg
           ("invalid json for builder_web package_of_json, expected a list: "
          ^ Utils.Json.to_string js))

let package_version_of_json = function
  | `Assoc xs -> (
      match
        Utils.Json.(get "name" xs, get "version_left" xs, get "version_right" xs)
      with
      | ( Some (`String name),
          Some (`String version_left),
          Some (`String version_right) ) ->
          Ok { name; version_left; version_right }
      | _ ->
          Error
            (`Msg
               ("invalid json for builder_web package_version_of_json: "
               ^ Utils.Json.to_string (`Assoc xs))))
  | js ->
      Error
        (`Msg
           ("invalid json for builder_web package_version_of_json, expected a \
             list: " ^ Utils.Json.to_string js))

let int_or_none field = function
  | None | Some `Null -> Ok None
  | Some (`Int v) -> Ok (Some v)
  | Some json ->
      Error
        (`Msg ("invalid json for " ^ field ^ ": " ^ Utils.Json.to_string json))

let build_of_json = function
  | `Assoc xs -> (
      match
        Utils.Json.
          ( get "job" xs,
            get "uuid" xs,
            get "platform" xs,
            get "start_time" xs,
            get "finish_time" xs,
            get "main_binary" xs,
            get "main_binary_size" xs )
      with
      | ( Some (`String job),
          Some (`String uuid),
          Some (`String platform),
          Some (`String start_time_str),
          Some (`String finish_time_str),
          Some (`Bool main_binary),
          main_binary_size ) ->
          let* start_time = Utils.TimeHelper.ptime_of_string start_time_str in
          let* finish_time = Utils.TimeHelper.ptime_of_string finish_time_str in
          let* main_binary_size =
            int_or_none "main_binary_size" main_binary_size
          in
          Ok
            {
              job;
              uuid;
              platform;
              start_time;
              finish_time;
              main_binary;
              main_binary_size;
            }
      | _ ->
          Error
            (`Msg
               ("invalid json for builder_web build_of_json: "
               ^ Utils.Json.to_string (`Assoc xs))))
  | js ->
      Error
        (`Msg
           ("invalid json for builder_web build_of_json, expected a dict: "
          ^ Utils.Json.to_string js))

let diff_of_json json =
  match
    Utils.Json.
      ( get "left_packages" json,
        get "right_packages" json,
        get "different_versions" json )
  with
  | ( Some (`List left_packages_json),
      Some (`List right_packages_json),
      Some (`List different_versions_json) ) ->
      let* left_packages = list_of_json package_of_json left_packages_json in
      let* right_packages = list_of_json package_of_json right_packages_json in
      let* different_versions =
        list_of_json package_version_of_json different_versions_json
      in
      Ok { left_packages; right_packages; different_versions }
  | _ ->
      Error
        (`Msg
           ("invalid json for builder_web env/package_diff_of_json: "
           ^ Utils.Json.to_string (`Assoc json)))

let opam_diff_of_json json =
  match
    Utils.Json.
      ( get "opam_diff" json,
        get "version_diff" json,
        get "only_in_left" json,
        get "only_in_right" json,
        get "duniverse_diff" json )
  with
  | ( Some (`List o_diff_json),
      Some (`List version_diff_json),
      Some (`List only_in_left_json),
      Some (`List only_in_right_json),
      Some (`Assoc duniverse_diff_json) ) ->
      let* opam_diff = list_of_json o_diff_of_json o_diff_json in
      let* version_diff =
        list_of_json package_version_of_json version_diff_json
      in
      let* only_in_left = list_of_json package_of_json only_in_left_json in
      let* only_in_right = list_of_json package_of_json only_in_right_json in
      let* duniverse_diff = duniverse_diff_of_json duniverse_diff_json in
      Ok
        { opam_diff; version_diff; only_in_left; only_in_right; duniverse_diff }
  | _ ->
      Error
        (`Msg
           ("invalid json for builder_web: "
           ^ Utils.Json.to_string (`Assoc json)))

let compare_of_json = function
  | `Assoc xs -> (
      match
        Utils.Json.
          ( get "left" xs,
            get "right" xs,
            get "env_diff" xs,
            get "package_diff" xs,
            get "opam_diff" xs )
      with
      | ( Some (`Assoc left_json),
          Some (`Assoc right_json),
          Some (`Assoc env_diff_json),
          Some (`Assoc package_diff_json),
          Some (`Assoc opam_diff_json) ) ->
          let* left = build_of_json (`Assoc left_json) in
          let* right = build_of_json (`Assoc right_json) in
          let* env_diff = diff_of_json env_diff_json in
          let* package_diff = diff_of_json package_diff_json in
          let* opam_diff = opam_diff_of_json opam_diff_json in
          Ok { left; right; env_diff; package_diff; opam_diff }
      | _ ->
          Error
            (`Msg
               ("invalid json for builder_web diff: "
               ^ Utils.Json.to_string (`Assoc xs))))
  | js ->
      Error
        (`Msg
           ("invalid json for builder_web diff, expected a dict: "
          ^ Utils.Json.to_string js))

type device = Network of string | Block of string
type device_manifest = { uuid : string; devices : device list }

let device_of_json = function
  | `Assoc xs -> (
      match (Utils.Json.get "name" xs, Utils.Json.get "type" xs) with
      | Some (`String name), Some (`String "NET_BASIC") -> Ok (Network name)
      | Some (`String name), Some (`String "BLOCK_BASIC") -> Ok (Block name)
      | _ -> Error (`Msg "Invalid device JSON"))
  | _ -> Error (`Msg "Invalid device JSON format")

let manifest_of_json (`Assoc xs) =
  match (Utils.Json.get "uuid" xs, Utils.Json.get "solo5_manifest" xs) with
  | Some (`String uuid), Some (`Assoc manifest) -> (
      match Utils.Json.get "devices" manifest with
      | Some (`List devices) ->
          let req_devices =
            List.fold_left
              (fun acc d ->
                match (acc, device_of_json d) with
                | Ok acc, Ok d -> Ok (d :: acc)
                | Error e, _ -> Error e
                | _, Error e -> Error e)
              (Ok []) devices
            |> Result.map List.rev
          in
          Result.map (fun devices -> { uuid; devices }) req_devices
      | Some _ -> Error (`Msg "solo5_manifest.devices is not a list")
      | None -> Ok { uuid; devices = [] } (* No devices required *))
  | Some (`String uuid), None -> Ok { uuid; devices = [] }
  | _ -> Error (`Msg "Missing uuid or solo5_manifest")

type job = { name : string; synopsis : string; manifest : device_manifest }

let job_of_json = function
  | `Assoc xs -> (
      let* name =
        match Utils.Json.get "name" xs with
        | Some (`String s) -> Ok s
        | _ -> Error (`Msg "No name in the job json or name is not a string")
      in
      let* synopsis =
        match Utils.Json.get "synopsis" xs with
        | Some (`String s) -> Ok s
        | _ ->
            Error
              (`Msg "no synopsis in the job json or synopsis is not a string")
      in
      match Utils.Json.get "latest" xs with
      | Some (`Assoc latest) -> (
          match Utils.Json.get "solo5_abi" latest with
          | Some (`Assoc abi) -> (
              match Utils.Json.get "target" abi with
              | Some (`String "hvt") -> (
                  match manifest_of_json (`Assoc latest) with
                  | Ok manifest -> Ok (Some { name; synopsis; manifest })
                  | Error e -> Error e)
              | _ ->
                  Logs.info (fun i ->
                      i
                        "builder_web.jobs_of_json: no valid target type. \
                         requires hvt unikernels.");
                  Ok None)
          | _ ->
              Logs.info (fun i ->
                  i
                    "builder_web.jobs_of_json: no solo5_abi section in the \
                     json.");
              Ok None)
      | _ ->
          Logs.info (fun i ->
              i "builder_web.jobs_of_json: no latest section in the json.");
          Ok None)
  | _ -> Error (`Msg "Invalid job JSON")

let jobs_of_json = function
  | `Assoc xs -> (
      match Utils.Json.get "jobs" xs with
      | Some (`List jobs_json) ->
          List.fold_left
            (fun acc j ->
              match (acc, job_of_json j) with
              | Ok acc, Ok (Some job) -> Ok (job :: acc)
              | Ok acc, Ok None -> Ok acc
              | Error e, _ -> Error e
              | _, Error e -> Error e)
            (Ok []) jobs_json
          |> Result.map List.rev
      | _ -> Error (`Msg "No jobs array found"))
  | _ -> Error (`Msg "Invalid JSON root")

let fetch_unikernel_jobs http_client =
  let open Lwt.Syntax in
  let* res =
    Utils.Http.send_http_request ~path:"/all-builds" ~base_url http_client
  in
  match res with
  | Ok body -> (
      match Utils.Json.from_string body with
      | Ok json -> (
          match jobs_of_json json with
          | Ok jobs -> Lwt.return jobs
          | Error (`Msg e) ->
              Logs.err (fun m -> m "jobs_of_json: Failed to fetch jobs: %s" e);
              Lwt.return [])
      | Error (`Msg e) ->
          Logs.err (fun m ->
              m "utils.json.from_string: Failed to fetch jobs: %s" e);
          Lwt.return [])
  | Error (`Msg msg) ->
      Logs.err (fun m ->
          m "utils.http.send_http_request: Failed to fetch jobs: %s" msg);
      Lwt.return []

let fetch_unikernel_binary_image http_client ~job ~version push_chunks =
  let open Lwt.Infix in
  let url = base_url ^ "/job/" ^ job ^ "/build/" ^ version ^ "/main-binary" in
  let f resp _acc chunk =
    if Http_mirage_client.Status.is_successful resp.Http_mirage_client.status
    then Lwt.return (push_chunks (Some chunk))
    else Lwt.return_unit
  in
  Lwt.both
    ( Http_mirage_client.request http_client ~follow_redirect:true url f ()
    >>= fun e ->
      push_chunks None;
      match e with
      | Error (`Msg err) -> Lwt.return (Error (`Msg err))
      | Error `Cycle -> Lwt.return (Error (`Msg "returned cycle"))
      | Error `Not_found -> Lwt.return (Error (`Msg "returned not found"))
      | Ok (resp, ()) ->
          if
            Http_mirage_client.Status.is_successful
              resp.Http_mirage_client.status
          then Lwt.return (Ok ())
          else
            Lwt.return
              (Error
                 (`Msg
                    ("accessing " ^ url ^ " resulted in an error: "
                    ^ Http_mirage_client.Status.to_string resp.status
                    ^ " " ^ resp.reason))) )
