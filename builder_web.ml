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

let ptime_jsont =
  Jsont.of_of_string ~kind:"RFC3339 timestamp"
    ~enc:Utils.TimeHelper.string_of_ptime (fun s ->
      Utils.TimeHelper.ptime_of_string s |> Result.map_error (fun (`Msg e) -> e))

let duniverse_detailed_diff_jsont =
  let open Jsont in
  Object.map Fun.id |> Object.mem "name" string |> Object.finish

let duniverse_jsont =
  let open Jsont in
  Object.map (fun name value_ -> { name; value_ })
  |> Object.mem "name" string |> Object.mem "value" string |> Object.finish

let duniverse_diff_jsont =
  let open Jsont in
  Object.map (fun left right detailed_diff -> { left; right; detailed_diff })
  |> Object.mem "left" (list duniverse_jsont)
  |> Object.mem "right" (list duniverse_jsont)
  |> Object.mem "detailed_diff" (list duniverse_detailed_diff_jsont)
  |> Object.finish

let o_diff_jsont =
  let open Jsont in
  Object.map (fun package_version effectively_equal diff ->
      { package_version; effectively_equal; diff })
  |> Object.mem "package_version" string
  |> Object.mem "effectively_equal" bool
  |> Object.mem "diff" string |> Object.finish

let package_jsont =
  let open Jsont in
  Object.map (fun name version -> { name; version })
  |> Object.mem "name" string
  |> Object.mem "version" string
  |> Object.finish

let package_version_jsont =
  let open Jsont in
  Object.map (fun name version_left version_right ->
      { name; version_left; version_right })
  |> Object.mem "name" string
  |> Object.mem "version_left" string
  |> Object.mem "version_right" string
  |> Object.finish

let build_jsont =
  let open Jsont in
  Object.map
    (fun
      job uuid platform start_time finish_time main_binary main_binary_size ->
      {
        job;
        uuid;
        platform;
        start_time;
        finish_time;
        main_binary;
        main_binary_size = Option.join main_binary_size;
      })
  |> Object.mem "job" string |> Object.mem "uuid" string
  |> Object.mem "platform" string
  |> Object.mem "start_time" ptime_jsont
  |> Object.mem "finish_time" ptime_jsont
  |> Object.mem "main_binary" bool
  |> Object.opt_mem "main_binary_size" (option int)
  |> Object.finish

let diff_jsont =
  let open Jsont in
  Object.map (fun left_packages right_packages different_versions ->
      { left_packages; right_packages; different_versions })
  |> Object.mem "left_packages" (list package_jsont)
  |> Object.mem "right_packages" (list package_jsont)
  |> Object.mem "different_versions" (list package_version_jsont)
  |> Object.finish

let opam_diff_jsont =
  let open Jsont in
  Object.map
    (fun opam_diff version_diff only_in_left only_in_right duniverse_diff ->
      { opam_diff; version_diff; only_in_left; only_in_right; duniverse_diff })
  |> Object.mem "opam_diff" (list o_diff_jsont)
  |> Object.mem "version_diff" (list package_version_jsont)
  |> Object.mem "only_in_left" (list package_jsont)
  |> Object.mem "only_in_right" (list package_jsont)
  |> Object.mem "duniverse_diff" duniverse_diff_jsont
  |> Object.finish

let compare_jsont =
  let open Jsont in
  Object.map (fun left right env_diff package_diff opam_diff ->
      { left; right; env_diff; package_diff; opam_diff })
  |> Object.mem "left" build_jsont
  |> Object.mem "right" build_jsont
  |> Object.mem "env_diff" diff_jsont
  |> Object.mem "package_diff" diff_jsont
  |> Object.mem "opam_diff" opam_diff_jsont
  |> Object.finish

type device = Network of string | Block of string
type device_manifest = { uuid : string; devices : device list }

let device_jsont =
  let open Jsont in
  Object.map' (fun meta name typ ->
      match typ with
      | "NET_BASIC" -> Network name
      | "BLOCK_BASIC" -> Block name
      | _ -> Error.msg meta "Invalid device JSON")
  |> Object.mem "name" string |> Object.mem "type" string |> Object.finish

let solo5_manifest_devices_jsont =
  let open Jsont in
  Object.map (fun devices -> Option.value ~default:[] (Option.join devices))
  |> Object.opt_mem "devices" (option (list device_jsont))
  |> Object.finish

let solo5_abi_jsont =
  let open Jsont in
  Object.map (fun target version -> (Option.join target, Option.join version))
  |> Object.opt_mem "target" (option string)
  |> Object.opt_mem "version" (option int)
  |> Object.finish

let latest_jsont =
  let open Jsont in
  Object.map (fun uuid solo5_manifest solo5_abi ->
      let devices = Option.value ~default:[] (Option.join solo5_manifest) in
      ({ uuid; devices }, Option.join solo5_abi))
  |> Object.mem "uuid" string
  |> Object.opt_mem "solo5_manifest" (option solo5_manifest_devices_jsont)
  |> Object.opt_mem "solo5_abi" (option solo5_abi_jsont)
  |> Object.finish

type job = {
  name : string;
  synopsis : string;
  manifest : device_manifest;
  abi_target : string;
  abi_version : int;
}

let job_opt_jsont =
  let open Jsont in
  Object.map (fun name synopsis latest ->
      match Option.join latest with
      | Some
          ( manifest,
            Some (Some (("hvt" | "spt") as abi_target), Some abi_version) ) ->
          Some { name; synopsis; manifest; abi_target; abi_version }
      | Some (_, Some _) ->
          Logs.debug (fun i ->
              i
                "builder_web.jobs_of_json: no valid target type. requires hvt \
                 or spt unikernels.");
          None
      | Some (_, None) ->
          Logs.debug (fun i ->
              i "builder_web.jobs_of_json: no solo5_abi section in the json.");
          None
      | None ->
          Logs.debug (fun i ->
              i "builder_web.jobs_of_json: no latest section in the json.");
          None)
  |> Object.mem "name" string
  |> Object.mem "synopsis" string
  |> Object.opt_mem "latest" (option latest_jsont)
  |> Object.finish

let jobs_jsont =
  let open Jsont in
  Object.map (List.filter_map Fun.id)
  |> Object.mem "jobs" (list job_opt_jsont)
  |> Object.finish

let job_summary_jsont =
  let open Jsont in
  let networks (j : job) =
    List.filter_map
      (function Network n -> Some n | Block _ -> None)
      j.manifest.devices
  in
  let blocks (j : job) =
    List.filter_map
      (function Block b -> Some b | Network _ -> None)
      j.manifest.devices
  in
  let synopsis (j : job) = j.synopsis in
  Object.enc_only ()
  |> Object.mem "network" (list string) ~enc:networks
  |> Object.mem "block" (list string) ~enc:blocks
  |> Object.mem "synopsis" string ~enc:synopsis
  |> Object.finish

let jobs_to_json_string (builder_jobs : job list) =
  let map =
    List.fold_left
      (fun acc (j : job) -> Utils.SM.add j.name j acc)
      Utils.SM.empty builder_jobs
  in
  match
    Jsont_bytesrw.encode_string
      (Jsont.Object.as_string_map job_summary_jsont)
      map
  with
  | Ok s -> s
  | Error _ -> "{}"

let send_http_request ?(path = "") ~base_url ~happy_eyeballs () =
  let uri = base_url ^ path in
  let fn _meta _req (resp : Mhttp_client.response) buf chunk =
    if Mhttp_client.Status.is_successful resp.status then
      match chunk with
      | Some str ->
          Buffer.add_string buf str;
          buf
      | None -> buf
    else buf
  in
  let buf = Buffer.create 0x1000 in
  let res =
    try
      Mhttp_client.request ?authenticator:Utils.authenticator ~happy_eyeballs
        ~headers:[ ("Accept", "application/json") ]
        ~fn ~uri buf
      |> Result.map_error (fun err ->
          `Msg (Format.asprintf "%a" Mhttp_client.pp_error err))
    with exn -> Error (`Msg (Printexc.to_string exn))
  in
  match res with
  | Error _ as err -> err
  | Ok (resp, buf) ->
      if Mhttp_client.Status.is_successful resp.status then
        Ok (Buffer.contents buf)
      else if resp.status = `Not_found then Error `Not_found
      else
        Error
          (`Msg
             ("accessing " ^ uri ^ " resulted in an error: "
             ^ Mhttp_client.Status.to_string resp.status
             ^ " " ^ resp.reason))

let fetch_unikernel_jobs happy_eyeballs =
  match send_http_request ~path:"/all-builds" ~base_url ~happy_eyeballs () with
  | Ok body -> (
      match Jsont_bytesrw.decode_string jobs_jsont body with
      | Ok jobs -> jobs
      | Error e ->
          Logs.info (fun m -> m "jobs_jsont: Failed to fetch jobs: %s" e);
          [])
  | Error (`Msg msg) ->
      Logs.info (fun m -> m "send_http_request: Failed to fetch jobs: %s" msg);
      []
  | Error `Not_found ->
      Logs.info (fun m ->
          m "send_http_request: Failed to fetch jobs: Not found");
      []

let fetch_unikernel_binary_image happy_eyeballs ~job ~version push_chunks =
  let uri = base_url ^ "/job/" ^ job ^ "/build/" ^ version ^ "/main-binary" in
  let fn _meta _req (resp : Mhttp_client.response) () chunk =
    if Mhttp_client.Status.is_successful resp.status then push_chunks chunk
  in
  let res =
    try
      Mhttp_client.request ?authenticator:Utils.authenticator ~happy_eyeballs
        ~follow_redirect:true ~fn ~uri ()
      |> Result.map_error (fun err ->
          `Msg (Format.asprintf "%a" Mhttp_client.pp_error err))
    with exn -> Error (`Msg (Printexc.to_string exn))
  in
  push_chunks None;
  match res with
  | Error _ as err -> err
  | Ok (resp, ()) ->
      if Mhttp_client.Status.is_successful resp.status then Ok ()
      else if resp.status = `Not_found then Error (`Msg "returned not found")
      else
        Error
          (`Msg
             ("accessing " ^ uri ^ " resulted in an error: "
             ^ Mhttp_client.Status.to_string resp.status
             ^ " " ^ resp.reason))
