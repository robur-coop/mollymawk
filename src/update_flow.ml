let ( let* ) = Result.bind

type user_unikernel_available_updates = {
  user : Models.User.user;
  available_updates :
    (Vmm_core.Name.t * Vmm_core.Unikernel.info * Builder_web.compare) list
    Utils.LM.t;
}

type error =
  | Albatross_err of string
  | Builder_req_err of string
  | Builder_parse_err of string
  | Builder_not_found of string

type success =
  | Update_available of
      (Vmm_core.Name.t * Vmm_core.Unikernel.info * Builder_web.compare)
  | No_update_needed of (Vmm_core.Name.t * string)

let fetch_json happy_eyeballs ~base_url ~path codec ctx_msg =
  let* body =
    Builder_web.send_http_request ~path ~base_url ~happy_eyeballs ()
    |> Result.map_error (function
      | `Not_found ->
          Builder_not_found
            (Printf.sprintf "Build not found during %s: %s" ctx_msg path)
      | `Msg e ->
          Builder_req_err
            (Printf.sprintf "Network error during %s: %s" ctx_msg e))
  in
  match Jsont_bytesrw.decode_string codec body with
  | Error e ->
      Error
        (Builder_parse_err
           (Printf.sprintf "JSON parse error (%s): %s" ctx_msg e))
  | Ok data -> Ok data

let error_response_params unikernel_name = function
  | Albatross_err e ->
      ( Printf.sprintf "Albatross error: %s" e,
        "Internal communication error with Albatross." )
  | Builder_req_err e ->
      ( Printf.sprintf "Builder network error for %s: %s" unikernel_name e,
        "Could not reach the build server. Please try again later." )
  | Builder_parse_err e ->
      ( Printf.sprintf "Builder data error for %s: %s" unikernel_name e,
        "Received unexpected data format from the build server." )
  | Builder_not_found e ->
      ( Printf.sprintf "Builder not found for %s: %s" unikernel_name e,
        Printf.sprintf "No build found on builds.robur.coop for %s."
          unikernel_name )

let check_for_update ?(base_url = Builder_web.base_url) name unikernel
    happy_eyeballs =
  (* Fetch current build info *)
  let* current_job =
    fetch_json happy_eyeballs ~base_url
      ~path:("/hash?sha256=" ^ Ohex.encode unikernel.Vmm_core.Unikernel.digest)
      Builder_web.build_jsont "current build"
  in
  (* Fetch latest build info *)
  let* latest_job =
    fetch_json happy_eyeballs ~base_url
      ~path:("/job/" ^ current_job.job ^ "/build/latest")
      Builder_web.build_jsont "latest build"
  in

  if String.equal latest_job.uuid current_job.uuid then
    Ok (No_update_needed (name, latest_job.uuid))
  else
    let* comparison =
      fetch_json happy_eyeballs ~base_url
        ~path:("/compare/" ^ current_job.uuid ^ "/" ^ latest_job.uuid)
        Builder_web.compare_jsont "build comparison"
    in
    Ok (Update_available (name, unikernel, comparison))
