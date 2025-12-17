open Lwt.Infix

let ( let* ) = Lwt_result.bind

type user_unikernel_available_updates = {
  user : User_model.user;
  available_updates :
    (Albatross.Albatross_map.key
    * (Vmm_core.Name.t * Vmm_core.Unikernel.info * Builder_web.compare) list)
    list;
}

type error =
  | Albatross_err of string
  | Builder_req_err of string
  | Builder_parse_err of string

type success =
  | Update_available of
      (Vmm_core.Name.t * Vmm_core.Unikernel.info * Builder_web.compare)
  | No_update_needed of (Vmm_core.Name.t * string)

let send_http_request ?(path = "") ~base_url http_client =
  let url = base_url ^ path in
  let body = "" in
  let body_f _ acc chunk = Lwt.return (acc ^ chunk) in
  Http_mirage_client.request http_client ~follow_redirect:true
    ~headers:[ ("Accept", "application/json") ]
    url body_f body
  >>= function
  | Error (`Msg err) -> Lwt.return (Error (`Msg err))
  | Error `Cycle -> Lwt.return (Error (`Msg "returned cycle"))
  | Error `Not_found -> Lwt.return (Error (`Msg "returned not found"))
  | Ok (resp, body) ->
      if Http_mirage_client.Status.is_successful resp.Http_mirage_client.status
      then Lwt.return (Ok body)
      else
        Lwt.return
          (Error
             (`Msg
                ("accessing " ^ url ^ " resulted in an error: "
                ^ Http_mirage_client.Status.to_string resp.status
                ^ " " ^ resp.reason)))

let fetch_json http_client ~base_url ~path parser ctx_msg =
  let* body =
    send_http_request ~path ~base_url http_client
    |> Lwt_result.map_error (fun (`Msg e) ->
        Builder_req_err (Printf.sprintf "Network error during %s: %s" ctx_msg e))
  in
  match Utils.Json.from_string body with
  | Error (`Msg e) ->
      Lwt.return
        (Error
           (Builder_parse_err
              (Printf.sprintf "JSON parse error (%s): %s" ctx_msg e)))
  | Ok json -> (
      match parser json with
      | Error (`Msg e) ->
          Lwt.return
            (Error
               (Builder_parse_err
                  (Printf.sprintf "Data format error (%s): %s" ctx_msg e)))
      | Ok data -> Lwt.return (Ok data))

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

let check_for_update name unikernel http_client =
  let base_url = Builder_web.base_url in
  (* Fetch current build info *)
  let* current_job =
    fetch_json http_client ~base_url
      ~path:("/hash?sha256=" ^ Ohex.encode unikernel.Vmm_core.Unikernel.digest)
      Builder_web.build_of_json "current build"
  in
  (* Fetch latest build info *)
  let* latest_job =
    fetch_json http_client ~base_url
      ~path:("/job/" ^ current_job.job ^ "/build/latest")
      Builder_web.build_of_json "latest build"
  in

  if String.equal latest_job.uuid current_job.uuid then
    Lwt.return (Ok (No_update_needed (name, latest_job.uuid)))
  else
    let* comparison =
      fetch_json http_client ~base_url
        ~path:("/compare/" ^ current_job.uuid ^ "/" ^ latest_job.uuid)
        Builder_web.compare_of_json "build comparison"
    in
    Lwt.return (Ok (Update_available (name, unikernel, comparison)))
