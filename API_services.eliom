(*
** API Services
** This module registre services of API
*)

module Yj = Yojson.Safe

(*** Services *)

(*
** Content
*)

let _ =
  Eliom_registration.String.register_service
    ~content_type:API_tools.content_type
    ~path:["api"; "content"; "detail"]
    ~get_params:Eliom_parameter.(suffix (string "content_id"))
    (fun content_id () ->
      Lwt.return (Yj.to_string (API_core.get_detail content_id),
                  API_tools.content_type))

let _ =
  Eliom_registration.String.register_service
    ~content_type:API_tools.content_type
    ~path:["api"; "content"; "detail_by_link"]
    ~get_params:Eliom_parameter.(suffix (string "link_id"))
    (fun link_id () ->
      Lwt.return (Yj.to_string (API_core.get_detail_by_link link_id),
                  API_tools.content_type))

(* Contents service is registered is twice step to allow
   pumgrana application to get its reference link. *)
let contents =
  Eliom_service.Http.service
    ~path:["api"; "content"; "list_content"]
    ~get_params:Eliom_parameter.(suffix (opt (string "filter") **
                                           opt (list "tags" (string "id"))))
    ()

let _ =
  Eliom_registration.String.register
    ~content_type:API_tools.content_type
    ~service:contents
    (fun (filter, tags_id) () ->
      Lwt.return (Yj.to_string (API_core.get_contents filter tags_id),
                  API_tools.content_type))


(*
** Tags
*)


let tags =
  Eliom_service.Http.service
    ~path:["api"; "tags"; "list_tag"]
    ~get_params:Eliom_parameter.(list "tags" (string "id"))
    ()

let _ =
  Eliom_registration.String.register
    ~content_type:API_tools.content_type
    ~service:tags
    (fun (tags_id) () ->
      Lwt.return (Yj.to_string (API_core.get_tags tags_id),
                  API_tools.content_type))
