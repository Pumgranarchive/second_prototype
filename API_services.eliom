(*
** API Services
** This module registre services of API
*)

module Yj = Yojson.Safe

(*** Services
     All services are registrer in twice step to allow the GUI
     to get reference on them and use it.
*)


(*
** Content
*)

(* Get_detail  *)
let get_detail =
  Eliom_service.Http.service
    ~path:["api"; "content"; "detail"]
    ~get_params:Eliom_parameter.(suffix (string "content_id"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:get_detail
    (fun content_id () ->
      Lwt.return (Yj.to_string (API_core.get_detail content_id),
                  API_tools.content_type))

(* Get_detail_by_link  *)
let get_detail_by_link =
  Eliom_service.Http.service
    ~path:["api"; "content"; "detail_by_link"]
    ~get_params:Eliom_parameter.(suffix (string "link_id"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:get_detail_by_link
    (fun link_id () ->
      Lwt.return (Yj.to_string (API_core.get_detail_by_link link_id),
                  API_tools.content_type))

(* get_contents *)
let get_contents =
  Eliom_service.Http.service
    ~path:["api"; "content"; "list_content"]
    ~get_params:Eliom_parameter.(suffix (opt (string "filter") **
                                           opt (list "tags" (string "id"))))
    ()

(** This function manage the computation of the contents service.  *)
let get_contents_handler (filter, tags_id) () =
  Lwt.return (Yj.to_string (API_core.get_contents filter tags_id),
              API_tools.content_type)


(** Simple contents service  *)
let _ =
  Eliom_registration.String.register
    ~service:get_contents
    get_contents_handler

(** This service allow a simpler matching url without superfluous slashs,
    due by the two optional parameters. *)
let _ =
  Eliom_registration.String.register_service
    ~path:["api"; "content"; "list_content"]
    ~get_params:Eliom_parameter.(suffix (opt (string "filter")))
    (fun filter -> get_contents_handler (filter, None))

(*
** Tags
*)


(* List_tag *)
let list_tags =
  Eliom_service.Http.service
    ~path:["api"; "tag"; "list_tag"]
    ~get_params:Eliom_parameter.(suffix (list "tags" (string "id")))
    ()

let _ =
  Eliom_registration.String.register
    ~service:list_tags
    (fun (tags_id) () ->
      Lwt.return (Yj.to_string (API_core.get_tags tags_id),
                  API_tools.content_type))

(* Get_tags_by_type *)
let get_tags_by_type =
  Eliom_service.Http.service
    ~path:["api"; "tag"; "list_by_type"]
    ~get_params:Eliom_parameter.(suffix (string "type_name"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:get_tags_by_type
    (fun (tag_type) () ->
      Lwt.return (Yj.to_string (API_core.get_tags_by_type tag_type),
                  API_tools.content_type))

(* Get_tag_from_content *)
let get_tags_from_content =
  Eliom_service.Http.service
    ~path:["api"; "tag"; "list_from_content"]
    ~get_params:Eliom_parameter.(suffix (string "content_id"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:get_tags_from_content
    (fun (content_id) () ->
      Lwt.return (Yj.to_string (API_core.get_tags_from_content content_id),
                  API_tools.content_type))


(* Get_tag_from_content_link *)
let get_tags_from_content_link =
  Eliom_service.Http.service
    ~path:["api"; "tag"; "list_from_content_links"]
    ~get_params:Eliom_parameter.(suffix (string "content_id"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:get_tags_from_content_link
    (fun (content_id) () ->
      Lwt.return (Yj.to_string (API_core.get_tags_from_content_link content_id),
                  API_tools.content_type))


(*
** links
*)

(* Get_links_from_content *)
let get_links_from_content =
  Eliom_service.Http.service
    ~path:["api"; "link"; "list_from_content"]
    ~get_params:Eliom_parameter.(suffix (string "content_id"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:get_links_from_content
    (fun content_id () ->
      Lwt.return (Yj.to_string (API_core.get_links_from_content content_id),
                  API_tools.content_type))


(* Get_links_from_content_tags *)
(* let get_links_from_content_tags =
     Eliom_service.Http.service
       ~path:["api"; "link"; "list_from_content_tags"]
       ~get_params:Eliom_parameter.(suffix (string "link_id") **
          (string "tags_id"))
       ()

(** This function manage the computation
   of the get_links_from_content_tags sercice *)
let get_links_from_content_tags_handler (links_id, tags_id) () =
   Lwt.return (Yj.to_string (API_core.get_links_from_content_tags link_id),
      API_tools.content_type)

(** Simple service registration *)
let _ =
  Eliom_registration.String.register
   ~service:get_links_from_content_tags
   get_links_from_content_tags_handler

(** This service allow a simpler matching url without superfluous slashs,
    due by the two optional parameters. *)
let _ =
   Eliom_registration.String.register_service
    ~path:["api"; "link"; "list_from_content_tags"]
    ~get_params:Eliom_parameter.(suffix (string "link_id"))
   (fun link_id -> get_links_from_content_tags_handler (link_id, None))

*)
