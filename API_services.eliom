(*
** API Services
** This module registre services of API
*)

open API_tools

module Yojson = Yojson.Basic

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
    (fun content_id () -> return_of_json (API_core.get_detail content_id))

(* Get_detail_by_link  *)
let get_detail_by_link =
  Eliom_service.Http.service
    ~path:["api"; "content"; "detail_by_link"]
    ~get_params:Eliom_parameter.(suffix (string "link_id"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:get_detail_by_link
    (fun link_id () -> return_of_json (API_core.get_detail_by_link link_id))

(* get_contents *)
let get_contents =
  Eliom_service.Http.service
    ~path:["api"; "content"; "list_content"]
    ~get_params:Eliom_parameter.(suffix (opt (string "filter") **
                                           opt (list "tags" (string "id"))))
    ()

(** This function manage the computation of the contents service.  *)
let get_contents_handler (filter, tags_id) () =
  return_of_json (API_core.get_contents filter tags_id)


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

(* Insert content *)
let fallback_insert_content =
  Eliom_service.Http.service
    ~path:["api"; "content"; "insert"]
    ~get_params:Eliom_parameter.unit
    ()

let _ =
  Eliom_registration.String.register
    ~service:fallback_insert_content
    (fun () () ->
      return_of_error (API_tools.bad_request
                          "title, summary and text parameters are mandatory"))

let insert_content_json =
  Eliom_service.Http.post_service
    ~fallback:fallback_insert_content
    ~post_params:Eliom_parameter.raw_post_data
    ()

let insert_content =
  Eliom_service.Http.post_service
    ~fallback:fallback_insert_content
    ~post_params:Eliom_parameter.(string "title" **
                                  string "summary" **
                                  string "text" **
                                  opt (list "tags" (string "id")))
    ()

let _ =
  Eliom_registration.String.register
    ~service:insert_content_json
    (fun () (input_type, ostream) ->
      let aux () =
        lwt yojson = API_tools.json_of_ocsigen_string_stream input_type ostream in
        let title, summary, text, tags_id =
          API_deserialize.get_insert_content_data yojson
        in
          return_of_json (API_core.insert_content title summary text tags_id)
      in
      API_tools.manage_bad_request aux)

let _ =
  Eliom_registration.String.register
    ~service:insert_content
    (fun () (title, (summary, (text, tags_id))) ->
      return_of_json (API_core.insert_content title summary text tags_id)
  )

(* Update content *)
let fallback_update_content =
  Eliom_service.Http.service
    ~path:["api"; "content"; "update"]
    ~get_params:Eliom_parameter.unit
    ()

let _ =
  Eliom_registration.String.register
    ~service:fallback_update_content
    (fun () () ->
      return_of_error (API_tools.bad_request
                          "content_id parameter is mandatory"))

let update_content_json =
  Eliom_service.Http.post_service
    ~fallback:fallback_update_content
    ~post_params:Eliom_parameter.raw_post_data
    ()

let update_content =
  Eliom_service.Http.post_service
    ~fallback:fallback_update_content
    ~post_params:Eliom_parameter.(string "content_id" **
                                  opt (string "title") **
                                  opt (string "summary") **
                                  opt (string "text") **
                                  opt (list "tags" (string "id")))
    ()

let _ =
  Eliom_registration.String.register
    ~service:update_content_json
    (fun () (input_type, ostream) ->
      let aux () =
        lwt yojson = API_tools.json_of_ocsigen_string_stream input_type ostream in
        let content_id, title, summary, text, tags_id =
          API_deserialize.get_update_content_data yojson
        in
        return_of_json (API_core.update_content content_id
                          title summary text tags_id)
      in
      API_tools.manage_bad_request aux)

let _ =
  Eliom_registration.String.register
    ~service:update_content
    (fun () (content_id, (title, (summary, (text, tags_id)))) ->
      return_of_json (API_core.update_content content_id
                        title summary text tags_id))

(* Delete content *)
let fallback_delete_contents =
  Eliom_service.Http.service
    ~path:["api"; "content"; "delete"]
    ~get_params:Eliom_parameter.unit
    ()

let _ =
  Eliom_registration.String.register
    ~service:fallback_delete_contents
    (fun () () ->
      return_of_error (API_tools.bad_request
                          "contents_id parameter is mandatory"))

let delete_contents_json =
  Eliom_service.Http.post_service
    ~fallback:fallback_delete_contents
    ~post_params:Eliom_parameter.raw_post_data
    ()

let delete_contents =
  Eliom_service.Http.post_service
    ~fallback:fallback_delete_contents
    ~post_params:Eliom_parameter.(list "contents" (string "id"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:delete_contents_json
    (fun () (input_type, ostream) ->
      let aux () =
        lwt yojson = API_tools.json_of_ocsigen_string_stream input_type ostream in
        let contents_id = API_deserialize.get_delete_contents_data yojson in
        return_of_json (API_core.delete_contents contents_id)
      in
      API_tools.manage_bad_request aux)

let _ =
  Eliom_registration.String.register
    ~service:delete_contents
    (fun () (contents_id) ->
      return_of_json (API_core.delete_contents contents_id))


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
      return_of_json (API_core.get_tags tags_id))

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
      return_of_json (API_core.get_tags_by_type tag_type))

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
      return_of_json (API_core.get_tags_from_content content_id))


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
      return_of_json (API_core.get_tags_from_content_link content_id))


(* Insert tags *)
let fallback_insert_tags =
  Eliom_service.Http.service
    ~path:["api"; "tag"; "insert"]
    ~get_params:Eliom_parameter.unit
    ()

let _ =
  Eliom_registration.String.register
    ~service:fallback_insert_tags
    (fun () () ->
      return_of_error (API_tools.bad_request
                          "tags_subject parameter is mandatory"))

let insert_tags_json =
  Eliom_service.Http.post_service
    ~fallback:fallback_insert_tags
    ~post_params:Eliom_parameter.raw_post_data
    ()

let insert_tags =
  Eliom_service.Http.post_service
    ~fallback:fallback_insert_tags
    ~post_params:Eliom_parameter.(string "type_name" **
                                  opt (string "id") **
                                  list "tags" (string "subject"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:insert_tags_json
    (fun () (input_type, ostream) ->
      let aux () =
        lwt yojson = API_tools.json_of_ocsigen_string_stream input_type ostream in
        let type_name, id, tags_subject =
          API_deserialize.get_insert_tags_data yojson
        in
        return_of_json (API_core.insert_tags type_name id tags_subject)
      in
      API_tools.manage_bad_request aux)

let _ =
  Eliom_registration.String.register
    ~service:insert_tags
    (fun () (type_name, (id, tags_subject)) ->
      return_of_json (API_core.insert_tags type_name id tags_subject))

(* Delete tags *)
let fallback_delete_tags =
  Eliom_service.Http.service
    ~path:["api"; "tag"; "delete"]
    ~get_params:Eliom_parameter.unit
    ()

let _ =
  Eliom_registration.String.register
    ~service:fallback_delete_tags
    (fun () () ->
      return_of_error (API_tools.bad_request "tags_id parameter is mandatory"))

let delete_tags_json =
  Eliom_service.Http.post_service
    ~fallback:fallback_delete_tags
    ~post_params:Eliom_parameter.raw_post_data
    ()

let delete_tags =
  Eliom_service.Http.post_service
    ~fallback:fallback_delete_tags
    ~post_params:Eliom_parameter.(list "tags" (string "id"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:delete_tags_json
    (fun () (input_type, ostream) ->
      let aux () =
        lwt yojson = API_tools.json_of_ocsigen_string_stream input_type ostream in
        let tags_id = API_deserialize.get_delete_tags_data yojson in
        return_of_json (API_core.delete_tags tags_id)
      in
      API_tools.manage_bad_request aux)

let _ =
  Eliom_registration.String.register
    ~service:delete_tags
    (fun () (tags_id) ->
      return_of_json (API_core.delete_tags tags_id))

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
      return_of_json (API_core.get_links_from_content content_id))


(* Get_links_from_content_tags *)
let get_links_from_content_tags =
     Eliom_service.Http.service
       ~path:["api"; "link"; "list_from_content_tags"]
       ~get_params:Eliom_parameter.(suffix ((string "content_id") **
          (opt (list "tags" (string "id")))))
       ()

(** This function manage the computation
   of the get_links_from_content_tags sercice *)
let get_links_from_content_tags_handler (content_id, tags_id) () =
   return_of_json (API_core.get_links_from_content_tags content_id tags_id)

(** Simple service registration *)
let _ =
  Eliom_registration.String.register
   ~service:get_links_from_content_tags
   get_links_from_content_tags_handler

(* Insert links *)
let fallback_insert_links =
  Eliom_service.Http.service
    ~path:["api"; "link"; "insert"]
    ~get_params:Eliom_parameter.unit
    ()

let _ =
  Eliom_registration.String.register
    ~service:fallback_insert_links
    (fun () () ->
      return_of_error (API_tools.bad_request "All parameters are mandatory"))

let insert_links_json =
  Eliom_service.Http.post_service
    ~fallback:fallback_insert_links
    ~post_params:Eliom_parameter.raw_post_data
    ()

let insert_links =
  Eliom_service.Http.post_service
    ~fallback:fallback_insert_links
    ~post_params:Eliom_parameter.(string "id_from" **
                                  list "ids" (string "to") **
                                  list "list" (list "tags" (string "id")))
    ()

let _ =
  Eliom_registration.String.register
    ~service:insert_links_json
    (fun () (input_type, ostream) ->
      let aux () =
        lwt yojson = API_tools.json_of_ocsigen_string_stream input_type ostream in
        let id_from, ids_to, list_tags_id =
          API_deserialize.get_insert_links_data yojson
        in
        return_of_json (API_core.insert_links id_from ids_to list_tags_id)
      in
      API_tools.manage_bad_request aux)

let _ =
  Eliom_registration.String.register
    ~service:insert_links
    (fun () (id_from, (ids_to, list_tags_id)) ->
      return_of_json (API_core.insert_links id_from ids_to list_tags_id))

(* Update links *)
let fallback_update_link =
  Eliom_service.Http.service
    ~path:["api"; "link"; "update"]
    ~get_params:Eliom_parameter.unit
    ()

let _ =
  Eliom_registration.String.register
    ~service:fallback_update_link
    (fun () () ->
      return_of_error (API_tools.bad_request "All parameter are mandatory"))

let update_link_json =
  Eliom_service.Http.post_service
    ~fallback:fallback_update_link
    ~post_params:Eliom_parameter.raw_post_data
    ()

let update_link =
  Eliom_service.Http.post_service
    ~fallback:fallback_update_link
    ~post_params:Eliom_parameter.(string "link_id" **
                                  list "tags" (string "id"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:update_link_json
    (fun () (input_type, ostream) ->
      let aux () =
        lwt yojson = API_tools.json_of_ocsigen_string_stream input_type ostream in
        let link_id, tags_id =
          API_deserialize.get_update_link_data yojson in
        return_of_json (API_core.update_link link_id tags_id)
      in
      API_tools.manage_bad_request aux)

let _ =
  Eliom_registration.String.register
    ~service:update_link
    (fun () (link_id, tags_id) ->
      return_of_json (API_core.update_link link_id tags_id))

(* Delete links *)
let fallback_delete_links =
  Eliom_service.Http.service
    ~path:["api"; "link"; "delete"]
    ~get_params:Eliom_parameter.unit
    ()

let _ =
  Eliom_registration.String.register
    ~service:fallback_delete_links
    (fun () () ->
      return_of_error (API_tools.bad_request
                          "links_id parameter is mandatory"))

let delete_links_json =
  Eliom_service.Http.post_service
    ~fallback:fallback_delete_links
    ~post_params:Eliom_parameter.raw_post_data
    ()

let delete_links =
  Eliom_service.Http.post_service
    ~fallback:fallback_delete_links
    ~post_params:Eliom_parameter.(list "links" (string "id"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:delete_links_json
    (fun () (input_type, ostream) ->
      let aux () =
        lwt yojson = API_tools.json_of_ocsigen_string_stream input_type ostream in
        let links_id = API_deserialize.get_delete_links_data yojson in
        return_of_json (API_core.delete_links links_id)
      in
      API_tools.manage_bad_request aux)

let _ =
  Eliom_registration.String.register
    ~service:delete_links
    (fun () (links_id) ->
      return_of_json (API_core.delete_links links_id))

(* Temporary delete links with from to parameters *)
let delete_links_from_to =
  Eliom_service.Http.post_service
    ~fallback:fallback_delete_links
    ~post_params:Eliom_parameter.(string "origin_id" **
                                  list "targets" (string "id"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:delete_links_from_to
    (fun () (origin_id, targets_id) ->
      return_of_json (API_core.delete_links_from_to origin_id targets_id))
