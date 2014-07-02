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

let map func = function
  | Some x -> Some (func x)
  | None   -> None

(*
** Content
*)

(* Get_detail  *)
let get_detail =
  Eliom_service.Http.service
    ~path:["api"; "content"; "detail"]
    ~get_params:Eliom_parameter.(suffix (string "content_uri"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:get_detail
    (fun content_uri () -> return_of_json (API_core.get_detail content_uri))

(* get_contents *)
let get_contents =
  Eliom_service.Http.service
    ~path:["api"; "content"; "list_content"]
    ~get_params:Eliom_parameter.(suffix (opt (string "filter") **
                                           opt (list "tags" (string "uri"))))
    ()

(** Simple contents service  *)
let _ =
  Eliom_registration.String.register
    ~service:get_contents
    (fun (filter, tags_uris) () ->
      let tags_uri_dcd = map (List.map Rdf_store.slash_decode) tags_uris in
      return_of_json (API_core.get_contents filter tags_uri_dcd))

(** This service allow a simpler matching url without superfluous slashs,
    due by the two optional parameters. *)
let _ =
  Eliom_registration.String.register_service
    ~path:["api"; "content"; "list_content"]
    ~get_params:Eliom_parameter.(suffix (opt (string "filter")))
    (fun filter () -> return_of_json (API_core.get_contents filter None))

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
                                  opt (list "tags" (string "uri")))
    ()

let _ =
  Eliom_registration.String.register
    ~service:insert_content_json
    (fun () (input_type, ostream) ->
      let aux () =
        lwt yojson = API_tools.json_of_ocsigen_string_stream input_type ostream in
        let title, summary, text, tags_uri =
          API_deserialize.get_insert_content_data yojson
        in
          return_of_json (API_core.insert_content title summary text tags_uri)
      in
      API_tools.manage_bad_request aux)

let _ =
  Eliom_registration.String.register
    ~service:insert_content
    (fun () (title, (summary, (text, tags_uri))) ->
      return_of_json (API_core.insert_content title summary text tags_uri)
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
                          "content_uri parameter is mandatory"))

let update_content_json =
  Eliom_service.Http.post_service
    ~fallback:fallback_update_content
    ~post_params:Eliom_parameter.raw_post_data
    ()

let update_content =
  Eliom_service.Http.post_service
    ~fallback:fallback_update_content
    ~post_params:Eliom_parameter.(string "content_uri" **
                                  opt (string "title") **
                                  opt (string "summary") **
                                  opt (string "text") **
                                  opt (list "tags" (string "uri")))
    ()

let _ =
  Eliom_registration.String.register
    ~service:update_content_json
    (fun () (input_type, ostream) ->
      let aux () =
        lwt yojson = API_tools.json_of_ocsigen_string_stream input_type ostream in
        let content_uri, title, summary, text, tags_uri =
          API_deserialize.get_update_content_data yojson
        in
        return_of_json (API_core.update_content content_uri
                          title summary text tags_uri)
      in
      API_tools.manage_bad_request aux)

let _ =
  Eliom_registration.String.register
    ~service:update_content
    (fun () (content_uri, (title, (summary, (text, tags_uri)))) ->
      return_of_json (API_core.update_content content_uri
                        title summary text tags_uri))

(* Update content tags *)
let fallback_update_content_tags =
  Eliom_service.Http.service
    ~path:["api"; "content"; "update_tags"]
    ~get_params:Eliom_parameter.unit
    ()

let _ =
  Eliom_registration.String.register
    ~service:fallback_update_content_tags
    (fun () () ->
      return_of_error (API_tools.bad_request
                          "content_uri parameter is mandatory"))

let update_content_tags_json =
  Eliom_service.Http.post_service
    ~fallback:fallback_update_content_tags
    ~post_params:Eliom_parameter.raw_post_data
    ()

let update_content_tags =
  Eliom_service.Http.post_service
    ~fallback:fallback_update_content_tags
    ~post_params:Eliom_parameter.(string "content_uri" **
                                  (list "tags" (string "uri")))
    ()

let _ =
  Eliom_registration.String.register
    ~service:update_content_tags_json
    (fun () (input_type, ostream) ->
      let aux () =
        lwt yojson = API_tools.json_of_ocsigen_string_stream input_type ostream in
        let content_uri, tags_uri =
          API_deserialize.get_update_content_tags_data yojson
        in
        return_of_json (API_core.update_content_tags content_uri tags_uri)
      in
      API_tools.manage_bad_request aux)

let _ =
  Eliom_registration.String.register
    ~service:update_content_tags
    (fun () (content_uri, tags_uri) ->
      return_of_json (API_core.update_content_tags content_uri tags_uri))


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
                          "contents_uri parameter is mandatory"))

let delete_contents_json =
  Eliom_service.Http.post_service
    ~fallback:fallback_delete_contents
    ~post_params:Eliom_parameter.raw_post_data
    ()

let delete_contents =
  Eliom_service.Http.post_service
    ~fallback:fallback_delete_contents
    ~post_params:Eliom_parameter.(list "contents" (string "uri"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:delete_contents_json
    (fun () (input_type, ostream) ->
      let aux () =
        lwt yojson = API_tools.json_of_ocsigen_string_stream input_type ostream in
        let contents_uri = API_deserialize.get_delete_contents_data yojson in
        return_of_json (API_core.delete_contents contents_uri)
      in
      API_tools.manage_bad_request aux)

let _ =
  Eliom_registration.String.register
    ~service:delete_contents
    (fun () (contents_uri) ->
      return_of_json (API_core.delete_contents contents_uri))


(*
** Tags
*)


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
    ~get_params:Eliom_parameter.(suffix (string "content_uri"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:get_tags_from_content
    (fun (content_uri) () ->
      return_of_json (API_core.get_tags_from_content content_uri))


(* Get_tag_from_content_link *)
let get_tags_from_content_link =
  Eliom_service.Http.service
    ~path:["api"; "tag"; "list_from_content_links"]
    ~get_params:Eliom_parameter.(suffix (string "content_uri"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:get_tags_from_content_link
    (fun (content_uri) () ->
      return_of_json (API_core.get_tags_from_content_link content_uri))


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
                                  opt (string "uri") **
                                  list "tags" (string "subject"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:insert_tags_json
    (fun () (input_type, ostream) ->
      let aux () =
        lwt yojson = API_tools.json_of_ocsigen_string_stream input_type ostream in
        let type_name, uri, tags_subject =
          API_deserialize.get_insert_tags_data yojson
        in
        return_of_json (API_core.insert_tags type_name uri tags_subject)
      in
      API_tools.manage_bad_request aux)

let _ =
  Eliom_registration.String.register
    ~service:insert_tags
    (fun () (type_name, (uri, tags_subject)) ->
      return_of_json (API_core.insert_tags type_name uri tags_subject))

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
      return_of_error (API_tools.bad_request "tags_uri parameter is mandatory"))

let delete_tags_json =
  Eliom_service.Http.post_service
    ~fallback:fallback_delete_tags
    ~post_params:Eliom_parameter.raw_post_data
    ()

let delete_tags =
  Eliom_service.Http.post_service
    ~fallback:fallback_delete_tags
    ~post_params:Eliom_parameter.(list "tags" (string "uri"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:delete_tags_json
    (fun () (input_type, ostream) ->
      let aux () =
        lwt yojson = API_tools.json_of_ocsigen_string_stream input_type ostream in
        let tags_uri = API_deserialize.get_delete_tags_data yojson in
        return_of_json (API_core.delete_tags tags_uri)
      in
      API_tools.manage_bad_request aux)

let _ =
  Eliom_registration.String.register
    ~service:delete_tags
    (fun () (tags_uri) ->
      return_of_json (API_core.delete_tags tags_uri))

(*
** links
*)

(* Get_link_detail *)
let get_link_detail =
  Eliom_service.Http.service
    ~path:["api"; "link"; "detail"]
    ~get_params:Eliom_parameter.(suffix (string "link_uri"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:get_link_detail
    (fun link_uri () ->
      return_of_json (API_core.get_link_detail link_uri))

(* Get_links_from_content *)
let get_links_from_content =
  Eliom_service.Http.service
    ~path:["api"; "link"; "list_from_content"]
    ~get_params:Eliom_parameter.(suffix (string "content_uri"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:get_links_from_content
    (fun content_uri () ->
      return_of_json (API_core.get_links_from_content content_uri))


(* Get_links_from_content_tags *)
let get_links_from_content_tags =
     Eliom_service.Http.service
       ~path:["api"; "link"; "list_from_content_tags"]
       ~get_params:Eliom_parameter.(suffix ((string "content_uri") **
          (opt (list "tags" (string "uri")))))
       ()

(** Simple service registration *)
let _ =
  Eliom_registration.String.register
   ~service:get_links_from_content_tags
    (fun (content_uri, tags_uris) () ->
      let content_uri_dcd = Rdf_store.slash_decode content_uri in
      let tags_uri_dcd = map (List.map Rdf_store.slash_decode) tags_uris in
      return_of_json
        (API_core.get_links_from_content_tags content_uri_dcd tags_uri_dcd))

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

let insert_links =
  Eliom_service.Http.post_service
    ~fallback:fallback_insert_links
    ~post_params:Eliom_parameter.(list "data"
                                    (string "origin_uri" **
                                       string "target_uri" **
                                       list "tags" (string "uri")))
    ()

let insert_links_json =
  Eliom_service.Http.post_service
    ~fallback:fallback_insert_links
    ~post_params:Eliom_parameter.raw_post_data
    ()

let _ =
  Eliom_registration.String.register
    ~service:insert_links_json
    (fun () (input_type, ostream) ->
      let aux () =
        lwt yojson = API_tools.json_of_ocsigen_string_stream input_type ostream in
        let data = API_deserialize.get_insert_links_data yojson in
        return_of_json (API_core.insert_links data)
      in
      API_tools.manage_bad_request aux)

let _ =
  Eliom_registration.String.register
    ~service:insert_links
    (fun () data ->
      let aux () =
        let to_triple (origin_uri, (target_uri, tags_uri)) =
          (origin_uri, target_uri, tags_uri)
        in
        let formated_data = List.map to_triple data in
        return_of_json (API_core.insert_links formated_data)
      in
      API_tools.manage_bad_request aux)

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

let update_links =
  Eliom_service.Http.post_service
    ~fallback:fallback_insert_links
    ~post_params:Eliom_parameter.(list "data"
                                    (string "link_uri" **
                                       list "tags" (string "uri")))
    ()

let update_links_json =
  Eliom_service.Http.post_service
    ~fallback:fallback_update_link
    ~post_params:Eliom_parameter.raw_post_data
    ()

let _ =
  Eliom_registration.String.register
    ~service:update_links_json
    (fun () (input_type, ostream) ->
      let aux () =
        lwt yojson = API_tools.json_of_ocsigen_string_stream input_type ostream in
        let data = API_deserialize.get_update_links_data yojson in
        return_of_json (API_core.update_links data)
      in
      API_tools.manage_bad_request aux)

let _ =
  Eliom_registration.String.register
    ~service:update_links
    (fun () data ->
      let aux () = return_of_json (API_core.update_links data) in
      API_tools.manage_bad_request aux)

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
                          "links_uri parameter is mandatory"))

let delete_links_json =
  Eliom_service.Http.post_service
    ~fallback:fallback_delete_links
    ~post_params:Eliom_parameter.raw_post_data
    ()

let delete_links =
  Eliom_service.Http.post_service
    ~fallback:fallback_delete_links
    ~post_params:Eliom_parameter.(list "links" (string "uri"))
    ()

let _ =
  Eliom_registration.String.register
    ~service:delete_links_json
    (fun () (input_type, ostream) ->
      let aux () =
        lwt yojson = API_tools.json_of_ocsigen_string_stream input_type ostream in
        let links_uri = API_deserialize.get_delete_links_data yojson in
        return_of_json (API_core.delete_links links_uri)
      in
      API_tools.manage_bad_request aux)

let _ =
  Eliom_registration.String.register
    ~service:delete_links
    (fun () (links_uri) ->
      return_of_json (API_core.delete_links links_uri))
