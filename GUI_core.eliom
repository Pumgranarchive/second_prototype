(*
  GUI Core
  This module compute input from GUI services to get results to display
  it GUI html
*)

open Pdeserialize
open GUI_deserialize

let null_id = Nosql_store.id_of_string "54232a2e40b43eec7507f24e"
let null_uri = Rdf_store.uri_of_string "http://foo.com"
let error_short_content =
  GUI_deserialize.Internal
    (GUI_deserialize.Id null_id,
     "Internal server error", "", "Internal server error")
let error_content =
    ((null_uri, null_uri), null_uri, null_uri, [])

let content_str_uri_of_str_id id =
  Rdf_store.(string_of_uri (uri_of_content_id (Nosql_store.id_of_string id)))

let get_detail_content uri =
  try_lwt
    let content_uri =
      if Nosql_store.is_nosql_id uri
      then content_str_uri_of_str_id uri
      else uri
    in
    lwt content = API_core.get_detail content_uri in
    let content = List.hd (get_service_return get_content_list content) in
    lwt content_tags = API_core.get_tags_from_content content_uri in
    let tags = (get_service_return get_tag_list content_tags) in
    lwt links = API_core.get_links_from_content content_uri in
    let link_list = (get_service_return get_link_list links) in
    lwt tags_link = API_core.get_tags_from_content_link content_uri in
    let tags_link_list = get_service_return get_tag_list tags_link in
    Lwt.return (content, tags, link_list, tags_link_list)
  with
  | e -> print_endline (Printexc.to_string e);
    Lwt.return (error_short_content, [], [], [])

let get_contents filter tags_uri =
  try_lwt
    lwt contents_json = API_core.get_contents filter tags_uri in
    lwt tags_json = API_core.get_tags_by_type API_conf.content_tag in
    let contents = get_service_return get_short_content_list contents_json in
    let tags = get_service_return get_tag_list tags_json in
    Lwt.return (contents, tags)
 with e -> print_endline (Printexc.to_string e); Lwt.return ([], [])

let get_link_detail link_uri =
  try_lwt
    lwt json_detail = API_core.get_link_detail link_uri in
    let detail = List.hd (get_service_return get_detail_link_list json_detail) in
    Lwt.return detail
  with e -> print_endline (Printexc.to_string e); Lwt.return error_content
