(*
  GUI Core
  This module compute input from GUI services to get results to display
  it GUI html
*)

open GUI_deserialize

let content_str_uri_of_str_id id =
  Rdf_store.(string_of_uri (uri_of_content_id (Nosql_store.id_of_string id)))

(** Get all data for get_detail html service. *)
let get_detail_content content_id =
  try_lwt
    let content_uri = content_str_uri_of_str_id content_id in
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
    Lwt.return (
      GUI_deserialize.(Internal
                         (Id (Nosql_store.id_of_string "0000000000000000"),
                          "title", "", "Internal server error")),
      [], [], [])

(** Get all data for get_contents html service. *)
let get_contents filter tags_id =
  lwt contents_json = API_core.get_contents filter tags_id in
  lwt tags_json = API_core.get_tags_by_type API_conf.content_tag in
  let contents = get_service_return get_short_content_list contents_json in
  let tags = get_service_return get_tag_list tags_json in
  Lwt.return (contents, tags)
