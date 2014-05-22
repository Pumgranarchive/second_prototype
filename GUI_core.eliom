(*
  GUI Core
  This module compute input from GUI services to get results to display
  it GUI html
*)

module Util = Intern_yj_util

(** Get all data for get_detail html service. *)
let get_detail_content content_id =
  try_lwt
    lwt content = API_core.get_detail content_id in
    let title, text, id =
      List.hd Util.(get_service_return get_content_list content)
    in
    lwt content_tags = API_core.get_tags_from_content content_id in
    let tags = Util.(get_service_return get_tag_list content_tags) in
    lwt links = API_core.get_links_from_content content_id in
    let link_list = Util.(get_service_return get_link_list links) in
    lwt tags_link = API_core.get_tags_from_content_link content_id in
    let tags_link_list =
      Util.(get_service_return get_tag_list tags_link)
    in
    Lwt.return ((title, text, id), tags, link_list, tags_link_list)
  with
  | e -> print_endline (Printexc.to_string e);
    Lwt.return (("title", "Internal server error", "id"), [], [], [])

(** Get all data for get_contents html service. *)
let get_contents filter tags_id =
  lwt contents_json = API_core.get_contents filter tags_id in
  lwt tags_json = API_core.get_tags_by_type API_conf.content_tag in
  let contents =
    Util.(get_service_return get_content_list contents_json)
  in
  let tags = Util.(get_service_return get_tag_list tags_json) in
  Lwt.return (contents, tags)
