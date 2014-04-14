(*
  GUI Core
  This module compute input from GUI services to get results to display
  it GUI html
*)

{shared{

module Yj = Yojson.Safe

(* let power num pow = *)
(*   let rec aux nb = function *)
(*     | 0.                  -> 1. *)
(*     | 1.                  -> num *. nb *)
(*     | p when p < 0.       -> failwith "Invalide power" *)
(*     | p                   -> aux (num *. nb) (p -. 1.) *)
(*   in aux 1. pow *)


}}

(** Get all data for get_detail html service. *)
let get_detail_content content_id =
  try
    let content = API_core.get_detail content_id in
    let title, text, id =
      List.hd Yojson_tools.(get_service_return get_content_list content)
    in
    let content_tags = API_core.get_tags_from_content content_id in
    let tags = Yojson_tools.(get_service_return get_tag_list content_tags) in
    let links = API_core.get_links_from_content content_id in
    let link_list = Yojson_tools.(get_service_return get_link_list links) in
    let tags_link = API_core.get_tags_from_content_link content_id in
    let tags_link_list =
      Yojson_tools.(get_service_return get_tag_list tags_link)
    in
    (title, text, id), tags, link_list, tags_link_list
  with
  | e -> print_endline (Printexc.to_string e);
    ("title", "Internal server error", "id"), [], [], []

(** Get all data for get_contents html service. *)
let get_contents filter tags_id =
  let contents_json = API_core.get_contents filter tags_id in
  let tags_json = API_core.get_tags_by_type API_conf.content_tag in
  let contents =
    Yojson_tools.(get_service_return get_content_list contents_json)
  in
  let tags = Yojson_tools.(get_service_return get_tag_list tags_json) in
  (contents, tags)
