(*
  GUI Core
  This module compute input from GUI services to get results to display
  it GUI html
*)

{shared{

module Yj = Yojson.Safe

let failure_string s = Printf.sprintf "Invalide content format %s" s

let power num pow =
  let rec aux nb = function
    | 0.                  -> 1.
    | 1.                  -> num *. nb
    | p when p < 0.       -> failwith "Invalide power"
    | p                   -> aux (num *. nb) (p -. 1.)
  in aux 1. pow

(** Unformat the API's service return  *)
let unformat_service_return func = function
  | `Assoc [(_, _); (_, data)]  -> func data
  | _                           -> failwith (failure_string "uf: service return")

(** Unformat the API's content return  *)
let unformat_content = function
  | `Assoc [(_, `String id);
            (_, `String title);
            (_, `String text)]    -> title, text, id
  | _                           -> failwith (failure_string "uf: content")

(** Unformat the API's tag return  *)
let unformat_tag = function
  | `Assoc [(_, `String id);
            (_, `String subject)]    -> subject, id
  | _                           -> failwith (failure_string "uf: tag")

(** Unformat the API's list return  *)
let unformat_list (func: Yj.json -> 'a) (l: Yj.json) = match l with
  | `List l                     -> List.map func l
  | `Null                       -> []
  | _                           -> failwith (failure_string "uf: list")

(* Some unformat shorcuts *)
let unformat_list_tag = unformat_list unformat_tag
let unformat_list_content = unformat_list unformat_content
let unformat_list_link = unformat_list unformat_content

}}

(** Get all data for get_detail html service. *)
let get_detail_content content_id =
  try
    let content = API_core.get_detail content_id in
    let title, text, id =
      List.hd (unformat_service_return unformat_list_content content)
    in
    let content_tags = API_core.get_tags_from_content content_id in
    let tags_id = unformat_service_return unformat_list_tag content_tags in
    let links = API_core.get_links_from_content content_id in
    let link_list = unformat_service_return unformat_list_link links in
    let tags_link = API_core.get_tags_from_content_link content_id in
    let tags_link_list = unformat_service_return unformat_list_tag tags_link in
    (title, text, id), tags_id, link_list, tags_link_list
  with
  | e -> print_endline (Printexc.to_string e);
    ("title", "text", "id"), [], [], []

(** Get all data for get_contents html service. *)
let get_contents filter tags_id =
  let contents_json = API_core.get_contents filter tags_id in
  let tags_json = API_core.get_tags_by_type API_conf.content_tag in
  let contents = unformat_service_return unformat_list_content contents_json in
  let tags = unformat_service_return unformat_list_tag tags_json in
  (contents, tags)
