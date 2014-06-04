(*
** API_deserializer
** This module provide tools to deserialize yojson for API's services.
 *)

module Yojson = Yojson.Basic

open Yojson.Util
open Intern_yj_util
open API_conf

(** Get insert content input data *)
let get_insert_content_data json_content =
  try
    let title = member "title" json_content in
    let summary = member "summary" json_content in
    let text = member "text" json_content in
    let tags_id = opt_member "tags_id" json_content in
    to_string title, to_string summary, to_string text,
    map (fun x -> List.map to_string (to_list x)) tags_id
  with
  | _ -> raise (Pum_exc (return_not_found, "Bad insert_content format"))

(** Get update content input data *)
let get_update_content_data json_content =
  try
    let content_id = member "content_id" json_content in
    let title = opt_member "title" json_content in
    let summary = opt_member "summary" json_content in
    let text = opt_member "text" json_content in
    let tags_id = opt_member "tags_id" json_content in
    to_string content_id, map to_string title, map to_string summary,
    map to_string text, map (fun x -> List.map to_string (to_list x)) tags_id
  with
  | _ -> raise (Pum_exc (return_not_found, "Bad update_content format"))

(** Get delete contents input data *)
let get_delete_contents_data json_content =
  try
    let contents_id = member "contents_id" json_content in
    List.map to_string (to_list contents_id)
  with
  | _ -> raise (Pum_exc (return_not_found, "Bad delete_contents format"))

(** Get insert tags input data *)
let get_insert_tags_data json_content =
  try
    let type_name = member "type_name" json_content in
    let id = opt_member "id" json_content in
    let tags_subject = member "tags_subject" json_content in
      to_string type_name, map to_string id,
      List.map to_string (to_list tags_subject)
  with
  | _ -> raise (Pum_exc (return_not_found, "Bad insert_tags format"))

(** Get delete tags input data *)
let get_delete_tags_data json_content =
  try
    let tags_id = member "tags_id" json_content in
    List.map to_string (to_list tags_id)
  with
  | _ -> raise (Pum_exc (return_not_found, "Bad delete_tags format"))

(** Get insert links input data *)
let get_insert_links_data json_content =
  try
    let data = to_list (member "data" json_content) in
    let aux json =
      let origin_uri = member "origin_uri" json in
      let target_uri = member "target_uri" json in
      let tags_uri = member "tags_uri" json in
      (to_string origin_uri, to_string target_uri,
       List.map to_string (to_list tags_uri))
    in
    List.map aux data
  with
  | _ -> raise (Pum_exc (return_not_found, "Bad insert_links format"))

(** Get update link input data *)
let get_update_links_data json_content =
  try
    let data = to_list (member "data" json_content) in
    let aux json =
      let link_uri = member "link_uri" json in
      let tags_uri = member "tags_uri" json in
      (to_string link_uri, List.map to_string (to_list tags_uri))
    in
    List.map aux data
  with
  | _ -> raise (Pum_exc (return_not_found, "Bad insert_links format"))

(** Get delete links input data *)
let get_delete_links_data json_content =
  try
    let links_id = member "links_id" json_content in
    List.map to_string (to_list links_id)
  with
  | _ -> raise (Pum_exc (return_not_found, "Bad delete_links format"))
