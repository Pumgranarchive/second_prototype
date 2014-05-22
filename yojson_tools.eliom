(*
** Json tools
** This module help the translate between json and ocaml
 *)

{shared{

module Yojson = Yojson.Basic

open Yojson.Util

exception Yojson_exc of string

(** return the Some member associate of name, and None if not found *)
let opt_member name json =
  try Some (member name json)
  with _ -> None

(** Extract a list from JSON array or raise Yojson_exc.
    `Null are assume as empty list. *)
let to_list = function
  | `Null   -> []
  | `List l -> l
  | _       -> raise (Yojson_exc "Bad list format")

(** map the given yojson with the given func *)
let map func = function
  | Some x      -> Some (func x)
  | None        -> None

(** {6 Pumgrana format deserializer}  *)


(** Get service return data *)
let get_service_return func json =
  let data =
    try
      print_endline (Yojson.to_string json);
      let assoc_list = to_assoc json in
      let idx_last_element = (List.length assoc_list) - 1 in
      let name, data = List.nth assoc_list idx_last_element in
      data
    with
    | _ -> raise (Yojson_exc "Bad service return format")
  in
  func data


(** Get id into content_id return  *)
let get_content_id_return json =
  try
    let json_content_id = member "content_id" json in
    to_string (List.hd (to_list json_content_id))
  with
  | _ -> raise (Yojson_exc "Bad content_id format")

(** deserialize content from yojson to ocaml format *)
let get_content json_content =
  try
    let id = member "_id" json_content in
    let title = member "title" json_content in
    (* Summary is currently not used *)
    (* let summary = member "summary" json_content in *)
    let text = member "text" json_content in
    to_string title, to_string text, to_string id
  with
  | _ -> raise (Yojson_exc  "Bad content format")

(** deserialize link from yojson to ocaml format *)
let get_link json_link =
  try
    (* Link id is currently not used *)
    (* let link_id = member "link_id" json_link in *)
    let content_id = member "content_id" json_link in
    let content_title = member "content_title" json_link in
    let content_summary = member "content_summary" json_link in
    to_string content_title, to_string content_summary, to_string content_id
  with
  | _ -> raise (Yojson_exc  "Bad link format")

(** deserialize tag from yojson to ocaml format *)
let get_tag json_tag =
  try
    let id = member "_id" json_tag in
    let subject = member "subject" json_tag in
    to_string subject, to_string id
  with
  | _ -> raise (Yojson_exc  "Bad tag format")

(** deserialize tag list from yojson to ocaml *)
let get_tag_list tl =
  List.map get_tag (to_list tl)

(** deserialize content list from yojson to ocaml *)
let get_content_list tl =
  List.map get_content (to_list tl)

(** deserialize link list from yojson to ocaml *)
let get_link_list tl =
  List.map get_link (to_list tl)

}}
