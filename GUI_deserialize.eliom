{shared{

(*
** GUI deserliaze
** Deserializer for API to GUI
 *)

module Yojson = Yojson.Basic

open Yojson.Util
open Intern_yj_util

type id =
| Id of Nosql_store.id
| Uri of Rdf_store.uri

type content =
| Internal of (id * string * string * string)
| External of (id * string * string)

let uri_of_id = function
  | Id id   -> Rdf_store.(string_of_uri (uri_of_content_id id))
  | Uri uri -> Rdf_store.string_of_uri uri

let string_of_id = function
  | Id id   -> Nosql_store.string_of_id id
  | Uri uri -> Rdf_store.string_of_uri uri

let id_of_str_uri uri =
    Rdf_store.(content_id_of_uri (uri_of_string uri))

(** {6 Pumgrana format deserializer}  *)

(** Get service return data *)
let get_service_return func json =
  let data =
    try
      let assoc_list = to_assoc json in
      let idx_last_element = (List.length assoc_list) - 1 in
      let name, data = List.nth assoc_list idx_last_element in
      data
    with
    | _ -> raise (Yojson_exc "Bad service return format")
  in
  func data

(** Get id into content_id return  *)
let get_content_uri_return json =
  try
    let json_content_uri = member "content_uri" json in
    to_string (List.hd (to_list json_content_uri))
  with
  | _ -> raise (Yojson_exc "Bad content_uri format")

(** deserialize content from yojson to ocaml format *)
let get_content json_content =
  try
    let uri = member "uri" json_content in
    let title = member "title" json_content in
    let summary = member "summary" json_content in
    let body = member "body" json_content in
    if body == `Null then
      External (Uri (Rdf_store.uri_of_string (to_string uri)),
                to_string title, to_string summary)
    else
      Internal (Id (id_of_str_uri (to_string uri)),
                to_string title, to_string summary, to_string body)
  with
  | e -> print_endline (Printexc.to_string e);
    raise (Yojson_exc  "Bad content format")

(** deserialize short content from yojson to ocaml format *)
let get_short_content json_content =
  try
    let uri = member "uri" json_content in
    let title = member "title" json_content in
    let summary = member "summary" json_content in
    to_string title, to_string summary,
    Nosql_store.string_of_id (id_of_str_uri (to_string uri))
  with
  | _ -> raise (Yojson_exc  "Bad short content format")

(** deserialize link from yojson to ocaml format *)
let get_link json_link =
  try
    let link_id = member "link_uri" json_link in
    let content_uri = member "content_uri" json_link in
    let content_title = member "content_title" json_link in
    let content_summary = member "content_summary" json_link in
    let content_str_uri = to_string content_uri in
    let uri =
      if Rdf_store.is_pumgrana_uri content_str_uri
      then Id (id_of_str_uri content_str_uri)
      else Uri (Rdf_store.uri_of_string content_str_uri)
    in
    Rdf_store.link_id_of_string (to_string link_id), uri,
    to_string content_title, to_string content_summary
  with
  | e -> print_endline (Printexc.to_string e);
    raise (Yojson_exc  "Bad link format")

(** deserialize tag from yojson to ocaml format *)
let get_tag json_tag =
  try
    let uri = member "uri" json_tag in
    let subject = member "subject" json_tag in
    to_string subject, to_string uri
  with
  | _ -> raise (Yojson_exc  "Bad tag format")

(** deserialize tag list from yojson to ocaml *)
let get_tag_list tl =
  List.map get_tag (to_list tl)

(** deserialize content list from yojson to ocaml *)
let get_content_list tl =
  List.map get_content (to_list tl)

(** deserialize short content list from yojson to ocaml *)
let get_short_content_list tl =
  List.map get_short_content (to_list tl)

(** deserialize link list from yojson to ocaml *)
let get_link_list tl =
  List.map get_link (to_list tl)

}}
