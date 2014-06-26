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

let id_of_str_uri str_uri =
  let uri = Rdf_store.uri_of_string str_uri in
  try
    if Rdf_store.is_pumgrana_uri str_uri
    then Id (Rdf_store.content_id_of_uri uri)
    else Uri uri
  with Nosql_store.Invalid_id s -> Uri uri

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
    | e -> print_endline (Printexc.to_string e);
      raise (Yojson_exc "Bad service return format")
  in
  func data

(** Get id into content_id return  *)
let get_content_uri_return json =
  try
    let json_content_uri = member "content_uri" json in
    to_string (List.hd (to_list json_content_uri))
  with
  | e -> print_endline (Printexc.to_string e);
    raise (Yojson_exc "Bad content_uri format")

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
      Internal (id_of_str_uri (to_string uri),
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
    id_of_str_uri (to_string uri),
    to_string title, to_string summary
  with
  | e -> print_endline (Printexc.to_string e);
    raise (Yojson_exc  "Bad short content format")

(** deserialize link from yojson to ocaml format *)
let get_link json_link =
  try
    let link_id = member "link_uri" json_link in
    let content_uri = member "content_uri" json_link in
    let content_title = member "content_title" json_link in
    let content_summary = member "content_summary" json_link in
    let content_str_uri = to_string content_uri in
    let uri = id_of_str_uri content_str_uri in
    Rdf_store.link_id_of_string (to_string link_id), uri,
    to_string content_title, to_string content_summary
  with
  | e -> print_endline (Printexc.to_string e);
    raise (Yojson_exc  "Bad link format")

(** deserialize tag from yojson to ocaml format *)
let get_tag json_tag =
  try
    let uri = to_string (member "uri" json_tag) in
    let subject = to_string (member "subject" json_tag) in
    Rdf_store.uri_of_string uri, subject
  with
  | e -> print_endline (Printexc.to_string e);
    raise (Yojson_exc  "Bad tag format")

(** deserialize json link detail to ocaml  *)
let get_link_detail json_detail =
  try
    let link_uri = to_string (member "link_uri" json_detail) in
    let origin_uri = to_string (member "origin_uri" json_detail) in
    let target_uri = to_string (member "target_uri" json_detail) in
    let tags = to_list (member "tags" json_detail) in
    Rdf_store.link_id_of_string link_uri,
    Rdf_store.uri_of_string origin_uri,
    Rdf_store.uri_of_string target_uri,
    List.map get_tag tags
  with
  | e -> print_endline (Printexc.to_string e);
    raise (Yojson_exc  "Bad link detail format")

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

(** deserialize detail link list from yojson to ocaml *)
let get_detail_link_list tl =
  List.map get_link_detail (to_list tl)


}}
