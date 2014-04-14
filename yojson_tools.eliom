(*
** Json tools
** This module help the translate between json and ocaml
 *)

{shared{

exception Yojson_exc of string

(** {6 Generic yojson deserializer}  *)

(** get the string value from the given yojson *)
let get_string = function
  | `String str -> str
  | _           -> raise (Yojson_exc "It is not a yojson string")

(** get the value from the given yojson assoc *)
let get_assoc = function
  | `Assoc yj   -> yj
  | _           -> raise (Yojson_exc "It is not a yojson assoc")

(** get the value from the given yojson list *)
let get_list = function
  | `List yj    -> yj
  | `Null       -> []
  | _           -> raise (Yojson_exc "It is not a yojson list")

(** Get the first value of the given name from a yojson list  *)
let rec get_from_list name = function
  | (n, v)::tail when n = name  -> Some v
  | _::tail                     -> get_from_list name tail
  | []                          -> None

(** get all the values from the given yojson string list *)
let get_string_list string_list =
  try
    List.map get_string string_list
  with
  | Yojson_exc _        -> raise (Yojson_exc "It is not a string list")

(** map the given yojson with the given func *)
let map func = function
  | Some x      -> Some (func x)
  | None        -> None


(** {6 Pumgrana format deserializer}  *)


(** Get service return data *)
let get_service_return func = function
  | `Assoc [(_, _); (_, data)]          -> func data
  | `Assoc [(_, _); (_, _); (_, data)]  -> func data
  | _                                   ->
    raise (Yojson_exc "Bad service return format")

(** Get content_id return  *)
let get_content_id_return = function
  | `Assoc [(_, _); ("content_id", `List [`String id])] -> id
  | _                                                   ->
    raise (Yojson_exc  "Bad service return format")

(** deserialize content from yojson to ocaml format *)
let get_content content_yojson =
  let content = get_assoc content_yojson in
  let id = get_from_list "_id" content in
  let title = get_from_list "title" content in
  let summary = get_from_list "summary" content in (* Currently not used *)
  let text = get_from_list "text" content in
  match title, text, id with
  | Some title, Some text, Some id    ->
    get_string title, get_string text, get_string id
  | _                                 ->
    raise (Yojson_exc  "Bad content format")

(** deserialize link from yojson to ocaml format *)
let get_link link_yojson =
  let link = get_assoc link_yojson in
  let link_id = get_from_list "link_id" link in  (* Currently not used *)
  let content_id = get_from_list "content_id" link in
  let content_title = get_from_list "content_title" link in
  let content_summary = get_from_list "content_summary" link in
  match content_title, content_summary, content_id with
  | Some title, Some summary, Some id   ->
    get_string title, get_string summary, get_string id
  | _                                 ->
    raise (Yojson_exc  "Bad link format")

(** deserialize tag from yojson to ocaml format *)
let get_tag tag_yojson =
  let tag = get_assoc tag_yojson in
  let id = get_from_list "_id" tag in
  let subject = get_from_list "subject" tag in
  match subject, id with
  | Some subject, Some id       -> get_string subject, get_string id
  | _                           -> raise (Yojson_exc  "Bad tag format")

(** deserialize tag list from yojson to ocaml *)
let get_tag_list tl =
  List.map get_tag (get_list tl)

(** deserialize content list from yojson to ocaml *)
let get_content_list tl =
  List.map get_content (get_list tl)

(** deserialize link list from yojson to ocaml *)
let get_link_list tl =
  List.map get_link (get_list tl)

}}
