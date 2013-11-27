(*
** API tools
** This module provides some tools to help API implementation
*)

module Yj = Yojson.Safe

(* content type return by API' services *)
let content_type = "application/json"

let id_field = "_id"
let tagsid_field = "tags_id"
let targetid_field = "target_id"
let originid_field = "origin_id"
let text_field = "text"
let title_field = "title"
let subject_field = "subject"
let type_field = "type"

(* DB's collection *)
let contents_coll = Mongo.create API_conf.db_url API_conf.db_port
  API_conf.db_name API_conf.contents_coll_name
let tags_coll = Mongo.create API_conf.db_url API_conf.db_port
  API_conf.db_name API_conf.tags_coll_name
let links_coll = Mongo.create API_conf.db_url API_conf.db_port
  API_conf.db_name API_conf.links_coll_name

let yes_value = Bson.create_boolean true

let content_format =
  Bson.add_element id_field yes_value
    (Bson.add_element title_field yes_value
       (Bson.add_element text_field yes_value Bson.empty))

let tag_format =
  Bson.add_element id_field yes_value
    (Bson.add_element subject_field yes_value Bson.empty)

(*** This part of code is currently not use  *)
let content ((id:int), (title:string), (text:string)) =
  `Assoc [("id", `Int id);
          ("title", `String title);
          ("text", `String text)]

let flist func l =
  let rec aux nl = function
  | h::t        -> aux ((func h)::nl) t
  | []          -> nl
  in `List (List.rev (aux [] l))

let content_list = flist content

let tag ((id:int), (subject:string)) =
    `Assoc [("id", `Int id);
            ("subject", `String subject)]

let tag_list = flist tag
(*** End of unused part of code  *)

(** Help to format the returned value *)
let return_f param_name status (param_value:Yj.json) =
  `Assoc [("status", `Int status);
          (param_name, param_value) ]

(** Help to format detail return *)
let detail_f = return_f "content"

(** Help to format contents return *)
let contents_f = return_f "contents"

(** Help to format API_service.tags return *)
let tags_f = return_f "tags"

(** Help to format API_service.tags return *)
let links_f = return_f "links"
