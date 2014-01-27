(*
** API tools
** This module provides some tools to help API implementation
*)

module Yj = Yojson.Safe

(*** Static string for configuration / selection *)

(** content type return by API' services *)
let content_type = "application/json"

let id_field = "_id"
let tagsid_field = "tags_id"
let tags_field = "tags"
let targetid_field = "target_id"
let originid_field = "origin_id"
let text_field = "text"
let title_field = "title"
let subject_field = "subject"
let type_field = "type"

let contents_ret_name = "contents"
let tags_ret_name = "tags"
let links_ret_name = "links"

(*** DB's collection *)

let contents_coll = Mongo.create API_conf.db_url API_conf.db_port
  API_conf.db_name API_conf.contents_coll_name
let tags_coll = Mongo.create API_conf.db_url API_conf.db_port
  API_conf.db_name API_conf.tags_coll_name
let links_coll = Mongo.create API_conf.db_url API_conf.db_port
  API_conf.db_name API_conf.links_coll_name

(*** Request format tools  *)

let yes_value = Bson.create_boolean true

let content_format =
  Bson.add_element id_field yes_value
    (Bson.add_element title_field yes_value
       (Bson.add_element text_field yes_value Bson.empty))

let tag_format =
  Bson.add_element id_field yes_value
    (Bson.add_element subject_field yes_value Bson.empty)

(*** Cast tools *)

(** Cast single bson document to yojson *)
let yojson_of_bson bson =
  Yj.from_string (Bson.to_simple_json bson)

(** Cast list of bson document to yojson *)
let yojson_of_bson_list bson_l =
  let rec aux yojson_l = function
    | []        -> yojson_l
    | h::t      -> aux ((yojson_of_bson h)::yojson_l) t
  in
  `List (List.rev (aux [] bson_l))

(* (\** Cast single mongreply document to yojson *\) *)
(* let yojson_of_mongoreply mrd = *)
(*     yojson_of_bson (List.hd (MongoReply.get_document_list mrd)) *)

(** Cast mongreply document list to yojson list *)
let yojson_of_mongoreply mrd =
    yojson_of_bson_list (MongoReply.get_document_list mrd)

(*** Manage return tools *)

(** Format the returned value *)
let format_ret param_name status (param_value:Yj.json) =
  `Assoc [("status", `Int status);
          (param_name, param_value) ]

(** [check_return func content_name]
    func: the function which return the result.
    content_name: the name of the content in the result.

    If the exception Failure "no content" is fired, 204 is returned.
    For any others exceptions, 500 is returned *)
let check_return func content_name =
  let null_return () =
    format_ret content_name API_conf.return_no_content `Null
  in
  let valided_return ret =
    format_ret content_name API_conf.return_ok (`List ret)
  in
  try
    match func () with
    | `Null     -> null_return ()
    | `List []  -> null_return ()
    | `List ret -> valided_return ret
    | ret       -> valided_return [ret]
  with
  | Failure "no content"        -> null_return ()
  | exc                         ->
    begin
      print_endline (Printexc.to_string exc);
      format_ret content_name API_conf.return_fail `Null
    end
