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

(** Convert string objectID from mongo in Hexa 12 char format
    into string in hex 24 char format *)
let string_of_id str_id =
  try
    let base = 16 in
    let length = 12 in
    let buf = Buffer.create length in
    let char_of_hex_int h =
      String.get (Printf.sprintf "%x" h) 0
    in
    let rec aux position =
      if (position >= length) then ()
      else
        begin
          let n = Char.code (String.get str_id position) in
          let n1 = n mod base in
          let n2 = (n - n1) / base in
          Buffer.add_char buf (char_of_hex_int n2);
          Buffer.add_char buf (char_of_hex_int n1);
          aux (position + 1)
        end
    in
    aux 0;
    Buffer.contents buf
  with
  | e -> print_endline (Printexc.to_string e); "0"

(** Cast single bson document to yojson *)
let yojson_of_bson bson =
  let tmp = Yj.from_string (Bson.to_simple_json bson) in
  let rec check_id_format newlist = function
    | []                        -> newlist
    | ("_id", `String id)::tail ->
      check_id_format (("_id", `String (string_of_id id))::newlist) tail
    | head::tail                -> check_id_format (head::newlist) tail
  in
  match tmp with
  | `Assoc list -> `Assoc (check_id_format [] list)
  | _           -> tmp

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

(** Removing the value from text field *)
let removing_text_field = function
  | `Assoc list ->
    let rec remover nl = function
      | []                -> nl
      | ("text", _)::tail -> remover (("text", `String "")::nl) tail
      | head::tail        -> remover (head::nl) tail
    in `Assoc (remover [] list)
  | yl          -> yl

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
