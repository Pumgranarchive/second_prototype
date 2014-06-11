(*
** API tools
** This module provides some tools to help API implementation
*)

open Lwt

module Yojson = Yojson.Basic

(*** Generation tools  *)

Random.init 0

let new_rand () = Random.float 200.

let process_rand = new_rand ()

let new_id () = process_rand +. new_rand () +. Unix.time ()

(*** Static string for configuration / selection *)

(** content type return by API' services *)
(* let content_type = "application/json" *)

let id_field = "_id"
let tagsid_field = "tags_id"
let targetid_field = "target_id"
let originid_field = "origin_id"
let title_field = "title"
let summary_field = "summary"
let text_field = "text"
let subject_field = "subject"
let type_field = "type"

let contents_ret_name = "contents"
let tags_ret_name = "tags"
let links_ret_name = "links"
let link_id_ret_name = "link_id"
let content_ud_ret_name = "content_"
let content_id_ret_name = "content_id"
let content_title_ret_name = "content_title"
let content_summary_ret_name = "content_summary"
let tagsid_ret_name = "tags_id"
let linksid_ret_name = "links_id"
let detail_ret_name = "links_id"

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
       (Bson.add_element summary_field yes_value
          (Bson.add_element text_field yes_value Bson.empty)))

let tag_format =
  Bson.add_element id_field yes_value
    (Bson.add_element subject_field yes_value Bson.empty)

let link_format =
  Bson.add_element id_field yes_value
    (Bson.add_element title_field yes_value
       (Bson.add_element summary_field yes_value Bson.empty))


(*** Cast tools *)

let objectid_of_string str =
  try
    Bson.create_objectId str
  with
  | Bson.Invalid_objectId     ->
    raise API_conf.(Pum_exc (return_no_content, errstr_not_objectid str))

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

let string_of_objectid id = string_of_id (Bson.get_objectId id)

(** Cast single bson document to yojson *)
let yojson_of_bson bson =
  let tmp = Yojson.from_string (Bson.to_simple_json bson) in
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

let json_of_ocsigen_string_stream input_type_opt ostream_opt =
  let _ = match input_type_opt with
    | None                                -> raise API_conf.(Pum_exc(return_not_found, "The content_type have to not be None."))
    | Some (("application", "json"), _)   -> ()
    | Some _                              -> raise API_conf.(Pum_exc(return_not_found, "The content_type have to be application/json."))
  in
  match ostream_opt with
  | None                -> raise API_conf.(Pum_exc(return_not_found, "The ocsigen stream have to not be None."))
  | Some ostream        ->
    let stream = Ocsigen_stream.get ostream in
    lwt str_json = Ocsigen_stream.string_of_stream 100000 stream in
    Lwt.return (Yojson.from_string str_json)

(*** Getter tools  *)

let get_id_state coll =
  List.map (fun e -> string_of_id
    (Bson.get_objectId (Bson.get_element id_field e)))
    (MongoReply.get_document_list (Mongo.find coll))

let get_last_created_id coll saved_state =
  let new_state = get_id_state coll in
  let rec aux ns = function
    | []        -> ns
    | h::t      ->
      if (List.exists (fun e -> (String.compare h e) == 0) saved_state)
      then aux ns t
      else aux (h::ns) t
  in
  let ret = aux [] new_state in
  if ret = []
  then failwith "not any new id found"
  else ret

(*** Checking tools  *)

let check_empty_yojson yj original_value =
  if (yj = `List [])
  then raise API_conf.(Pum_exc(return_not_found,
                               errstr_not_found original_value))
  else yj

let check_empty_ocaml_list l original_value =
  if (l = [])
  then raise API_conf.(Pum_exc(return_not_found,
                               errstr_not_found original_value))
  else l

let check_empty_bson bs =
  check_empty_ocaml_list (MongoReply.get_document_list bs)

let check_exist coll id =
  let object_id = objectid_of_string id in
  let bson_query = Bson.add_element id_field object_id Bson.empty in
  let result = MongoReply.get_document_list (Mongo.find_q coll bson_query) in
  if (result = [])
  then raise API_conf.(Pum_exc(return_not_found, API_conf.errstr_not_found id))

let check_not_exist coll field bson original =
  let bson_query = Bson.add_element field bson Bson.empty in
  let result = MongoReply.get_document_list (Mongo.find_q coll bson_query) in
  if (result != [])
  then raise API_conf.(Pum_exc(return_not_found, API_conf.errstr_exist original))

(*** Checking and cast tools  *)

let objectid_of_tagstr id =
  check_exist tags_coll id;
  objectid_of_string id

(*** Http tools  *)

let return_of_json yojson =
  yojson >>= (fun yj -> Lwt.return (Yojson.to_string yj, "application/json"))

let return_of_error str =
  str >>= (fun s -> Lwt.return (s, "application/json"))

(*** Manage return tools *)

(** Removing the value from text field *)
let link_format_ret link_ids json_l =
  let rec formater id nl = function
    | []                -> (link_id_ret_name, `String id)::(List.rev nl)
    | (name, v)::tail   ->
      let new_name = if (String.compare name id_field == 0)
        then content_id_ret_name
        else content_ud_ret_name ^ name
      in
      formater id ((new_name, v)::nl) tail
  in
  let rec scanner nl = function
    | (`Assoc link)::assoc_t, id_h::id_t        ->
      scanner ((`Assoc (formater id_h [] link))::nl) (assoc_t, id_t)
    | assocs_h::assocs_t, _::id_t               ->
      scanner (assocs_h::nl) (assocs_t, id_t)
    | _::_, []                                  ->
      raise API_conf.(Pum_exc (return_not_found, "link_formater: no enought ids."))
    | [], _::_                                  ->
      raise API_conf.(Pum_exc (return_not_found, "link_formater: too many enought ids."))
    | [], []                                    -> nl
  in
  match json_l with
  | `List assocs        -> `List (scanner [] (assocs, List.rev link_ids))
  | yl                  -> yl

(** Format the returned value *)
let format_ret ?param_name status ?error_str (param_value:Yojson.json) =
  let assoc_list = [] in
  let assoc_list_1 =
    match param_name with
    | None      -> assoc_list
    | Some name -> (name, param_value)::assoc_list
  in
  let assoc_list_2 =
    match error_str with
    | None      -> assoc_list_1
    | Some str  -> ("error", `String str)::assoc_list_1
  in
  `Assoc (("status", `Int status)::assoc_list_2)

(** [check_return func content_name]
    func: the function which return the result.
    content_name: the name of the content in the result.

    If the exception API_conf.Pum_exc (value, error_str) is fired,
    this data are use for the return.
    For any others exceptions, internal server error (500) is returned *)
let check_return ?(default_return=API_conf.return_ok) ?param_name func =
  let null_return () =
    Lwt.return (format_ret ?param_name API_conf.return_no_content `Null)
  in
  let error_return status error_str =
    Lwt.return (format_ret ?param_name status ~error_str `Null)
  in
  let valided_return ret =
    Lwt.return (format_ret ?param_name default_return (`List ret))
  in
  try_lwt
    lwt res = func () in
    match_lwt Lwt.return (res, param_name) with
    | `Null, _          -> null_return ()
    | `List [], _       -> null_return ()
    | `List ret, _      -> valided_return ret
    | ret, _            -> valided_return [ret]
  with
  | API_conf.Pum_exc (v, str)   ->
    begin
      print_endline ((string_of_int v) ^ ": " ^ str);
      error_return v str
    end
  | exc                         ->
    begin
      print_endline (Printexc.to_string exc);
      error_return API_conf.return_internal_error API_conf.errstr_internal_error
    end

let bad_request ?(error_value=API_conf.return_not_found) error_str =
  print_endline ((string_of_int error_value) ^ ": " ^ error_str);
  Lwt.return (Yojson.to_string (format_ret error_value ~error_str `Null))

let manage_bad_request aux =
  try_lwt
    aux ()
  with
  | API_conf.Pum_exc (_, str)   ->
    return_of_error (bad_request str)
  | exc                         ->
    print_endline (Printexc.to_string exc);
    return_of_error (bad_request ~error_value:API_conf.return_internal_error
                       API_conf.errstr_internal_error)
