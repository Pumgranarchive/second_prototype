(*
  API Core
  This Module do request to data base and format well to return it in service
 *)

module Yj = Yojson.Safe

(*
** Content
*)

(*** Getters *)
let get_detail content_id =
  let aux () =
    let object_id = API_tools.objectid_of_string content_id in
    let bson_query = Bson.add_element API_tools.id_field object_id Bson.empty
    in
    let result = Mongo.find_q_s_one API_tools.contents_coll bson_query
      API_tools.content_format in
    let ret = API_tools.yojson_of_mongoreply result in
    API_tools.check_empty_yojson ret content_id
  in
  API_tools.check_return ~param_name:API_tools.contents_ret_name aux

let get_content_id_from_link link_id =
  let object_id = API_tools.objectid_of_string link_id in
  let bson_query =
    Bson.add_element API_tools.id_field object_id Bson.empty
  in
  let result = Mongo.find_q_one API_tools.links_coll bson_query in
  let result_bson =
    List.hd (API_tools.check_empty_ocaml_list
               (MongoReply.get_document_list result)
               link_id)
  in
  Bson.get_element API_tools.targetid_field result_bson

let get_detail_by_link link_id =
  let aux () =
    let target_id = get_content_id_from_link link_id in
    let bson_query =
      Bson.add_element API_tools.id_field target_id Bson.empty
    in
    let result = Mongo.find_q_s_one API_tools.contents_coll bson_query
      API_tools.content_format in
    let ret = API_tools.yojson_of_mongoreply result in
    API_tools.check_empty_yojson ret link_id
  in
  API_tools.check_return ~param_name:API_tools.contents_ret_name aux

(* Currently, filter is not used,
   because we haven't enought informations in the DB

   Warning: if one tag_id does not exist, no error will be returned,
   but no content neither.
*)
let get_contents filter tags_id =
  let aux () =
    let () =
      match filter with
      | None                    -> ()
      | Some "MOST_USED"        -> ()
      | Some "MOST_VIEW"        -> ()
      | Some "MOST_RECENT"      -> ()
      | Some x                  ->
        raise API_conf.(Pum_exc (return_not_found, errstr_not_expected x))
    in
    let bson_query = match tags_id with
      | None    -> Bson.empty
      | Some x  -> Bson.add_element API_tools.tagsid_field
        (Bson.create_doc_element
           (MongoQueryOp.all (List.map API_tools.objectid_of_string x)))
        Bson.empty
    in
    let results = Mongo.find_q_s API_tools.contents_coll bson_query
      API_tools.content_format in
    API_tools.yojson_of_mongoreply results
  in
  API_tools.check_return ~param_name:API_tools.contents_ret_name aux

(*** Setters  *)

(* Currently tags_subject is not used *)
let insert_content title text tags_subject =
  let aux () =
    let bson_title = Bson.create_string title in
    let bson_text = Bson.create_string text in
    let content = Bson.add_element API_tools.title_field bson_title
      (Bson.add_element API_tools.text_field bson_text Bson.empty)
    in
  (* ! THE ID NEED TO BE GET BY ANOTHER MANNER ! *)
    let saved_state = API_tools.get_id_state API_tools.contents_coll in
    Mongo.insert API_tools.contents_coll [content];
    `String (API_tools.get_last_created_id API_tools.contents_coll saved_state)
  in
  API_tools.check_return
    ~param_name:API_tools.content_id_ret_name
    ~default_return:API_conf.return_created aux

let update_content content_id title text =
  let aux () =
    API_tools.check_exist API_tools.contents_coll content_id;
    let object_id = API_tools.objectid_of_string content_id in
    let bson_query = Bson.add_element API_tools.id_field object_id Bson.empty
    in
    let content = Bson.empty in
    let content_1 = match title with
      | None      -> content
      | Some x    -> Bson.add_element
        API_tools.title_field (Bson.create_string x) content
    in
    let content_2 = match text with
      | None      -> content_1
      | Some x    -> Bson.add_element
        API_tools.text_field (Bson.create_string x) content_1
    in
    if content = Bson.empty
    then raise API_conf.(Pum_exc (return_not_found,
                                  "title and text can not be both null"));
    Mongo.update_one API_tools.contents_coll (bson_query, content_2);
    `Null
  in
  API_tools.check_return aux

let delete_content content_id =
  let aux () =
    API_tools.check_exist API_tools.contents_coll content_id;
    let object_id = API_tools.objectid_of_string content_id in
    let bson_query = Bson.add_element API_tools.id_field object_id Bson.empty
    in
    Mongo.delete_one API_tools.contents_coll bson_query;
    `Null
  in
  API_tools.check_return aux

let delete_contents contents_id =
  let aux () =
    let rec deleter ret = function
      | []      -> ret
      | id::t   -> deleter ((delete_content id)::ret) t
    in `List (deleter [] contents_id)
  in
  API_tools.check_return ~param_name:API_tools.detail_ret_name aux

(*
** Tags
*)

(*** Getters *)

(* Warning: if one tag_id does not exist, no error will be fire. *)
let get_tags tags_id =
  let aux () =
    let document_of_tag tag_id =
      (Bson.add_element API_tools.id_field
         (API_tools.objectid_of_string tag_id) Bson.empty)
    in

    let bson_query = match tags_id with
      | []      -> Bson.empty
      | id::t	->  (MongoQueryOp.or_op (List.map document_of_tag tags_id))

    in
    let results = Mongo.find_q_s API_tools.tags_coll bson_query
      API_tools.tag_format in
    API_tools.yojson_of_mongoreply results
  in
  API_tools.check_return ~param_name:API_tools.tags_ret_name aux


let get_tags_by_type tag_type =
  let aux () =
    let bson_type =
      match tag_type with
      | "LINK"     -> Bson.create_string API_conf.link_tag_str
      | "CONTENT"  -> Bson.create_string API_conf.content_tag_str
      | x          ->
        raise API_conf.(Pum_exc (return_not_found, errstr_not_expected x))
    in
    let bson_query =
      Bson.add_element API_tools.type_field bson_type Bson.empty
    in
    let result = Mongo.find_q_s API_tools.tags_coll bson_query
      API_tools.tag_format in
    API_tools.yojson_of_mongoreply result
  in
  API_tools.check_return ~param_name:API_tools.tags_ret_name aux

(* Warning: if one tag_id does not exist, no error will be fire. *)
let get_tags_from_content content_id =
  let aux () =
    (* step 1: request the content *)
    let content_object_id = API_tools.objectid_of_string content_id in
    let content_bson_query =
      Bson.add_element API_tools.id_field content_object_id Bson.empty
    in
    let result_content =
      Mongo.find_q_one API_tools.contents_coll content_bson_query
    in
    let content_bson = List.hd
      (API_tools.check_empty_ocaml_list
         (MongoReply.get_document_list result_content)
         content_id)
    in

    (* step 2: request the associated tags *)
    let tag_id_list =
      Bson.get_list(Bson.get_element API_tools.tagsid_field content_bson)
    in
    let document_of_tag_id_list tag_id_list =
      (Bson.add_element API_tools.id_field tag_id_list Bson.empty)
    in
    let _ = API_tools.check_empty_ocaml_list tag_id_list content_id
    in
    let tag_bson_query =
      (MongoQueryOp.or_op
      	 (List.map document_of_tag_id_list tag_id_list))
    in
    let results_tag = Mongo.find_q_s API_tools.tags_coll tag_bson_query
      API_tools.tag_format in
    API_tools.yojson_of_mongoreply results_tag
  in
  API_tools.check_return ~param_name:API_tools.tags_ret_name aux

(* Warning: if a tag_id does not exist no error will be fire. *)
let get_tags_from_content_link content_id =
  let aux () =

    (* step 1: get links related to the content*)
    let content_object_id = API_tools.objectid_of_string content_id in
    let originid_bson_query =
      Bson.add_element API_tools.originid_field content_object_id Bson.empty
    in
    let result_links = Mongo.find_q API_tools.links_coll originid_bson_query
    in
    let links_bson = MongoReply.get_document_list result_links in
    if links_bson = [] then
      raise API_conf.(Pum_exc (return_no_content, "This content has no link."));

    (* step 2: request the related tags *)
    let rec remove_duplicate list =
      let rec aux new_list = function
        | []      -> new_list
        | e::t    ->
          if (List.exists (fun c -> (String.compare c e) = 0) new_list)
          then aux new_list t
          else aux (e::new_list) t
      in
      List.map API_tools.objectid_of_string
        (aux [] (List.map Bson.get_objectId list))
    in
    let rec create_tag_list list =
      let get_tags current_link = Bson.get_list
        (Bson.get_element API_tools.tags_field current_link)
      in
      let rec aux new_list = function
        | []	-> new_list
        | l::t	-> aux ((get_tags l)@new_list) t
      in
      aux [] list

    in
    let tags_id = remove_duplicate (create_tag_list links_bson) in
    if tags_id = [] then
      raise API_conf.(Pum_exc (return_no_content, "These links has no tag."));

    let document_of_tag tag_id =
      Bson.add_element API_tools.id_field tag_id Bson.empty
    in
    let bson_tags_id_list = List.map document_of_tag tags_id in
    let bson_query = MongoQueryOp.or_op bson_tags_id_list in
    let results = Mongo.find_q_s API_tools.tags_coll bson_query
      API_tools.tag_format in
    API_tools.yojson_of_mongoreply results
  in
  API_tools.check_return ~param_name:API_tools.tags_ret_name aux

(*
** Links
*)

(*** Getters *)
let get_links_from_content content_id =
  let aux () =
    (* getting every link with 'content_id' as origin *)
    let content_objectId = API_tools.objectid_of_string content_id in

    let result_content =
      Mongo.find_q API_tools.links_coll
        (Bson.add_element API_tools.originid_field content_objectId Bson.empty)
    in
    let content_bson_list = MongoReply.get_document_list result_content in
    if content_bson_list = [] then
      raise API_conf.(Pum_exc (return_no_content, "This content has no link."));

    let make_link_id_list content_bson =
      Bson.add_element API_tools.id_field (Bson.get_element API_tools.targetid_field content_bson) Bson.empty in

    let link_id_list = List.map make_link_id_list content_bson_list in

    (* getting content to return *)
    let link_query    = (MongoQueryOp.or_op link_id_list)
    in
    let result_query  = Mongo.find_q_s API_tools.contents_coll link_query
      API_tools.link_format
    in
    API_tools.removing_text_field
      (API_tools.yojson_of_mongoreply result_query)
  in
  API_tools.check_return ~param_name:API_tools.links_ret_name aux

let get_links_from_content_tags content_id tags_id =
  let aux () =
    (* getting every link with 'content_id' as origin *)
    let content_objectId = API_tools.objectid_of_string content_id in

    let document_of_tag tag_id =
      Bson.add_element API_tools.tags_field
        (API_tools.objectid_of_string tag_id) Bson.empty
    in
    let bson_tags_id_list = List.map document_of_tag tags_id in
    let bson_tags_condition = MongoQueryOp.and_op bson_tags_id_list in
    let result_content =
      Mongo.find_q API_tools.links_coll
        (Bson.add_element API_tools.originid_field content_objectId bson_tags_condition)
    in
    let content_bson_list = MongoReply.get_document_list result_content in
    if content_bson_list = [] then
      raise API_conf.(Pum_exc (return_no_content, "This content has no link."));

    let make_link_id_list content_bson =
      Bson.add_element API_tools.id_field (Bson.get_element API_tools.targetid_field content_bson) Bson.empty in

    let link_id_list = List.map make_link_id_list content_bson_list in

    (* getting content to return *)
    let link_query    = (MongoQueryOp.or_op link_id_list)
    in
    let result_query  = Mongo.find_q_s API_tools.contents_coll link_query
      API_tools.link_format
    in
    API_tools.removing_text_field
      (API_tools.yojson_of_mongoreply result_query)
  in
  API_tools.check_return ~param_name:API_tools.links_ret_name aux
