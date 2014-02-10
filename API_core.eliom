(*
  API Core
  This Module do request to data base and format well to return it in service
 *)

module Yj = Yojson.Safe

(*
** Generic request
*)

let delete_from coll ids =
  let aux () =
    let objectid_of_idstr str =
      let object_id = API_tools.objectid_of_string str in
      API_tools.check_exist coll str;
      Bson.add_element API_tools.id_field object_id Bson.empty
    in
    let bson_query = MongoQueryOp.or_op (List.map objectid_of_idstr ids) in
    Mongo.delete_all coll bson_query;
    `Null
  in
  API_tools.check_return aux

(*
** Content
*)

(*** Getters *)
let get_detail content_id =
  let aux () =
    let object_id = API_tools.objectid_of_string content_id in
    let bson_query = Bson.add_element API_tools.id_field object_id Bson.empty in
    let result = Mongo.find_q_s_one API_tools.contents_coll bson_query
      API_tools.content_format in
    let ret = API_tools.yojson_of_mongoreply result in
    API_tools.check_empty_yojson ret content_id
  in
  API_tools.check_return ~param_name:API_tools.contents_ret_name aux

let get_content_id_from_link link_id =
  let object_id = API_tools.objectid_of_string link_id in
  let bson_query = Bson.add_element API_tools.id_field object_id Bson.empty in
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
    let bson_query = Bson.add_element API_tools.id_field target_id Bson.empty in
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
let insert_content title text tags_id =
  let aux () =
    let bson_title = Bson.create_string title in
    let bson_text = Bson.create_string text in
    let bson_tags_list = match tags_id with
      | None    -> []
      | Some x  -> List.map API_tools.objectid_of_tagstr x
    in
    let bson_tagsid = Bson.create_list bson_tags_list in
    let content = Bson.add_element API_tools.title_field bson_title
      (Bson.add_element API_tools.text_field bson_text
         (Bson.add_element API_tools.tagsid_field bson_tagsid Bson.empty))
    in
    let saved_state = API_tools.get_id_state API_tools.contents_coll in
    Mongo.insert API_tools.contents_coll [content];
    `String (API_tools.string_of_id
               (List.hd (API_tools.get_last_created_id
                           API_tools.contents_coll saved_state)))
  in
  API_tools.check_return
    ~param_name:API_tools.content_id_ret_name
    ~default_return:API_conf.return_created aux

let update_content content_id title text tags_id =
  let aux () =
    API_tools.check_exist API_tools.contents_coll content_id;
    let object_id = API_tools.objectid_of_string content_id in
    let bson_query = Bson.add_element API_tools.id_field object_id Bson.empty in
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
    let content_3 = match tags_id with
      | None      -> content_2
      | Some x    ->
        let bson_objid = List.map API_tools.objectid_of_tagstr x in
        let bson_list = Bson.create_list bson_objid in
        Bson.add_element API_tools.tagsid_field bson_list content_2
    in
    if content_3 = Bson.empty
    then raise API_conf.(Pum_exc (return_not_found,
                                  "title, text and tags_id can not be all null"));
    Mongo.update_one API_tools.contents_coll (bson_query, content_3);
    `Null
  in
  API_tools.check_return aux

let delete_contents content_ids =
  let aux content_id =
    let object_id = API_tools.objectid_of_string content_id in
    [Bson.add_element API_tools.originid_field object_id Bson.empty;
     Bson.add_element API_tools.targetid_field object_id Bson.empty]
  in
  let ret = delete_from API_tools.contents_coll content_ids in
  let or_list = List.flatten (List.map aux content_ids) in
  let bson_query = MongoQueryOp.or_op or_list in
  Mongo.delete_all API_tools.contents_coll bson_query;
  ret


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
    if (tag_id_list = [])
    then raise API_conf.(Pum_exc(return_not_found, "This content has no tags"));
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

(* Warning: does not return status list, only the id list *)
let insert_tags type_name id_opt subjects =
  let aux () =
    let build_updater coll =

    (* Check existing of the id parameter *)
      match id_opt with
      | Some id  ->

        (* Check existing of content *)
        let object_id = API_tools.objectid_of_string id in
        let bson_query = Bson.add_element API_tools.id_field object_id Bson.empty in
        API_tools.check_exist coll id;

        (* Update tags in list of content tags *)
        (fun bson_ids ->
          let each = MongoQueryOp.each bson_ids in
          let elem = Bson.add_element API_tools.tagsid_field
            (Bson.create_doc_element each) Bson.empty
          in
          let bson_update = MongoQueryOp.addToSet elem in
          Mongo.update_one coll (bson_query, bson_update))

      | None    -> (fun _ -> ())
    in

    (* Check and select the collection *)
    let type_n, update_fun = match type_name with
      | "CONTENT"      -> API_conf.content_tag_str,
        build_updater API_tools.contents_coll
      | "LINK"         -> API_conf.link_tag_str,
        build_updater API_tools.links_coll
      | x              ->
        raise API_conf.(Pum_exc (return_not_found, errstr_not_expected x))
    in

    (* Checking the not existing of the subjects *)
    let bson_of_str s =
      let bson = Bson.create_string s in
      let bson_type_n = Bson.create_string type_n in
      API_tools.check_not_exist API_tools.tags_coll API_tools.subject_field bson s;
      Bson.add_element API_tools.subject_field bson
        (Bson.add_element API_tools.type_field bson_type_n Bson.empty)
    in
    let bson_subjects = List.map bson_of_str subjects in

    (* Creating tags part *)
    let saved_state = API_tools.get_id_state API_tools.tags_coll in
    Mongo.insert API_tools.tags_coll bson_subjects;
    let new_ids =
      API_tools.get_last_created_id API_tools.tags_coll saved_state
    in
    let bson_ids = List.map (fun e -> API_tools.objectid_of_string e) new_ids in

    (* Call the updating part *)
    update_fun bson_ids;

    (* Building the return *)
    `List (List.map (fun e -> `Assoc [(API_tools.id_field, `String e)]) new_ids)

  in
  API_tools.check_return
    ~param_name:API_tools.tagsid_ret_name
    ~default_return:API_conf.return_created
    aux

let delete_tags = delete_from API_tools.tags_coll

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


let insert_links id_from ids_to tags_id =
  let aux () =
    let objectid_of_strid coll field doc str =
      let object_id = API_tools.objectid_of_string str in
      API_tools.check_exist coll str;
      Bson.add_element field object_id doc
    in
    let check_not_exist_link bson_query =
      let result = MongoReply.get_document_list
        (Mongo.find_q API_tools.links_coll bson_query)
      in
      let str = Bson.to_simple_json bson_query in
      if (result != [])
      then raise API_conf.(Pum_exc(return_not_found, API_conf.errstr_exist str))
    in
    let bson_of_tag tag =
      let bson = API_tools.objectid_of_string tag in
      API_tools.check_not_exist API_tools.tags_coll
        API_tools.tagsid_field bson tag;
      bson
    in
    let rec build_docs docs = function
      | [], []            -> docs
      |  _, []            ->
        raise API_conf.(Pum_exc (return_not_found, "Not enought tags_id"));
      | [], _             ->
        raise API_conf.(Pum_exc (return_not_found, "Too many tags_id"));
      | fth::ftt, th::tt  ->
        if th = []
        then raise API_conf.(Pum_exc (return_not_found,
                                      "All link need to have at least one tag"));
        let bson_tags = List.map bson_of_tag th in
        let bson_ltags = Bson.create_list bson_tags in
        let bson_doc = Bson.add_element API_tools.tagsid_field bson_ltags fth in
        build_docs (bson_doc::docs) (ftt,tt)
    in
    let bson_from = objectid_of_strid API_tools.contents_coll
      API_tools.originid_field Bson.empty id_from
    in
    let bson_fromtos = List.map (objectid_of_strid API_tools.contents_coll
                                   API_tools.targetid_field bson_from) ids_to
    in
    List.iter check_not_exist_link bson_fromtos;
    let docs = build_docs [] (bson_fromtos,tags_id) in
    Mongo.insert API_tools.links_coll docs;
    `Null
  in
  API_tools.check_return ~default_return:API_conf.return_created aux

let update_link link_id tags_id =
  let aux () =
    API_tools.check_exist API_tools.links_coll link_id;
    let object_id = API_tools.objectid_of_string link_id in
    let bson_query = Bson.add_element API_tools.id_field object_id Bson.empty in
    let content =
      if tags_id = []
      then raise API_conf.(Pum_exc (return_not_found,
                                    "All link need to have at least one tag"));
      let bson_objid = List.map API_tools.objectid_of_tagstr tags_id in
      let bson_list = Bson.create_list bson_objid in
      Bson.add_element API_tools.tagsid_field bson_list Bson.empty
    in
    Mongo.update_one API_tools.links_coll (bson_query, content);
    `Null
  in
  API_tools.check_return aux

let delete_links = delete_from API_tools.links_coll

let delete_links_from_to origin_id targets_id =
  let object_id = API_tools.objectid_of_string origin_id in
  API_tools.check_exist API_tools.contents_coll origin_id;
  let query = Bson.add_element API_tools.originid_field object_id Bson.empty in
  let aux () =
    let objectid_of_idstr str =
      let object_id = API_tools.objectid_of_string str in
      API_tools.check_exist API_tools.contents_coll str;
      Bson.add_element API_tools.targetid_field object_id query
    in
    let bson_query = MongoQueryOp.or_op (List.map objectid_of_idstr targets_id)
    in
    Mongo.delete_all API_tools.links_coll bson_query;
    `Null
  in
  API_tools.check_return aux
