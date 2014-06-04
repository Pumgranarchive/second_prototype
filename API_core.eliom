(*
  API Core
  This Module do request to data base and format well to return it in service
 *)

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
    Lwt.return (`Null)
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
    Lwt.return (API_tools.check_empty_yojson ret content_id)
  in
  API_tools.check_return ~param_name:API_tools.contents_ret_name aux

let get_detail_by_link str_link_id =
  let aux () =
    let link_id = Rdf_store.link_id_of_string str_link_id in
    let target_id = Nosql_store.string_of_id
      Rdf_store.(content_id_of_uri (target_uri_from_link_id link_id))
    in
    let btarget_id = Bson.create_objectId target_id in
    let bson_query = Bson.add_element API_tools.id_field btarget_id Bson.empty in
    let result = Mongo.find_q_s_one API_tools.contents_coll bson_query
      API_tools.content_format in
    let ret = API_tools.yojson_of_mongoreply result in
    Lwt.return (API_tools.check_empty_yojson ret str_link_id)
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
    Lwt.return (API_tools.yojson_of_mongoreply results)
  in
  API_tools.check_return ~param_name:API_tools.contents_ret_name aux

(*** Setters  *)

let insert_content title summary text tags_id =
  let aux () =
    let bson_title = Bson.create_string title in
    let bson_summary = Bson.create_string summary in
    let bson_text = Bson.create_string text in
    let bson_tags_list = match tags_id with
      | None    -> []
      | Some x  -> List.map API_tools.objectid_of_tagstr x
    in
    let bson_tagsid = Bson.create_list bson_tags_list in
    let content = Bson.add_element API_tools.title_field bson_title
      (Bson.add_element API_tools.summary_field bson_summary
         (Bson.add_element API_tools.text_field bson_text
            (Bson.add_element API_tools.tagsid_field bson_tagsid Bson.empty)))
    in
    let saved_state = API_tools.get_id_state API_tools.contents_coll in
    Mongo.insert API_tools.contents_coll [content];
    Lwt.return (`String (API_tools.string_of_id
                           (List.hd (API_tools.get_last_created_id
                                       API_tools.contents_coll saved_state))))
  in
  API_tools.check_return
    ~param_name:API_tools.content_id_ret_name
    ~default_return:API_conf.return_created aux

let update_content content_id title summary text tags_id =
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
    let content_2 = match summary with
      | None      -> content_1
      | Some x    -> Bson.add_element
        API_tools.summary_field (Bson.create_string x) content_1
    in
    let content_3 = match text with
      | None      -> content_2
      | Some x    -> Bson.add_element
        API_tools.text_field (Bson.create_string x) content_2
    in
    let content_4 = match tags_id with
      | None      -> content_3
      | Some x    ->
        let bson_objid = List.map API_tools.objectid_of_tagstr x in
        let bson_list = Bson.create_list bson_objid in
        Bson.add_element API_tools.tagsid_field bson_list content_3
    in
    if content_4 = Bson.empty
    then raise API_conf.(Pum_exc (return_not_found, "title, summary, text and tags_id can not be all null"));
    Mongo.update_one API_tools.contents_coll (bson_query, content_4);
    Lwt.return (`Null)
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
    Lwt.return (API_tools.yojson_of_mongoreply results)
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
    Lwt.return (API_tools.yojson_of_mongoreply result)
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
    Lwt.return (API_tools.yojson_of_mongoreply results_tag)
  in
  API_tools.check_return ~param_name:API_tools.tags_ret_name aux

(* Warning: if a tag_id does not exist no error will be fire. *)
let get_tags_from_content_link content_id =
  let aux () =

    (* step 1: get links related to the content*)
    let content_uri =
      Rdf_store.uri_of_content_id (Nosql_store.id_of_string content_id)
    in
    lwt links = Rdf_store.links_from_content content_uri in
    if List.length links == 0 then
      raise API_conf.(Pum_exc (return_no_content, "This content has no link."));

    (* step 2: request the related tags *)
    let objects_id_of_tags_id tags_id =
      let cleaned_tags_id =
        List.fold_left (fun new_list e ->
          if (List.exists (fun c -> (String.compare c e) == 0) new_list)
          then new_list else e::new_list)
          [] tags_id
      in
      List.map API_tools.objectid_of_string cleaned_tags_id
    in
    let get_tags new_list (link_id, target_id, tags_uri) =
      let tags_id = List.map Rdf_store.tag_id_link_of_uri tags_uri in
      tags_id@new_list
    in
    let tags_id = objects_id_of_tags_id (List.fold_left get_tags [] links) in
    if List.length tags_id == 0 then
      raise API_conf.(Pum_exc (return_no_content, "These links has no tag."));

    let document_of_tag tag_id =
      Bson.add_element API_tools.id_field tag_id Bson.empty
    in
    let bson_tags_id_list = List.map document_of_tag tags_id in
    let bson_query = MongoQueryOp.or_op bson_tags_id_list in
    let results = Mongo.find_q_s API_tools.tags_coll bson_query
      API_tools.tag_format in
    Lwt.return (API_tools.yojson_of_mongoreply results)
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
    Lwt.return (`List (List.map (fun e ->
      `Assoc [(API_tools.id_field, `String e)]) new_ids))

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
let get_links_from_content_tags content_id opt_tags_id =
  let aux tags_id () =
    (* getting every link with 'content_id' as origin *)
    let content_uri =
      Rdf_store.uri_of_content_id (Nosql_store.id_of_string content_id)
    in
    let tags_uri = List.map Rdf_store.uri_of_tag_id_content tags_id in
    lwt link_list = Rdf_store.links_from_content_tags content_uri tags_uri in
    if List.length link_list == 0 then
      raise API_conf.(Pum_exc (return_no_content,
                               "This content has no link on given tags"));

    (* getting content to return *)
    let make_target_id_bson (link_id, target_uri, tags_uri) =
      let target_id =
        Nosql_store.string_of_id (Rdf_store.content_id_of_uri target_uri)
      in
      Bson.add_element API_tools.id_field
        (Bson.create_objectId target_id) Bson.empty
    in
    let make_link_id (link_id, target_uri, tags_uri) =
      Rdf_store.string_of_link_id link_id
    in
    let target_id_bson_list = List.map make_target_id_bson link_list in
    let link_id_list = List.map make_link_id link_list in

    let target_query = (MongoQueryOp.or_op target_id_bson_list) in
    let result_query = Mongo.find_q_s API_tools.contents_coll
      target_query API_tools.link_format
    in
    Lwt.return (API_tools.link_format_ret link_id_list
                  (API_tools.yojson_of_mongoreply result_query))
  in
  let tags_id = match opt_tags_id with
    | Some x -> x
    | None   -> []
  in
  API_tools.check_return ~param_name:API_tools.links_ret_name (aux tags_id)

let get_links_from_content content_id =
  get_links_from_content_tags content_id None

let insert_links data =
  let aux () =
    let triple_uri (origin_str_uri, target_str_uri, tags_str_uri) =
      Rdf_store.uri_of_content_id (Nosql_store.id_of_string origin_str_uri),
      Rdf_store.uri_of_content_id (Nosql_store.id_of_string target_str_uri),
      List.map Rdf_store.uri_of_tag_id_link tags_str_uri
    in
    let triple_list = List.map triple_uri data in
    lwt links_id = Rdf_store.insert_links triple_list in
    let json_of_link_id link_id =
      `String (Rdf_store.string_of_link_id link_id)
    in
    let json_link_id = List.map json_of_link_id links_id in
    Lwt.return (`List json_link_id)
  in
  API_tools.check_return
    ~default_return:API_conf.return_created
    ~param_name:API_tools.linksid_ret_name
    aux

let update_link data =
  let aux () =
    let tuple_uri (str_link_id, tags_str_uri) =
      Rdf_store.link_id_of_string str_link_id,
      List.map Rdf_store.uri_of_tag_id_link tags_str_uri
    in
    let tuple_list = List.map tuple_uri data in
    lwt () = Rdf_store.update_link tuple_list in
    Lwt.return (`Null)
  in
  API_tools.check_return aux

let delete_links str_links_id =
  let aux () =
    let links_id = List.map Rdf_store.link_id_of_string str_links_id in
    lwt () = Rdf_store.delete_links links_id in
    Lwt.return (`Null)
  in
  API_tools.check_return aux

(*** Warning: badly transform to link_id  *)
let delete_links_from_to origin_id targets_id =
  let uri_of_id id = "http://pumgrana.com/content/detail/" ^  id in
  let origin_uri = uri_of_id origin_id in
  let link_id_of_string target_id = origin_uri ^ "@" ^ (uri_of_id target_id) in
  let links_id = List.map link_id_of_string targets_id in
  delete_links links_id
