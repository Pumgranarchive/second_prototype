(*
  API Core
  This Module do request to data base and format well to return it in service
 *)

module Yj = Yojson.Safe

(*** Tools *)
let yojson_of_bson bson =
  Yj.from_string (Bson.to_simple_json bson)

let yojson_of_bson_document bson_l =
  let rec aux yojson_l = function
    | []        -> yojson_l
    | h::t      -> aux ((yojson_of_bson h)::yojson_l) t
  in
  `List (List.rev (aux [] bson_l))

(*
** Content
*)

(*** Getters *)
let get_detail content_id =
  try
    let objectId = Bson.create_objectId content_id in
    let bson_condition = Bson.add_element API_tools.id_field objectId Bson.empty
    in
    let result = Mongo.find_q_s_one API_tools.contents_coll bson_condition
      API_tools.content_format in
    let result_bson = List.hd (MongoReply.get_document_list result) in
    let jcontent =  yojson_of_bson result_bson in
    API_tools.detail_f API_conf.return_ok jcontent
  with
  | e -> print_endline (Printexc.to_string e);
    API_tools.detail_f API_conf.return_fail `Null

let get_content_id_from_link link_id =
  let objectId = Bson.create_objectId link_id in
  let bson_condition =
    Bson.add_element API_tools.id_field objectId Bson.empty
  in
  let result = Mongo.find_q_one API_tools.links_coll bson_condition in
  let result_bson = List.hd (MongoReply.get_document_list result) in
  Bson.get_element API_tools.targetid_field result_bson

let get_detail_by_link link_id =
  try
    let target_id = get_content_id_from_link link_id in
    let bson_condition =
      Bson.add_element API_tools.id_field target_id Bson.empty
    in
    let result = Mongo.find_q_s_one API_tools.contents_coll bson_condition
      API_tools.content_format in
    let result_bson = List.hd (MongoReply.get_document_list result) in
    let jcontent = yojson_of_bson result_bson in
    API_tools.detail_f API_conf.return_ok jcontent
  with
  | e -> print_endline (Printexc.to_string e);
    API_tools.detail_f API_conf.return_fail `Null

(* Currently, filter is not used,
   because we haven't enought informations in the DB *)
let get_contents filter tags_id =
  try
    let () =
      match filter with
      | None                    -> ()
      | Some "MOST_USED"        -> ()
      | Some "MOST_VIEW"        -> ()
      | Some "MOST_RECENT"      -> ()
      | Some x                  -> failwith "get_contents has a bad value."
    in
    let bson_condition = match tags_id with
      | None    -> Bson.empty
      | Some x  -> Bson.add_element API_tools.tagsid_field
        (Bson.create_doc_element
           (MongoQueryOp.all (List.map Bson.create_objectId x)))
        Bson.empty
    in
    let results = Mongo.find_q_s API_tools.contents_coll bson_condition
      API_tools.content_format in
    let results_bson = MongoReply.get_document_list results in
    let jcontents = yojson_of_bson_document results_bson in
    API_tools.contents_f API_conf.return_ok jcontents
  with
  | e -> print_endline (Printexc.to_string e);
    API_tools.contents_f API_conf.return_fail `Null


(*
** Tags
*)

(*** Getters *)

let get_tags tags_id =
  try

    let document_of_tag tag_id = 
      (Bson.add_element API_tools.id_field (Bson.create_objectId tag_id) Bson.empty)
    in

    let bson_condition = match tags_id with
      | []      -> Bson.empty
      | id::t	-> (MongoQueryOp.or_op (List.map (document_of_tag) tags_id))

    in
    let results = Mongo.find_q_s API_tools.tags_coll bson_condition
      API_tools.tag_format in
    let results_bson = MongoReply.get_document_list results in
    let jcontents = yojson_of_bson_document results_bson in
    API_tools.tags_f API_conf.return_ok jcontents
  with
  | e -> print_endline (Printexc.to_string e);
    API_tools.tags_f API_conf.return_fail `Null


let get_tags_by_type tag_type = 
  try
    let convert_param = API_conf.(if tag_type = link_tag then link_tag_str else content_tag_str)
    in
    let objectId = Bson.create_string convert_param in
    let bson_condition = Bson.add_element API_tools.type_field objectId Bson.empty
    in
    let result = Mongo.find_q_s API_tools.tags_coll bson_condition
      API_tools.tag_format in
    let result_bson = MongoReply.get_document_list result in
    let jcontent =  yojson_of_bson_document result_bson in
    API_tools.tags_f API_conf.return_ok jcontent
  with
  | e -> print_endline (Printexc.to_string e);
    API_tools.tags_f API_conf.return_fail `Null


let get_tags_from_content content_id =
  try
    (* step 1: request the content *)
    let content_objectId = Bson.create_objectId content_id in
    let content_bson_condition = Bson.add_element API_tools.id_field content_objectId Bson.empty
    in
    let result_content = Mongo.find_q_one API_tools.contents_coll content_bson_condition in
    let content_bson = List.hd(MongoReply.get_document_list result_content) in


    (* step 2: request the associated tags *)
    let tagsid_field = Bson.get_element API_tools.tagsid_field content_bson in
    let document_of_tag tag_id = 
      (Bson.add_element API_tools.id_field (Bson.create_objectId tag_id) Bson.empty)
    in
    let tag_bson_condition =
      (MongoQueryOp.or_op
      	 (List.map document_of_tag
            (List.map Bson.get_objectId
    	       (Bson.get_list tagsid_field))))
    in
    let results_tag = Mongo.find_q_s API_tools.tags_coll tag_bson_condition
      API_tools.tag_format in
    let results_bson = MongoReply.get_document_list results_tag in
    let jcontent = yojson_of_bson_document results_bson in
    API_tools.tags_f API_conf.return_ok jcontent

  with
  | e -> print_endline (Printexc.to_string e);
    API_tools.detail_f API_conf.return_fail `Null


