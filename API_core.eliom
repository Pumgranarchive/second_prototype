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

(*** Getters *)
let get_detail content_id =
  try
    let objectId = Bson.create_objectId content_id in
    let bson_condition = Bson.add_element API_tools.id_field objectId Bson.empty
    in
    let result = Mongo.find_q_one API_tools.contents_coll bson_condition in
    let result_bson = List.hd (MongoReply.get_document_list result) in
    let lt_rbson = Bson.remove_element API_tools.tagsid_field result_bson in
    let jcontent =  yojson_of_bson lt_rbson in
    API_tools.detail_f API_conf.return_ok jcontent
  with
  | e -> print_endline (Printexc.to_string e);
    API_tools.detail_f API_conf.return_fail `Null

let get_detail_by_link link_id =
  try
    let l_objectId = Bson.create_objectId link_id in
    let l_bson_condition =
      Bson.add_element API_tools.id_field l_objectId Bson.empty
    in
    let l_result = Mongo.find_q_one API_tools.links_coll l_bson_condition in
    let l_result_bson = List.hd (MongoReply.get_document_list l_result) in
    let target_id = Bson.get_element API_tools.targetid_field l_result_bson in
    let ct_bson_condition =
      Bson.add_element API_tools.id_field target_id Bson.empty
    in
    let ct_result = Mongo.find_q_one API_tools.contents_coll ct_bson_condition
    in
    let ct_result_bson = List.hd (MongoReply.get_document_list ct_result) in
    let lt_ct_rbson = Bson.remove_element API_tools.tagsid_field ct_result_bson
    in
    let jcontent = yojson_of_bson lt_ct_rbson in
    API_tools.detail_f API_conf.return_ok jcontent
  with
  | e -> print_endline (Printexc.to_string e);
    API_tools.detail_f API_conf.return_fail `Null

(* Currently filter does not use,
   because we are not enought informations in the DB *)
let get_contents filter tags_id =
  try
    (* This condition is not good
       It is like OR gate on each tag. *)
    let rec make_bson_condition bson_condition = function
      | []      -> bson_condition
      | id::t   -> make_bson_condition
        (Bson.add_element API_tools.tagsid_field
           (Bson.create_objectId id)
           bson_condition) t
    in

    let bson_condition = match tags_id with
      | None    -> Bson.empty
      | Some x  -> make_bson_condition Bson.empty x
    in
    let results = Mongo.find_q API_tools.contents_coll bson_condition in
    let results_bson = MongoReply.get_document_list results in
    let jcontents = yojson_of_bson_document results_bson in

    (* The return contain tags_id field *)
    API_tools.contents_f API_conf.return_ok jcontents
  with
  | e -> print_endline (Printexc.to_string e);
    API_tools.contents_f API_conf.return_fail `Null
