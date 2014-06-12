(*
  API Core
  This Module do request to data base and format well to return it in service
 *)

(* Tools *)

let id_of_string_uri uri =
  try Rdf_store.(content_id_of_uri (uri_of_string uri))
  with
  | Rdf_store.Invalid_uri str_err
  | Nosql_store.Invalid_id str_err ->
    raise API_conf.(Pum_exc (return_not_found, str_err))

let string_uri_of_id id =
  Rdf_store.(string_of_uri (uri_of_content_id id))

let uri_of_string str =
  try Rdf_store.uri_of_string str
  with Rdf_store.Invalid_uri str_err ->
    raise API_conf.(Pum_exc (return_not_found, str_err))

let link_id_of_string str =
  try Rdf_store.link_id_of_string str
  with Rdf_store.Invalid_link_id str_err ->
    raise API_conf.(Pum_exc (return_not_found, str_err))

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
let get_detail content_str_uri =
  let aux () =
    (* Lack the implementation of external content *)
    let content_id = id_of_string_uri content_str_uri in
    try_lwt
      lwt (id, title, summary, body) = Nosql_store.get_detail content_id in
      Lwt.return (`Assoc [(API_tools.uri_field, `String content_str_uri);
                          (API_tools.title_field, `String title);
                          (API_tools.summary_field, `String summary);
                          (API_tools.body_field, `String body)])
    with Not_found -> Lwt.return `Null
  in
  API_tools.check_return ~param_name:API_tools.contents_ret_name aux

let get_contents filter tags_str_uri =
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
    let tags_uri =
      match tags_str_uri with
      | Some t -> List.map uri_of_string t
      | None   -> []
    in
    lwt res = Rdf_store.get_triple_contents tags_uri in
    let aux_format (id, title, summary) =
      `Assoc [(API_tools.uri_field, `String (string_uri_of_id id));
              (API_tools.title_field, `String title);
              (API_tools.summary_field, `String summary)]
    in
    let json = `List (List.map aux_format res) in
    Lwt.return json
  in
  API_tools.check_return ~param_name:API_tools.contents_ret_name aux

(*** Setters  *)

(* Insert on both (nosql and rdf) stores *)
let insert_content title summary body tags_str_uri =
  let aux () =
    let tags_uri =
      match tags_str_uri with
      | Some t -> List.map uri_of_string t
      | None   -> []
    in
    lwt id = Nosql_store.insert_content title summary body in
    lwt () =
      try_lwt
        Rdf_store.insert_content id title summary tags_uri
      with Invalid_argument str_err ->
        raise API_conf.(Pum_exc (return_not_found, str_err))
    in
    Lwt.return (`String (Nosql_store.string_of_id id))
  in
  API_tools.check_return
    ~param_name:API_tools.content_id_ret_name
    ~default_return:API_conf.return_created aux

(* Insert on both (nosql and rdf) stores *)
let update_content content_str_uri title summary body tags_str_uri =
  let aux () =
    let id = id_of_string_uri content_str_uri in
    let tags_uri =
      match tags_str_uri with
      | Some t -> Some (List.map uri_of_string t)
      | None   -> None
    in
    lwt () =
      try_lwt
        lwt () = Nosql_store.update_content id ?title ?summary ?body () in
        Rdf_store.update_content id ?title ?summary ?tags_uri ()
      with
      | Not_found ->
        raise API_conf.(Pum_exc (return_not_found, errstr_not_found content_str_uri))
      | Invalid_argument str_err ->
        raise API_conf.(Pum_exc (return_not_found, str_err))
    in
    Lwt.return `Null
  in
  API_tools.check_return aux

(* Insert on rdf store only *)
let update_content_tags content_str_uri tags_str_uri =
  let aux () =
    let content_uri = uri_of_string content_str_uri in
    let tags_uri = List.map uri_of_string tags_str_uri in
    lwt () = Rdf_store.update_content_tags content_uri tags_uri in
    Lwt.return `Null
  in
  API_tools.check_return aux

(* Delete on both (nosql and rdf) stores *)
let delete_contents content_uris =
  let aux content_id =
    let ids = List.map id_of_string_uri content_uris in
    lwt () = Nosql_store.delete_contents ids in
    lwt () = Rdf_store.delete_contents ids in
    Lwt.return `Null
  in
  API_tools.check_return aux

(*
** Tags
*)

(*** Getters *)

let link_format (uri, subject) =
  `Assoc [(API_tools.uri_field, `String (Rdf_store.string_of_uri uri));
          (API_tools.subject_field, `String subject)]

let to_tag_type = function
  | "LINK"     -> Rdf_store.TagLink
  | "CONTENT"  -> Rdf_store.TagContent
  | x          ->
    raise API_conf.(Pum_exc (return_not_found, errstr_not_expected x))


let get_tags_by_type type_name =
  let aux () =
    let tag_type = to_tag_type type_name in
    lwt tags = Rdf_store.get_tags tag_type [] in
    let result = `List (List.map link_format tags) in
    Lwt.return result
  in
  API_tools.check_return ~param_name:API_tools.tags_ret_name aux

let get_tags_from_content content_str_uri =
  let aux () =
    let content_uri = uri_of_string content_str_uri in
    lwt tags = Rdf_store.get_tags_from_content content_uri in
    let result = `List (List.map link_format tags) in
    Lwt.return result
  in
  API_tools.check_return ~param_name:API_tools.tags_ret_name aux

let get_tags_from_content_link content_str_uri =
  let aux () =
    let content_uri = uri_of_string content_str_uri in
    lwt tags = Rdf_store.get_tags_from_content_link content_uri in
    let result = `List (List.map link_format tags) in
    Lwt.return result
  in
  API_tools.check_return ~param_name:API_tools.tags_ret_name aux

let insert_tags type_name uri_opt subjects =
  let aux () =
    let tag_type = to_tag_type type_name in
    let link_id, content_uri = match tag_type, uri_opt with
      | Rdf_store.TagLink, Some id      ->
        Some (link_id_of_string id), None
      | Rdf_store.TagContent, Some uri  ->
        None, Some (uri_of_string uri)
      | _, None -> None, None
    in
    lwt uris = Rdf_store.insert_tags tag_type ?link_id ?content_uri subjects in
    let format uri =
      `Assoc [(API_tools.uri_field, `String (Rdf_store.string_of_uri uri))]
    in
    let result = `List (List.map format uris) in
    Lwt.return result
  in
  API_tools.check_return
    ~param_name:API_tools.tagsid_ret_name
    ~default_return:API_conf.return_created
    aux

let delete_tags tags_str_uri =
  let aux () =
    let tags_uri = List.map uri_of_string tags_str_uri in
    lwt () = Rdf_store.delete_tags tags_uri in
    Lwt.return `Null
  in
  API_tools.check_return aux

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

let update_links data =
  let aux () =
    let tuple_uri (str_link_id, tags_str_uri) =
      Rdf_store.link_id_of_string str_link_id,
      List.map Rdf_store.uri_of_tag_id_link tags_str_uri
    in
    let tuple_list = List.map tuple_uri data in
    lwt () = Rdf_store.update_links tuple_list in
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
