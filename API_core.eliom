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
    let uri = Rdf_store.uri_of_content_id id in
    Lwt.return (`String (Rdf_store.string_of_uri uri))
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

let tag_format (uri, subject) =
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
    let result = `List (List.map tag_format tags) in
    Lwt.return result
  in
  API_tools.check_return ~param_name:API_tools.tags_ret_name aux

let get_tags_from_content content_str_uri =
  let aux () =
    let content_uri = uri_of_string content_str_uri in
    lwt tags = Rdf_store.get_tags_from_content content_uri in
    let result = `List (List.map tag_format tags) in
    Lwt.return result
  in
  API_tools.check_return ~param_name:API_tools.tags_ret_name aux

let get_tags_from_content_link content_str_uri =
  let aux () =
    let content_uri = uri_of_string content_str_uri in
    lwt tags = Rdf_store.get_tags_from_content_link content_uri in
    let result = `List (List.map tag_format tags) in
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

let get_link_detail str_link_id =
  let aux () =
    let link_id = Rdf_store.link_id_of_string str_link_id in
    lwt result = Rdf_store.get_link_detail link_id in
    let build_json (link_id, origin_uri, target_uri, tags) =
      let str_link_id = Rdf_store.string_of_link_id link_id in
      let origin_str_uri = Rdf_store.string_of_uri origin_uri in
      let target_str_uri = Rdf_store.string_of_uri target_uri in
      let tags_json = List.map tag_format tags in
      `Assoc [(API_tools.link_id_ret_name, `String str_link_id);
              (API_tools.originid_field, `String origin_str_uri);
              (API_tools.targetid_field, `String target_str_uri);
              (API_tools.tags_ret_name, `List tags_json)]
    in
    let json = build_json result in
    Lwt.return json
  in
  API_tools.check_return ~param_name:API_tools.links_ret_name aux

let get_links_from_content_tags content_uri opt_tags_uri =
  let aux tags_str_uri () =
    let content_uri = Rdf_store.uri_of_string content_uri in
    let tags_uri = List.map Rdf_store.uri_of_string tags_str_uri in
    lwt linked_cs = Rdf_store.links_from_content_tags content_uri tags_uri in
    let build_json (link_id, content_uri, title, summary) =
      let str_link_id = Rdf_store.string_of_link_id link_id in
      let content_str_uri = Rdf_store.string_of_uri content_uri in
      `Assoc [(API_tools.link_id_ret_name, `String str_link_id);
              (API_tools.content_id_ret_name, `String content_str_uri);
              (API_tools.content_title_ret_name, `String title);
              (API_tools.content_summary_ret_name, `String summary)]
    in
    let json = `List (List.map build_json linked_cs) in
    Lwt.return json
  in
  let tags_str_uri = match opt_tags_uri with
    | Some x -> x
    | None   -> []
  in
  API_tools.check_return ~param_name:API_tools.links_ret_name (aux tags_str_uri)

let get_links_from_content content_uri =
  get_links_from_content_tags content_uri None

let insert_links data =
  let aux () =
    let triple_uri (origin_str_uri, target_str_uri, tags_str_uri) =
      Rdf_store.uri_of_string origin_str_uri,
      Rdf_store.uri_of_string target_str_uri,
      List.map Rdf_store.uri_of_string tags_str_uri
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
      List.map Rdf_store.uri_of_string tags_str_uri
    in
    let tuple_list = List.map tuple_uri data in
    lwt () = Rdf_store.update_links tuple_list in
    Lwt.return `Null
  in
  API_tools.check_return aux

let delete_links str_links_id =
  let aux () =
    let links_id = List.map Rdf_store.link_id_of_string str_links_id in
    lwt () = Rdf_store.delete_links links_id in
    Lwt.return (`Null)
  in
  API_tools.check_return aux
