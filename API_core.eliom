(*
  API Core
  This Module do request to data base and format well to return it in service
 *)

open Utils

module OrderedUri =
  struct
    type t = Rdf_store.uri
    let compare = Rdf_store.compare_uri
  end

module Map = Map.Make(OrderedUri)

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

let cut_research str =
  let regex = Str.regexp "[ \t]+" in
  Str.split regex str

let deep_cout str_list =
  List.fold_left (fun c s -> (String.length s) + c) 0 str_list

(*
** Content
*)

let is_something_else uri = true

let get_nosql_store_detail uri =
  let id = Rdf_store.content_id_of_uri uri in
  lwt (id, title, summary, body) = Nosql_store.get_detail id in
  Lwt.return (uri, title, summary, body, false)

let detail_platforms =
  [(Rdf_store.is_pumgrana_uri,  get_nosql_store_detail);
   (Pyoutube.is_youtube_uri,    Pyoutube.get_youtube_detail);
   (is_something_else,          Preadability.get_readability_detail)]

let rec get_data_from uri = function
  | (condiction, getter)::next ->
    if condiction uri
    then getter uri
    else get_data_from uri next
  | [] -> raise Not_found

let triple_platforms =
  [(Pyoutube.is_youtube_uri,    Pyoutube.get_youtube_triple);
   (is_something_else,          Preadability.get_readability_triple)]

let get_data_list_from uris platforms =
  let launch lwt_data (condiction, getter) =
    lwt uris, requests = lwt_data in
    let plt_uris = List.filter condiction uris in
    let not_know uri =
      List.for_all (fun x -> Ptype.compare_uri uri x != 0) plt_uris
    in
    let other_uris = List.filter not_know uris in
    try_lwt
      let new_requests =
        if List.length plt_uris = 0
        then Lwt.return []
        else getter plt_uris
      in
      Lwt.return (other_uris, new_requests::requests)
    with e ->
      let () = print_endline (Printexc.to_string e) in
      Lwt.return (other_uris, requests)
  in
  lwt uris, requests = List.fold_left launch (Lwt.return (uris,[])) platforms in
  if List.length uris != 0 then raise Not_found;
  lwt answers = Lwt_list.(map_s_exc wait requests) in
  lwt results = Lwt_list.(map_s_exc wait (List.concat answers)) in
  Lwt.return results

(*** Getters *)
let uri_from_platform plateform content_name =
  print_endline "from_platform";
  let aux () =
    try_lwt
      print_endline "call";
      lwt uri = Rdf_store.uri_from_platform plateform content_name in
      print_endline "ret";
      Lwt.return (`String (Rdf_store.string_of_uri uri))
    with e -> (print_endline (Printexc.to_string e); Lwt.return `Null)
  in
  API_tools.check_return ~param_name:API_tools.content_id_ret_name aux

let get_detail content_str_uri =
  let aux () =
    let uri = Rdf_store.uri_of_string content_str_uri in
    try_lwt
      lwt (uri, title, summary, body, v_external) =
        get_data_from uri detail_platforms
      in
      Lwt.return (`Assoc [(API_tools.uri_field, `String content_str_uri);
                          (API_tools.title_field, `String title);
                          (API_tools.summary_field, `String summary);
                          (API_tools.body_field, `String body);
                          (API_tools.external_field, `Bool v_external)])
    with Not_found -> Lwt.return `Null
  in
  API_tools.check_return ~param_name:API_tools.contents_ret_name aux

let content_assoc (uri, title, summary) =
  `Assoc [(API_tools.uri_field, `String (Rdf_store.string_of_uri uri));
          (API_tools.title_field, `String title);
          (API_tools.summary_field, `String summary)]

let data_of_internal res =
  let aux (id, t, s) = Rdf_store.uri_of_content_id id, t, s in
  List.map aux res

let data_of_external res =
  get_data_list_from res triple_platforms

let get_data = function
  | Rdf_store.Cc_internal r -> Lwt.return (data_of_internal r)
  | Rdf_store.Cc_external r -> data_of_external r

let on_both func p =
  let ons = Rdf_store.([Internal; External]) in
  lwt requests = Lwt_list.map_exc (fun on -> func on p) ons in
  lwt results = Lwt_list.(map_s_exc wait requests) in
  lwt data = Lwt_list.(map_s_exc get_data results) in
  Lwt.return (List.concat data)

let content_filter = function
  | None                    -> ()
  | Some "MOST_USED"        -> ()
  | Some "MOST_VIEW"        -> ()
  | Some "MOST_RECENT"      -> ()
  | Some x                  ->
    raise API_conf.(Pum_exc (return_not_found, errstr_not_expected x))

let get_contents filter tags_str_uri =
  let aux () =
    let () = content_filter filter in
    let tags_uri =
      match tags_str_uri with
      | Some t -> List.map uri_of_string t
      | None   -> []
    in
    lwt results = on_both Rdf_store.get_triple_contents tags_uri in
    let json = List.map content_assoc results in
    Lwt.return (`List json)
  in
  API_tools.check_return ~param_name:API_tools.contents_ret_name aux

let find regexps (uri, title, summary) =
  Str.exists regexps title || Str.exists regexps summary

let research_contents filter research =
  let compressed_search = Str.global_replace (Str.regexp " ") "" research in
  let aux () =
    let () = content_filter filter in
    let research = cut_research research in
    let regexps = Str.regexps research in

    (* Rdf research *)
    let length = deep_cout research in
    lwt results =
        if length > 2
        then on_both Rdf_store.research_contents research
        else Lwt.return []
    in

    (* title / summary research *)
    lwt results' = on_both Rdf_store.get_triple_contents [] in
    let results' = List.filter (find regexps) results' in

    let compare (uri1, _, _) (uri2, _, _) =
      Rdf_store.compare_uri uri1 uri2 = 0
    in
    let merged = List.merge compare results results' in
    let limited = List.limit 20 merged in
    let json = List.map content_assoc limited in
    Lwt.return (`List json)
  in
  if (String.length compressed_search == 0)
  then get_contents filter None
  else API_tools.check_return ~param_name:API_tools.contents_ret_name aux

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
    let uri = uri_of_string content_str_uri in
    let tags_uri = List.map uri_of_string tags_str_uri in
    lwt () = Rdf_store.update_content_tags uri tags_uri in
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

let get_tags_from_research research =
  let aux () =
    let research = cut_research research in
    let length = deep_cout research in
    if length <= 2 then Lwt.return `Null else
      lwt tags = Rdf_store.get_tags_from_research research in
      let json = `List (List.map tag_format tags) in
      Lwt.return json
  in
  API_tools.check_return ~param_name:API_tools.tags_ret_name aux

let get_tags_from_content content_str_uri =
  let aux () =
    let content_uri = uri_of_string content_str_uri in
    lwt tags = Rdf_store.get_tags_from_content content_uri in
    if List.length tags = 0 then PumBot.launch [content_uri];
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
    if (List.length subjects == 0)
    then raise API_conf.(Pum_exc (return_not_found, "Null subject list"));
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

let build_assoc (link_id, uri, title, summary) =
  let str_link_id = Rdf_store.string_of_link_id link_id in
  let str_uri = Rdf_store.string_of_uri uri in
  `Assoc [(API_tools.link_id_ret_name, `String str_link_id);
          (API_tools.content_id_ret_name, `String str_uri);
          (API_tools.content_title_ret_name, `String title);
          (API_tools.content_summary_ret_name, `String summary)]

let data_of_external l =
  let uris = List.map (fun (li, u) -> u) l in
  let mlink = List.fold_left (fun m (li, u) -> Map.add u li m) Map.empty l in
  let format (uri, title, summary) = Map.find uri mlink, uri, title, summary in
  lwt ret = get_data_list_from uris triple_platforms in
  Lwt.return (List.map format ret)

let get_data = function
  | Rdf_store.Cl_internal l -> Lwt.return l
  | Rdf_store.Cl_external l -> data_of_external l

let on_both func p1 p2 =
  let ons = Rdf_store.([Internal; External]) in
  lwt requests = Lwt_list.map_exc (fun on -> func on p1 p2) ons in
  lwt results = Lwt_list.(map_s_exc wait requests) in
  lwt data = Lwt_list.(map_s_exc get_data results) in
  Lwt.return (List.concat data)

let get_links_from_content_tags str_content_uri opt_tags_uri =
  let aux tags_str_uri () =
    let uri = Rdf_store.uri_of_string str_content_uri in
    let tags_uri = List.map Rdf_store.uri_of_string tags_str_uri in
    lwt results = on_both Rdf_store.links_from_content_tags uri tags_uri in
    let list = List.map build_assoc results in
    if List.length list = 0 then PumBot.launch [uri];
    Lwt.return (`List list)
  in
  let tags_str_uri = match opt_tags_uri with
    | Some x -> x
    | None   -> []
  in
  API_tools.check_return ~param_name:API_tools.links_ret_name (aux tags_str_uri)

let get_links_from_content content_uri =
  get_links_from_content_tags content_uri None

let find regexps (link_id, uri, title, summary) =
  Str.exists regexps title || Str.exists regexps summary

let get_links_from_research content_uri research =
  let compressed_search = Str.global_replace (Str.regexp " ") "" research in
  let aux () =
    let research = cut_research research in
    let regexps = Str.regexps research in
    let content_uri = Rdf_store.uri_of_string content_uri in

    (* Rdf research on tags *)
    let length = deep_cout research in
    lwt results =
        if length > 2
        then on_both Rdf_store.links_from_research content_uri research
        else Lwt.return []
    in

    (* title / summary research *)
    lwt results' = on_both Rdf_store.links_from_content_tags content_uri [] in
    let results' = List.filter (find regexps) results' in

    let compare (_, uri1, _, _) (_, uri2, _, _) =
      Rdf_store.compare_uri uri1 uri2 = 0
    in
    let merged = List.merge compare results results' in
    let limited = List.limit 20 merged in
    let json = List.map build_assoc limited in

    Lwt.return (`List json)
  in
  if (String.length compressed_search == 0)
  then get_links_from_content content_uri
  else API_tools.check_return ~param_name:API_tools.links_ret_name aux

let click_onlink link_id =
  let aux () =
    lwt () = Nosql_store.click_onlink link_id in
    Lwt.return `Null
  in
  API_tools.check_return aux

let back_button link_id =
  let aux () =
    lwt () = Nosql_store.back_button link_id in
    Lwt.return `Null
  in
  API_tools.check_return aux

let internal_insert_links data =
  let aux () =
    let link_of_uri (origin_str_uri, target_str_uri, tags_str_uri, score) =
      let data =
        Rdf_store.uri_of_string origin_str_uri,
        Rdf_store.uri_of_string target_str_uri,
        List.map Rdf_store.uri_of_string tags_str_uri,
        score
      in
      data
    in
    let link_list = List.map link_of_uri data in
    lwt links_id = Rdf_store.insert_links link_list in
    let format link_id =
      let str_link_id = Rdf_store.string_of_link_id link_id in
      `Assoc [(API_tools.uri_field, `String str_link_id)]
    in
    let json_link_id = List.map format links_id in
    Lwt.return (`List json_link_id)
  in
  API_tools.check_return
    ~default_return:API_conf.return_created
    ~param_name:API_tools.linksid_ret_name
    aux

let insert_links data =
  let link_of_tripe (ouri, turi, tags) = (ouri, turi, tags, -1) in
  let data' = List.map link_of_tripe data in
  internal_insert_links data'

let insert_scored_links = internal_insert_links

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
