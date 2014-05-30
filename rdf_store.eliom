open Lwt
open Rdf_sparql_protocol

module SMap = Map.Make(String)

type link_id = string * string
type link = link_id * string * string list

type tag_type = TagLink | TagContent

(******************************************************************************
****************************** Configuration **********************************
*******************************************************************************)

let domain = "http://pumgrana.com/"
let base_ressouce_url = domain ^ "ressource/"
let base_content_url = domain ^ "content/detail/"
let base_tag_url = domain ^ "tag/"
let base_tag_link_url = base_tag_url ^ "link/"
let base_tag_content_url = base_tag_url ^ "content/"
let tagged_content = base_ressouce_url ^ "tagged_content"
let tag_link_ressouce = base_ressouce_url ^ "tag_link"
let tag_content_ressouce = base_ressouce_url ^ "tag_content"
let base_url = Rdf_uri.uri "http://127.0.0.1:8000"
let get_url = Rdf_uri.append base_url "/sparql/"
let update_url = Rdf_uri.append base_url "/update/"

(******************************************************************************
********************************** Tools **************************************
*******************************************************************************)

let string_of_link_id (origin_uri, target_uri) =
  origin_uri ^ "@" ^ target_uri

let uri_of_content_id str = base_content_url ^ str

let uri_of_tag_id_link str = base_tag_link_url ^ str

let link_id origin_uri target_uri = origin_uri, target_uri

let link_id_of_string link_id =
  let regexp = Str.regexp "@" in
  try
    let strings = Str.split regexp link_id in
    List.hd strings, List.hd (List.tl strings)
  with e ->
    raise API_conf.(Pum_exc(return_not_found, "Invalid Link ID"))

let content_id_of_uri str =
  let regexp = Str.regexp base_content_url in
  Str.replace_first regexp "" str

let tag_id_link_of_uri str =
  let regexp = Str.regexp base_tag_link_url in
  Str.replace_first regexp "" str

let string_of_term = function
  | Rdf_term.Iri iri     -> Rdf_iri.string iri
  | Rdf_term.Literal lit -> Rdf_utf8.utf8_escape lit.Rdf_term.lit_value
  | Rdf_term.Blank       -> "_"
  | Rdf_term.Blank_ id   ->  "_:" ^ (Rdf_term.string_of_blank_id id)

let from_solution name solution =
  try string_of_term (Rdf_sparql.get_term solution name)
  with Not_found -> failwith (name ^ ": Not found into the solution")

let tuple_link_from solution =
  from_solution "target" solution, from_solution "tag" solution

let tuple_tag_from solution =
  from_solution "tag" solution, from_solution "subject" solution

let links_of_solutions origin_uri solutions =
  let build_tag_list map solution =
    let target_uri, tag_uri = tuple_link_from solution in
    let link_tag = tag_id_link_of_uri tag_uri in
    let tags =
      try SMap.find target_uri map
      with Not_found -> []
    in
    SMap.add target_uri (link_tag::tags) map
  in
  let tags = List.fold_left build_tag_list SMap.empty solutions in
  let build_links target_uri tags links =
    let link_id = link_id origin_uri target_uri in
    (link_id, content_id_of_uri target_uri, tags)::links
  in
  SMap.fold build_links tags []

let get_solutions = function
  | Rdf_sparql.Solutions s -> s
  | _                      -> failwith "None a solutions format result"

let get_result = function
  | Ok          -> failwith "No solutions returned"
  | Result r    -> r
  | Error e     -> failwith (string_of_error e)

let check_ok = function
  | Ok          -> ()
  | Error e     -> failwith (string_of_error e)
  | Result _    -> failwith "Unexpected result return"

let target_id_from_link_id (origin_uri, target_uri) =
  Lwt.return (content_id_of_uri target_uri)

let origin_id_from_link_id (origin_uri, target_uri) =
  Lwt.return (content_id_of_uri origin_uri)

let next_query query sep =
  if String.length query == 0 then query else query ^ sep

(*** Shortcut ***)

let get_from_4store query =
  let base = Rdf_iri.iri domain in
  let msg = {in_query = query; in_dataset = empty_dataset} in
  lwt results = Rdf_4s_lwt.get ~base get_url msg in
  let solutions = get_solutions (get_result results) in
  Lwt.return (solutions)

let post_on_4store query =
  let fake_base = Rdf_iri.iri ~check:false domain in
  let msg = {in_query = query; in_dataset = empty_dataset} in
  lwt res = Rdf_4s_lwt.post_update ~base:fake_base update_url msg in
  Lwt.return (check_ok res)

(******************************************************************************
******************************** Tags *****************************************
*******************************************************************************)

let get_tags tag_type tags_uri =
  let ressource_url = match tag_type with
    | TagLink    -> tag_link_ressouce
    | TagContent -> tag_content_ressouce
  in
  let build_query query tag_uri =
    let q = if String.length query == 0 then query else query ^ " . " in
    q ^"{?tag  <"^ ressource_url ^"> ?subject . FILTER regex(str(?tag), \""^ tag_uri ^"\")}"
  in
  let half_query =
    if List.length tags_uri != 0
    then List.fold_left build_query "" tags_uri
    else "?tag <"^ ressource_url ^"> ?subject"
  in
  let query = "SELECT ?tag ?subject WHERE { " ^ half_query ^ " }" in
  lwt solutions = get_from_4store query in
  let tuple_tags = List.map tuple_tag_from solutions in
  Lwt.return (tuple_tags)

let get_tags_from_link link_id =
  let o_uri, t_uri = link_id in
  let query = "SELECT ?tag ?subject WHERE
{ <"^ o_uri ^"> ?tag <"^ t_uri ^"> .
?tag <" ^ tag_link_ressouce ^ "> ?subject }"
  in
  lwt solutions = get_from_4store query in
  let tuple_tags = List.map tuple_tag_from solutions in
  Lwt.return (tuple_tags)

let get_tags_from_content content_id =
  let c_uri = uri_of_content_id content_id in
  let query = "SELECT ?tag ?subject WHERE
{ <"^ c_uri ^"> <"^ tagged_content ^"> ?tag .
  ?tag <" ^ tag_content_ressouce ^ "> ?subject }"
  in
  lwt solutions = get_from_4store query in
  let tuple_tags = List.map tuple_tag_from solutions in
  Lwt.return (tuple_tags)

let get_tags_from_content_link content_id =
  let o_uri = uri_of_content_id content_id in
  let query = "SELECT ?tag ?subject WHERE
{ <" ^ o_uri ^ "> ?tag ?target.
  ?tag <" ^ tag_link_ressouce ^ "> ?subject }"
  in
  lwt solutions = get_from_4store query in
  let tags_tuple = List.map tuple_tag_from solutions in
  Lwt.return (tags_tuple)

let insert_tags tag_type ?link_id ?content_id subjects =
  let ressource_url, base_tag_url =
    match tag_type, link_id, content_id with
    | TagContent, None, _ -> tag_content_ressouce, base_tag_content_url
    | TagLink, _, None    -> tag_link_ressouce, base_tag_link_url
    | _, _, _             -> failwith "Bad association"
  in
  let content_uri = match content_id with
    | Some id   -> Some (uri_of_content_id id)
    | None      -> None
  in
  let tag_uri_of_subject subject =
    let rdf_uri = Rdf_uri.(neturl (uri (base_tag_url ^ subject))) in
    let neturl = Neturl.modify_url ~encoded:true rdf_uri in
    Rdf_uri.string (Rdf_uri.of_neturl neturl)
  in
  let insert_tag_on query tag_uri =
    let q = next_query query " . " in
    match link_id, content_uri with
    | None, Some content_uri    ->
        q ^ "<" ^ content_uri ^ ">  <" ^ tagged_content ^ "> <" ^ tag_uri ^ ">"
    | Some (o_uri, t_uri), None ->
        q ^ "<" ^ o_uri ^ ">  <" ^ tag_uri ^ "> <" ^ t_uri ^ ">"
    | _, _ -> query
  in
  let insert_tag query uri subject =
    let q = next_query query " . " in
    let q' = q ^ "<" ^ uri ^ ">  <" ^ ressource_url ^ "> \"" ^ subject ^ "\"" in
    insert_tag_on q' uri
  in
  let tags_uri = List.map tag_uri_of_subject subjects in
  let half_query = List.fold_left2 insert_tag "" tags_uri subjects in
  let query = "INSERT DATA { " ^ half_query ^ " }" in
  lwt () = post_on_4store query in
  Lwt.return (tags_uri)

let delete_tags tags_uri =
  let build_query query tag_uri =
    let q = next_query query " . " in
    q ^ "{ <" ^ tag_uri ^ "> ?res1 ?sub. {?tag ?res1 ?sub.} UNION {?x ?y ?z} }.
{ ?origin <" ^ tag_uri ^ "> ?target. {?origin ?tag ?target.} UNION {?x ?y ?z} }.
{ ?content ?res2 <" ^ tag_uri ^ ">. {?content ?res2 ?tag.} UNION {?x ?y ?z} }"
  in
  let half_query = List.fold_left build_query "" tags_uri in
  let query = "DELETE {?x ?y ?z.} WHERE { " ^ half_query ^ " }" in
  post_on_4store query

(******************************************************************************
******************************** Links ****************************************
*******************************************************************************)

let build_tags_query content_uri tags =
  let filter_query = if List.length tags == 0 then "" else
      let build_rgx rgx tag_id =
        let rgx' = if String.length rgx == 0 then rgx else rgx ^ "|" in
        let tag_uri = uri_of_tag_id_link tag_id in
        rgx' ^ "(" ^ tag_uri ^ ")"
      in
      let regex = List.fold_left build_rgx "" tags in
      " . FILTER regex(str(?tag), \"" ^ regex ^ "\")"
  in
  "SELECT ?tag ?target WHERE { <"^content_uri^"> ?tag ?target" ^ filter_query ^ " }"

let links_from_content_tags content_id tags =
  let content_uri = uri_of_content_id content_id in
  let query = build_tags_query content_uri tags in
  lwt solutions = get_from_4store query in
  let links = links_of_solutions content_uri solutions in
  Lwt.return (links)

let links_from_content content_id =
  links_from_content_tags content_id []

let build_query origin_uri target_uri query tag_id =
  let q = if String.length query == 0 then query else query ^ " . " in
  let tag_uri = uri_of_tag_id_link tag_id in
  q ^ "<" ^ origin_uri ^ ">  <" ^ tag_uri ^ "> <" ^ target_uri ^ ">"

(*** Not protected if link already exist ! *)
let insert_links origin_id targets_id tags =
  let origin_uri = uri_of_content_id origin_id in
  let targets_uri = List.map uri_of_content_id targets_id in
  let query_of_target query target_uri tags =
    if List.length tags == 0 then
      raise API_conf.(Pum_exc(return_not_found,
                              "Empty tag list are not allowed"));
    List.fold_left (build_query origin_uri target_uri) query tags
  in
  if List.length targets_uri == 0 then
      raise API_conf.(Pum_exc(return_not_found,
                              "Empty target list are not allowed"));
  let half_query = List.fold_left2 query_of_target "" targets_uri tags in
  let query = "INSERT DATA { " ^ half_query ^ " }" in
  lwt () = post_on_4store query in
  Lwt.return (List.map (link_id origin_uri) targets_uri)

let build_delete_query_tag links_id tags =
  let manager query link_id tags =
    let o_uri, t_uri = link_id in
    if List.length tags == 0 then
      raise API_conf.(Pum_exc(return_not_found,
                              "Empty tag list are not allowed"));
    List.fold_left (build_query o_uri t_uri) query tags
  in
  let half_query = List.fold_left2 manager "" links_id tags in
  "DELETE DATA { " ^ half_query ^ " }"

let build_delete_query links_id =
  let build_query query link_id =
    let o_uri, t_uri = link_id in
    let q = if String.length query == 0 then query else query ^ " UNION " in
    q ^ "{ <" ^ o_uri ^ "> ?tag <" ^ t_uri ^ "> . {?origin ?tag ?target.} UNION {?x ?y ?z} }"
  in
  let half_query = List.fold_left build_query "" links_id in
  "DELETE {?origin ?tag ?target.} WHERE { " ^ half_query ^ " }"

(*** Not protected if link does not exist ! *)
let delete_links links_id tags =
  let query = if List.length tags == 0
    then build_delete_query links_id
    else build_delete_query_tag links_id tags
  in
  lwt () = post_on_4store query in
  Lwt.return ()

(*** Not protected if link_id does not exist ! *)
let update_link link_id new_tags =
  let o_uri, t_uri = link_id in
  lwt old_tags_uri = get_tags_from_link link_id in
  let old_tags = List.map (fun (x, _) -> tag_id_link_of_uri x) old_tags_uri in
  let are_equal tag e = String.compare tag e == 0 in
  let build_list ref_list build_list tag =
    if List.exists (are_equal tag) ref_list then build_list else tag::build_list
  in
  let deleting_list = List.fold_left (build_list new_tags) [] old_tags in
  let adding_list = List.fold_left (build_list old_tags) [] new_tags in
  let origin_id = content_id_of_uri o_uri in
  let target_id = content_id_of_uri t_uri in
  let update list update_func =
    if List.length list != 0 then update_func list else Lwt.return ()
  in
  lwt () = update deleting_list (fun l -> delete_links [link_id] [l]) in
  lwt () = update adding_list (fun l ->
    lwt res = (insert_links origin_id [target_id] [l]) in
    if List.length l != 0 && List.length res == 0
    then raise API_conf.(Pum_exc(return_internal_error, "Updating failed"));
    Lwt.return ())
  in
  Lwt.return ()
