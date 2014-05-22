open Lwt
open Rdf_sparql_protocol

module SMap = Map.Make(String)

type link_id = string * string
type link = link_id * string * string list

let base_content_url = "http://pumgrana.com/content/detail/"
let base_url = Rdf_uri.uri "http://127.0.0.1:8000"
let get_url = Rdf_uri.append base_url "/sparql/"
let update_url = Rdf_uri.append base_url "/update/"

let string_of_link_id (origin_uri, target_uri) =
  origin_uri ^ "@" ^ target_uri

let uri_of_content_id str = base_content_url ^ str

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

let string_of_term = function
  | Rdf_term.Iri iri     -> Rdf_iri.string iri
  | Rdf_term.Literal lit -> Rdf_utf8.utf8_escape lit.Rdf_term.lit_value
  | Rdf_term.Blank       -> "_"
  | Rdf_term.Blank_ id   ->  "_:" ^ (Rdf_term.string_of_blank_id id)

let target_from_solution solution =
  try string_of_term (Rdf_sparql.get_term solution "p")
  with Not_found -> failwith "Target not found"

let tag_from_solution solution =
  try string_of_term (Rdf_sparql.get_term solution "o")
  with Not_found -> failwith "Tag not found"

let tuple_from_solution solution =
  target_from_solution solution, tag_from_solution solution

let links_of_solutions origin_uri solutions =
  let build_tag_list map solution =
    let target_uri, link_tag = tuple_from_solution solution in
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

let build_tags_query content_uri tags =
  let filter_query = if List.length tags == 0 then "" else
      let build_rgx rgx tag =
        let rgx' = if String.length rgx == 0 then rgx else rgx ^ "|" in
        rgx' ^ "(" ^ tag ^ ")"
      in
      let regex = List.fold_left build_rgx "" tags in
      " . FILTER regex(?o, \"" ^ regex ^ "\")"
  in
  "SELECT ?p ?o WHERE { <"^content_uri^"> ?p ?o" ^ filter_query ^ " }"

let links_from_content_tags content_id tags =
  let content_uri = uri_of_content_id content_id in
  let query = build_tags_query content_uri tags in
  let base = Rdf_iri.iri "http://pumgrana.com" in
  let msg = {in_query = query; in_dataset = empty_dataset} in
  lwt results = Rdf_4s_lwt.get ~base get_url msg in
  let solutions = get_solutions (get_result results) in
  Lwt.return (links_of_solutions content_uri solutions)

let links_from_content content_id =
  links_from_content_tags content_id []

(*** Not protected if link already exist ! *)
let insert_links origin_id targets_id tags =
  let origin_uri = uri_of_content_id origin_id in
  let targets_uri = List.map uri_of_content_id targets_id in
  let build_query origin_uri target_uri query tag =
    let query' = if String.length query == 0 then query else query ^ " . " in
    query' ^ "<" ^ origin_uri ^ "> <" ^ target_uri ^ "> \"" ^ tag ^ "\""
  in
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
  let fake_base = Rdf_iri.iri ~check:false "" in
  let msg = {in_query = query; in_dataset = empty_dataset} in
  lwt res = Rdf_4s_lwt.post_update ~base:fake_base update_url msg in
  check_ok res;
  Lwt.return (List.map (link_id origin_uri) targets_uri)

let build_delete_query_tag links_id tags =
  let build_query o_uri t_uri query tag =
    let q = if String.length query == 0 then query else query ^ " . " in
    q ^ "<" ^ o_uri ^ "> <" ^ t_uri ^ "> \"" ^ tag ^ "\""
  in
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
    q ^ "{ <" ^ o_uri ^ "> <" ^ t_uri ^ "> ?o . {?s ?p ?o.} UNION {?x ?y ?z} }"
  in
  let half_query = List.fold_left build_query "" links_id in
  "DELETE {?s ?p ?o.} WHERE { " ^ half_query ^ " }"

(*** Not protected if link does not exist ! *)
let delete_links links_id tags =
  let query = if List.length tags == 0
    then build_delete_query links_id
    else build_delete_query_tag links_id tags
  in
  let fake_base = Rdf_iri.iri ~check:false "" in
  let msg = {in_query = query; in_dataset = empty_dataset} in
  lwt res = Rdf_4s_lwt.post_update ~base:fake_base update_url msg in
  Lwt.return (check_ok res)

(*** Not protected if link_id does not exist ! *)
let update_link link_id new_tags =
  let o_uri, t_uri = link_id in
  let query = "SELECT ?o WHERE { <" ^ o_uri ^ "> <" ^ t_uri ^ "> ?o }" in
  let base = Rdf_iri.iri "http://pumgrana.com" in
  let msg = {in_query = query; in_dataset = empty_dataset} in
  lwt results = Rdf_4s_lwt.get ~base get_url msg in
  let solutions = get_solutions (get_result results) in
  let old_tags = List.map tag_from_solution solutions in
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
