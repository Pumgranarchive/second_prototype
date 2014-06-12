open Lwt
open Rdf_sparql_protocol

module SMap = Map.Make(String)

exception Invalid_uri = Rdf_uri.Invalid_uri
exception Invalid_link_id of string
exception Internal_error of string

type uri = Rdf_uri.uri

type link_id = uri * uri
type link = link_id * uri * uri list

type tag_type = TagLink | TagContent

type update_mode = Adding | Replacing

(******************************************************************************
****************************** Configuration **********************************
*******************************************************************************)

let domain = "http://pumgrana.com/"
let base_ressouce_url = domain ^ "ressource/"
let base_content_url = domain ^ "content/detail/"
let base_tag_url = domain ^ "tag/"
let base_tag_link_url = base_tag_url ^ "link/"
let base_tag_content_url = base_tag_url ^ "content/"
let base_content_ressource = base_ressouce_url ^ "content/"

let tagged_content_r = base_content_ressource ^ "tagged"
let content_title_r = base_content_ressource ^ "title"
let content_summary_r = base_content_ressource ^ "summary"
let tag_link_r = base_ressouce_url ^ "tag_link"
let tag_content_r = base_ressouce_url ^ "tag_content"

let base_url = Rdf_uri.uri "http://127.0.0.1:8000"
let get_url = Rdf_uri.append base_url "/sparql/"
let update_url = Rdf_uri.append base_url "/update/"

(******************************************************************************
********************************** Tools **************************************
*******************************************************************************)

let uri_of_string = Rdf_uri.uri ~check:true
let string_of_uri = Rdf_uri.string

(* Link's tools *)

let link_id (origin_uri:uri) (target_uri:uri) = origin_uri, target_uri

let str_tuple_of_link_id (origin_uri, target_uri) =
  string_of_uri origin_uri, string_of_uri target_uri

let string_of_link_id link_id =
  let origin_uri, target_uri = str_tuple_of_link_id link_id in
  origin_uri ^ "@" ^ target_uri

let link_id_of_string link_id =
  let regexp = Str.regexp "@" in
  try
    let strings = Str.split regexp link_id in
    if List.length strings > 2 then raise (Invalid_argument "Too many @");
    let origin_str_uri = List.hd strings in
    let target_str_uri = List.hd (List.tl strings) in
    let origin_uri = uri_of_string origin_str_uri in
    let target_uri = uri_of_string target_str_uri in
    origin_uri, target_uri
  with e ->
    raise (Invalid_link_id (link_id ^ ": is not a valid link_id"))

let target_uri_from_link_id (origin_uri, target_uri) = target_uri
let origin_uri_from_link_id (origin_uri, target_uri) = origin_uri

(* Id's tools *)

let uri_of_content_id id =
  uri_of_string (base_content_url ^ (Nosql_store.string_of_id id))

let uri_of_tag_id_link id = uri_of_string (base_tag_link_url ^ id)
let uri_of_tag_id_content id = uri_of_string (base_tag_content_url ^ id)

let syntax =
  let stx = Hashtbl.find Neturl.common_url_syntax "http" in
  let stx = Neturl.partial_url_syntax stx in
  { stx with Neturl.url_enable_fragment = Neturl.Url_part_allowed }

let uri_of_subject base subject =
  let encode_subject = Netencoding.Url.encode subject in
  Rdf_uri.uri (base ^ encode_subject)

let uri_of_tag_link_subject = uri_of_subject base_tag_link_url
let uri_of_tag_content_subject = uri_of_subject base_tag_content_url

let pumgrana_id_of_uri base uri =
  let str = string_of_uri uri in
  let regexp = Str.regexp base in
  let pos =
    try (Str.search_forward regexp str 0) + (String.length base)
    with Not_found -> raise (Invalid_uri (str ^ ": is not a Pumgrana URI."))
  in
  let _ =
    try
      let _ = Str.search_forward regexp str pos in
      raise (Invalid_uri (str ^ ": looks to be an invalid URI."))
    with Not_found -> ()
  in
  Str.replace_first regexp "" str


let content_id_of_uri uri =
  Nosql_store.id_of_string (pumgrana_id_of_uri base_content_url uri)

let tag_id_link_of_uri = pumgrana_id_of_uri base_tag_link_url
let tag_id_content_of_uri = pumgrana_id_of_uri base_tag_content_url

(* Other tools  *)

let string_of_term = function
  | Rdf_term.Iri iri     -> Rdf_iri.string iri
  | Rdf_term.Literal lit -> Rdf_utf8.utf8_escape lit.Rdf_term.lit_value
  | Rdf_term.Blank       -> "_"
  | Rdf_term.Blank_ id   ->  "_:" ^ (Rdf_term.string_of_blank_id id)

let from_solution name solution =
  try string_of_term (Rdf_sparql.get_term solution name)
  with Not_found -> raise (Internal_error (name ^ ": Not found into the solution"))

let triple_link_from solution =
  from_solution "origin" solution,
  from_solution "target" solution,
  from_solution "tag" solution

let tuple_link_from solution =
  from_solution "target" solution,
  from_solution "tag" solution

let tuple_tag_from solution =
  uri_of_string (from_solution "tag" solution),
  from_solution "subject" solution

let triple_content_from solution =
  content_id_of_uri (uri_of_string (from_solution "content" solution)),
  from_solution "title" solution,
  from_solution "summary" solution

let links_of_solutions origin_uri solutions =
  let build_tag_list map solution =
    let target_uri, tag_str_uri = tuple_link_from solution in
    let tags_uri = uri_of_string tag_str_uri in
    let tags =
      try SMap.find target_uri map
      with Not_found -> []
    in
    SMap.add target_uri (tags_uri::tags) map
  in
  let solution_map = List.fold_left build_tag_list SMap.empty solutions in
  let build_links target_str_uri tags_uri links =
    let target_uri = uri_of_string target_str_uri in
    let link_id = link_id origin_uri target_uri in
    (link_id, target_uri, tags_uri)::links
  in
  SMap.fold build_links solution_map []

let get_solutions = function
  | Rdf_sparql.Solutions s -> s
  | _                      -> raise (Internal_error "None a solution format")

let get_boolean = function
  | Rdf_sparql.Bool b -> b
  | _                 -> raise (Internal_error "None a boolean format")

let get_result = function
  | Ok          -> raise (Internal_error "No result returned")
  | Result r    -> r
  | Error e     -> raise (Internal_error (string_of_error e))

let check_ok = function
  | Ok          -> ()
  | Error e     -> raise (Internal_error (string_of_error e))
  | Result _    -> raise (Internal_error "Unexpected result return")

let next_query query sep =
  if String.length query == 0 then query else query ^ sep

let build_list ref_list new_list element =
  let are_equal element e = String.compare element e == 0 in
  if List.exists (are_equal element) ref_list
  then new_list
  else (uri_of_string element)::new_list

(*** Lwt tools  *)

let lwt_ignore l =
  lwt _ = l in
  Lwt.return ()

(*** Shortcut ***)

let half_get_from_4store query =
  let base = Rdf_iri.iri domain in
  let msg = {in_query = query; in_dataset = empty_dataset} in
  Rdf_4s_lwt.get ~base get_url msg

let get_from_4store query =
  lwt results = half_get_from_4store query in
  let solutions = get_solutions (get_result results) in
  Lwt.return (solutions)

let ask_to_4store query =
  lwt results = half_get_from_4store query in
  let boolean = get_boolean (get_result results) in
  Lwt.return (boolean)

let post_on_4store query =
  let fake_base = Rdf_iri.iri ~check:false domain in
  let msg = {in_query = query; in_dataset = empty_dataset} in
  lwt res = Rdf_4s_lwt.post_update ~base:fake_base update_url msg in
  Lwt.return (check_ok res)

(******************************************************************************
******************************** Tags *****************************************
*******************************************************************************)

let get_tags tag_type tags_uri =
  let r_url = match tag_type with
    | TagLink    -> tag_link_r
    | TagContent -> tag_content_r
  in
  let build_query q tag_uri =
    let uri = string_of_uri tag_uri in
    q^"{?tag  <"^r_url^"> ?subject . FILTER regex(str(?tag), \""^uri^"\")} . "
  in
  let half_query =
    if List.length tags_uri != 0
    then List.fold_left build_query "" tags_uri
    else "?tag <"^r_url^"> ?subject"
  in
  let query = "SELECT ?tag ?subject WHERE { " ^ half_query ^ " }" in
  lwt solutions = get_from_4store query in
  let tuple_tags = List.map tuple_tag_from solutions in
  Lwt.return (tuple_tags)

let get_tags_from_link link_id =
  let o_str_uri, t_str_uri = str_tuple_of_link_id link_id in
  let query = "SELECT ?tag ?subject WHERE
  { <"^ o_str_uri ^"> ?tag <"^ t_str_uri ^"> .
    ?tag <" ^ tag_link_r ^ "> ?subject }"
  in
  lwt solutions = get_from_4store query in
  let tuple_tags = List.map tuple_tag_from solutions in
  Lwt.return (tuple_tags)

let get_tags_from_content content_uri =
  let content_str_uri = string_of_uri content_uri in
  let query = "SELECT ?tag ?subject WHERE
  { <"^ content_str_uri ^"> <"^ tagged_content_r ^"> ?tag .
    ?tag <" ^ tag_content_r ^ "> ?subject }"
  in
  lwt solutions = get_from_4store query in
  let tuple_tags = List.map tuple_tag_from solutions in
  Lwt.return (tuple_tags)

let get_tags_from_content_link content_uri =
  let o_uri = string_of_uri content_uri in
  let query = "SELECT ?tag ?subject WHERE
  { <" ^ o_uri ^ "> ?tag ?target.
    ?tag <" ^ tag_link_r ^ "> ?subject }"
  in
  lwt solutions = get_from_4store query in
  let tags_tuple = List.map tuple_tag_from solutions in
  Lwt.return (tags_tuple)

let insert_tags tag_type ?link_id ?content_uri subjects =

  let ressource_url, uri_of_subject =
    match tag_type, link_id, content_uri with
    | TagContent, None, _ -> tag_content_r, uri_of_tag_content_subject
    | TagLink, _, None    -> tag_link_r, uri_of_tag_link_subject
    | _, _, _             -> raise (Invalid_argument "Bad association")
  in
  let content_str_uri = match content_uri with
    | Some uri  -> Some (string_of_uri uri)
    | None      -> None
  in
  let link_str_uri = match link_id with
    | Some id   -> Some (str_tuple_of_link_id id)
    | None      -> None
  in

  (* Check if subject does not already exist *)
  let build_ask query subject =
    let q = next_query query " || " in
    q ^ "?sub = \"" ^ subject ^ "\""
  in
  let half_ask = List.fold_left build_ask "" subjects in
  let ask_query = "ASK { ?tag ?res ?sub . FILTER(" ^ half_ask ^ ") }" in
  lwt exist = ask_to_4store ask_query in
  if exist then raise (Invalid_argument "One subject or more already exist.");

  let insert_tag_on query tag_uri =
    let q = next_query query " . " in
    match link_str_uri, content_str_uri with
    | None, Some content_str_uri ->
        q ^ "<"^ content_str_uri ^">  <"^ tagged_content_r ^"> <"^ tag_uri ^">"
    | Some (o_uri, t_uri), None  ->
        q ^ "<" ^ o_uri ^ ">  <" ^ tag_uri ^ "> <" ^ t_uri ^ ">"
    | _, _ -> query
  in
  let insert_tag query uri subject =
    let q = next_query query " . " in
    let q' = q ^ "<" ^ uri ^ ">  <" ^ ressource_url ^ "> \"" ^ subject ^ "\"" in
    insert_tag_on q' uri
  in
  let tags_uri = List.map uri_of_subject subjects in
  let tags_str_uri = List.map string_of_uri tags_uri in
  let half_query = List.fold_left2 insert_tag "" tags_str_uri subjects in
  let query = "INSERT DATA { " ^ half_query ^ " }" in
  lwt () = post_on_4store query in
  Lwt.return (tags_uri)

let delete_tags tags_uri =
  let build_query q tag_uri =
    let uri = string_of_uri tag_uri in
    q ^ "{ <" ^ uri ^ "> ?res1 ?sub. {?tag ?res1 ?sub.} UNION {?x ?y ?z} }.
    { ?origin <" ^ uri ^ "> ?target. {?origin ?tag ?target.} UNION {?x ?y ?z} }.
    { ?content ?res2 <" ^ uri ^ ">. {?content ?res2 ?tag.} UNION {?x ?y ?z} }. "
  in
  let half_query = List.fold_left build_query "" tags_uri in
  let query = "DELETE {?x ?y ?z.} WHERE { " ^ half_query ^ " }" in
  post_on_4store query

let insert_tags_on_content content_uri tags_uri =
  let content_str_uri = string_of_uri content_uri in
  let insert_tag_on q tag_uri =
    let t_uri = string_of_uri tag_uri in
    q ^ "<"^ content_str_uri ^"> <"^ tagged_content_r ^"> <"^ t_uri ^"> . "
  in
  let half_query = List.fold_left insert_tag_on "" tags_uri in
  let query = "INSERT DATA { " ^ half_query ^ " }" in
  lwt () = post_on_4store query in
  Lwt.return (tags_uri)

let delete_tags_on_content content_uri tags_uri =
  let content_uri = string_of_uri content_uri in
  let build_query q tag_uri =
    let t_uri = string_of_uri tag_uri in
    q^" <"^content_uri^"> <"^tagged_content_r^"> <"^t_uri^"> . "
  in
  let half_query = List.fold_left build_query "" tags_uri in
  let query = "DELETE DATA { " ^ half_query ^ " }" in
  post_on_4store query

(******************************************************************************
******************************* Contents ***************************************
*******************************************************************************)

let get_triple_contents tags_uri =
  let build_regexp query tag_uri =
    let q = next_query query "|" in
    q ^ "(" ^ (string_of_uri tag_uri) ^ ")"
  in
  let half_query =
    if List.length tags_uri == 0 then "" else
      let regexp = List.fold_left build_regexp "" tags_uri in
      "?content <" ^ tagged_content_r ^ "> ?tag .
       FILTER regex(str(?tag), \"" ^ regexp ^ "\")"
  in
  let query = "SELECT ?content ?title ?summary WHERE
  { ?content <" ^ content_title_r ^ "> ?title .
    ?content <" ^ content_summary_r ^ "> ?summary .
  " ^ half_query ^ " }"
  in
  lwt solutions = get_from_4store query in
  let triple_contents = List.map triple_content_from solutions in
  Lwt.return (triple_contents)

let insert_content content_id title summary tags_uri =
  let content_str_uri = string_of_uri (uri_of_content_id content_id) in

  (* Check if the content does not already exist *)
  let ask_query = "ASK { <" ^ content_str_uri ^ "> ?p ?o }" in
  lwt exist = ask_to_4store ask_query in
  if exist then raise (Invalid_argument "The content is already registered.");

  let build_tag_data q tag_uri =
    let t_uri = string_of_uri tag_uri in
    q ^ "<" ^ content_str_uri ^ "> <" ^ tagged_content_r ^ "> <" ^ t_uri ^ "> . "
  in
  let tags_data = List.fold_left build_tag_data "" tags_uri in
  let content_data =
    "<" ^ content_str_uri ^ "> <" ^ content_title_r ^ "> \"" ^ title ^ "\" . " ^
    "<" ^ content_str_uri ^ "> <" ^ content_summary_r ^ "> \"" ^ summary ^ "\" . " ^
      tags_data
  in
  let query = "INSERT DATA { " ^ content_data ^ " }" in
  post_on_4store query

let delete_contents contents_id =
  let build_query q content_id =
    let content_uri = string_of_uri (uri_of_content_id content_id) in
    q ^ "{ <" ^ content_uri ^ "> ?r ?v. {?u ?r ?v.} UNION {?x ?y ?z} } . "
  in
  let half_query = List.fold_left build_query "" contents_id in
  let query = "DELETE {?u ?r ?v.} WHERE { " ^ half_query ^ " }" in
  post_on_4store query

let update_content content_id ?title ?summary ?tags_uri () =
  let content_uri = uri_of_content_id content_id in
  let c_str_uri = string_of_uri content_uri in

  (* Check if at least one parameter is given *)
  let () = match title, summary, tags_uri with
    | None, None, None ->
      raise (Invalid_argument "At least one argument have to be setted")
    | _, _, _ -> ()
  in

  (* Check if the content does already exist *)
  let ask_query = "ASK { <" ^ c_str_uri ^ "> ?p ?o }" in
  lwt exist = ask_to_4store ask_query in
  if not exist then raise Not_found;

  (* Build the update query *)
  let d_query, i_query =
    match title with
    | Some t ->
      "{<"^c_str_uri^"> <"^content_title_r^"> ?ti. {?s ?p ?o.} UNION {?x ?y ?z}}. ",
      "<" ^ c_str_uri ^ "> <" ^ content_title_r ^ "> \"" ^ t ^ "\" . "
    | None -> "", ""
  in
  let d_query', i_query' =
    match summary with
    | Some s ->
      d_query^"{<"^c_str_uri^"> <"^content_summary_r^"> ?su. {?s ?p ?o.} UNION {?x ?y ?z}}. ",
      i_query ^ "<"^c_str_uri^"> <" ^ content_summary_r ^ "> \"" ^ s ^ "\" . "
    | None -> d_query, i_query
  in
  lwt d_query'', i_query'' =
    match tags_uri with
    | Some tags_uri ->
      lwt old_tuple_tags = get_tags_from_content content_uri in
      let old_tags = List.map (fun (x, _) -> string_of_uri x) old_tuple_tags in
      let are_equal tag e = String.compare tag e == 0 in
      let build_list ref_list new_list tag =
        if List.exists (are_equal tag) ref_list then new_list else tag::new_list
      in
      let new_tags = List.map string_of_uri tags_uri in
      let deleting_list = List.fold_left (build_list new_tags) [] old_tags in
      let adding_list = List.fold_left (build_list old_tags) [] new_tags in
      let build_delete q uri =
        q^"{<"^c_str_uri^"> <"^tagged_content_r^"> <"^uri^">. {?s ?p ?o.} UNION {?x ?y ?z}}. "
      in
      let build_insert q uri =
        q ^ "<"^ c_str_uri ^"> <"^ tagged_content_r ^"> <"^ uri ^"> . "
      in
      let delete_query_tags = List.fold_left build_delete "" deleting_list in
      let insert_query_tags = List.fold_left build_insert "" adding_list in
      Lwt.return (d_query' ^ delete_query_tags, i_query' ^ insert_query_tags)
    | None -> Lwt.return(d_query', i_query')
  in
  let delete_query = "DELETE {?s ?p ?o.} WHERE { " ^ d_query'' ^ " }" in
  let insert_query = "INSERT DATA { " ^ i_query'' ^ " }" in
  lwt () = post_on_4store delete_query in
  post_on_4store insert_query

let update_content_tags content_uri tags_uri =
  lwt old_tuple_tags = get_tags_from_content content_uri in
  let old_tags = List.map (fun (x, _) -> string_of_uri x) old_tuple_tags in
  let new_tags = List.map string_of_uri tags_uri in
  let deleting_list = List.fold_left (build_list new_tags) [] old_tags in
  let adding_list = List.fold_left (build_list old_tags) [] new_tags in
  lwt () = if List.length deleting_list != 0
    then delete_tags_on_content content_uri deleting_list
    else Lwt.return ()
  in
  lwt _ = if List.length adding_list != 0
    then insert_tags_on_content content_uri adding_list
    else Lwt.return ([])
  in
  Lwt.return ()


(******************************************************************************
******************************** Links ****************************************
*******************************************************************************)

let build_tags_query content_uri tags =
  let filter_query = if List.length tags == 0 then "" else
      let build_rgx rgx tag_id =
        let rgx' = if String.length rgx == 0 then rgx else rgx ^ "|" in
        let tag_uri = string_of_uri tag_id in
        rgx' ^ "(" ^ tag_uri ^ ")"
      in
      let regex = List.fold_left build_rgx "" tags in
      " . FILTER regex(str(?tag), \"" ^ regex ^ "\")"
  in
  let content_str_uri = string_of_uri content_uri in
  "SELECT ?tag ?target WHERE
  { <"^content_str_uri^"> ?tag ?target"^filter_query^" }"

let links_from_content_tags content_uri tags_uri =
  let query = build_tags_query content_uri tags_uri in
  lwt solutions = get_from_4store query in
  let links = links_of_solutions content_uri solutions in
  Lwt.return (links)

let links_from_content content_uri =
  links_from_content_tags content_uri []

let build_query origin_str_uri target_str_uri q tag_uri =
  let tag_str_uri = string_of_uri tag_uri in
  q ^"<"^ origin_str_uri ^">  <"^ tag_str_uri ^"> <"^ target_str_uri ^"> . "

let build_delete_query_tag links_id tags_uri =
  let manager query link_id tags_uri =
    let o_str_uri, t_str_uri = str_tuple_of_link_id link_id in
    if List.length tags_uri == 0
    then raise (Invalid_argument "Empty tag list is not allowed");
    List.fold_left (build_query o_str_uri t_str_uri) query tags_uri
  in
  let half_query = List.fold_left2 manager "" links_id tags_uri in
  "DELETE DATA { " ^ half_query ^ " }"

let build_delete_query links_id =
  let build_query query link_id =
    let o_uri, t_uri = str_tuple_of_link_id link_id in
    let q = next_query query " UNION " in
    q ^ "{ <" ^ o_uri ^ "> ?tag <" ^ t_uri ^ "> . {?origin ?tag ?target.} UNION {?x ?y ?z} }"
  in
  let half_query = List.fold_left build_query "" links_id in
  "DELETE {?origin ?tag ?target.} WHERE { " ^ half_query ^ " }"

let internal_delete_links links_id tags_uri =
  let query = if List.length tags_uri == 0
    then build_delete_query links_id
    else build_delete_query_tag links_id tags_uri
  in
  lwt () = post_on_4store query in
  Lwt.return ()

let delete_links links_id =
  internal_delete_links links_id []


let internal_update_links mode triple_list =

  (* Format part *)
  let to_str_triple new_list (origin_uri, target_uri, tags_uri) =
    if List.length tags_uri == 0
    then raise (Invalid_argument "Empty tags list is not allowed");
    let list = List.map (fun t ->
      string_of_uri origin_uri, string_of_uri target_uri, string_of_uri t)
      tags_uri
    in
   list@new_list
  in
  let str_triple_list = List.fold_left to_str_triple [] triple_list in

  (* Getting part *)
  let build_select s (origin_str_uri, target_str_uri, _) =
    s ^"{ ?origin ?tag ?target .
          FILTER regex(str(origin), \"" ^ origin_str_uri ^ "\") .
          FILTER regex(str(target), \"" ^ target_str_uri ^ "\") } . "
  in
  let half_select = List.fold_left build_select "" str_triple_list in
  let select = "SELECT {?origin ?target ?tag} WHERE { " ^ half_select ^ " }" in
  lwt solutions = get_from_4store select in
  let existing_l = List.map triple_link_from solutions in

  (* Adding / Deleting tools *)
  let build_list ref_list new_list (origin, target, tag) =
    let are_equal (ori, tar, ta) =
      (String.compare origin ori) == 0 &&
      (String.compare target tar) == 0 &&
      (String.compare tag ta) == 0
    in
    if List.exists are_equal ref_list then new_list else
      (origin, target, tag)::new_list
  in
  let build_query q (ori_uri, tar_uri, tag_uri) =
    q ^"<"^ ori_uri ^">  <"^ tag_uri ^"> <"^ tar_uri ^"> . "
  in

  (* Adding part *)
  let a_list = List.fold_left (build_list existing_l) [] str_triple_list in
  let half_insert_query = List.fold_left build_query "" a_list in
  let insert_query = "INSERT DATA { " ^ half_insert_query ^ " }" in

  (* Delete part *)
  lwt () = if mode != Replacing then Lwt.return () else
      let d_list = List.fold_left (build_list str_triple_list) [] existing_l in
      let half_delete_query = List.fold_left build_query "" d_list in
      let delete_query = "DELETE DATA { " ^ half_delete_query ^ " }" in
      post_on_4store delete_query
  in

  lwt () = post_on_4store insert_query in

  (* Return format tools *)
  let build_link_id (origin_uri, target_uri, tags_uri) =
    link_id origin_uri target_uri
  in
  let link_ids = List.map build_link_id triple_list in
  Lwt.return (link_ids)

let insert_links triple_list =
  internal_update_links Adding triple_list

let update_links tuple_list =
  let triple_from_tuple (link_id, tags_uri) =
    let origin_uri, target_uri = link_id in
    (origin_uri, target_uri, tags_uri)
  in
  let triple_list = List.map triple_from_tuple tuple_list in
  lwt_ignore (internal_update_links Replacing triple_list)
