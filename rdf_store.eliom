open Rdf_sparql_protocol

module SMap = Map.Make(String)

let (>>=) = Lwt.bind

let base_content_url = "http://pumgrana.com/content/detail/"
let base_url = Rdf_uri.uri "http://127.0.0.1:8000"
let get_url = Rdf_uri.append base_url "/sparql/"
let update_url = Rdf_uri.append base_url "/update/"

let uri_of_content_id str =
  base_content_url ^ str

let link_id origin_uri target_uri =
  origin_uri ^ "@" ^ target_uri

let data_of_link_id link_id =
  let regexp = Str.regexp "@" in
  try
    let strings = Str.split regexp link_id in
    List.hd strings, List.hd (List.tl strings)
  with e -> failwith "Invalid Link ID"

let content_id_of_uri str =
  let regexp = Str.regexp base_content_url in
  Str.replace_first regexp "" str

let string_of_term = function
  | Rdf_term.Iri iri     -> Rdf_iri.string iri
  | Rdf_term.Literal lit -> Rdf_utf8.utf8_escape lit.Rdf_term.lit_value
  | Rdf_term.Blank       -> "_"
  | Rdf_term.Blank_ id   ->  "_:" ^ (Rdf_term.string_of_blank_id id)

let data_from_solution origin_uri solution =
  let p = ref None in
  let o = ref None in
  let get n t = match n with
    | "p"       -> p := Some t
    | "o"       -> o := Some t
    | _         -> ()
  in
  Rdf_sparql.solution_iter get solution;
  let target_uri, link_tag = match !p, !o with
    | None, _        -> failwith "Predicate not found"
    | _, None        -> failwith "Object not found"
    | Some p, Some o -> string_of_term p, string_of_term o
  in
  target_uri, link_tag

let links_of_solutions origin_uri solutions =
  let build_tag_list map solution =
    let target_uri, link_tag = data_from_solution origin_uri solution in
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

let tag_from_solution solution =
  let get n t v = match n with
    | "o"       -> Some t
    | _         -> v
  in
  match Rdf_sparql.solution_fold get solution None with
  | Some o    -> string_of_term o
  | None      -> failwith "Object not found"

let get_solutions = function
  | Rdf_sparql.Solutions s -> s
  | _                      -> failwith "None a solutions format result"

let get_result = function
  | Ok          -> failwith "No solutions returned"
  | Result r    -> r
  | Error e     -> failwith (string_of_error e)

let is_ok = function
  | Ok          -> true
  | Error e     -> failwith (string_of_error e)
  | _           -> false

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

let get_links_from_content_tags content_id tags =
  let content_uri = uri_of_content_id content_id in
  let query = build_tags_query content_uri tags in
  print_endline query;
  let base = Rdf_iri.iri "http://pumgrana.com" in
  let msg = {in_query = query; in_dataset = empty_dataset} in
  lwt results = Rdf_4s_lwt.get ~base get_url msg in
  let solutions = get_solutions (get_result results) in
  Lwt.return (links_of_solutions content_uri solutions)

let get_links_from_content content_id =
  get_links_from_content_tags content_id []

let insert_links origin_id targets_id tags =
  let origin_uri = uri_of_content_id origin_id in
  let targets_uri = List.map uri_of_content_id targets_id in
  let build_query origin_uri target_uri query tag =
    let query' = if String.length query == 0 then query else query ^ " . " in
    query' ^ "<" ^ origin_uri ^ "> <" ^ target_uri ^ "> \"" ^ tag ^ "\""
  in
  let query_of_target query target_uri tags =
    if List.length tags == 0 then failwith "Empty tag list are not allowed";
    List.fold_left (build_query origin_uri target_uri) query tags
  in
  if List.length targets_uri == 0
  then failwith "Empty target list are not allowed";
  let half_query = List.fold_left2 query_of_target "" targets_uri tags in
  let query = "INSERT DATA { " ^ half_query ^ " }" in
  print_endline query;
  let fake_base = Rdf_iri.iri ~check:false "" in
  let msg = {in_query = query; in_dataset = empty_dataset} in
  lwt res = Rdf_4s_lwt.post_update ~base:fake_base update_url msg in
  if is_ok res
  then Lwt.return (List.map (link_id origin_uri) targets_uri)
  else Lwt.return ([])

let build_delete_query_tag links_id tags =
  let build_query o_uri t_uri query tag =
    let q = if String.length query == 0 then query else query ^ " . " in
    q ^ "<" ^ o_uri ^ "> <" ^ t_uri ^ "> \"" ^ tag ^ "\""
  in
  let manager query link_id tags =
    let o_uri, t_uri = data_of_link_id link_id in
    if List.length tags == 0 then failwith "Empty tag list are not allowed";
    List.fold_left (build_query o_uri t_uri) query tags
  in
  let half_query = List.fold_left2 manager "" links_id tags in
  "DELETE DATA { " ^ half_query ^ " }"

let build_delete_query links_id =
  let build_query query link_id =
    let o_uri, t_uri = data_of_link_id link_id in
    let q = if String.length query == 0 then query else query ^ " UNION " in
    q ^ "{ <" ^ o_uri ^ "> <" ^ t_uri ^ "> ?o . {?s ?p ?o.} UNION {?x ?y ?z} }"
  in
  let half_query = List.fold_left build_query "" links_id in
  "DELETE {?s ?p ?o.} WHERE { " ^ half_query ^ " }"

let delete_links links_id tags =
  let query = if List.length tags == 0
    then build_delete_query links_id
    else build_delete_query_tag links_id tags
  in
  print_endline query;
  let fake_base = Rdf_iri.iri ~check:false "" in
  let msg = {in_query = query; in_dataset = empty_dataset} in
  lwt res = Rdf_4s_lwt.post_update ~base:fake_base update_url msg in
  Lwt.return (is_ok res)

let update_link link_id new_tags =
  let o_uri, t_uri = data_of_link_id link_id in
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
    if list != [] then update_func list else Lwt.return (true)
  in
  let is_not_empty res =
    res >>= fun result -> Lwt.return (result != [])
  in
  lwt deleting_ok = update deleting_list (fun l ->
    delete_links [link_id] [l])
  in
  lwt adding_ok = update adding_list (fun l ->
    is_not_empty (insert_links origin_id [target_id] [l]))
  in
  Lwt.return (deleting_ok && adding_ok)

(* let _ = *)
(*   lwt res = insert_links "http://pumgrana.com" ["http://patate.com";"http://test02.com"] [["Bidon01";"Bindon02"];["T01";"T02"]] in *)
(*   if res != [] then print_endline "Ok" else print_endline "Nop"; *)
(*   Lwt.return () *)

(* let _ = *)
(*   lwt res = update_link "http://pumgrana.com/content/detail/http://pumgrana.com@http://pumgrana.com/content/detail/http://patate.com" ["Chou";"Patate"] in *)
(*   if res then print_endline "Ok" else print_endline "Nop"; *)
(*   Lwt.return () *)


(* let _ = *)
(*   Lwt.async (fun () -> *)
(*     lwt res = delete_links ["http://pumgrana.com/content/detail/http://pumgrana.com@http://pumgrana.com/content/detail/http://patate.com"; "http://pumgrana.com/content/detail/http://pumgrana.com@http://pumgrana.com/content/detail/http://test02.com"] in *)
(*     if res then print_endline "Ok" else print_endline "Nop"; *)
(*     Lwt.return ()) *)

(* let _ = *)
(*   let print_link (id, target, tags) = *)
(*     let tags_string = *)
(*       if List.length tags == 0 then "" else *)
(*         List.fold_left (fun s tag -> s ^ " | " ^ tag) (List.hd tags) (List.tl tags) *)
(*     in *)
(*     print_endline ("ID: " ^ id ^ " ,Target: " ^ target ^ " ,Tags: " ^ tags_string) *)
(*   in *)
(*   (\* let content_id = "52780cbdc21477f7aa5b9107" in *\) *)
(*   let content_id = "http://pumgrana.com" in *)
(*   Lwt.async (fun () -> *)
(*     lwt links = get_links_from_content_tags content_id [] in *)
(*     if links == [] then print_endline "Nothing !"; *)
(*     List.iter print_link links; *)
(*     Lwt.return ()) *)
