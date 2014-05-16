open Rdf_sparql_protocol

module SMap = Map.Make(String)

let base_content_url = "http://pumgrana.com/content/detail/"
let base_url = Rdf_uri.uri "http://127.0.0.1:8000"
let get_url = Rdf_uri.append base_url "/sparql/"
let update_url = Rdf_uri.append base_url "/update/"

let uri_of_content_id str =
  base_content_url ^ str

(* let print_solution s = *)
(*   let print n t = print_endline (n^": "^(Rdf_term.string_of_term t)) in *)
(*   Rdf_sparql.solution_iter print s *)

(* let print_solutions = List.iter (fun s -> print_endline ""; print_solution s) *)

let link_id origin_uri target_uri =
  origin_uri ^ "@" ^ target_uri

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
(*     lwt links = get_links_from_content_tags content_id ["T01"; "Bidon01"] in *)
(*     if links == [] then print_endline "Nothing !"; *)
(*     List.iter print_link links; *)
(*     Lwt.return ()) *)

let rec double_fold_left func var = function
  | h1::t1, h2::t2 -> double_fold_left func (func var h1 h2) (t1, t2)
  | [], []         -> var
  | _, []          -> failwith "too many elements in list 1"
  | [], _          -> failwith "too many elements in list 2"

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
  let half_query = double_fold_left query_of_target "" (targets_uri, tags) in
  let query = "INSERT DATA { " ^ half_query ^ " }" in
  let fake_base = Rdf_iri.iri ~check:false "" in
  let msg = {in_query = query; in_dataset = empty_dataset} in
  lwt res = Rdf_4s_lwt.post_update ~base:fake_base update_url msg in
  if is_ok res
  then Lwt.return (List.map (link_id origin_uri) targets_uri)
  else Lwt.return ([])

(* let _ = *)
(*   lwt res = insert_links "http://pumgrana.com" ["http://patate.com";"http://test02.com"] [["Bidon01";"Bindon02"];["T01"]] in *)
(*   if res != [] then print_endline "Ok" else print_endline "Nop"; *)
(*   Lwt.return () *)

(* let delete_links strids = *)
(*   let aux strid = *)
(*     let tmp = Str.split regex_sep strid in *)
(*     let sub = Rdf_term.Iri (iri_of_strid (List.nth tmp 0)) in *)
(*     let pred = iri_of_strid (List.nth tmp 1) in *)
(*     let obj = Rdf_term.Iri (iri_of_strid (List.nth tmp 2)) in *)
(*     g.Rdf_graph.rem_triple ~sub ~pred ~obj *)
(*   in *)
(*   List.iter aux strids *)
