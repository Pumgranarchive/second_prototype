open Rdf_sparql_protocol

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

let link_id_of_triple origin_id target_id tag =
  origin_id ^ "@" ^ target_id ^ "@" ^ tag

let content_id_of_uri str =
  let regexp = Str.regexp base_content_url in
  Str.replace_first regexp "" str

let string_of_term = function
  | Rdf_term.Iri iri     -> Rdf_iri.string iri
  | Rdf_term.Literal lit -> Rdf_utf8.utf8_escape lit.Rdf_term.lit_value
  | Rdf_term.Blank       -> "_"
  | Rdf_term.Blank_ id   ->  "_:" ^ (Rdf_term.string_of_blank_id id)

let link_of_solution origin_uri solution =
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
  let link_id = link_id_of_triple origin_uri target_uri link_tag in
  link_id, content_id_of_uri target_uri, link_tag

let links_of_solutions content_uri solutions =
  List.map (link_of_solution content_uri) solutions

let get_solutions = function
  | Rdf_sparql.Solutions s -> s
  | _                      -> failwith "None a solutions format result"

let get_result = function
  | Ok          -> failwith "No solutions returned"
  | Result r    -> r
  | Error e     -> failwith (string_of_error e)

let is_ok = function
  | Ok  -> true
  | _   -> false

let build_tags_query content_uri tags =
  let base_query = "SELECT ?p ?o WHERE { <"^content_uri^"> ?p ?o" in
  let half_query = if tags == [] then base_query else
      let regex_of_tag tag = "(" ^ tag ^ ")" in
      let regexs = List.map regex_of_tag tags in
      let concat r1 r2 = r1 ^ "|" ^ r2 in
      let regex = List.fold_left concat (List.hd regexs) (List.tl regexs) in
      base_query ^ " . FILTER regex(?o, \"" ^ regex ^ "\")"
  in
  half_query ^ " }"

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
(*   let print_link (id, target, tag) = *)
(*     print_endline ("ID: " ^ id ^ " ,Target: " ^ target ^ " ,Tag: " ^ tag) *)
(*   in *)
(*   let content_id = "52780cbdc21477f7aa5b9107" in *)
(*   Lwt.async (fun () -> *)
(*     print_endline "Launch"; *)
(*     lwt links = get_links_from_content_tags content_id ["Part of"] in *)
(*     print_endline "End"; *)
(*     if links == [] then print_endline "Nothing !"; *)
(*     List.iter print_link links; *)
(*     Lwt.return ()) *)

(* let insert_link origin_id target_id tag = *)
(*   let sub = Rdf_term.Iri (iri_of_strid origin_id) in *)
(*   let pred = iri_of_strid target_id in *)
(*   let obj = Rdf_term.Iri (iri_of_strid tag) in *)
(*   let _ = g.Rdf_graph.add_triple ~sub ~pred ~obj in *)
(*   origin_id ^ separator ^ target_id ^ separator ^ tag *)

(* let insert_links origin_id target_ids tags = *)
(*   let rec manage_tags result target_id = function *)
(*     | h_tag::t_tag      -> manage_tags *)
(*       ((insert_link origin_id target_id h_tag)::result) target_id t_tag *)
(*     | []                -> result *)
(*   in *)
(*   let rec manage_target result = function *)
(*     | h_tar::t_tar, h_tag::t_tag  -> *)
(*       manage_target ((manage_tags result h_tar h_tag)@result) (t_tar, t_tag) *)
(*     | [], []                      -> result *)
(*     | [], _                       -> failwith "too many tags" *)
(*     | _, []                       -> failwith "not enough tags" *)
(*   in *)
(*   manage_target [] (target_ids, tags) *)

(* let delete_links strids = *)
(*   let aux strid = *)
(*     let tmp = Str.split regex_sep strid in *)
(*     let sub = Rdf_term.Iri (iri_of_strid (List.nth tmp 0)) in *)
(*     let pred = iri_of_strid (List.nth tmp 1) in *)
(*     let obj = Rdf_term.Iri (iri_of_strid (List.nth tmp 2)) in *)
(*     g.Rdf_graph.rem_triple ~sub ~pred ~obj *)
(*   in *)
(*   List.iter aux strids *)
