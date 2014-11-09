(*
  GUI Register
  This module implement services of GUI
 *)

(** [identity x] return x *)
let identity x = x

(** [tuple_of first second] return a tuple build with [f] and [s] *)
let tuple_of f s = f, s

(** [uri_of_id id] translate id to uri *)
let uri_of_id encoded_id =
  let id = Rdf_store.uri_decode encoded_id in
  if Nosql_store.is_nosql_id id
  then Rdf_store.uri_of_content_id (Nosql_store.id_of_string id)
  else Rdf_store.uri_of_string id

(** [launcher func]
    Launch the [func], catch all exceptions, display error on html and prompt *)
let launcher func get_process post_process get_params post_params =
  let get_p = get_process get_params in
  let post_p = post_process post_params in
  try_lwt func get_p post_p
  with e ->
    let err_msg = Printexc.to_string e in
    let () = print_endline err_msg in
    Lwt.return (GUI_html.empty_html ~msg:err_msg ())

let _ =
  Pumgrana.App.register
    ~service:GUI_services.home
    (launcher GUI_html.home identity identity)

let _ =
  Pumgrana.App.register
    ~service:GUI_services.contents
    (launcher GUI_html.contents (tuple_of None) identity)

(* let _ = *)
(*   Pumgrana.App.register *)
(*     ~service:GUI_services.contents_two *)
(*     (launcher GUI_html.contents identity identity) *)

let _ =
  Pumgrana.App.register
    ~service:GUI_services.content_detail
    (launcher GUI_html.content_detail uri_of_id identity)

(* let _ = *)
(*   Pumgrana.App.register *)
(*     ~service:GUI_services.content_update_service *)
(*     (fun content_uri () -> *)
(*       let uri = Rdf_store.uri_decode content_uri in *)
(*       lwt data = GUI_core.get_detail_content uri in *)
(*       Lwt.return (GUI_html.content_update data)) *)

(* let _ = *)
(*   Pumgrana.App.register *)
(*     ~service:GUI_services.content_insert_service *)
(*     (fun () () -> Lwt.return (GUI_html.content_insert ())) *)

(* let map f = function *)
(*   | Some x -> Some (f x) *)
(*   | None   -> None *)

(* let _ = *)
(*   Pumgrana.App.register *)
(*     ~service:GUI_services.link_insert_service *)
(*     (fun (origin_uri, target_uri) () -> *)
(*       let o_uri = map Rdf_store.uri_decode origin_uri in *)
(*       let t_uri = map Rdf_store.uri_decode target_uri in *)
(*       Lwt.return (GUI_html.link_insert o_uri t_uri)) *)

(* let _ = *)
(*   Pumgrana.App.register *)
(*     ~service:GUI_services.link_update_service *)
(*     (fun link_uri () -> *)
(*       let uri = Rdf_store.uri_decode link_uri in *)
(*       lwt link_detail = GUI_core.get_link_detail uri in *)
(*       Lwt.return (GUI_html.link_update link_detail)) *)
