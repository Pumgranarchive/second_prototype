(*
  GUI Register
  This module implement services of GUI
 *)

let _ =
  Pumgrana.App.register
    ~service:GUI_services.home
    (fun () () ->
      ignore {unit{
        Eliom_client.change_url
        ~service:%GUI_services.contents None }};
      lwt data = GUI_core.get_contents None None in
      Lwt.return (GUI_html.home_html data None))

let _ =
  Pumgrana.App.register
    ~service:GUI_services.contents
    (fun research () ->
      lwt data = GUI_core.get_contents None research in
      Lwt.return (GUI_html.home_html data research))

(* let _ = *)
(*   Pumgrana.App.register *)
(*     ~service:GUI_services.contents_two *)
(*     (fun (filter, research) () -> *)
(*       lwt data = GUI_core.get_contents filter research in *)
(*       Lwt.return (GUI_html.home_html data research)) *)

let _ =
  Pumgrana.App.register
    ~service:GUI_services.content_detail
    (fun content_uri () ->
      let uri = Rdf_store.uri_decode content_uri in
      lwt data = GUI_core.get_detail_content uri in
      Lwt.return (GUI_html.content_detail data))

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
