(*
  GUI Register
  This module implement services of GUI
 *)

let _ =
  Pumgrana.App.register
    ~service:GUI_services.starting_service
    (fun () () ->
      ignore {unit{
        Eliom_client.change_url
        ~service:%GUI_services.home_service_without None }};
      lwt data = GUI_core.get_contents None None in
      Lwt.return (GUI_html.home_html data None))

let _ =
  Pumgrana.App.register
    ~service:GUI_services.home_service_without
    (fun (filter) () ->
      lwt data = GUI_core.get_contents filter None in
      Lwt.return (GUI_html.home_html data None))

let map f = function
  | Some x -> Some (f x)
  | None   -> None

let _ =
  Pumgrana.App.register
    ~service:GUI_services.home_service
    (fun (filter, research) () ->
      lwt data = GUI_core.get_contents filter research in
      Lwt.return (GUI_html.home_html data research))

let _ =
  Pumgrana.App.register
    ~service:GUI_services.content_detail_service
    (fun content_uri () ->
      let uri = Rdf_store.uri_decode content_uri in
      lwt data = GUI_core.get_detail_content uri in
      Lwt.return (GUI_html.content_detail data))

let _ =
  Pumgrana.App.register
    ~service:GUI_services.content_update_service
    (fun content_uri () ->
      let uri = Rdf_store.uri_decode content_uri in
      lwt data = GUI_core.get_detail_content uri in
      Lwt.return (GUI_html.content_update data))

let _ =
  Pumgrana.App.register
    ~service:GUI_services.content_insert_service
    (fun () () -> Lwt.return (GUI_html.content_insert ()))

let _ =
  Pumgrana.App.register
    ~service:GUI_services.link_insert_service
    (fun (origin_uri, target_uri) () ->
      let o_uri = map Rdf_store.uri_decode origin_uri in
      let t_uri = map Rdf_store.uri_decode target_uri in
      Lwt.return (GUI_html.link_insert o_uri t_uri))

let _ =
  Pumgrana.App.register
    ~service:GUI_services.link_update_service
    (fun link_uri () ->
      let uri = Rdf_store.uri_decode link_uri in
      lwt link_detail = GUI_core.get_link_detail uri in
      Lwt.return (GUI_html.link_update link_detail))
