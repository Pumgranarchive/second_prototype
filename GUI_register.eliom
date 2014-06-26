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
      Lwt.return (GUI_html.home_html data))

let _ =
  Pumgrana.App.register
    ~service:GUI_services.home_service_without
    (fun (filter) () ->
      lwt data = GUI_core.get_contents filter None in
      Lwt.return (GUI_html.home_html data))

let _ =
  Pumgrana.App.register
    ~service:GUI_services.home_service
    (fun (filter, tags_id) () ->
      lwt data = GUI_core.get_contents filter tags_id in
      Lwt.return (GUI_html.home_html data))

let _ =
  Pumgrana.App.register
    ~service:GUI_services.content_detail_service
    (fun content_id () ->
      lwt data = GUI_core.get_detail_content content_id in
      Lwt.return (GUI_html.content_detail data))

let _ =
  Pumgrana.App.register
    ~service:GUI_services.content_update_service
    (fun content_id () ->
      lwt data = GUI_core.get_detail_content content_id in
      Lwt.return (GUI_html.content_update data))

let _ =
  Pumgrana.App.register
    ~service:GUI_services.content_insert_service
    (fun () () -> Lwt.return (GUI_html.content_insert ()))

(* let _ = *)
(*   Pumgrana.App.register *)
(*     ~service:GUI_services.link_insert_service *)
(*     (fun (origin_uri, target_uri) () -> *)
(*       Lwt.return (GUI_html.link_insert origin_uri target_uri)) *)

(* let _ = *)
(*   Pumgrana.App.register *)
(*     ~service:GUI_services.link_update_service *)
(*     (fun link_uri () -> *)
(*       Lwt.return (GUI_html.link_update link_uri)) *)
