(*
  GUI Register
  This module implement services of GUI
 *)

(* let _ = *)
(*   Pumgrana.App.register *)
(*     ~service:GUI_services.starting_service *)
(*     (fun () () -> *)
(*       ignore {unit{ *)
(*         Eliom_client.change_url *)
(*         ~service:%GUI_services.home_service_without None }}; *)
(*       Lwt.return (GUI_html.home_html (GUI_core.get_contents None None))) *)

let _ =
  Pumgrana.App.register
    ~service:GUI_services.home_service_without
    (fun (filter) () ->
      Lwt.return (GUI_html.home_html (GUI_core.get_contents filter None)))

let _ =
  Pumgrana.App.register
    ~service:GUI_services.home_service
    (fun (filter, tags_id) () ->
      Lwt.return (GUI_html.home_html (GUI_core.get_contents filter tags_id)))

let _ =
  Pumgrana.App.register
    ~service:GUI_services.content_detail_service
    (fun content_id () ->
      Lwt.return (GUI_html.content_detail
                    (GUI_core.get_detail_content content_id)))

let _ =
  Pumgrana.App.register
    ~service:GUI_services.content_update_service
    (fun content_id () ->
      Lwt.return (GUI_html.content_update
                    (GUI_core.get_detail_content content_id)))

let _ =
  Pumgrana.App.register
    ~service:GUI_services.content_insert_service
    (fun () () -> Lwt.return (GUI_html.content_insert ()))
