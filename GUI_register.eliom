(*
  GUI Register
  This module implement services of GUI
 *)

let _ =
  Pumgrana.App.register
    ~service:GUI_services.starting_service
    (fun () () ->
      Lwt.return (GUI_html.home_html (GUI_core.get_contents None None)))

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
