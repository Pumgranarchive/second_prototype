open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

open Pdeserialize
open GUI_deserialize

let get opt_filter research =
  try_lwt
    lwt json = API_core.research_contents opt_filter research in
    Lwt.return (get_service_return get_short_content_list json)
  with e -> (print_endline (Printexc.to_string e); Lwt.return [])

let make opt_filter research =
  lwt contents = get opt_filter research in
  let content_list = GUI_tools.build_contents_list contents in
  let html = D.div ~a:[a_class["content_main_list"]] content_list in
  Lwt.return html
