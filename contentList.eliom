open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

open Pdeserialize
open GUI_deserialize

let get filter research =
  try_lwt Http.research_contents research
  with e -> (print_endline (Printexc.to_string e); Lwt.return [])

let make opt_filter research =
  lwt contents = get opt_filter research in
  let content_list = GUI_tools.build_contents_list contents in
  let html = D.div ~a:[a_class["content_main_list"]] content_list in
  Lwt.return html
