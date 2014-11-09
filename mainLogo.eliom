open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

let make () =
  let html = div ~a:[a_id "main_logo"] [] in
  Lwt.return html
