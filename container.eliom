open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

open Utils

let make lwt_elements =
  lwt elements = Lwt_list.(map_s wait lwt_elements) in
  let html = div ~a:[a_id "container"] elements in
  Lwt.return html
