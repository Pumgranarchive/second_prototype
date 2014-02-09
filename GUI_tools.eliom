
(*
  GUI Tools
  This module help GUI_html to make the html.
*)

{shared{


open Eliom_lib
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

module Yj = Yojson.Safe


(*** Build tools  *)

let build_header elt_list =
  div ~a:[a_class["header"]]
    ([h4 ~a:[a_class["pumgrana"]] [pcdata "Pumgrana"]]@elt_list)

let build_header_back_forward elt_list =
  let back_button = D.raw_input ~input_type:`Submit ~value:"Back" () in
  let forward_button = D.raw_input ~input_type:`Submit ~value:"Forward" () in
  let header_elt = build_header ([back_button; forward_button]@elt_list) in
  back_button, forward_button, header_elt

let build_contents_header () =
  build_header_back_forward []

let build_detail_content_header () =
  let update_button = D.raw_input ~input_type:`Submit ~value:"Update" () in
  let delete_button = D.raw_input ~input_type:`Submit ~value:"Delete" () in
  let back_button, forward_button, header_elt =
    build_header_back_forward [update_button; delete_button]
  in
  back_button, forward_button, update_button, delete_button, header_elt

let build_update_content_header () =
  let cancel_button = D.raw_input ~input_type:`Submit ~value:"Cancel" () in
  let save_button = D.raw_input ~input_type:`Submit ~value:"Save" () in
  let header_elt = build_header [cancel_button; save_button] in
  cancel_button, save_button, header_elt

(** Build the tags html formular from tag list *)
let build_tags_form tags =
  let rec aux inputs full_html = function
    | []                -> inputs, full_html
    | (subject, id)::t  ->
      let input = D.raw_input ~input_type:`Checkbox ~name:id () in
      let html = div [pcdata subject; input] in
      aux (input::inputs) (html::full_html) t
  in
  let tags_inputs, tags_html = aux [] [] tags in
  let submit = D.raw_input ~input_type:`Submit ~value:"Submit" () in
  submit, tags_inputs, List.rev ((div [submit])::tags_html)

(** Build the links html display link list *)
let build_links_list links =
  let aux (title, _, id) =
    div [a ~service:%GUI_services.content_detail_service
            [h5 [pcdata title]] id]
  in
  List.map aux links

let build_contents_list contents =
  let aux (title, text, id) =
    div [a ~service:%GUI_services.content_detail_service [pcdata title] id;
        br ();
        span [pcdata text]]
  in
  List.map aux contents

}}
