(*
  GUI Html
  This module make the html of GUI services.
*)

{shared{

open Eliom_lib
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

module Yj = Yojson.Safe

}}

(** Display the home html service *)
let home_html (contents, tags) =
  let submit, tags_inputs, tags_html_list = GUI_tools.build_tags_form tags in
  let contents_html =
    D.div ~a:[a_class["contents"]] (GUI_tools.build_contents_list contents)
  in
  let tags_html =
    div ~a:[a_class["contents_tags"]]
      ((h4 [pcdata "Tags"])::tags_html_list)
  in
  ignore {unit{ GUI_client_core.handle_refresh_contents %contents_html
                %tags_inputs %submit}};
  Eliom_tools.F.html
    ~title:"Pumgrana"
    ~css:[["css";"pumgrana.css"]]
    Html5.F.(body [
      div ~a:[a_class["header"]]
        [h2 [pcdata "Pumgrana"]];
      contents_html;
      tags_html (* ; *)
      (* br (); br (); *)
      (* post_form *)
      (*   ~service:API_services.insert_content *)
      (*   (fun (title, (text, tas_subject)) -> *)
      (*     [fieldset *)
      (*         [string_input ~input_type:`Hidden *)
      (*             ~name:title ~value:"title" (); *)
      (*          string_input ~input_type:`Hidden *)
      (*            ~name:text ~value:"text" (); *)
      (*          string_input ~input_type:`Submit *)
      (*            ~value:"Sub" ()]]) () *)
    ])

(** Display the content detail html service *)
let content_detail ((c_title, c_text, c_id), tags_id, links, tags_link) =
  try
    let aux (subject, id) =  div [pcdata subject] in
    let tags_subjects = List.map aux tags_id in
    let links_html = D.div (GUI_tools.build_links_list links) in
    let submit, links_tags_inputs, links_tags_html =
      GUI_tools.build_tags_form tags_link
    in
    let backb, updateb, deleteb, header_elt =
      GUI_tools.build_detail_content_header ()
    in
    ignore {unit{ GUI_client_core.bind_back %backb }};
    ignore {unit{ GUI_client_core.handle_refresh_links %c_id %links_html
                  %links_tags_inputs %submit }};
    Eliom_tools.F.html
      ~title:"Pumgrana"
      ~css:[["css";"pumgrana.css"]]
      Html5.F.(body [
        header_elt;
        div ~a:[a_class["detail"]]
          [h3 [pcdata c_title];
           p [pcdata c_text];
           div ~a:[a_class["detail_tags"]]
             [h5 [pcdata "Tags"];
              div tags_subjects]];
        div ~a:[a_class["links"]]
          [h4 [pcdata "Links"];
           links_html;
           div ~a:[a_class["links_tags"]]
             [h5 [pcdata "Link Tags"];
              div links_tags_html]]
      ])
  with
  | e -> print_endline (Printexc.to_string e);
    Eliom_tools.F.html
      ~title:"Pumgrana"
      ~css:[["css";"pumgrana.css"]]
      Html5.F.(body [])
