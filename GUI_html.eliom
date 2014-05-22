(*
  GUI Html
  This module make the html of GUI services.
*)

{shared{

open Eliom_lib
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

module Yojson = Yojson.Basic

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
  let insertb, backb, forwardb, header_elt =
    GUI_tools.build_contents_header ()
  in
  ignore {unit{ GUI_client_core.bind_back %backb }};
  ignore {unit{ GUI_client_core.bind_forward %forwardb }};
  ignore {unit{ GUI_client_core.bind_insert_content %insertb }};
  ignore {unit{ GUI_client_core.handle_refresh_contents %contents_html
                %tags_inputs %submit}};
  Eliom_tools.F.html
    ~title:"Pumgrana"
    ~css:[["css";"pumgrana.css"]]
    Html5.F.(body [
      header_elt;
      contents_html;
      tags_html])

(** Display the content detail html service *)
let content_detail ((c_title, c_text, c_id), tags_id, links, tags_link) =
  try
    let aux (subject, id) =  div [pcdata subject] in
    let tags_subjects = List.map aux tags_id in
    let links_html = D.div (GUI_tools.build_links_list links) in
    let submit, links_tags_inputs, links_tags_html =
      GUI_tools.build_tags_form tags_link
    in
    let backb, forwardb, updateb, deleteb, header_elt =
      GUI_tools.build_detail_content_header ()
    in
    ignore {unit{ GUI_client_core.bind_back %backb }};
    ignore {unit{ GUI_client_core.bind_forward %forwardb }};
    ignore {unit{ GUI_client_core.bind_update_content %updateb %c_id }};
    ignore {unit{ GUI_client_core.bind_delete_content %deleteb %c_id }};
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

(** Update content detail html service *)
let content_update ((c_title, c_text, c_id), tags, links, tags_link) =
  try
    let tags_inputs, tags_html = GUI_tools.build_ck_tags_list tags in
    let links_inputs, links_html = GUI_tools.build_ck_links_list links in
    let links_tags_html = GUI_tools.build_tags_list tags_link in
    let cancelb, saveb, header_elt = GUI_tools.build_update_content_header () in
    let div_tags_html = D.div tags_html in
    let add_tag_input, submit_tag, tags_input_list, add_tag_html =
      GUI_tools.build_add_tag ()
    in
    let title_elt =
      D.raw_input ~a:[a_class ["title_update"]]
        ~input_type:`Text ~name:"title" ~value:c_title ()
    in
    let text_elt =
      D.raw_textarea ~a:[a_class ["text_update"]]
        ~name:"text" ~value:c_text ()
    in
    ignore {unit{ GUI_client_core.bind_cancel_update_content %cancelb %c_id}};
    ignore {unit{ GUI_client_core.bind_save_update_content %saveb %c_id
                  %title_elt %text_elt %tags_inputs %links_inputs
                  %tags_input_list}};
    ignore {unit{ GUI_client_core.bind_add_tag_content %submit_tag
                  %div_tags_html %add_tag_input %tags_input_list}};
    Eliom_tools.F.html
      ~title:"Pumgrana"
      ~css:[["css";"pumgrana.css"]]
      Html5.F.(body [
        header_elt;
        div ~a:[a_class["detail"]]
          [title_elt; br (); text_elt;
           div ~a:[a_class["detail_tags"]]
             [h5 [pcdata "Tags"];
              span [pcdata "Select to remove"];
              div_tags_html;
              div add_tag_html]];
        div ~a:[a_class["links"]]
          [h4 [pcdata "Links"];
           span [pcdata "Select to remove"];
           div links_html;
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

(** Insert content html service *)
let content_insert () =
  try
    let _, tags_html = GUI_tools.build_ck_tags_list [] in
    let cancelb, saveb, header_elt = GUI_tools.build_update_content_header () in
    let div_tags_html = D.div tags_html in
    let add_tag_input, submit_tag, tags_input_list, add_tag_html =
      GUI_tools.build_add_tag ()
    in
    let title_elt =
      D.raw_input ~a:[a_class ["title_update"]]
        ~input_type:`Text ~name:"title" ()
    in
    let text_elt =
      D.raw_textarea ~a:[a_class ["text_update"]]
        ~name:"text" ()
    in
    ignore {unit{ GUI_client_core.bind_back %cancelb}};
    ignore {unit{ GUI_client_core.bind_add_tag_content %submit_tag
                    %div_tags_html %add_tag_input %tags_input_list}};
    ignore {unit{ GUI_client_core.bind_save_insert_content %saveb
                    %title_elt %text_elt %tags_input_list}};
    Eliom_tools.F.html
      ~title:"Pumgrana"
      ~css:[["css";"pumgrana.css"]]
      Html5.F.(body [
        header_elt;
        div ~a:[a_class["detail"]]
          [title_elt; br (); text_elt;
           div ~a:[a_class["detail_tags"]]
             [h5 [pcdata "Tags"];
              span [pcdata "Select to unadd"];
              div_tags_html;
              div add_tag_html]]])
  with
  | e -> print_endline (Printexc.to_string e);
    Eliom_tools.F.html
      ~title:"Pumgrana"
      ~css:[["css";"pumgrana.css"]]
      Html5.F.(body [])
