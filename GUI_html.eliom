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

let empty_html ?msg () =
  let body_list = match msg with
    | Some s    -> [pcdata s]
    | None      -> []
  in
  Eliom_tools.F.html
    ~title:"Pumgrana"
    ~css:[["css";"pumgrana.css"]]
    Html5.F.(body body_list)

let internal_error_html () =
  empty_html ~msg:"Internal Error" ()

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
let content_detail (content, tags_id, links, tags_link) =
  try
    let aux (id, subject) =  div [pcdata subject] in
    let tags_subjects = List.map aux tags_id in
    let links_html = D.div (GUI_tools.build_links_list links) in
    let submit, links_tags_inputs, links_tags_html =
      GUI_tools.build_tags_form tags_link
    in
    let backb, forwardb, updateb, deleteb, header_elt =
      GUI_tools.build_detail_content_header ()
    in
    let c_id, content_elt = match content with
      | GUI_deserialize.Internal (c_id, c_title, c_summary, c_body) ->
        c_id, div [h3 [pcdata c_title]; p [pcdata c_summary]; p [pcdata c_body]]
      | GUI_deserialize.External (c_id, c_title, c_summary, c_html_body) ->
        let div_id = "html_body" in
        ignore {unit{ GUI_client_core.insert_html %div_id %c_html_body }};
        c_id, div ~a:[a_id div_id] []
        (* let uri = Xml.uri_of_string (GUI_deserialize.uri_of_id c_id) in *)
        (* c_id, div [iframe ~a:[a_src uri] []] *)
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
          [content_elt;
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
  | e ->
    let err_msg = Printexc.to_string e in
    print_endline err_msg;
    empty_html ~msg:err_msg ()

(** Update content detail html service *)
let content_update (content, tags, links, tags_link) =
  try
    let c_id, c_title, c_summary, c_body = match content with
      | GUI_deserialize.Internal (c_id, c_title, c_summary, c_body) ->
        c_id, c_title, c_summary, c_body
      | GUI_deserialize.External (c_id, c_title, c_summary, c_body) ->
        failwith "Not allow on External content"
    in
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
    let summary_elt =
      D.raw_input ~a:[a_class ["summary_update"]]
        ~input_type:`Text ~name:"summary" ~value:c_summary ()
    in
    let body_elt =
        D.raw_textarea ~a:[a_class ["body_update"]]
          ~name:"body" ~value:c_body ()
    in
    ignore {unit{ GUI_client_core.bind_cancel_update_content %cancelb %c_id}};
    ignore {unit{ GUI_client_core.bind_save_update_content %saveb %c_id
                  %title_elt %summary_elt %body_elt %tags_inputs %links_inputs
                  %tags_input_list}};
    ignore {unit{ GUI_client_core.bind_add_tag_content %submit_tag
                  %div_tags_html %add_tag_input %tags_input_list}};
    Eliom_tools.F.html
      ~title:"Pumgrana"
      ~css:[["css";"pumgrana.css"]]
      Html5.F.(body [
        header_elt;
        div ~a:[a_class["detail"]]
          [title_elt; br (); summary_elt; br (); body_elt;
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
  | e ->
    let err_str = Printexc.to_string e in
    print_endline err_str;
    empty_html ~msg:err_str ()

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
    let summary_elt =
      D.raw_input ~a:[a_class ["summary_update"]]
        ~input_type:`Text ~name:"summary" ()
    in
    let body_elt =
      D.raw_textarea ~a:[a_class ["body_update"]]
        ~name:"body" ()
    in
    ignore {unit{ GUI_client_core.bind_back %cancelb}};
    ignore {unit{ GUI_client_core.bind_add_tag_content %submit_tag
                    %div_tags_html %add_tag_input %tags_input_list}};
    ignore {unit{ GUI_client_core.bind_save_insert_content %saveb
                    %title_elt %summary_elt %body_elt %tags_input_list}};
    Eliom_tools.F.html
      ~title:"Pumgrana"
      ~css:[["css";"pumgrana.css"]]
      Html5.F.(body [
        header_elt;
        div ~a:[a_class["detail"]]
          [title_elt; br (); summary_elt; br (); body_elt;
           div ~a:[a_class["detail_tags"]]
             [h5 [pcdata "Tags"];
              span [pcdata "Select to unadd"];
              div_tags_html;
              div add_tag_html]]])
  with
  | e ->
    let err_msg = Printexc.to_string e in
    print_endline err_msg;
    empty_html ~msg:err_msg ()

(** Insert link html service *)
let link_insert opt_origin_uri opt_target_uri =
  try
    Eliom_tools.F.html
      ~title:"Pumgrana"
      ~css:[["css";"pumgrana.css"]]
      Html5.F.(body [])
  with
  | e ->
    let err_msg = Printexc.to_string e in
    print_endline err_msg;
    empty_html ~msg:err_msg ()

(** Update link html service *)
let link_update (link_uri, origin_uri, target_uri, tags) =
  try
    Eliom_tools.F.html
      ~title:"Pumgrana"
      ~css:[["css";"pumgrana.css"]]
      Html5.F.(body [])
  with
  | e ->
    let err_msg = Printexc.to_string e in
    print_endline err_msg;
    empty_html ~msg:err_msg ()
