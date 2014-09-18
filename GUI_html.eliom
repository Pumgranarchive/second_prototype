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
  let tags_ul = GUI_tools.build_tags_ul tags in
  let contents_html =
    div ~a:[a_id "content"]
      [div ~a:[a_class["content_main_list"]]
          (GUI_tools.build_contents_list contents)]
  in
  let side_button = div ~a:[a_class["side_button";"side_button_home";"side_button_img_home"]] [div ~a:[a_class["side_button_circle"]] []] in
  let side_action = div ~a:[a_class["side_button_bottom"]]
    [ul ~a:[a_class["side_button_bottom_list"]]
        [li ~a:[a_class["side_button";"side_button_img_plus"]]
            [div ~a:[a_class["side_button_circle"]] []]]]
  in
  let side_arrow = img ~a:[a_class["side_arrow"]]
    ~alt:("")
    ~src:(make_uri ~service:(Eliom_service.static_dir ())
            ["images";"arrow_side_bar.png"]) ()
  in
  let side_bar = div ~a:[a_id "sidebar"] [side_arrow; side_button; br (); tags_ul; side_action] in
  let main_logo = div ~a:[a_id "main_logo"] [] in
  let container = div ~a:[a_id "container"] [side_bar; contents_html; main_logo] in
  (* ignore {unit{ GUI_client_core.bind_back *)
  (*               (%backb:[Html5_types.input] Eliom_content.Html5.elt) }}; *)
  (* ignore {unit{ GUI_client_core.bind_forward *)
  (*               (%forwardb:[Html5_types.input] Eliom_content.Html5.elt) }}; *)
  (* ignore {unit{ GUI_client_core.bind_insert_content *)
  (*               (%insertb:[Html5_types.input] Eliom_content.Html5.elt) }}; *)
  (* ignore {unit{ GUI_client_core.handle_refresh_contents *)
  (*               (%contents_html:[Html5_types.div] Eliom_content.Html5.elt) *)
  (*               (%tags_inputs:[Html5_types.input] Eliom_content.Html5.elt list) *)
  (*               (%submit:[Html5_types.input] Eliom_content.Html5.elt)}}; *)
  Eliom_tools.F.html
    ~title:"Pumgrana"
    ~css:[["css";"style.css"]]
    Html5.F.(body [container])

(** Display the content detail html service *)
let content_detail (content, tags_id, links, tags_link) =
  try
    let tags_ul = GUI_tools.build_tags_ul tags_id in
    let link_list = GUI_tools.build_links_list links in
    (* let submit, links_tags_inputs, links_tags_html = *)
    (*   GUI_tools.build_tags_form tags_link *)
    (* in *)
    let c_id, content_elt = match content with
      | GUI_deserialize.Internal (c_id, c_title, c_summary, c_body) ->
        c_id, div [h3 [pcdata c_title]; p [pcdata c_summary]; p [pcdata c_body]]
      | GUI_deserialize.External (c_id, c_title, c_summary, c_html_body) ->
        let id = (GUI_deserialize.string_of_id c_id) in
        let regexp = Str.regexp ".*youtube.*" in
        (* let iframe_bool = Str.string_match regexp id 0 in *)
        (* if iframe_bool *)
        (* then  *)c_id,
        div ~a:[a_class["content_current"]]
          [div [h3 [pcdata c_title]; F.Unsafe.data c_html_body]]
        (* else *)
        (*   c_id, iframe ~a:[a_class ["pum_iframe"]; *)
        (*                    a_src (Eliom_content.Xml.uri_of_string id)] [] *)
    in
    let side_button = div ~a:[a_class["side_button";"side_button_home";"side_button_img_content"]] [div ~a:[a_class["side_button_circle"]] []] in
    let side_action = div ~a:[a_class["side_button_bottom"]]
      [ul ~a:[a_class["side_button_bottom_list"]]
          [li ~a:[a_class["side_button";"side_button_img_link"]]
              [div ~a:[a_class["side_button_circle"]] []];
           li ~a:[a_class["side_button";"side_button_img_home"]]
             [a ~service:GUI_services.home_service_without
                 [div ~a:[a_class["side_button_circle"]] []] None];
           li ~a:[a_class["side_button";"side_button_img_plus"]]
              [div ~a:[a_class["side_button_circle"]] []]]]
    in
    let side_button_add = div ~a:[a_class["side_button_add"]] [pcdata "Add a tag"] in
    let side_arrow = img ~a:[a_class["side_arrow"]]
      ~alt:("")
      ~src:(make_uri ~service:(Eliom_service.static_dir ())
              ["images";"arrow_side_bar.png"]) ()
    in
    let side_bar = div ~a:[a_id "sidebar"] [side_arrow; side_button; tags_ul; side_button_add; side_action] in
    let main_logo = div ~a:[a_id "main_logo"] [] in
    let linked_arrow = img ~a:[a_class["link_arrow"]]
      ~alt:("")
      ~src:(make_uri ~service:(Eliom_service.static_dir ())
              ["images";"linked_content_arrow.png"]) ()
    in
    let link_bar = div ~a:[a_class["content_current_linked"]]
      [linked_arrow; div ~a:[a_class["content_main_list"]] link_list]
    in
    let content = div ~a:[a_id "content"] [content_elt; link_bar; main_logo] in
    let container = div ~a:[a_id "container"] [side_bar; content] in

    (* ignore {unit{ GUI_client_core.bind_back *)
    (*               (%backb:[Html5_types.input] Eliom_content.Html5.elt) }}; *)
    (* ignore {unit{ GUI_client_core.bind_forward *)
    (*               (%forwardb:[Html5_types.input] Eliom_content.Html5.elt) }}; *)
    (* ignore {unit{ GUI_client_core.bind_update_content *)
    (*               (%updateb:[Html5_types.input] Eliom_content.Html5.elt) *)
    (*               (%c_id:GUI_deserialize.id) }}; *)
    (* ignore {unit{ GUI_client_core.bind_delete_content *)
    (*               (%deleteb:[Html5_types.input] Eliom_content.Html5.elt) *)
    (*               (%c_id:GUI_deserialize.id) }}; *)
    (* ignore {unit{ GUI_client_core.handle_refresh_links *)
    (*               (%c_id:GUI_deserialize.id) *)
    (*               (%links_html:[Html5_types.div] Eliom_content.Html5.elt) *)
    (*               (%links_tags_inputs: *)
    (*                   [Html5_types.input] Eliom_content.Html5.elt list) *)
    (*               (%submit:[Html5_types.input] Eliom_content.Html5.elt) }}; *)

    Eliom_tools.F.html
      ~title:"Pumgrana"
      ~js:[["js";"jquery-2.1.1.min.js"]]
      ~css:[["css";"style.css"]]
      Html5.F.(body [container])
      (*   div ~a:[a_class["detail"]] *)
      (*     [content_elt; *)
      (*      div ~a:[a_class["detail_tags"]] *)
      (*        [h5 [pcdata "Tags"]; *)
      (*         div tags_subjects]]; *)
      (*   div ~a:[a_class["links"]] *)
      (*     [h4 [pcdata "Links"]; *)
      (*      links_html; *)
      (*      div ~a:[a_class["links_tags"]] *)
      (*        [h5 [pcdata "Link Tags"]; *)
      (*         div links_tags_html]] *)
      (* ]) *)
  with
  | e ->
    let err_msg = Printexc.to_string e in
    print_endline err_msg;
    empty_html ~msg:err_msg ()

(** Update content detail html service *)
let content_update (content, tags, links, tags_link) =
  try
    let c_id, content_elt, content_html = match content with
      | GUI_deserialize.Internal (c_id, c_title, c_summary, c_body) ->
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
        c_id, (Some title_elt, Some summary_elt, Some body_elt),
        span [title_elt; br (); summary_elt; br (); body_elt]
      | GUI_deserialize.External (c_id, c_title, c_summary, c_html_body) ->
        let id = (GUI_deserialize.string_of_id c_id) in
        let regexp = Str.regexp ".*youtube.*" in
        let iframe_bool = Str.string_match regexp id 0 in
        if iframe_bool
        then c_id, (None, None, None),
          div [h3 [pcdata c_title]; F.Unsafe.data c_html_body]
        else
          c_id, (None, None, None),
          iframe ~a:[a_class ["pum_iframe"];
                     a_src (Eliom_content.Xml.uri_of_string id)] []
    in
    let links_inputs, links_html = GUI_tools.build_ck_links_list links in
    let tags_inputs, tags_html = GUI_tools.build_ck_tags_list tags in
    let add_tag_input, submit_tag, tags_input_list, add_tag_html =
      GUI_tools.build_add_tag ()
    in
    let cancelb, saveb, header_elt = GUI_tools.build_update_content_header () in
    let links_tags_html = GUI_tools.build_tags_list tags_link in
    let div_tags_html = D.div tags_html in
    let link_insert_elt = GUI_tools.build_link_header () in

    ignore {unit{ GUI_client_core.bind_save_update_content
                  (%saveb:[Html5_types.input] Eliom_content.Html5.elt)
                  (%c_id:GUI_deserialize.id)
                  (%content_elt:
                      [Html5_types.input ] Eliom_content.Html5.D.elt option *
                      [Html5_types.input ] Eliom_content.Html5.D.elt option *
                      [Html5_types.textarea ] Eliom_content.Html5.D.elt option)
                  (%tags_inputs:[Html5_types.input] Eliom_content.Html5.elt list)
                  (%links_inputs:[Html5_types.input] Eliom_content.Html5.elt list)
                  %tags_input_list
                }};

    ignore {unit{ GUI_client_core.bind_cancel_update_content
                  (%cancelb:[Html5_types.input] Eliom_content.Html5.elt)
                  (%c_id:GUI_deserialize.id)}};
    ignore {unit{ GUI_client_core.bind_add_tag_content
                  (%submit_tag:[Html5_types.input] Eliom_content.Html5.elt)

                  (%div_tags_html:[Html5_types.div] Eliom_content.Html5.elt)
                  (%add_tag_input:[Html5_types.input] Eliom_content.Html5.elt)
                  %tags_input_list}};
    ignore {unit{ GUI_client_core.bind_insert_link
                  (%link_insert_elt:[Html5_types.input] Eliom_content.Html5.elt)
                  (Some (%c_id:GUI_deserialize.id))}};

    Eliom_tools.F.html
      ~title:"Pumgrana"
      ~css:[["css";"pumgrana.css"]]
      Html5.F.(body [
        header_elt;
        div ~a:[a_class["detail"]]
          [content_html;
           div ~a:[a_class["detail_tags"]]
             [h5 [pcdata "Tags"];
              span [pcdata "Select to remove"];
              div_tags_html;
              div add_tag_html]];
        div ~a:[a_class["links"]]
          [h4 [pcdata "Links"];
           link_insert_elt; br ();
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
    let link_insert_elt = GUI_tools.build_link_header () in

    ignore {unit{ GUI_client_core.bind_back
                  (%cancelb:[Html5_types.input] Eliom_content.Html5.elt)}};
    ignore {unit{ GUI_client_core.bind_add_tag_content
                  (%submit_tag:[Html5_types.input] Eliom_content.Html5.elt)
                  %div_tags_html
                  (%add_tag_input:[Html5_types.input] Eliom_content.Html5.elt)
                  %tags_input_list}};
    ignore {unit{ GUI_client_core.bind_save_insert_content
                  (%saveb:[Html5_types.input] Eliom_content.Html5.elt)

                  (%title_elt:[Html5_types.input] Eliom_content.Html5.elt)
                  (%summary_elt:[Html5_types.input] Eliom_content.Html5.elt)
                  %body_elt
                  %tags_input_list}};
    ignore {unit{ GUI_client_core.bind_insert_link
                  (%link_insert_elt:[Html5_types.input] Eliom_content.Html5.elt) None}};

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
              div add_tag_html]];
        div ~a:[a_class["links"]]
          [h4 [pcdata "Links"];
           link_insert_elt]
      ])
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
