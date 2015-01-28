(*
  GUI Html
  This module make the html of GUI services.
*)

{shared{

open Utils
open Eliom_lib
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

module Yojson = Yojson.Basic

}}

(******************************************************************************
********************************* Modules *************************************
*******************************************************************************)

module Msg =
struct
  let make m = Lwt.return (div m)
end

module HomeButton =
struct
  let make () =
    let text = "Back to the Pumgrana Home" in
    let button = a ~service:GUI_services.home [div [pcdata text]] () in
    let wrapper = div [button] in
    Lwt.return wrapper
end

(******************************************************************************
********************************** Pages **************************************
*******************************************************************************)

(** Display the 404 error as html service *)
let error_404 () () =
  let main_logo = MainLogo.make () in
  let msg = Msg.make [div ~a:[a_class["middle_404"]] [
  div ~a:[a_class["container404"]] [
  div ~a:[a_class["smiley404"]] [pcdata ": /"];
  div ~a:[a_class["sep404"]] [];
  p ~a:[a_class["code404"]] [pcdata "404!"];
  p ~a:[a_class["text404"]]
  [pcdata "Sorry, this page does not exist anymore :("]]]]
  in
  let button = HomeButton.make () in
  let container = Container.make [msg; button; main_logo] in
  GUI_tools.make_html [container]

(** Display the home html service *)
let home () () =
  let middle_search = MiddleSearch.make () in
  let main_logo = MainLogo.make () in
  let container = Container.make [middle_search; main_logo] in
  GUI_tools.make_html [container]

(** Display the home html service *)
let contents (opt_filter, opt_research) () =
  let research = Opt.get_not_null "" opt_research in
  let content_list = ContentList.make opt_filter research in
  let mode = `Contents (content_list, opt_filter, research) in
  let add_content = AddContent.make mode in
  let side_bar = SideBar.make mode add_content in
  let main_logo = MainLogo.make () in
  let content = Content.make [content_list] in
  let html_add_content = AddContent.to_html add_content in
  let container = Container.make [side_bar; content; main_logo; html_add_content] in
  GUI_tools.make_html [container]

(** Display the content detail html service *)
let content_detail content_uri () =
  let mode = `Detail content_uri in
  lwt title, content_elt = Content.get_data content_uri in
  let add_content = AddContent.make mode in
  let side_bar = SideBar.make mode add_content in
  let link_bar = LinkBar.make content_uri in
  let main_logo = MainLogo.make () in
  let content = Content.make [content_elt; link_bar; main_logo] in
  let html_add_content = AddContent.to_html add_content in
  let container = Container.make [side_bar; content; html_add_content] in
  GUI_tools.make_html ~title [container]



(* (\** Update content detail html service *\) *)
(* let content_update (content, tags, links, tags_link) = *)
(*   try *)
(*     let c_id, content_elt, content_html = match content with *)
(*       | GUI_deserialize.Internal (c_id, c_title, c_summary, c_body) -> *)
(*         let title_elt = *)
(*           D.raw_input ~a:[a_class ["title_update"]] *)
(*             ~input_type:`Text ~name:"title" ~value:c_title () *)
(*         in *)
(*         let summary_elt = *)
(*           D.raw_input ~a:[a_class ["summary_update"]] *)
(*             ~input_type:`Text ~name:"summary" ~value:c_summary () *)
(*         in *)
(*         let body_elt = *)
(*           D.raw_textarea ~a:[a_class ["body_update"]] *)
(*             ~name:"body" ~value:c_body () *)
(*         in *)
(*         c_id, (Some title_elt, Some summary_elt, Some body_elt), *)
(*         span [title_elt; br (); summary_elt; br (); body_elt] *)
(*       | GUI_deserialize.External (c_id, c_title, c_summary, c_html_body) -> *)
(*         let id = (GUI_deserialize.string_of_id c_id) in *)
(*         let regexp = Str.regexp ".*youtube.*" in *)
(*         let iframe_bool = Str.string_match regexp id 0 in *)
(*         if iframe_bool *)
(*         then c_id, (None, None, None), *)
(*           div [h3 [pcdata c_title]; F.Unsafe.data c_html_body] *)
(*         else *)
(*           c_id, (None, None, None), *)
(*           iframe ~a:[a_class ["pum_iframe"]; *)
(*                      a_src (Eliom_content.Xml.uri_of_string id)] [] *)
(*     in *)
(*     let links_inputs, links_html = GUI_tools.build_ck_links_list links in *)
(*     let tags_inputs, tags_html = GUI_tools.build_ck_tags_list tags in *)
(*     let add_tag_input, submit_tag, tags_input_list, add_tag_html = *)
(*       GUI_tools.build_add_tag () *)
(*     in *)
(*     let cancelb, saveb, header_elt = GUI_tools.build_update_content_header () in *)
(*     let links_tags_html = GUI_tools.build_tags_list tags_link in *)
(*     let div_tags_html = D.div tags_html in *)
(*     let link_insert_elt = GUI_tools.build_link_header () in *)

(*     ignore {unit{ GUI_client_core.bind_save_update_content *)
(*                   (%saveb:[Html5_types.input] Eliom_content.Html5.elt) *)
(*                   (%c_id:GUI_deserialize.id) *)
(*                   (%content_elt: *)
(*                       [Html5_types.input ] Eliom_content.Html5.D.elt option * *)
(*                       [Html5_types.input ] Eliom_content.Html5.D.elt option * *)
(*                       [Html5_types.textarea ] Eliom_content.Html5.D.elt option) *)
(*                   (%tags_inputs:[Html5_types.input] Eliom_content.Html5.elt list) *)
(*                   (%links_inputs:[Html5_types.input] Eliom_content.Html5.elt list) *)
(*                   %tags_input_list *)
(*                 }}; *)

(*     ignore {unit{ GUI_client_core.bind_cancel_update_content *)
(*                   (%cancelb:[Html5_types.input] Eliom_content.Html5.elt) *)
(*                   (%c_id:GUI_deserialize.id)}}; *)
(*     ignore {unit{ GUI_client_core.bind_add_tag_content *)
(*                   (%submit_tag:[Html5_types.input] Eliom_content.Html5.elt) *)

(*                   (%div_tags_html:[Html5_types.div] Eliom_content.Html5.elt) *)
(*                   (%add_tag_input:[Html5_types.input] Eliom_content.Html5.elt) *)
(*                   %tags_input_list}}; *)
(*     ignore {unit{ GUI_client_core.bind_insert_link *)
(*                   (%link_insert_elt:[Html5_types.input] Eliom_content.Html5.elt) *)
(*                   (Some (%c_id:GUI_deserialize.id))}}; *)

(*     Eliom_tools.F.html *)
(*       ~title:"Pumgrana" *)
(*       ~css:[["css";"pumgrana.css"]] *)
(*       Html5.F.(body [ *)
(*         header_elt; *)
(*         div ~a:[a_class["detail"]] *)
(*           [content_html; *)
(*            div ~a:[a_class["detail_tags"]] *)
(*              [h5 [pcdata "Tags"]; *)
(*               span [pcdata "Select to remove"]; *)
(*               div_tags_html; *)
(*               div add_tag_html]]; *)
(*         div ~a:[a_class["links"]] *)
(*           [h4 [pcdata "Links"]; *)
(*            link_insert_elt; br (); *)
(*            span [pcdata "Select to remove"]; *)
(*            div links_html; *)
(*            div ~a:[a_class["links_tags"]] *)
(*              [h5 [pcdata "Link Tags"]; *)
(*               div links_tags_html]] *)
(*       ]) *)
(*   with *)
(*   | e -> *)
(*     let err_str = Printexc.to_string e in *)
(*     print_endline err_str; *)
(*     empty_html ~msg:err_str () *)

(* (\** Insert content html service *\) *)
(* let content_insert () = *)
(*   try *)
(*     let _, tags_html = GUI_tools.build_ck_tags_list [] in *)
(*     let cancelb, saveb, header_elt = GUI_tools.build_update_content_header () in *)
(*     let div_tags_html = D.div tags_html in *)
(*     let add_tag_input, submit_tag, tags_input_list, add_tag_html = *)
(*       GUI_tools.build_add_tag () *)
(*     in *)
(*     let title_elt = *)
(*       D.raw_input ~a:[a_class ["title_update"]] *)
(*         ~input_type:`Text ~name:"title" () *)
(*     in *)
(*     let summary_elt = *)
(*       D.raw_input ~a:[a_class ["summary_update"]] *)
(*         ~input_type:`Text ~name:"summary" () *)
(*     in *)
(*     let body_elt = *)
(*       D.raw_textarea ~a:[a_class ["body_update"]] *)
(*         ~name:"body" () *)
(*     in *)
(*     let link_insert_elt = GUI_tools.build_link_header () in *)

(*     ignore {unit{ GUI_client_core.bind_back *)
(*                   (%cancelb:[Html5_types.input] Eliom_content.Html5.elt)}}; *)
(*     ignore {unit{ GUI_client_core.bind_add_tag_content *)
(*                   (%submit_tag:[Html5_types.input] Eliom_content.Html5.elt) *)
(*                   %div_tags_html *)
(*                   (%add_tag_input:[Html5_types.input] Eliom_content.Html5.elt) *)
(*                   %tags_input_list}}; *)
(*     ignore {unit{ GUI_client_core.bind_save_insert_content *)
(*                   (%saveb:[Html5_types.input] Eliom_content.Html5.elt) *)

(*                   (%title_elt:[Html5_types.input] Eliom_content.Html5.elt) *)
(*                   (%summary_elt:[Html5_types.input] Eliom_content.Html5.elt) *)
(*                   %body_elt *)
(*                   %tags_input_list}}; *)
(*     ignore {unit{ GUI_client_core.bind_insert_link *)
(*                   (%link_insert_elt:[Html5_types.input] Eliom_content.Html5.elt) None}}; *)

(*     Eliom_tools.F.html *)
(*       ~title:"Pumgrana" *)
(*       ~css:[["css";"pumgrana.css"]] *)
(*       Html5.F.(body [ *)
(*         header_elt; *)
(*         div ~a:[a_class["detail"]] *)
(*           [title_elt; br (); summary_elt; br (); body_elt; *)
(*            div ~a:[a_class["detail_tags"]] *)
(*              [h5 [pcdata "Tags"]; *)
(*               span [pcdata "Select to unadd"]; *)
(*               div_tags_html; *)
(*               div add_tag_html]]; *)
(*         div ~a:[a_class["links"]] *)
(*           [h4 [pcdata "Links"]; *)
(*            link_insert_elt] *)
(*       ]) *)
(*   with *)
(*   | e -> *)
(*     let err_msg = Printexc.to_string e in *)
(*     print_endline err_msg; *)
(*     empty_html ~msg:err_msg () *)

(* (\** Insert link html service *\) *)
(* let link_insert opt_origin_uri opt_target_uri = *)
(*   try *)
(*     Eliom_tools.F.html *)
(*       ~title:"Pumgrana" *)
(*       ~css:[["css";"pumgrana.css"]] *)
(*       Html5.F.(body []) *)
(*   with *)
(*   | e -> *)
(*     let err_msg = Printexc.to_string e in *)
(*     print_endline err_msg; *)
(*     empty_html ~msg:err_msg () *)

(* (\** Update link html service *\) *)
(* let link_update (link_uri, origin_uri, target_uri, tags) = *)
(*   try *)
(*     Eliom_tools.F.html *)
(*       ~title:"Pumgrana" *)
(*       ~css:[["css";"pumgrana.css"]] *)
(*       Html5.F.(body []) *)
(*   with *)
(*   | e -> *)
(*     let err_msg = Printexc.to_string e in *)
(*     print_endline err_msg; *)
(*     empty_html ~msg:err_msg () *)
