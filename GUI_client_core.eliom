(*
  GUI Client core
  This module manage the cliento process
*)

{client{

open Eliom_lib
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

module Yojson = Yojson.Basic

open Pjson
open Pdeserialize
open GUI_deserialize

(** Get all name of checked dom_inputs and return them in a list. *)
let get_checked_inputs dom_inputs =
  let rec aux n = function
    | []      -> n
    | e::t    ->
      let new_n =
        if e##checked == Js._true
        then (Js.to_string e##name)::n
        else n
      in
      aux new_n t
  in aux [] dom_inputs

(** Get all name of no checked dom_inputs and return them in a list. *)
let get_no_checked_inputs dom_inputs =
  let rec aux n = function
    | []      -> n
    | e::t    ->
      let new_n =
        if e##checked == Js._false
        then (Js.to_string e##name)::n
        else n
      in
      aux new_n t
  in aux [] dom_inputs

(** Call back function from browser  *)
let go_back () =
  Dom_html.window##history##back()

(** Call back function from browser  *)
let go_forward () =
  Dom_html.window##history##forward()

let go_update_content id =
  let str_id = GUI_deserialize.string_of_id id in
  Eliom_client.change_page
    ~service:%GUI_services.content_update_service str_id ()

let go_insert_content () =
  Eliom_client.change_page
    ~service:%GUI_services.content_insert_service () ()

let cancel_update_content id =
  let str_id = GUI_deserialize.string_of_id id in
  Eliom_client.change_page
    ~service:%GUI_services.content_detail_service str_id ()

let rec get_uri_of name = function
  | []                -> raise Not_found
  | (uri, sub)::t     ->
    if String.compare sub name == 0
    then Rdf_store.string_of_uri uri
    else get_uri_of name t

let rec build_tags_list all_tags nl not_found_list = function
  | []                -> nl, not_found_list
  | subject::t        ->
    let new_list, nf_list =
      try (get_uri_of subject all_tags)::nl, not_found_list
      with | Not_found -> nl, subject::not_found_list
    in
    build_tags_list all_tags new_list nf_list t

let save_update_content id title summary body tags remove_links new_tags =
  let uri = GUI_deserialize.uri_of_id id in
  let str_id = GUI_deserialize.string_of_id id in
  lwt res = Eliom_client.call_service ~service:%API_services.get_tags_by_type
    %API_conf.content_tag ()
  in
  let all_tags = get_service_return get_tag_list (Yojson.from_string res) in
  let new_tags_list, not_found_list = build_tags_list all_tags tags [] new_tags in
  lwt _ = Eliom_client.call_service ~service:%API_services.update_content ()
      (uri, (Some title, (Some summary, (Some body, Some new_tags_list))))
  in
  lwt res = Eliom_client.call_service ~service:%API_services.insert_tags ()
      (%API_conf.content_tag, (Some uri, not_found_list))
  in
  lwt _ = Eliom_client.call_service ~service:%API_services.delete_links
      () remove_links
  in
  Eliom_client.change_page ~service:%GUI_services.content_detail_service str_id ()

let save_insert_content title summary body new_tags =
  lwt res = Eliom_client.call_service ~service:%API_services.get_tags_by_type
    %API_conf.content_tag ()
  in
  let all_tags = get_service_return get_tag_list (Yojson.from_string res) in
  let tags_list, not_found_list = build_tags_list all_tags [] [] new_tags in
  lwt res = Eliom_client.call_service ~service:%API_services.insert_content ()
      (title, (summary, (body, Some tags_list)))
  in
  let uri = get_content_uri_return (Yojson.from_string res) in
  let str_uri = Rdf_store.string_of_uri uri in
  let id = Nosql_store.string_of_id Rdf_store.(content_id_of_uri uri) in
  lwt _ = Eliom_client.call_service ~service:%API_services.insert_tags ()
      (%API_conf.content_tag, (Some str_uri, not_found_list))
  in
  Eliom_client.change_page
    ~service:%GUI_services.content_detail_service id ()

let submit_tag_content dom_tags_html tag input_list =
  let input = D.raw_input ~input_type:`Checkbox ~name:tag () in
  input_list := input::!input_list;
  let elt = div [input; pcdata tag] in
  let dom_elt = To_dom.of_div elt in
  Dom.appendChild dom_tags_html dom_elt

(** [bind_button button_elt func] bind the button
    on click event to call func each time. *)
let bind_button button_elt func =
  let dom_button = To_dom.of_input button_elt in
  Lwt.async (fun () -> Lwt_js_events.clicks dom_button
    (fun _ _ -> func ()))

(** [bind_back button_elt] bind the button
    on click event to call go_back each time. *)
let bind_back back_button =
  bind_button back_button (fun () -> Lwt.return (go_back ()))

(** [bind_forward button_elt] bind the button
    on click event to call go_forward each time. *)
let bind_forward forward_button =
  bind_button forward_button (fun () -> Lwt.return (go_forward ()))

(** [bind_update button_elt] bind the button
    on click event to call go_update_content each time. *)
let bind_update_content update_button content_id =
  bind_button update_button (fun () -> go_update_content content_id)

(** [bind_insert_content button_elt] bind the button
    on click event to call go_insert_content each time. *)
let bind_insert_content button_elt =
  bind_button button_elt (fun () -> go_insert_content ())

(** [bind_save_update button_elt] bind the button
    on click event to call save_update_content each time. *)
let bind_save_update_content save_update_button content_id
    title_elt summary_elt body_elt tags_inputs links_inputs tags_input_list =
  let dom_title = To_dom.of_input title_elt in
  let dom_summary = To_dom.of_input summary_elt in
  let dom_body = To_dom.of_textarea body_elt in
  let dom_tagsi = List.map To_dom.of_input tags_inputs in
  let dom_linksi = List.map To_dom.of_input links_inputs in
  bind_button save_update_button
    (fun () ->
      let title = Js.to_string dom_title##value in
      let summary = Js.to_string dom_summary##value in
      let body = Js.to_string dom_body##value in
      let tags_list = get_no_checked_inputs dom_tagsi in
      let removelist_links = get_checked_inputs dom_linksi in
      let dom_new_tagsi = List.map To_dom.of_input !tags_input_list in
      let newlist_tags = get_no_checked_inputs dom_new_tagsi in
      save_update_content content_id title summary body tags_list
        removelist_links newlist_tags)

(** [bind_cancel_update button_elt] bind the button
    on click event to call cancel_update_content each time. *)
let bind_cancel_update_content cancel_update_button content_id =
  bind_button cancel_update_button
    (fun () -> cancel_update_content content_id)

(** [bind_add_tag button_elt] bind the button
    on click event to call go_update_content each time. *)
let bind_add_tag_content submit_tag div_tags_html add_tag_input input_list =
  let dom_tags = To_dom.of_div div_tags_html in
  let dom_input = To_dom.of_input add_tag_input in
  bind_button submit_tag (fun () ->
    let value = Js.to_string dom_input##value in
    dom_input##value <- Js.string "";
    Lwt.return (submit_tag_content dom_tags value input_list))

(** [bind_delete_content button_elt content_id] bind the button
    on click event to remove the content with the given content_id. *)
let bind_delete_content button_elt content_id =
  let uri = GUI_deserialize.uri_of_id content_id in
  let action () =
    lwt _ = Eliom_client.call_service
    ~service:%API_services.delete_contents () [uri] in
    Lwt.return (go_back ())
  in
  bind_button button_elt action

(** [bind_save_insert button_elt] bind the button
    on click event to call save_update_content each time. *)
let bind_save_insert_content save_insert_button title_elt
    summary_elt body_elt tags_input_list =
  let dom_title = To_dom.of_input title_elt in
  let dom_summary = To_dom.of_input summary_elt in
  let dom_body = To_dom.of_textarea body_elt in
  bind_button save_insert_button
    (fun () ->
      let title = Js.to_string dom_title##value in
      let summary = Js.to_string dom_summary##value in
      let body = Js.to_string dom_body##value in
      let dom_new_tagsi = List.map To_dom.of_input !tags_input_list in
      let newlist_tags = get_no_checked_inputs dom_new_tagsi in
      save_insert_content title summary body newlist_tags)

(** Remove all child from the given dom element.  *)
let rec remove_all_child dom =
  let c = dom##firstChild in
  Js.Opt.iter c
    (fun c ->
      Dom.removeChild dom c;
      remove_all_child dom)

(** Append all div element of given list in the dom element. *)
let rec append_all dom = function
  | []          -> ()
  | block::tail ->
    let dom_block = To_dom.of_div block in
    Dom.appendChild dom dom_block; append_all dom tail

(** Manage html list refreshing by getting data from API's serice. *)
let handle_refresh_list html_elt submit_elt div_of_yojson fun_request =
  let dom_html = To_dom.of_div html_elt in
  let dom_submit = To_dom.of_input submit_elt in
  let refresh_html () =
    let display str_results =
      let div_res_list = div_of_yojson (Yojson.from_string str_results) in
      remove_all_child dom_html;
      append_all dom_html div_res_list
    in
    lwt str_results = fun_request () in
    Lwt.return (display str_results)
  in
  Lwt.async (fun () -> Lwt_js_events.clicks dom_submit
    (fun _ _ -> refresh_html ()))

(** Manage link's refreshing by getting data from API's serice. *)
let handle_refresh_links content_id html_elt inputs_elt submit_elt =
  let uri = GUI_deserialize.uri_of_id content_id in
  let dom_inputs = List.map To_dom.of_input inputs_elt in
  handle_refresh_list html_elt submit_elt
    (fun r -> GUI_tools.build_links_list
      (get_service_return get_link_list r))
    (fun () ->
      let list = get_checked_inputs dom_inputs in
      let encoded_uri = Rdf_store.uri_encode uri in
      let encoded_list = List.map Rdf_store.uri_encode list in
      Eliom_client.call_service
        ~service:%API_services.get_links_from_content_tags
        (encoded_uri, Some encoded_list) ())

(** Manage content's refreshing by getting data from API's serice. *)
let handle_refresh_contents html_elt inputs_elt submit_elt =
  let dom_inputs = List.map To_dom.of_input inputs_elt in
  handle_refresh_list html_elt submit_elt
    (fun r -> GUI_tools.build_contents_list
      (get_service_return get_short_content_list r))
    (fun () ->
      let list = get_checked_inputs dom_inputs in
      let encoded_list = List.map Rdf_store.uri_encode list in
      Eliom_client.call_service
        ~service:%API_services.get_contents
        (None, Some encoded_list) ())

}}
