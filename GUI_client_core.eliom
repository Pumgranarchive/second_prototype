(*
  GUI Client core
  This module manage the cliento process
*)

{shared{

open Eliom_lib
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

module Yj = Yojson.Safe

}}

{client{

(** Call back function from browser  *)
let go_back () =
  Dom_html.window##history##back()

(** Call back function from browser  *)
let go_forward () =
  Dom_html.window##history##forward()

(** [bind_button button_elt func] bind the button
    on click event to call func each time. *)
let bind_button button_elt func =
  let dom_button = To_dom.of_input button_elt in
  Lwt.async (fun () -> Lwt_js_events.clicks dom_button
    (fun _ _ -> Lwt.return (func ())))

(** [bind_back button_elt] bind the button
    on click event to call go_back each time. *)
let bind_back back_button =
  bind_button back_button go_back

(** [bind_forward button_elt] bind the button
    on click event to call go_forward each time. *)
let bind_forward forward_button =
  bind_button forward_button go_forward

(** [bind_delete_content button_elt content_id] bind the button
    on click event to remove the content with the given content_id. *)
let bind_delete_content button_elt content_id =
  let action () =
    lwt _ = Eliom_client.call_service
    ~service:%API_services.delete_contents () [content_id] in
    Lwt.return (go_back ())
  in
  let dom_button = To_dom.of_input button_elt in
  Lwt.async (fun () -> Lwt_js_events.clicks dom_button
    (fun _ _ -> action ()))

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

(** Get all name of checked dom_inputs and return them in a list. *)
let get_checked_tags dom_inputs =
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

(** Manage html list refreshing by getting data from API's serice. *)
let handle_refresh_list html_elt submit_elt div_of_yojson fun_request =
  let dom_html = To_dom.of_div html_elt in
  let dom_submit = To_dom.of_input submit_elt in
  let refresh_html () =
    let display str_results =
      let div_res_list = div_of_yojson (Yj.from_string str_results) in
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
  let dom_inputs = List.map To_dom.of_input inputs_elt in
  handle_refresh_list html_elt submit_elt
    (fun r -> GUI_tools.build_links_list
      (GUI_core.unformat_service_return
         GUI_core.unformat_list_link r))
    (fun () -> Eliom_client.call_service
      ~service:%API_services.get_links_from_content_tags
      (content_id, get_checked_tags dom_inputs) ())

(** Manage content's refreshing by getting data from API's serice. *)
let handle_refresh_contents html_elt inputs_elt submit_elt =
  let dom_inputs = List.map To_dom.of_input inputs_elt in
  handle_refresh_list html_elt submit_elt
    (fun r -> GUI_tools.build_contents_list
      (GUI_core.unformat_service_return
         GUI_core.unformat_list_content r))
    (fun () -> Eliom_client.call_service
      ~service:%API_services.get_contents
      (None, Some (get_checked_tags dom_inputs)) ())

}}
