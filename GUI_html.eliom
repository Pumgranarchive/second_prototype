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

{client{

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
    (fun r -> build_links_list
      (GUI_core.unformat_service_return
         GUI_core.unformat_list_link r))
    (fun () -> Eliom_client.call_service
      ~service:%API_services.get_links_from_content_tags
      (content_id, get_checked_tags dom_inputs) ())

(** Manage content's refreshing by getting data from API's serice. *)
let handle_refresh_contents html_elt inputs_elt submit_elt =
  let dom_inputs = List.map To_dom.of_input inputs_elt in
  handle_refresh_list html_elt submit_elt
    (fun r -> build_contents_list
      (GUI_core.unformat_service_return
         GUI_core.unformat_list_content r))
    (fun () -> Eliom_client.call_service
      ~service:%API_services.get_contents
      (None, Some (get_checked_tags dom_inputs)) ())

}}

(** Display the home html service *)
let home_html (contents, tags) =
  let submit, tags_inputs, tags_html_list = build_tags_form tags in
  let contents_html =
    D.div ~a:[a_class["contents"]]
    (build_contents_list contents)
  in
  let tags_html =
    div ~a:[a_class["contents_tags"]]
      ((h4 [pcdata "Tags"])::tags_html_list)
  in
  ignore {unit{ handle_refresh_contents %contents_html
                %tags_inputs %submit}};
  Eliom_tools.F.html
    ~title:"Pumgrana"
    ~css:[["css";"pumgrana.css"]]
    Html5.F.(body [
      h2 [pcdata "Pumgrana"];
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
    let links_html = D.div (build_links_list links) in
    let submit, links_tags_inputs, links_tags_html =
      build_tags_form tags_link in
    ignore {unit{ handle_refresh_links %c_id %links_html
                  %links_tags_inputs %submit }};
    Eliom_tools.F.html
      ~title:"Pumgrana"
      ~css:[["css";"pumgrana.css"]]
      Html5.F.(body [
        h2 [pcdata "Pumgrana"];
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
