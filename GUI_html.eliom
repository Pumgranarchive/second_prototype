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
  let rec aux (title, _, id) =
    div [a ~service:%GUI_services.content_detail_service
            [h5 [pcdata title]] id]
  in
  List.map aux links

}}

{client{

let rec remove_all_child dom =
  let c = dom##firstChild in
  Js.Opt.iter c
    (fun c ->
      Dom.removeChild dom c;
      remove_all_child dom)

let rec append_all dom = function
  | []          -> ()
  | block::tail ->
    let dom_block = To_dom.of_div block in
    Dom.appendChild dom dom_block; append_all dom tail

(** Manage link's refreshing by getting data from API's serice. *)
let handle_refresh_links content_id links_html links_tags_inputs submit =
  let dom_links = To_dom.of_div links_html in
  let dom_links_tags_inputs = List.map To_dom.of_input links_tags_inputs in
  let dom_submit = To_dom.of_input submit in
  let get_checked_tags () =
    let rec aux n = function
      | []      -> n
      | e::t    ->
        let new_n =
          if e##checked == Js._true
          then (Js.to_string e##name)::n
          else n
        in
        aux new_n t
    in aux [] dom_links_tags_inputs
  in
  let refresh_links_html () =
    let display_links links_json =
      let yojson_links = Yj.from_string links_json in
      let links = GUI_core.unformat_service_return
        GUI_core.unformat_list_link yojson_links
      in
      let div_links = build_links_list links in
      remove_all_child dom_links;
      append_all dom_links div_links
    in
    let lwt_links_json = Eliom_client.call_service
      ~service:%API_services.get_links_from_content_tags
      (content_id, get_checked_tags ()) ()
    in
    lwt links_json = lwt_links_json in
    Lwt.return (display_links links_json)
  in
  Lwt.async (fun () -> Lwt_js_events.clicks dom_submit (fun _ _ ->
    refresh_links_html ()))

}}

(** Display the home html service *)
let home_html contents_and_tags =
  let (contents, tags) = contents_and_tags in
  let content_html_list =
    List.map (fun (title, text, id) ->
      li [a ~service:GUI_services.content_detail_service [pcdata title] id;
         br ();
         span [pcdata text];
         br ()])
      contents
  in
  Eliom_tools.F.html
    ~title:"Pumgrana"
    ~css:[["css";"pumgrana.css"]]
    Html5.F.(body [
      h2 [pcdata "Pumgrana"];
      p [pcdata "Content list"];
      ul content_html_list;
      p [pcdata "Tags list"];
      post_form
        ~service:API_services.insert_content
        (fun (title, (text, tas_subject)) ->
          [fieldset
              [string_input ~input_type:`Hidden
                  ~name:title ~value:"title" ();
               string_input ~input_type:`Hidden
                 ~name:text ~value:"text" ();
               string_input ~input_type:`Submit
                 ~value:"Sub" ()]]) ()
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
        h3 [pcdata "Content detail"];
        h4 [pcdata c_title];
        p [pcdata c_text];
        h5 [pcdata "Tags"];
        div tags_subjects;
        h5 [pcdata "Links"];
        links_html;
        h5 [pcdata "Link Tags"];
        div links_tags_html;
      ])
  with
  | e -> print_endline (Printexc.to_string e);
    Eliom_tools.F.html
      ~title:"Pumgrana"
      ~css:[["css";"pumgrana.css"]]
      Html5.F.(body [])
