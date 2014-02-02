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
  let tags_html = List.flatten
    (List.map (fun (subject, id) ->
      [br ();
       pcdata subject;
       raw_input ~input_type:`Checkbox ~name:id ()])
       tags)
  in
  let submit = D.raw_input ~input_type:`Submit ~value:"Submit" () in
  submit, List.rev (submit::tags_html)

(** Build the links html display link list *)
let build_links_list links =
  let rec aux (title, _, id) =
    div [a ~service:%GUI_services.content_detail_service
            [h5 [pcdata title]] id]
  in
  List.map aux links

}}

{client{

(** Manage link's refreshing by getting data from API's serice. *)
let handle_refresh_links content_id links_html submit =
  let dom_links = To_dom.of_div links_html in
  let dom_submit = To_dom.of_input submit in
  let refresh_links_html () =
    let rec remove_all_child () =
      let c = dom_links##firstChild in
      Js.Opt.iter c
        (fun c ->
          Dom.removeChild dom_links c;
            remove_all_child ())
    in
    let display_links links_json =
      let yojson_links = Yj.from_string links_json in
      let links = GUI_core.unformat_service_return
        GUI_core.unformat_list_link yojson_links
      in
      let div_links = build_links_list links in
      remove_all_child ();
      let rec display = function
        | []          -> ()
        | block::tail ->
          let dom_block = To_dom.of_div block in
          Dom.appendChild dom_links dom_block; display tail
      in display div_links
    in
    let lwt_links_json = Eliom_client.call_service
      ~service:%API_services.get_links_from_content content_id () in
    lwt links_json = lwt_links_json in
    Lwt.return (display_links links_json)
  in
  Lwt.async (fun () -> Lwt_js_events.click dom_submit >>= fun _ ->
    refresh_links_html ())

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
      p [pcdata "Tags list"]
    ])

(** Display the content detail html service *)
let content_detail ((c_title, c_text, c_id), tags_id, links, tags_link) =
  try
    let aux (subject, id) =  pcdata (subject ^ " ") in
    let tags_subjects = List.map aux tags_id in
    let links_html = D.div (build_links_list links) in
    let submit, links_tags_html = build_tags_form tags_link in
    ignore {unit{ handle_refresh_links %c_id %links_html %submit }};
    Eliom_tools.F.html
      ~title:"Pumgrana"
      ~css:[["css";"pumgrana.css"]]
      Html5.F.(body [
        h2 [pcdata "Pumgrana"];
        h3 [pcdata "Content detail"];
        h4 [pcdata c_title];
        p [pcdata c_text];
        h5 [pcdata "Tags"];
        p tags_subjects;
        h5 [pcdata "Links"];
        links_html;
        h5 [pcdata "Link Tags"];
        p links_tags_html;
      ])
  with
  | e -> print_endline (Printexc.to_string e);
    Eliom_tools.F.html
      ~title:"Pumgrana"
      ~css:[["css";"pumgrana.css"]]
      Html5.F.(body [])
