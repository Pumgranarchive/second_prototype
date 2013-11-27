(*
  GUI Html
  This module make the html of GUI services.
*)

open Eliom_lib
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

let build_tags_form tags =
  let tags_html = List.flatten
    (List.map (fun (subject, id) ->
      [raw_input ~input_type:`Checkbox ~name:id ();
       pcdata subject;
       br ()])
       tags)
  in
  tags_html

let main_html contents =
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

let content_detail ((c_title, c_text, c_id), tags_id, links, tags_link) =
  try
    let aux (subject, id) =  pcdata (subject ^ " ") in
    let tags_subjects = List.map aux tags_id in
    let links_tags_html = build_tags_form tags_link in
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
        h5 [pcdata "Link Tags"];
        p links_tags_html;
      ])
  with
  | e -> print_endline (Printexc.to_string e);
    Eliom_tools.F.html
      ~title:"Pumgrana"
      ~css:[["css";"pumgrana.css"]]
      Html5.F.(body [])
