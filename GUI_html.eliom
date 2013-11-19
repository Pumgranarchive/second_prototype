(*
  GUI Html
  This module make the html of GUI services.
*)

open Eliom_lib
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

let main_html contents =
  Eliom_tools.F.html
    ~title:"Pumgrana"
    ~css:[["css";"pumgrana.css"]]
    Html5.F.(body [
      h2 [pcdata "Pumgrana"];
      p [pcdata "Content list"];
      p [pcdata "Tags list"];
      (* Test link to main service *)
      p [a ~service:GUI_services.main_service
            [pcdata "main_service"] (Some "t", None)]
    ])

let content_detail ((c_title, c_text, c_id), tags_id) =
  let aux (subject, id) =  pcdata (subject ^ " ") in
  let tags_subjects = List.map aux tags_id in
  Eliom_tools.F.html
    ~title:"Pumgrana"
    ~css:[["css";"pumgrana.css"]]
    Html5.F.(body [
      h2 [pcdata "Pumgrana"];
      h3 [pcdata "Content detail"];
      h5 [pcdata c_title];
      p [pcdata c_text];
      h4 [pcdata "Tags"];
      p tags_subjects
    ])
