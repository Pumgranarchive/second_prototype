(*
  GUI Core
  This module compute input from GUI services to get results to display
  it GUI html
*)

let get_contents filter tags_id =
  []

let get_detail_content content_id =
  let content = API_core.get_detail content_id in
  let title, text, id = match content with
    | `Assoc [(_, _); (_, `Assoc [(_, `String text);
                                  (_, `String title);
                                  (_, `String id)])]
                -> title, text, id
    | _         -> failwith "invalide content format"
  in
  title, text, id
