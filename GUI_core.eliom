(*
  GUI Core
  This module compute input from GUI services to get results to display
  it GUI html
*)

let get_detail_content content_id =
  let content = API_core.get_detail content_id in
  let title, text, id = match content with
    | `Assoc [(_, _); (_, `Assoc [(_, `String text);
                                  (_, `String title);
                                  (_, `String id)])]
                -> title, text, id
    | _         -> failwith "invalide content format"
  in
  let content_tags = API_core.get_tags_from_content content_id in
  let tags_id = match content_tags with
    | `Assoc [(_, _); (_, `List l)]     ->
    let aux = function
      | `Assoc [(_, `String subject);
                (_, `String id)]        -> subject, id
      | _                               -> failwith "invalide tags format"
    in List.map aux l
    | _                                 -> failwith "invalide tags format"
  in
  (title, text, id), tags_id




let get_contents filter tags_id =
  let get_contents = function
    | `Assoc [(_, _); (_, `List l)] -> l
    | _         -> failwith "invalide content format"
  in
  let get_content = function
    | `Assoc [(_, `String text);
              (_, `String title);
              (_, `String id)]
                -> title, text, id
    | _         -> failwith "invalide content format"
  in
  let contents = get_contents (API_core.get_contents filter tags_id) in

  let return contents =
    let rec aux new_list = function
      | []                -> new_list
      | content::t        -> aux ((get_content content)::new_list) t
    in
    aux [] contents

  in
  return contents
