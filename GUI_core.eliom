(*
  GUI Core
  This module compute input from GUI services to get results to display
  it GUI html
*)

let failure_string = "Invalide content format"

let get_contents filter tags_id =
  []

let unformat_service_return func = function
  | `Assoc [(_, _); (_, data)]  -> func data
  | _                           -> failwith failure_string

let unformat_content = function
  | `Assoc [(_, `String text);
            (_, `String title);
            (_, `String id)]    -> text, title, id
  | _                           -> failwith failure_string

let unformat_tag = function
  | `Assoc [(_, `String subject);
            (_, `String id)]    -> subject, id
  | _                           -> failwith failure_string

let unformat_list func = function
  | `List l                     -> List.map func l
  | _                           -> failwith failure_string

let unformat_list_tag = unformat_list unformat_tag

let get_detail_content content_id =
  let content = API_core.get_detail content_id in
  let title, text, id = unformat_service_return unformat_content content in
  let content_tags = API_core.get_tags_from_content content_id in
  let tags_id = unformat_service_return unformat_list_tag content_tags in
  (title, text, id), tags_id
