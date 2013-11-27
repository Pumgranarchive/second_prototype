(*
  GUI Core
  This module compute input from GUI services to get results to display
  it GUI html
*)

module Yj = Yojson.Safe

let failure_string = "Invalide content format"

let get_contents filter tags_id =
  []

let power num pow =
  let rec aux nb = function
    | 0.                  -> 1.
    | 1.                  -> num *. nb
    | p when p < 0.       -> failwith "Invalide power"
    | p                   -> aux (num *. nb) (p -. 1.)
  in aux 1. pow

(* Convert string objectID from mongo in Hexa 12 char format
   into string in hex 24 char format *)
let string_of_id str_id =
  try
    let base = 16 in
    let length = 12 in
    let buf = Buffer.create length in
    let char_of_hex_int h =
      String.get (Printf.sprintf "%x" h) 0
    in
    let rec aux position =
      if (position >= length) then ()
      else
        begin
          let n = Char.code (String.get str_id position) in
          let n1 = n mod base in
          let n2 = (n - n1) / base in
          Buffer.add_char buf (char_of_hex_int n2);
          Buffer.add_char buf (char_of_hex_int n1);
          aux (position + 1)
        end
    in
    aux 0;
    Buffer.contents buf
  with
  | e -> print_endline (Printexc.to_string e); ""

let unformat_service_return func = function
  | `Assoc [(_, _); (_, data)]  -> func data
  | _                           -> failwith failure_string

let unformat_content = function
  | `Assoc [(_, `String text);
            (_, `String title);
            (_, `String id)]    -> title, text, string_of_id id
  | _                           -> failwith failure_string

let unformat_tag = function
  | `Assoc [(_, `String subject);
            (_, `String id)]    -> subject, string_of_id id
  | _                           -> failwith failure_string

let unformat_list (func: Yj.json -> 'a) (l: Yj.json) = match l with
  | `List l                     -> List.map func l
  | _                           -> failwith failure_string

let unformat_list_tag = unformat_list unformat_tag
let unformat_list_content = unformat_list unformat_content

let get_detail_content content_id =
  let content = API_core.get_detail content_id in
  let title, text, id = unformat_service_return unformat_content content in
  let content_tags = API_core.get_tags_from_content content_id in
  let tags_id = unformat_service_return unformat_list_tag content_tags in
  (* let links = API_core.get_links_from_content content_id in *)
  (* let link_list = unformat_service_return unformat_list_content links in *)
  let link_list = [] in
  let tags_link = API_core.get_tags_from_content_link content_id in
  let tags_link_list = unformat_service_return unformat_list_tag tags_link in
  (title, text, id), tags_id, link_list, tags_link_list


let get_contents filter tags_id =
  let contents = API_core.get_contents filter tags_id in
  unformat_service_return unformat_list_content contents