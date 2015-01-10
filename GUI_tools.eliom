
(*
  GUI Tools
  This module help GUI_html to make the html.
*)

{shared{


open Eliom_lib
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

open Utils

(*** Build tools  *)

}}

{server{

let make_html ?(title="Pumgrana") lwt_contents =
  lwt contents = Lwt_list.(map_s wait lwt_contents) in
  let html = Eliom_tools.F.html ~title
    ~js:[["js";"jquery-2.1.1.min.js"]]
    ~css:[["css";"style.css"]]
    Html5.F.(body contents)
  in
  Lwt.return html

let redirect_link html_body =
  let href_regexp = Str.regexp "href=" in
  let quote_regexp = Str.regexp "\\(\"\\|\'\\)" in
  let rec replace idx html =
    try
      let match_idx = Str.search_forward href_regexp html idx in
      let start_idx = (Str.search_forward quote_regexp html match_idx) + 1 in
      let end_idx = Str.search_forward quote_regexp html start_idx in
      let url = String.sub html start_idx (end_idx - start_idx) in
      let encoded_url = Rdf_store.uri_encode url in
      let new_url = "/view/" ^ encoded_url in
      let previous_html = Str.string_before html start_idx in
      let following_html = Str.string_after html end_idx in
      let new_html = previous_html ^ new_url ^ following_html in
      replace end_idx new_html
    with _ -> html
  in
  replace 0 html_body

}}

{shared{

module Str =
struct

let search_forward ?(start=false) str1 str2 start_pos =
  let end1 = String.length str1 in
  let end2 = String.length str2 in
  let rec aux pos1 pos2 =
    if pos1 >= end1 then (pos2 - 1)
    else if pos2 >= end2 then raise Not_found else
      let c1 = String.get str1 pos1 in
      let c2 = String.get str2 pos2 in
      if Char.compare c1 c2 == 0 then aux (pos1 + 1) (pos2 + 1)
      else if start then raise Not_found
      else aux 0 (pos2 + 1)
  in
  aux 0 start_pos

let search_backward str1 str2 start2 =
  let start1 = (String.length str1) - 1 in
  let rec aux pos1 pos2 =
    if pos1 < 0 then (pos2 + 1)
    else if pos2 < 0 then raise Not_found else
      let c1 = String.get str1 pos1 in
      let c2 = String.get str2 pos2 in
      if Char.compare c1 c2 == 0 then aux (pos1 - 1) (pos2 - 1)
      else aux start1 (pos2 - 1)
  in
  aux start1 start2

let remove_http_prefix str_uri =
  let size =
    try ignore (search_forward ~start:true "http://" str_uri 0); 7
    with Not_found -> ignore (search_forward ~start:true "https://" str_uri 0); 8
  in
  String.sub str_uri size ((String.length str_uri) - size)

let sub str start length =
  if length == 0 then ""
  else String.sub str start length

end

let tuple_of_id str_uri =
  print_endline str_uri;
  let str_uri = Str.remove_http_prefix str_uri in
  let end_pos = (String.length str_uri) - 1 in
  let slash_pos =
    try Str.search_forward "/" str_uri 0
    with Not_found -> end_pos
  in
  let dot_pos = Str.search_backward "." str_uri slash_pos in
  let start_pos =
    try (Str.search_backward "." str_uri (dot_pos - 1)) + 1
    with Not_found -> 0
  in
  let platform_name = String.sub str_uri start_pos (dot_pos - start_pos) in
  let ct_pos =
    try Str.search_backward "=" str_uri end_pos
    with Not_found -> Str.search_backward "/" str_uri end_pos
  in
  let content_name = Str.sub str_uri (ct_pos + 1) (end_pos - ct_pos) in
  print_endline (str_uri^" => "^platform_name^" "^content_name);
  platform_name, content_name

module Tag =
struct

(** Build tags li list *)
  let build_li ?(active_click=false) tags =
    let rec aux lis = function
      | []                -> List.rev lis
      | (uri, subject)::t  ->
        let fill = pcdata subject in
        let li = if active_click
          then li [a ~service:%GUI_services.contents [div [fill]] (Some subject)]
          else li [fill]
        in
        aux (li::lis) t
    in
    aux [] tags

  (** Build tags ul list *)
  let build_ul ?(active_click=false) tags =
    let lis = build_li ~active_click tags in
    div ~a:[a_class["side_taglist"]]
      [ul ~a:[a_class["side_taglist_list"]] lis]

  (** Build add tag html *)
  let build_add () =
    let input = D.raw_input ~input_type:`Text () in
    let add = D.raw_input ~input_type:`Submit ~value:"Add" () in
    input, add, ref [], [input; add]

end

module Link =
struct

  (** Build a links list html *)
  let build_list links =
    let aux html (link_id, id, title, summary) =
      let str_id = GUI_deserialize.string_of_id id in
      let linked = a ~service:%GUI_services.content_detail
        [div ~a:[a_class["content_current_linked_main_list_elem"]]
            [h3 [pcdata title]; p [span ~a:[a_class["content_current_linked_main_list_elem_url"]] [pcdata str_id] ; br (); pcdata summary]]]
        (Ptype.uri_encode str_id)
      in
      if List.length html == 0
      then [linked]
      else linked::(div ~a:[a_class["content_current_linked_main_list_elem_sep"]] [])::html
    in
    List.rev (List.fold_left aux [] links)

end

module Content =
struct

  (** Build a content list html *)
  let build_list contents =
    let aux html (id, title, summary) =
      let str_id = GUI_deserialize.string_of_id id in
      let content =
        D.a ~service:%GUI_services.content_detail
          [div ~a:[a_class ["content_main_list_elem"]]
              [h3 [pcdata title]; p [pcdata summary]]] (Ptype.uri_encode str_id)
      in
      if List.length html == 0
      then [content]
      else content::(div ~a:[a_class["content_main_list_elem_sep"]] [])::html
    in
    List.rev (List.fold_left aux [] contents)

end

}}
