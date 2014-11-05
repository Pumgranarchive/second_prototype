
(*
  GUI Tools
  This module help GUI_html to make the html.
*)

{shared{


open Eliom_lib
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

(*** Build tools  *)

}}

{server{

let make_html ?(title="Pumgrana") content =
  Eliom_tools.F.html ~title
    ~js:[["js";"jquery-2.1.1.min.js"]]
    ~css:[["css";"style.css"]]
    Html5.F.(body content)

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
      let new_url = "/content/detail/" ^ encoded_url in
      let previous_html = Str.string_before html start_idx in
      let following_html = Str.string_after html end_idx in
      let new_html = previous_html ^ new_url ^ following_html in
      replace end_idx new_html
    with _ -> html
  in
  replace 0 html_body

}}

{shared{

let build_header elt_list =
  div ~a:[a_class["header"]]
    ([span ~a:[a_class["pumgrana"]]
         [a ~service:%GUI_services.home
             [img ~a:[a_class ["pumgrana_logo"]]
                 ~alt:("Pumgrana Logo")
                 ~src:(make_uri
                         ~service:(Eliom_service.static_dir ())
                         ["images"; "LOGO_Pumgrana.png"]) ()] ()]]@elt_list)

let build_header_back_forward elt_list =
  let back_button = D.raw_input ~input_type:`Submit ~value:"Back" () in
  let forward_button = D.raw_input ~input_type:`Submit ~value:"Forward" () in
  let header_elt = build_header ([back_button; forward_button]@elt_list) in
  back_button, forward_button, header_elt

let build_contents_header () =
  let insert_button =
    D.raw_input ~input_type:`Submit ~value:"New" ()
  in
  let back_button, forward_button, header_elt =
    build_header_back_forward [insert_button]
  in
  insert_button, back_button, forward_button, header_elt

let build_detail_content_header () =
  let update_button = D.raw_input ~input_type:`Submit ~value:"Edit" () in
  let delete_button = D.raw_input ~input_type:`Submit ~value:"Delete" () in
  let back_button, forward_button, header_elt =
    build_header_back_forward [update_button; delete_button]
  in
  back_button, forward_button, update_button, delete_button, header_elt

let build_update_content_header () =
  let cancel_button = D.raw_input ~input_type:`Submit ~value:"Cancel" () in
  let save_button = D.raw_input ~input_type:`Submit ~value:"Save" () in
  let header_elt = build_header [cancel_button; save_button] in
  cancel_button, save_button, header_elt

let build_link_header () =
  let insert = D.raw_input ~input_type:`Submit ~value:"New" () in
  insert

(** Build tags list with checkbox *)
let build_ck_tags_list tags =
  let rec aux inputs full_html = function
    | []                -> inputs, List.rev full_html
    | (uri, subject)::t  ->
      let str_uri = Rdf_store.string_of_uri uri in
      let input = D.raw_input ~input_type:`Checkbox ~name:str_uri () in
      let html = div [input; pcdata subject] in
      aux (input::inputs) (html::full_html) t
  in
  aux [] [] tags

(** Build tags li list *)
let build_tags_li ?(active_click=false) tags =
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

let build_tags_ul ?(active_click=false) tags =
  let lis = build_tags_li ~active_click tags in
  div ~a:[a_class["side_taglist"]]
    [ul ~a:[a_class["side_taglist_list"]] lis]

(** Build the tags html formular from tag list *)
let build_tags_form tags =
  let tags_inputs, tags_html_0 = build_ck_tags_list tags in
  let submit = D.raw_input ~input_type:`Submit ~value:"Submit" () in
  submit, tags_inputs, List.rev ((div [submit])::(List.rev tags_html_0))

(** Build a simple tags list html *)
let build_tags_list tags =
  let aux (uri, subject) = div [pcdata subject] in
  List.map aux tags

(** Build the links list with checkbox *)
let build_ck_links_list links =
  let rec aux inputs full_html = function
    | [] -> inputs, List.rev full_html
    | (link_id, content_id, title, summary)::t ->
      let str_link_id = Rdf_store.string_of_link_id link_id in
      let input = D.raw_input ~input_type:`Checkbox ~name:str_link_id () in
      let html =
        div [input;
             (* a ~service:%GUI_services.link_update_service *)
             (*   [ *)pcdata title(* ] (Rdf_store.uri_encode str_link_id) *);
             br (); pcdata summary]
      in
      aux (input::inputs) (html::full_html) t
  in
  aux [] [] links

(** Build a links list html *)
let build_links_list links =
  let aux html (link_id, id, title, summary) =
    let str_id = Rdf_store.uri_encode (GUI_deserialize.string_of_id id) in
    let linked = D.a ~service:%GUI_services.content_detail
      [div ~a:[a_class["content_current_linked_main_list_elem"]]
          [h3 [pcdata title]; p [pcdata summary]]] str_id
    in
    if List.length html == 0
    then [linked]
    else linked::(div ~a:[a_class["content_current_linked_main_list_elem_sep"]] [])::html
  in
  List.rev (List.fold_left aux [] links)

let build_contents_list contents =
  let aux html (id, title, summary) =
    let str_id = Rdf_store.uri_encode (GUI_deserialize.string_of_id id) in
    let content =
      D.a ~service:%GUI_services.content_detail
        [div ~a:[a_class ["content_main_list_elem"]]
          [h3 [pcdata title]; p [pcdata summary]]] str_id
    in
    if List.length html == 0
    then [content]
    else content::(div ~a:[a_class["content_main_list_elem_sep"]] [])::html
  in
  List.rev (List.fold_left aux [] contents)

let build_add_tag () =
  let input = D.raw_input ~input_type:`Text () in
  let add = D.raw_input ~input_type:`Submit ~value:"Add" () in
  input, add, ref [], [input; add]

}}
