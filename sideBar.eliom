{shared{

open Eliom_lib
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

module Yojson = Yojson.Basic

open Pjson
open Pdeserialize
open GUI_deserialize
open GUI_tools

}}

type mode =
[ `Contents of (Html5_types.div Eliom_content.Html5.D.elt Lwt.t * string option * string)
| `Detail of Rdf_store.uri
| `Link ]

type 'a action_type =
| Ahome
| Acontent
| Alink
| Aplus of AddContent.mode AddContent.t

{client{

open Utils.Client

let refresh_contents input div_content =
  let make_request () = Http.research_contents (get_research input) in
  let elm_of_result contents = [div (Content.build_list contents)] in
  refresh_list ~make_request ~elm_of_result To_dom.of_div input div_content

let refresh_tags input div_tag =
  let active_click = true in
  let make_request () = Http.tags_from_research (get_research input) in
  let elm_of_result tags = [div [Tag.build_ul ~active_click tags]] in
  refresh_list ~make_request ~elm_of_result To_dom.of_div input div_tag

}}


let make_button mode =
  let img_class = match mode with
    | `Contents _  -> "side_button_img_home_nh"
    | `Detail _ -> "side_button_img_content_nh"
    | `Link    -> "side_button_img_link_nh"
  in
  div ~a:[a_class["side_button";"side_button_home";img_class]]
    [div ~a:[a_class["side_button_link"]] []]

let make_search_input div_tags = function
  | `Contents (lwt_div_contents, filter, research) ->
    lwt div_contents = lwt_div_contents in
    let name = "research" in
    let value = research in
    let search_input = D.raw_input ~input_type:`Text ~name ~value () in
    let () = ignore {unit{ refresh_contents %search_input %div_contents }} in
    let () = ignore {unit{ refresh_tags %search_input %div_tags }} in
    Lwt.return search_input
  | _ -> Lwt.return (div [])

let action atype =
  let img_class = match atype with
    | Ahome    -> "side_button_img_home"
    | Alink    -> "side_button_img_link"
    | Acontent -> "side_button_img_content"
    | Aplus _  -> "side_button_img_plus"
  in
  let fill = D.div ~a:[a_class["side_button_link"]] [] in
  let link = match atype with
    | Ahome    -> a ~service:GUI_services.contents [fill] None
    | Aplus ac -> AddContent.switch_onclick ac fill; fill
    | _        -> fill
  in
  li ~a:[a_class["side_button";img_class]] [link]

let make_action mode add_content =
  let li_list = match mode with
    | `Contents _ -> [action (Aplus add_content)]
    | `Detail _ -> [(* action Alink; *) action Ahome(* ; action (Aplus add_content) *)]
    | `Link   -> [action Acontent; action Ahome; action (Aplus add_content)]
  in
  div ~a:[a_class["side_button_bottom"]]
    [ul ~a:[a_class["side_button_bottom_list"]]
        li_list]

let make_arrow () =
  img ~a:[a_class["side_arrow"]] ~alt:("")
    ~src:(make_uri ~service:(Eliom_service.static_dir ())
            ["images";"arrow_side_bar.png"]) ()

let get_tags mode =
  try_lwt
    match mode with
      | `Contents (_, _, research) -> Http.tags_from_research research
      | `Detail content_uri -> Http.tags_from_content content_uri
      | `Link -> failwith "Not implemented"
  with e -> (print_endline (Printexc.to_string e); Lwt.return [])

let make_tags mode =
  lwt tags_id = get_tags mode in
  let tags_ul = D.div [Tag.build_ul ~active_click:true tags_id] in
  (* let button_add = div ~a:[a_class["side_button_add"]] [pcdata "Add a tag"] in *)
  let html = match mode with
    | `Detail _ -> tags_ul, div [tags_ul(* ; button_add *)]
    | _         -> tags_ul, div [tags_ul]
  in
  Lwt.return html

let make mode add_content =
  let arrow = make_arrow () in
  let button = make_button mode in
  lwt tags_div, tags = make_tags mode in
  lwt search_input = make_search_input tags_div mode  in
  let action = make_action mode add_content in
  let elements = [arrow; button; search_input; tags; action] in
  let html = div ~a:[a_id "sidebar"] elements in
  Lwt.return html
