{shared{

open Eliom_lib
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

module Yojson = Yojson.Basic

open Pjson
open Pdeserialize
open GUI_deserialize

}}

type mode =
[ `Home of (Html5_types.div Eliom_content.Html5.D.elt * string)
| `Content
| `Link ]

type 'a action_type =
| Ahome
| Acontent
| Alink
| Aplus of AddContent.mode AddContent.t

{client{

open Utils.Client

let refresh_contents input div_content =
  let make_request () =
    let research = get_research input in
    Eliom_client.call_service
        ~service:%API_services.research_contents
        (None, research) ()
  in
  let elm_of_result r_contents =
    let json = Yojson.from_string r_contents in
    let contents = get_service_return get_short_content_list json in
    [div (GUI_tools.build_contents_list contents)]
  in
  refresh_list ~make_request ~elm_of_result To_dom.of_div input div_content

let refresh_tags input div_tag =
  let make_request () =
    let research = get_research input in
    Eliom_client.call_service
      ~service:%API_services.get_tags_from_research research ()
  in
  let elm_of_result r_tags =
    let json = Yojson.from_string r_tags in
    let tags = get_service_return get_tag_list json in
    [div [GUI_tools.build_tags_ul ~active_click:true tags]]
  in
  refresh_list ~make_request ~elm_of_result To_dom.of_div input div_tag

}}


let make_button mode =
  let img_class = match mode with
    | `Home _  -> "side_button_img_home_nh"
    | `Content -> "side_button_img_content_nh"
    | `Link    -> "side_button_img_link_nh"
  in
  div ~a:[a_class["side_button";"side_button_home";img_class]]
    [div ~a:[a_class["side_button_link"]] []]

let make_search_input div_tags = function
  | `Home (div_contents, value) ->
    let search_input = D.raw_input ~input_type:`Text ~name:"research" ~value () in
    let () = ignore {unit{ refresh_contents %search_input %div_contents }} in
    let () = ignore {unit{ refresh_tags %search_input %div_tags }} in
    search_input
  | _ -> div []

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
    | `Home _  -> [action (Aplus add_content)]
    | `Content -> [(* action Alink; *) action Ahome; action (Aplus add_content)]
    | `Link    -> [action Acontent; action Ahome; action (Aplus add_content)]
  in
  div ~a:[a_class["side_button_bottom"]]
    [ul ~a:[a_class["side_button_bottom_list"]]
        li_list]

let make_arrow () =
  img ~a:[a_class["side_arrow"]] ~alt:("")
    ~src:(make_uri ~service:(Eliom_service.static_dir ())
            ["images";"arrow_side_bar.png"]) ()

let make_tags mode tags_id =
  let tags_ul = D.div [GUI_tools.build_tags_ul ~active_click:true tags_id] in
  let button_add = div ~a:[a_class["side_button_add"]] [pcdata "Add a tag"] in
  match mode with
  | `Content -> tags_ul, div [tags_ul; button_add]
  | _        -> tags_ul, div [tags_ul]

let make mode tags_id add_content =
  let arrow = make_arrow () in
  let button = make_button mode in
  let tags_div, tags = make_tags mode tags_id in
  let search_input = make_search_input tags_div mode  in
  let action = make_action mode add_content in
  let elements = [arrow; button; search_input; tags; action] in
  div ~a:[a_id "sidebar"] elements
