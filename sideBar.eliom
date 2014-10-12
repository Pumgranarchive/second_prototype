open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

type mode = [`Home | `Content | `Link]

type 'a action_type =
| Ahome
| Acontent
| Alink
| Aplus of mode AddContent.t

let make_button mode =
  let img_class = match mode with
    | `Home    -> "side_button_img_home"
    | `Content -> "side_button_img_content"
    | `Link    -> "side_button_img_link"
  in
  div ~a:[a_class["side_button";"side_button_home";img_class]]
    [div ~a:[a_class["side_button_circle"]] []]

let action atype =
  let img_class = match atype with
    | Ahome    -> "side_button_img_home"
    | Alink    -> "side_button_img_link"
    | Acontent -> "side_button_img_content"
    | Aplus _  -> "side_button_img_plus"
  in
  let fill = D.div ~a:[a_class["side_button_circle"]] [] in
  let link = match atype with
    | Ahome    -> a ~service:GUI_services.home_service_without [fill] None
    | Aplus ac -> AddContent.switch_onclick ac fill; fill
    | _        -> fill
  in
  li ~a:[a_class["side_button";img_class]] [link]

let make_action mode add_content =
  let li_list = match mode with
    | `Home    -> [action (Aplus add_content)]
    | `Content -> [action Alink; action Ahome; action (Aplus add_content)]
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
  let active_click = match mode with
    | `Content -> true
    | _        -> false
  in
  let tags_ul = GUI_tools.build_tags_ul ~active_click tags_id in
  let button_add = div ~a:[a_class["side_button_add"]] [pcdata "Add a tag"] in
  match mode with
  | `Content -> tags_ul::button_add::[]
  | _        -> tags_ul::[]

let make mode tags_id add_content =
  let arrow = make_arrow () in
  let button = make_button mode in
  let tags = make_tags mode tags_id in
  let action = make_action mode add_content in
  let elements = arrow::button::tags@[action] in
  div ~a:[a_id "sidebar"] elements
