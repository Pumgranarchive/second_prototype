open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

type t = Home | Content | Link

type action_type = Ahome | Acontent | Alink | Aplus

let make_button btype =
  let img_class = match btype with
    | Home    -> "side_button_img_home"
    | Content -> "side_button_img_content"
    | Link    -> "side_button_img_link"
  in
  div ~a:[a_class["side_button";"side_button_home";img_class]]
    [div ~a:[a_class["side_button_circle"]] []]

let action atype =
  let img_class = match atype with
    | Ahome    -> "side_button_img_home"
    | Alink    -> "side_button_img_link"
    | Acontent -> "side_button_img_content"
    | Aplus    -> "side_button_img_plus"
  in
  let fill = div ~a:[a_class["side_button_circle"]] [] in
  let link = match atype with
    | Ahome -> a ~service:GUI_services.home_service_without [fill] None
    | _     -> fill
  in
  li ~a:[a_class["side_button";img_class]] [link]

let make_action btype =
  let li_list = match btype with
    | Home    -> [action Aplus]
    | Content -> [action Alink; action Ahome; action Aplus]
    | Link    -> [action Acontent; action Ahome; action Aplus]
  in
  div ~a:[a_class["side_button_bottom"]]
    [ul ~a:[a_class["side_button_bottom_list"]]
        li_list]

let make_arrow () =
  img ~a:[a_class["side_arrow"]] ~alt:("")
    ~src:(make_uri ~service:(Eliom_service.static_dir ())
            ["images";"arrow_side_bar.png"]) ()

let make_tags btype tags_id =
  let tags_ul = GUI_tools.build_tags_ul tags_id in
  let button_add = div ~a:[a_class["side_button_add"]] [pcdata "Add a tag"] in
  match btype with
  | Content -> tags_ul::button_add::[]
  | _       -> tags_ul::[]

let make btype tags_id =
  let arrow = make_arrow () in
  let button = make_button btype in
  let tags = make_tags btype tags_id in
  let action = make_action btype in
  let elements = arrow::button::tags@[action] in
  div ~a:[a_id "sidebar"] elements
