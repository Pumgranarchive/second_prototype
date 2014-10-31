open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

let make_img class_name name =
  img ~a:[a_class[class_name]] ~alt:("")
    ~src:(make_uri ~service:(Eliom_service.static_dir ())
            ["images"; name]) ()

let make_links links =
  let link_list = GUI_tools.build_links_list links in
  div ~a:[a_class["content_current_linked_main_list"]] link_list

let make links =
  (* let link_sub_arrow = make_img "link_sub_arrow" "Icons_web-links_red.png" in *)
  let link_arrow = make_img "link_arrow" "linked_content_arrow.png" in
  let links = make_links links in
  div ~a:[a_class["content_current_linked"]]
    [div ~a:[a_class["content_current_linked_mainlogo"]]
	[link_arrow; links]]
