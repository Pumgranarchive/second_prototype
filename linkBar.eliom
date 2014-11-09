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

{client{

open Utils.Client

let refresh_links content_uri input div_list =
  let str_uri = Rdf_store.string_of_uri content_uri in
  let encoded_uri = Rdf_store.uri_encode str_uri in
  let make_request () =
    let research = get_research input in
    Eliom_client.call_service
      ~service:%API_services.get_links_from_research
      (encoded_uri, research) ()
  in
  let elm_of_result result =
    let json = Yojson.from_string result in
    let links = get_service_return get_link_list json in
    [div (GUI_tools.build_links_list links)]
  in
  refresh_list ~make_request ~elm_of_result To_dom.of_div input div_list

}}

let make_img class_name name =
  img ~a:[a_class[class_name]] ~alt:("")
    ~src:(make_uri ~service:(Eliom_service.static_dir ())
            ["images"; name]) ()

let make_search_input content_uri div_links =
  let search_input = D.raw_input ~input_type:`Text ~name:"research" () in
  let () = ignore {unit{ refresh_links %content_uri %search_input %div_links }} in
  search_input

let get_links content_uri =
  try_lwt
    let str_uri = Rdf_store.string_of_uri content_uri in
    lwt json = API_core.get_links_from_content str_uri in
    Lwt.return (get_service_return get_link_list json)
  with e -> (print_endline (Printexc.to_string e); Lwt.return [])

let make_links content_uri =
  lwt links = get_links content_uri in
  let link_div = D.div (GUI_tools.build_links_list links) in
  let html = div ~a:[a_class["content_current_linked_main_list"]] [link_div] in
  Lwt.return (link_div, html)

let make content_uri =
  (* let link_sub_arrow = make_img "link_sub_arrow" "Icons_web-links_red.png" in *)
  let link_arrow = make_img "link_arrow" "linked_content_arrow.png" in
  lwt div_links, links = make_links content_uri in
  let search_input = make_search_input content_uri div_links in
  let contents = [link_arrow; search_input; links] in
  let clinked = div ~a:[a_class["content_current_linked_mainlogo"]] contents in
  let html = div ~a:[a_class["content_current_linked"]] [clinked] in
  Lwt.return html
