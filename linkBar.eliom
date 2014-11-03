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

let refresh content_id input div_list =
  let uri = GUI_deserialize.uri_of_id content_id in
  let encoded_uri = Rdf_store.uri_encode uri in
  let dom_input = To_dom.of_input input in
  let make_request () =
    let research = Js.to_string (dom_input##value) in
    Eliom_client.call_service
      ~service:%API_services.get_links_from_research
      (encoded_uri, research) ()
  in
  let elm_of_result result =
    let json = Yojson.from_string result in
    let links = get_service_return get_link_list json in
    [div (GUI_tools.build_links_list links)]
  in
  let dom_of_elm = To_dom.of_div in
  refresh_list ~make_request ~elm_of_result ~dom_of_elm input div_list

}}

let make_img class_name name =
  img ~a:[a_class[class_name]] ~alt:("")
    ~src:(make_uri ~service:(Eliom_service.static_dir ())
            ["images"; name]) ()

let make_search_input content_id div_links =
  let search_input = D.raw_input ~input_type:`Text ~name:"filter" () in
  let () = ignore {unit{ refresh %content_id %search_input %div_links }} in
  search_input

let make_links links =
  let link_list = GUI_tools.build_links_list links in
  D.div ~a:[a_class["content_current_linked_main_list"]] link_list

let make content_id links =
  (* let link_sub_arrow = make_img "link_sub_arrow" "Icons_web-links_red.png" in *)
  let link_arrow = make_img "link_arrow" "linked_content_arrow.png" in
  let div_links = make_links links in
  let search_input = make_search_input content_id div_links in
  div ~a:[a_class["content_current_linked"]]
    [div ~a:[a_class["content_current_linked_mainlogo"]]
	[link_arrow; search_input; div_links]]
