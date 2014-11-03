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
  let dom_input = To_dom.of_input input in
  let dom_list = To_dom.of_div div_list in
  let refresh_html _ =
    let display str_results =
      let json = Yojson.from_string str_results in
      let links = get_service_return get_link_list json in
      Eliom_lib.debug "%d results" (List.length links);
      let new_list = GUI_tools.build_links_list links in
      remove_all_child dom_list;
      append_all To_dom.of_div dom_list [new_list]
    in
    let make_request () =
      let encoded_uri = Rdf_store.uri_encode uri in
      let research = Js.to_string (dom_input##value) in
      Eliom_client.call_service
        ~service:%API_services.get_links_from_research
        (encoded_uri, research) ()
    in
    lwt answer = make_request () in
    Lwt.return (display answer)
  in
  Lwt.async (fun () -> Lwt_js_events.inputs dom_input
    (fun _ _ -> refresh_html ()))

}}

let make_img class_name name =
  img ~a:[a_class[class_name]] ~alt:("")
    ~src:(make_uri ~service:(Eliom_service.static_dir ())
            ["images"; name]) ()

let make_search_input () =
    D.raw_input ~input_type:`Text ~name:"filter" ()

let make_links links =
  let link_list = GUI_tools.build_links_list links in
  D.div ~a:[a_class["content_current_linked_main_list"]] [link_list]

let make content_id links =
  (* let link_sub_arrow = make_img "link_sub_arrow" "Icons_web-links_red.png" in *)
  let link_arrow = make_img "link_arrow" "linked_content_arrow.png" in
  let search_input = make_search_input () in
  let div_links = make_links links in
  ignore {unit{ refresh %content_id %search_input %div_links }};
  div ~a:[a_class["content_current_linked"]]
    [div ~a:[a_class["content_current_linked_mainlogo"]]
	[link_arrow; search_input; div_links]]
