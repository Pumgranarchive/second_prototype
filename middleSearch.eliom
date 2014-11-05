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

let refresh input div_tag =
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

let redirect input =
  let dom_input = To_dom.of_input input in
  let go () =
    let research = get_research input in
    Eliom_client.change_page ~service:%GUI_services.contents (Some research) ()
  in
  Lwt.async (fun () ->
    Lwt_js_events.keyups dom_input
      (fun e _ -> if e##keyCode == 13 then go () else Lwt.return ()))

}}

let make_search_input div_tags =
    let search_input = D.raw_input ~input_type:`Text ~name:"research" () in
    let () = ignore {unit{ refresh %search_input %div_tags }} in
    let () = ignore {unit{ redirect %search_input }} in
    search_input

let make_tags () =
  D.div []

let make () =
  let tags = make_tags () in
  let search_input = make_search_input tags  in
  div [search_input; tags]
