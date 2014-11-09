open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

open Pdeserialize
open GUI_deserialize


let make elements =
  div ~a:[a_id "content"] elements

let get_data content_uri =
  let str_uri = Rdf_store.string_of_uri content_uri in
  lwt json = API_core.get_detail str_uri in
  let content = List.hd (get_service_return get_content_list json) in
  let ret = match content with
    | GUI_deserialize.Internal (c_id, c_title, c_summary, c_body) ->
      c_title,
      div [h3 [pcdata c_title]; p [pcdata c_summary]; p [pcdata c_body]]
    | GUI_deserialize.External (c_id, c_title, c_summary, c_html_body) ->
      let revise_html = GUI_tools.redirect_link c_html_body in
      c_title,
      div ~a:[a_class["content_current"]]
        [div [h3 [pcdata c_title]; F.Unsafe.data revise_html]]
  in
  Lwt.return ret
