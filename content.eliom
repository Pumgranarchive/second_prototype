open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

open Utils
open Pdeserialize
open GUI_deserialize


let make lwt_elements =
  lwt elements = Lwt_list.(map_s wait lwt_elements) in
  let html = div ~a:[a_id "content"] elements in
  Lwt.return html

let get_data content_uri =
  lwt content = Http.get_content_detail content_uri in
  let ret = match content with
    | GUI_deserialize.Internal (c_id, c_title, c_summary, c_body) ->
      c_title,
      Lwt.return (div [h3 [pcdata c_title];
                       p [pcdata c_summary];
                       p [pcdata c_body]])
    | GUI_deserialize.External (c_id, c_title, c_summary, c_html_body) ->
      let revise_html = GUI_tools.redirect_link c_html_body in
      c_title,
      Lwt.return (div ~a:[a_class["content_current"]]
                    [div [h3 [pcdata c_title];
                          F.Unsafe.data revise_html]])
  in
  Lwt.return ret
