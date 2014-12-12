
{shared{

open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

let (>>=) = Lwt.bind

}}

type mode =
[`Contents of (Html5_types.div Eliom_content.Html5.D.elt Lwt.t * string option * string)
| `Detail of Rdf_store.uri]

type 'a t = ('a * Html5_types.div Eliom_content.Html5.D.elt)

{client{

let vcenter_this elm =
  let elm = To_dom.of_div elm in
  let vcenter _ _ =
      let wheight = Js.Optdef.case (Dom_html.window##innerHeight)
        (fun () -> 0) (fun x -> x)
      in
      let eheight = 400 (* Doesn't look like it's finding the good value *)
        (* try int_of_string (Js.to_string (elm##style##height))
        with _ -> 0 *)
      in
      let hposition = string_of_int (((wheight - eheight) / 2)) in
      Lwt.return (elm##style##marginTop <- Js.string (hposition ^ "px"))
  in
  Lwt.async (vcenter ());
  Lwt.async (fun () -> Lwt_js_events.onresizes vcenter);
  Lwt.async (fun () -> Lwt_js_events.scrolls Dom_html.document vcenter)

let scroll_adjust_this elm =
  let elm = To_dom.of_div elm in
  let adjust _ _ =
      let scrolltop = Dom_html.document##body##scrollTop in
      let sposition = string_of_int scrolltop in
      Lwt.return (elm##style##top <- Js.string (sposition ^ "px"))
  in
  Lwt.async (adjust ());
  Lwt.async (fun () -> Lwt_js_events.scrolls Dom_html.document adjust)

let bind_click_to_switch click_elm target_elm =
  let click_elm = To_dom.of_div click_elm in
  let target_elm = To_dom.of_div target_elm in
  let switch _ _ =
    let current = Js.to_string (target_elm##style##visibility) in
    let new_v = match current with
      | "visible" -> "hidden"
      | _         -> "visible"
    in
    Lwt.return (target_elm##style##visibility <- Js.string new_v)
  in
  Lwt.async (fun () -> Lwt_js_events.clicks click_elm switch)

}}

let make mode =
  let my_elm =
    D.div ~a:[a_class["window_pop_up"]]
          [h1 ~a:[a_class["window_pop_up_button_add"]] [pcdata "Add a content"];
	  raw_input ~input_type:`Text ~name:"content_name" ();
	  h1 [pcdata "Add Tags"];
	  raw_input ~input_type:`Text ~name:"content_tags" ();
	  raw_input ~a:[a_class["window_pop_up_form_submit"]] ~input_type:`Submit ~value:"Add" ()]
  in
  let background =
    D.div ~a:[a_class["window_pop_up_bg"]] [my_elm]
  in
  ignore {unit{ vcenter_this %my_elm }};
  ignore {unit{ scroll_adjust_this %background }};
  ignore {unit{ bind_click_to_switch %background %background }};
  mode, background

let to_html (mode, elm) = Lwt.return elm

let switch_onclick (mode, elm) target =
  ignore {unit{ bind_click_to_switch %target %elm }}
