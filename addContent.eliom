
{shared{

open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

let (>>=) = Lwt.bind

}}

type t = Home | Content | Link

{client{

let hcenter_this elm =
  let elm = To_dom.of_div elm in
  let hcenter () =
      Eliom_lib.debug "enter";
      let wheight = Js.Optdef.case (Dom_html.window##innerHeight)
        (fun () -> 0) (fun x -> x)
      in
      let eheight =
        try int_of_string (Js.to_string (elm##style##height))
        with _ -> 0
      in
      let hposition = string_of_int ((wheight - eheight) / 2) in
      elm##style##top <- Js.string (hposition ^ "px")
  in
  hcenter ();
  Lwt.async (fun () ->
    Lwt_js_events.onresizes (fun event _ -> Lwt.return (hcenter ()) ))

}}

let make ctype =
  let my_elm =
    D.div ~a:[a_style "background-color: #0000ff; position: absolute; left: 200px;"]
      [pcdata "!!! HERE !!!!"]
  in
  ignore {unit{ hcenter_this %my_elm }};
  my_elm
