open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

type t = Home | Content | Link

let make ctype =
  div ~a:[a_style "visibility: hidden;"] [pcdata "my div"]
