{server{

(**
   {b HTML Container module}
*)

(** [make lwt_elements]
    Generate the HTML container with given [lwt_elements] as a list of lwt element *)
val make :
  [< Html5_types.div_content_fun ] Eliom_content.Html5.F.elt Lwt.t list
  Eliom_content.Html5.F.wrap ->
  Html5_types.div Eliom_content.Html5.F.elt Lwt.t

}}
