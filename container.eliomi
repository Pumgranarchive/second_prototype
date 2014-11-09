(** [make lwt_elements]
    Generate the container html with given [lwt_elements *)
val make :
  [< Html5_types.div_content_fun ] Eliom_content.Html5.F.elt Lwt.t list
  Eliom_content.Html5.F.wrap ->
  Html5_types.div Eliom_content.Html5.F.elt Lwt.t
