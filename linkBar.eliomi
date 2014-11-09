(** [make links]
    Genetate the link bar *)
val make :
  Rdf_store.uri ->
  Html5_types.div Eliom_content.Html5.F.elt Lwt.t
