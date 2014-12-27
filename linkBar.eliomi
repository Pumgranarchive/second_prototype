(**
   {b LinkBar HTML Module}
*)

(** [make links]
    Genetate the LinkBar *)
val make :
  Rdf_store.uri ->
  Html5_types.div Eliom_content.Html5.F.elt Lwt.t
