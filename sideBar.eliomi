type mode = [`Home | `Content | `Link]

(** [make type tags_id add_content]
    Generate the appropriate side bar in function of the given type *)
val make : mode -> (Rdf_store.uri * string) list -> mode AddContent.t ->
  Html5_types.div Eliom_content.Html5.F.elt
