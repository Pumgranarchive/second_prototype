type t = Home | Content | Link

(** make type tags_id
    Generate the appropriate side bar in function of the given type *)
val make : t -> (Rdf_store.uri * string) list ->
  Html5_types.div Eliom_content.Html5.F.elt
