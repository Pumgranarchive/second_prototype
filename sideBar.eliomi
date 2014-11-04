type mode =
[ `Home of (Html5_types.div Eliom_content.Html5.D.elt * string)
| `Content
| `Link ]


(** [make type tags_id add_content]
    Generate the appropriate side bar in function of the given type *)
val make :
  mode ->
  (Rdf_store.uri * string) list ->
  AddContent.mode AddContent.t ->
  Html5_types.div Eliom_content.Html5.F.elt
