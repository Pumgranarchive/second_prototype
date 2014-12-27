(**
   {b Content HTML module}
*)

(** [make lwt_elements]
    Generate the HTML content with given [lwt_elements] as list of lwt elements. *)
val make :
  [< Html5_types.div_content_fun ] Eliom_content.Html5.F.elt Lwt.t list
  Eliom_content.Html5.F.wrap ->
  Html5_types.div Eliom_content.Html5.F.elt Lwt.t

(** [get_data content_uri]
    return data of the given content by uri
    The return is formated as (title, html_body) *)
val get_data : Rdf_store.uri ->
  (string * Html5_types.div Eliom_content.Html5.F.elt Lwt.t) Lwt.t
