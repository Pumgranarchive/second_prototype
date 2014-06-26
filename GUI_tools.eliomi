
{shared{

(**
  {b GUI Tools -
   This module help GUI_html to build the html}
*)

(** Build the header fore the contents list html *)
val build_contents_header: unit -> Html5_types.input Eliom_content.Html5.F.elt * Html5_types.input Eliom_content.Html5.F.elt * Html5_types.input Eliom_content.Html5.F.elt * Html5_types.div Eliom_content.Html5.F.elt

(** Build the header fore the content detailt html *)
val build_detail_content_header: unit -> Html5_types.input Eliom_content.Html5.F.elt * Html5_types.input Eliom_content.Html5.F.elt * Html5_types.input Eliom_content.Html5.F.elt * Html5_types.input Eliom_content.Html5.F.elt * Html5_types.div Eliom_content.Html5.F.elt

(** Build the header fore the update content html *)
val build_update_content_header: unit -> Html5_types.input Eliom_content.Html5.F.elt * Html5_types.input Eliom_content.Html5.F.elt * Html5_types.div Eliom_content.Html5.F.elt

(** Build tags html list with checkbox *)
val build_ck_tags_list: (Rdf_store.uri * string) list -> Html5_types.input Eliom_content.Html5.F.elt list * Html5_types.div Eliom_content.Html5.F.elt list

(** Build the tags html formular from tag list *)
val build_tags_form: (Rdf_store.uri * string) list -> Html5_types.input Eliom_content.Html5.F.elt * Html5_types.input Eliom_content.Html5.F.elt list * [ > Html5_types.div ] Eliom_content.Html5.F.elt list

(** Build a simple tags html list *)
val build_tags_list: (Rdf_store.uri * string) list -> Html5_types.div Eliom_content.Html5.F.elt list

(** Build the links html list with checkbox *)
val build_ck_links_list:
  (Rdf_store.link_id * GUI_deserialize.id * string * string) list ->
  Html5_types.input Eliom_content.Html5.F.elt list * Html5_types.div Eliom_content.Html5.F.elt list

(** Build a links list html *)
val build_links_list:
  (Rdf_store.link_id * GUI_deserialize.id * string * string) list ->
  Html5_types.div Eliom_content.Html5.F.elt list

(** Build a contents list html *)
val build_contents_list: (GUI_deserialize.id * string * string) list -> Html5_types.div Eliom_content.Html5.F.elt list

(** Build a input add tag html *)
val build_add_tag: unit -> Html5_types.input Eliom_content.Html5.F.elt * Html5_types.input Eliom_content.Html5.F.elt * 'a list ref * Html5_types.input Eliom_content.Html5.F.elt list

}}
