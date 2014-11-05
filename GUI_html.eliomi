
{server{

(**
  {b GUI Html -
   This module build the GUI html services}
*)

(** [home_html data research] Display the home html service *)
val home_html:
  (GUI_deserialize.id * string * string) list *
  (Rdf_store.uri * string) list ->
  string option ->
  Html5_types.html Eliom_content.Html5.elt

(** Display the content detail html service *)
val content_detail:
  GUI_deserialize.content *
  (Rdf_store.uri * string) list *
  (Rdf_store.link_id * GUI_deserialize.id * string * string) list *
  (Rdf_store.uri * string) list ->
  Html5_types.html Eliom_content.Html5.elt

(* (\** Update content detail html service *\) *)
(* val content_update: *)
(*   GUI_deserialize.content * *)
(*   (Rdf_store.uri * string) list * *)
(*   (Rdf_store.link_id * GUI_deserialize.id * string * string) list * *)
(*   (Rdf_store.uri * string) list -> *)
(*   Html5_types.html Eliom_content.Html5.elt *)

(* (\** Insert content html service *\) *)
(* val content_insert: unit -> Html5_types.html Eliom_content.Html5.elt *)

(* (\** Insert link html service *\) *)
(* val link_insert : string option -> string option -> *)
(*   Html5_types.html Eliom_content.Html5.elt *)

(* (\** Update link html service *\) *)
(* val link_update : *)
(*   (Rdf_store.link_id * Rdf_store.uri * Rdf_store.uri * *)
(*      (Rdf_store.uri * string) list) -> *)
(*   Html5_types.html Eliom_content.Html5.elt *)


}}
