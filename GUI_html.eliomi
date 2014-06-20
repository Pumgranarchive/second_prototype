
{server{

(**
  {b GUI Html -
   This module build the GUI html services}
*)

(** Display the home html service *)
val home_html: (string * string * string) list * (string * string) list -> Html5_types.html Eliom_content.Html5.elt

(** Display the content detail html service *)
val content_detail: GUI_deserialize.content * (string * string) list * (string * string * string) list * (string * string) list -> Html5_types.html Eliom_content.Html5.elt

(** Update content detail html service *)
val content_update: GUI_deserialize.content * (string * string) list * (string * string * string) list * (string * string) list -> Html5_types.html Eliom_content.Html5.elt

(** Insert content html service *)
val content_insert: unit -> Html5_types.html Eliom_content.Html5.elt

}}
