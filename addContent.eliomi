{server{

(**
   {b AddContent HTML Module}
*)

(** The two possible mode to build AddContent *)
type mode =
[`Contents of (Html5_types.div Eliom_content.Html5.D.elt Lwt.t * string option * string)
| `Detail of Ptype.uri]

(** The AddContent type  *)
type 'a t

(** [make mode]
    build an addContent HTML element in function of the given mode *)
val make : mode -> mode t

(** [to_html t]
    @return the html of [t] *)
val to_html : mode t -> Html5_types.div Eliom_content.Html5.D.elt Lwt.t

(** [switch_onclick t target]
    Bind the click event of the [target] to switch the [t] visibility *)
val switch_onclick : mode t -> Html5_types.div Eliom_content.Html5.D.elt -> unit

}}
