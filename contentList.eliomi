{server{

(**
   {b ContentList HTML Module}
*)

(** [make opt_filter research]
    Generate the HTML ContentList *)
val make : string option -> string ->
  Html5_types.div Eliom_content.Html5.F.elt Lwt.t

}}
