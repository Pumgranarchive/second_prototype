(**
   {b Pumgrana Bot abstraction module}
*)

(** [launch uris]
    Launch the PumBot.
    Protected against double launching. *)
val launch : Ptype.uri list -> unit
