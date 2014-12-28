{server{

(**
  {b Pumgrana Readability Abstraction Module}
*)

(** [get_readability_data uris]
    @return the data assossiated with given [uris]
    formated as (uri, title, summary, body, is_external)
*)
val get_readability_data : Ptype.uri list ->
  (Ptype.uri * string * string * string * bool) Lwt.t list Lwt.t

(** [get_readability_detail uri]
    @return the data assossiated with the given [uri]
    formated as (uri, title, summary, body, is_external)
*)
val get_readability_detail : Ptype.uri ->
  (Ptype.uri * string * string * string * bool) Lwt.t

(** [get_readability_triple uri]
    @return the data assossiated with the given [uri]
    formated as (uri, title, summary)
*)
val get_readability_triple : Ptype.uri list ->
  (Ptype.uri * string * string) Lwt.t list Lwt.t

(** [get_readability_body uri]
    @return the body assossiated with the given [uri]
*)
val get_readability_body : Ptype.uri -> string Lwt.t

}}
