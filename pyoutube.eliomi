(**
  {b Pumgrana Youtube Abstraction Module}
*)

(** Check if the given [uri] is a Youtube one. *)
val is_youtube_uri : Ptype.uri -> bool

(** [get_youtube_triple uris]
    Return data assossiated with given [uris]
    Data is formated as (uri, title, summary) *)
val get_youtube_triple : Ptype.uri list ->
  (Ptype.uri * string * string) Lwt.t list Lwt.t

(** [get_youtube_detail uri]
    Return the data assossiated with the given [uri]
    Data are formated as (uri, title, summary, body, is_external)
*)
val get_youtube_detail : Ptype.uri ->
  (Ptype.uri * string * string * string * bool) Lwt.t
