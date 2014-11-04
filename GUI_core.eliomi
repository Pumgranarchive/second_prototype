(**
  {b GUI Core -
   This module compute input from API' services and get results all together}
*)

(** {6 Data gettor and formator}  *)

(** Get all data for get_detail html service. *)
val get_detail_content: string ->
  (GUI_deserialize.content *
     (Rdf_store.uri * string) list *
     (Rdf_store.link_id * GUI_deserialize.id * string * string) list *
     (Rdf_store.uri * string) list) Lwt.t

(** Get all data for get_contents html service. *)
val get_contents: string option -> string option ->
  ((GUI_deserialize.id * string * string) list *
      (Rdf_store.uri * string) list) Lwt.t

(** Get all data for link detail html service  *)
val get_link_detail : string ->
  (Rdf_store.link_id * Rdf_store.uri * Rdf_store.uri *
     (Rdf_store.uri * string) list) Lwt.t
