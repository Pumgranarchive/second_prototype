(**
   {b rdf_store -
   This Module do request to the rdf store}
*)

(** {6 Links}  *)

type link_id

(** link_id * target_id * tag_id list  *)
type link = link_id * string * string list

val string_of_link_id : link_id -> string
val link_id_of_string : string -> link_id

val target_id_from_link_id : link_id -> string Lwt.t
val origin_id_from_link_id : link_id -> string Lwt.t

(** [get_links_from_content content_id]  *)
val links_from_content : string -> link list Lwt.t

(** [get_links_from_content_tags content_id tags_id]  *)
val links_from_content_tags : string -> string list -> link list Lwt.t

(** [insert_links origin_id targets_id tags_id]  *)
val insert_links : string -> string list -> string list list ->
  link_id list Lwt.t

(** [update_link link_id new_tags]  *)
val update_link : link_id -> string list -> unit Lwt.t

(** [delete_links links_id tags_id]
    [tags_id] could be empty *)
val delete_links : link_id list -> string list list -> unit Lwt.t
