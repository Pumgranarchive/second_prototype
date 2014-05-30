(**
   {b rdf_store -
   This Module do request to the rdf store}
*)

type link_id

(** link_id * target_id * tag_id list  *)
type link = link_id * string * string list

type tag_type = TagLink | TagContent

(** {6 Links}  *)

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

(** {6 Tags}
    Tag's getter return a tuple list of (tag_uri, subject) *)

(** [get_tags tag_type tags_uri]
    if [tags_uri] list is null, all tag of tag_type are returned *)
val get_tags : tag_type -> string list -> (string * string) list Lwt.t

(** [get_tags_from_link link_id] *)
val get_tags_from_link : link_id -> (string * string) list Lwt.t

(** [get_tags_from_content content_id] *)
val get_tags_from_content: string -> (string * string) list Lwt.t

(** [get_tags_from_content_link content_id]  *)
val get_tags_from_content_link : string -> (string * string) list Lwt.t

(** [insert_tags tag_type ?link_id ?content_id subjects]
    [link_id] and [content_id] have to correspond with the tag_type,
    [link_id] or [content_id] can be setted to associate all new tags on,
    return a list of created tags_uri *)
val insert_tags : tag_type -> ?link_id:link_id -> ?content_id:string ->
  string list -> string list Lwt.t

(** [delete_tags tags_uri]  *)
val delete_tags : string list -> unit Lwt.t
