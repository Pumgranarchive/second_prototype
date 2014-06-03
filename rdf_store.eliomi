(**
   {b rdf_store -
   This Module do request to the rdf store}
*)

exception Invalid_uri of string
exception Invalid_link_id of string

type uri

type link_id

(** link_id * target_uri * tag_uri list  *)
type link = link_id * uri * uri list

type tag_type = TagLink | TagContent


(** {6 Utils}  *)

(** Create a URI from a string.
    @raise Invalid_uri in case of the string does not represent an URI. *)
val uri_of_string : string -> uri

(** Create a string from a URI  *)
val string_of_uri : uri -> string

(** Create a link_id from a string
    @raise Invalid_link_id *)
val link_id_of_string : string -> link_id

(** Create a string from a link_id  *)
val string_of_link_id : link_id -> string

val target_uri_from_link_id : link_id -> uri
val origin_uri_from_link_id : link_id -> uri

val uri_of_content_id : Nosql_store.id -> uri
val content_id_of_uri : uri -> Nosql_store.id

(* To remove in the futur *)

(** Create a URI from a tag_id link string. *)
val uri_of_tag_id_link : string -> uri

(** Create a URI from a tag_id content string. *)
val uri_of_tag_id_content : string -> uri

val tag_id_link_of_uri : uri -> string

val tag_id_content_of_uri : uri -> string

(* End area of removing *)

(** {6 Contents}  *)

(** [get_triple_contents tags_uri]
    if [tags_uri] is an empty list, return all contents' triple.
    return a triple (content_id, title, summary) *)
val get_triple_contents : uri list ->
  (Nosql_store.id * string * string) list Lwt.t

(** [insert_content content_id title summary tags_uri]
    [tags_uri] can be an empty list *)
val insert_content : Nosql_store.id -> string -> string -> uri list -> unit Lwt.t

(** [delete_contents contents_id *)
val delete_contents : Nosql_store.id list -> unit Lwt.t

(** [update_content content_id ?title ?summary ?tags_uri ()]
    @param at least one have to be gived. *)
val update_content : Nosql_store.id -> ?title: string -> ?summary: string ->
  ?tags_uri: uri list -> unit -> unit Lwt.t

(** [update_content_tags content_uri tags_uri] *)
val update_content_tags : uri -> uri list -> unit Lwt.t


(** {6 Links}  *)

(** [get_links_from_content content_uri]  *)
val links_from_content : uri -> link list Lwt.t

(** [get_links_from_content_tags content_id tags_uri]  *)
val links_from_content_tags : uri -> uri list -> link list Lwt.t

(** [insert_links origin_uri targets_uri tags_uri]  *)
val insert_links : uri -> uri list -> uri list list -> link_id list Lwt.t

(** [update_link link_id new_tags_uri]  *)
val update_link : link_id -> uri list -> unit Lwt.t

(** [delete_links links_id tags_uri]
    [tags_id] could be empty *)
val delete_links : link_id list -> uri list list -> unit Lwt.t


(** {6 Tags}
    Tag's getter return a tuple list of (tag_uri, subject) *)

(** [get_tags tag_type tags_uri]
    if [tags_uri] list is null, all tags of the given tag_type are returned *)
val get_tags : tag_type -> uri list -> (uri * string) list Lwt.t

(** [get_tags_from_link link_id] *)
val get_tags_from_link : link_id -> (uri * string) list Lwt.t

(** [get_tags_from_content content_uri] *)
val get_tags_from_content: uri -> (uri * string) list Lwt.t

(** [get_tags_from_content_link content_uri]  *)
val get_tags_from_content_link : uri -> (uri * string) list Lwt.t

(** [insert_tags tag_type ?link_id ?content_uri subjects]
    @param [link_id] and [content_uri] have to correspond with the tag_type,
    @param [link_id] or [content_uri] can be setted to associate all new tags on,
    return a list of created tags_uri *)
val insert_tags : tag_type -> ?link_id:link_id -> ?content_uri:uri ->
  string list -> uri list Lwt.t

(** [delete_tags tags_uri]  *)
val delete_tags : uri list -> unit Lwt.t
