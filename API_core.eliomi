(**
   {b API Core -
   This Module do request to database and format the return}
*)


(** {6 Content} *)

(** [uri_from_platform plateform_name content_name]
    Return the known uri from [plateform_name] and [content_name] *)
val uri_from_platform : string -> string -> Yojson.Basic.json Lwt.t

(** [get_detail content_uri]  *)
val get_detail: string -> Yojson.Basic.json Lwt.t

(** [get_contents filter tags_uri]
    Currently, filter is not used,
    because we haven't enought informations in the DB

    Warning: if one tag_id does not exist, no error will be returned,
    and no content neither.
*)
val get_contents: string option -> string list option -> Yojson.Basic.json Lwt.t

(** [research_contents filter research]
    Same as get_contents but with research string rather than a tag list
*)
val research_contents: string option -> string -> Yojson.Basic.json Lwt.t

(** [insert_content title summary text tags_uri]  *)
val insert_content: string -> string -> string -> string list option ->
  Yojson.Basic.json Lwt.t

(** [update_content content_uri title summary text tags_uri]  *)
val update_content: string -> string option -> string option -> string option ->
  string list option -> Yojson.Basic.json Lwt.t

(** [update_content_tags content_uri tags_uri]  *)
val update_content_tags: string -> string list -> Yojson.Basic.json Lwt.t

(** [delete_contents content_uris]  *)
val delete_contents: string list -> Yojson.Basic.json Lwt.t


(** {6 Tag} *)

(** [get_tags_by_type tag_type]  *)
val get_tags_by_type: string -> Yojson.Basic.json Lwt.t

(** [get_tags_from_research string]  *)
val get_tags_from_research: string -> Yojson.Basic.json Lwt.t

(** [get_tags_from_content content_id]
    Warning: if one tag_id does not exist, no error will be fire. *)
val get_tags_from_content: string -> Yojson.Basic.json Lwt.t

(** [get_tags_from_content_link content_id]
    Warning: if a tag_id does not exist no error will be fire. *)
val get_tags_from_content_link: string -> Yojson.Basic.json Lwt.t

(** [insert_tags type_name id_opt subjects]
    Warning: does not return status list, only the id list *)
val insert_tags: string -> string option -> string list ->
  Yojson.Basic.json Lwt.t

(** [delete_tags tags_id]  *)
val delete_tags: string list -> Yojson.Basic.json Lwt.t


(** {6 Link} *)

(** [get_link_detail link_id] *)
val get_link_detail : string -> Yojson.Basic.json Lwt.t

(** [get_links_from_content content_id]  *)
val get_links_from_content: string -> Yojson.Basic.json Lwt.t

(** [get_links_from_content_tags content_id tags_id]  *)
val get_links_from_content_tags: string -> string list option ->
  Yojson.Basic.json Lwt.t

(** [get_links_from_research content_id research]  *)
val get_links_from_research: string -> string ->
  Yojson.Basic.json Lwt.t

(** [insert_links (origin_uri, target_uri, tags_uri) list]  *)
val insert_links: (string * string * string list) list ->
  Yojson.Basic.json Lwt.t

(** [insert_scored_links (origin_uri, target_uri, tags_uri, score) list]  *)
val insert_scored_links: (string * string * string list * int) list ->
  Yojson.Basic.json Lwt.t

(** [update_link (link_id tag_uri list) list]  *)
val update_links: (string * string list) list -> Yojson.Basic.json Lwt.t

(** [delete_links links_id]  *)
val delete_links: string list -> Yojson.Basic.json Lwt.t
