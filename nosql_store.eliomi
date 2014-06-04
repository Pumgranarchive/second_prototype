(**
   {b rdf_store -
   This Module do request to the rdf store}
*)

exception Invalid_id of string

type id

(** {6 Utils} *)

(** Create an id from a string
    @raise Invalid_id if the given string is not a valid id *)
val id_of_string : string -> id

(** Create a string from an id *)
val string_of_id : id -> string


(** {6 Contents} *)

(** [get_detail content_id]
    @raise Not_found if no content exist with this id,
    return (content_id, title, summary, body) *)
val get_detail: id -> (id * string * string * string) Lwt.t

(** [insert_content title summary body]
    return the id of the created content *)
val insert_content: string -> string -> string -> id Lwt.t

(** [update_content content_id ?title ?summary ?body ()]
    @param one have to be given at least, ore raise Invalid_argument,
    @raise Not_found if not content exist with this id. *)
val update_content: id -> ?title:string -> ?summary:string ->
  ?body:string -> unit -> unit Lwt.t

(** [delete_contents content_ids]  *)
val delete_contents: id list -> unit Lwt.t
