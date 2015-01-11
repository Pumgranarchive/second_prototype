{shared{

(**
   {b Nosql_store abstraction module}
*)

(** Raised in case of invalid id  *)
exception Invalid_id of string

type id

(** {6 Utils} *)

(** Create an id from a string
    @raise Invalid_id if the given string is not a valid id *)
val id_of_string : string -> id

(** Create a string from an id *)
val string_of_id : id -> string

(** Check if the given id is a nosql id *)
val is_nosql_id : string -> bool

}}

{server{

(** {6 Contents} *)

(** [get_detail content_id]
    @raise Not_found if no content exist with this id,
    @return (content_id, title, summary, body) *)
val get_detail: id -> (id * string * string * string) Lwt.t

(** [insert_content title summary body]
    @return the id of the created content *)
val insert_content: string -> string -> string -> id Lwt.t

(** [update_content content_id ?title ?summary ?body ()]
    @param one have to be given at least, or raise Invalid_argument,
    @raise Not_found if not content exist with this id. *)
val update_content: id -> ?title:string -> ?summary:string ->
  ?body:string -> unit -> unit Lwt.t

(** [delete_contents content_ids]  *)
val delete_contents: id list -> unit Lwt.t

(** [click_onlink link_id]
    Increment the user's mark of the given link
*)
val click_onlink : Ptype.link_id -> unit Lwt.t

(** [back_button link_id]
    Decrement the user's mark of the given link *)
val back_button : Ptype.link_id -> unit Lwt.t

}}
