(**
   {b rdf_store -
   This Module do request to the rdf store}
*)

exception Invalid_id of string

type id

(** Create an id from a string
    @raise Invalid_id if the given string is not a valid id *)
val id_of_string : string -> id

(** Create a string from an id *)
val string_of_id : id -> string
