(**
  {b Pcash -
   This module manage persistant cash and refreshing}
*)

type 'a t
type 'a listenner = (Rdf_store.uri -> 'a -> 'a Lwt.t)

(** [make name listenner]
    [listenner] will be used for refreshing data *)
val make : string -> 'a listenner -> 'a t Lwt.t

(** [add cash key data]  *)
val add : 'a t -> Rdf_store.uri -> 'a -> unit Lwt.t

(** [exists cash key]  *)
val exists : 'a t -> Rdf_store.uri -> bool Lwt.t

(** [not_exists cash key] *)
val not_exists : 'a t -> Rdf_store.uri -> bool Lwt.t

(** [get cash key]
    @raise Not_found if no such binding exists *)
val get : 'a t -> Rdf_store.uri -> 'a Lwt.t
