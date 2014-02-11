{shared{

(**
  {b GUI Core -
   This module compute input from API' services and get results all together}
*)

(** {6 Unformat tools} *)

(** [unformat_service_return func json]
    Unformat the API's service return

    The [func] is call with the extrated data from the given json
*)
val unformat_service_return: (Yojson.Safe.json -> 'b) -> Yojson.Safe.json -> 'b

(** Unformat the API's content_id return *)
val unformat_content_id_return: Yojson.Safe.json -> string

(** Unformat the API's content return *)
val unformat_content: Yojson.Safe.json -> (string * string * string)

(** Unformat the API's link return  *)
val unformat_link: Yojson.Safe.json -> (string * string * string)

(** Unformat the API's tag return  *)
val unformat_tag: Yojson.Safe.json -> (string * string)

(** [unformat_list func json]
    Unformat the API's list return

    The [func] is called on each extrated element in the given json
    to buils the returned list *)
val unformat_list: (Yojson.Safe.json -> 'a) -> Yojson.Safe.json -> 'a list

(** {b Unformat shorcuts} *)

(** Shortcut to unformate a list of tags *)
val unformat_list_tag: Yojson.Safe.json -> (string * string) list

(** Shortcut to unformate a list of contents *)
val unformat_list_content: Yojson.Safe.json -> (string * string * string) list

(** Shortcut to unformate a list of links *)
val unformat_list_link: Yojson.Safe.json -> (string * string * string) list

}}

{server{

(** {6 Data gettor and formator}  *)

(** Get all data for get_detail html service. *)
val get_detail_content: string -> ((string * string * string) * (string * string) list * (string * string * string) list * (string * string) list)

(** Get all data for get_contents html service. *)
val get_contents: string option -> string list option -> (string * string * string) list * (string * string) list

}}
