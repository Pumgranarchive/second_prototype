(**
  {b GUI Core -
   This module compute input from API' services and get results all together}
*)

(** {6 Data gettor and formator}  *)

(** Get all data for get_detail html service. *)
val get_detail_content: string ->
  ((string * string * string) * (string * string) list *
      (string * string * string) list * (string * string) list) Lwt.t

(** Get all data for get_contents html service. *)
val get_contents: string option -> string list option ->
  ((string * string * string) list * (string * string) list) Lwt.t
