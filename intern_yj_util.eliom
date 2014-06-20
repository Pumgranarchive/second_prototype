{shared{

(*
** Internal Json utils
** This module help the translate between json and ocaml
 *)

module Yojson = Yojson.Basic

open Yojson.Util

exception Yojson_exc of string

(** return the Some member associate of name, and None if not found *)
let opt_member name json =
  try Some (member name json)
  with _ -> None

(** Extract a list from JSON array or raise Yojson_exc.
    `Null are assume as empty list. *)
let to_list = function
  | `Null   -> []
  | `List l -> l
  | _       -> raise (Yojson_exc "Bad list format")

(** map the given yojson with the given func *)
let map func = function
  | Some x      -> Some (func x)
  | None        -> None

}}
