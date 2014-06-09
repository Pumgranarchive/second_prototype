module Yojson = Yojson.Basic

open Yojson.Util
open Intern_yj_util

exception Invalid_id of string

type id = Bson.element

(******************************************************************************
****************************** Configuration **********************************
*******************************************************************************)

let id_length = 24

(** {6 DB configuration} *)
let db_url = "127.0.0.1"
let db_name = "pumgrana"
let db_port = 27017

let contents_coll_name = "contents"

let id_field = "_id"
let title_field = "title"
let summary_field = "summary"
let body_field = "body"

(*** DB's collection *)

let contents_coll = Mongo_lwt.create db_url db_port
  db_name contents_coll_name

(*** Request format tools  *)

let yes_value = Bson.create_boolean true

let content_format =
  Bson.add_element id_field yes_value
    (Bson.add_element title_field yes_value
       (Bson.add_element summary_field yes_value
          (Bson.add_element body_field yes_value Bson.empty)))

(******************************************************************************
********************************** Tools **************************************
*******************************************************************************)

let id_of_string str =
  try Bson.create_objectId str
  with e -> raise (Invalid_id str)

let string_of_id id = Bson.get_objectId id

(* Initialize the random *)
let _ = Random.self_init ()

let new_id () =
  let hex = String.make id_length '0' in
  let rec aux i =
    let remainder = Random.int 16 in
    let char_remainder =
      if remainder > 9
      then Char.chr (87 + remainder)
      else Char.chr (48 + remainder)
    in
    String.set hex i char_remainder;
    if i < (id_length - 1) then aux (i + 1)
  in
  aux 0;
  hex

(* let min = 4951760157141521099596496896. *)
(* let max = 79228162514264337593543950336. *)
(* let diff = max -. min *)
(* let rand () = min +. (Random.float diff) *)
(* let init_vector = rand () *)

(* let hexadecimal f = *)
(*   let base = 16. in *)
(*   let letter = "abcdef" in *)
(*   let rec aux i hex f = *)
(*     let quotient = floor (f /. base) in *)
(*     let remainder = Float.modulo f base in *)
(*     let str_remainder = *)
(*       if remainder > 9. *)
(*       then Char.escaped (String.get letter (int_of_float (remainder -. 10.))) *)
(*       else string_of_int (int_of_float remainder) *)
(*     in *)
(*     let new_hex = str_remainder ^ hex in *)
(*     if i < id_length *)
(*     then aux (i + 1) new_hex quotient *)
(*     else new_hex *)
(*   in *)
(*   aux 1 "" f *)

(* let new_id () = *)
(*   let round_vector = rand () in *)
  (* let float_id = Float.modulo round_vector max in *)
  (* Printf.printf "float_id : %f\n" float_id; *)
  (* hexadecimal float_id *)

(*** Cast tools *)

(** Convert string objectID from mongo in Hexa 12 char format
    into string in hex 24 char format *)
let reformat_id str_id =
  try
    let base = 16 in
    let length = 12 in
    let buf = Buffer.create length in
    let char_of_hex_int h =
      String.get (Printf.sprintf "%x" h) 0
    in
    let rec aux position =
      if (position >= length) then ()
      else
        begin
          let n = Char.code (String.get str_id position) in
          let n1 = n mod base in
          let n2 = (n - n1) / base in
          Buffer.add_char buf (char_of_hex_int n2);
          Buffer.add_char buf (char_of_hex_int n1);
          aux (position + 1)
        end
    in
    aux 0;
    Buffer.contents buf
  with
  | e -> print_endline (Printexc.to_string e); "0"

let yojson_of_bson bson =
  let tmp = Yojson.from_string (Bson.to_simple_json bson) in
  let rec check_id_format newlist = function
    | []                        -> newlist
    | ("_id", `String id)::tail ->
      check_id_format (("_id", `String (reformat_id id))::newlist) tail
    | head::tail                -> check_id_format (head::newlist) tail
  in
  match tmp with
  | `Assoc list -> `Assoc (check_id_format [] list)
  | _           -> tmp

(* Check tools *)

let check_exist coll object_id =
  let bson_query = Bson.add_element id_field object_id Bson.empty in
  lwt results = Mongo_lwt.find_q coll bson_query in
  let doc = MongoReply.get_document_list results in
  Lwt.return (List.length doc != 0)

(******************************************************************************
********************************* Content *************************************
*******************************************************************************)

let get_detail content_id =
  let bson_query = Bson.add_element id_field content_id Bson.empty in
  lwt coll = contents_coll in
  lwt results = Mongo_lwt.find_q_s_one coll  bson_query content_format in
  let res = MongoReply.get_document_list results in
  if List.length res < 1 then raise Not_found;
  let json = yojson_of_bson (List.hd res) in
  let id = to_string (member "_id" json) in
  let title = to_string (member "title" json) in
  let summary = to_string (member "summary" json) in
  let body = to_string (member "body" json) in
  Lwt.return (id_of_string id, title, summary, body)

let insert_content title summary body =
  let object_id = id_of_string (new_id ()) in
  let bson_title = Bson.create_string title in
  let bson_summary = Bson.create_string summary in
  let bson_body = Bson.create_string body in
  let content = Bson.add_element id_field object_id
    (Bson.add_element title_field bson_title
       (Bson.add_element summary_field bson_summary
          (Bson.add_element body_field bson_body Bson.empty)))
  in
  lwt coll = contents_coll in
  lwt () = Mongo_lwt.insert coll [content] in
  Lwt.return (object_id)

let update_content content_id ?title ?summary ?body () =
  lwt coll = contents_coll in
  lwt exist = check_exist coll content_id in
  if not exist then raise Not_found;
  let bson_query = Bson.add_element id_field content_id Bson.empty in
  let opt_add field element document =
    match element with
    | None      -> document
    | Some x    -> Bson.add_element field (Bson.create_string x) document
  in
  let content = opt_add title_field title
    (opt_add summary_field summary
       (opt_add body_field body Bson.empty))
  in
  if Bson.is_empty content
  then raise (Invalid_argument "At least one parameter have to be given.");
  Mongo_lwt.update_one coll (bson_query, content)

let delete_contents content_ids =
  let build_query object_id = Bson.add_element id_field object_id Bson.empty in
  let bson_query = MongoQueryOp.or_op (List.map build_query content_ids) in
  lwt coll = contents_coll in
  Mongo_lwt.delete_all coll bson_query
