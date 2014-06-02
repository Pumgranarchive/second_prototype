open Batteries

exception Invalid_id of string

type id = string

let id_length = 24

let id_of_string str =
  let regex = Str.regexp "^[0-9a-fA-F]*$" in
  let length = String.length str in
  let hex = Str.string_match regex str 0 in
  if length != id_length || hex == false then raise (Invalid_id str);
  str

let string_of_id id = id

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
