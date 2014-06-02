exception Invalid_id of string

type id = string

let id_of_string str =
  let regex = Str.regexp "^[0-9a-fA-F]*$" in
  let length = String.length str in
  let hex = Str.string_match regex str 0 in
  if length != 24 || hex == false then raise (Invalid_id str);
  str

let string_of_id id = id
