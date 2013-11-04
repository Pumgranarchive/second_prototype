(*
** API Format
** This module provides some functions to easily format API services' return
*)

module Yj = Yojson.Safe

let content_type = "JSON"

let content ((id:int), (title:string), (text:string)) =
  `Assoc [("id", `Int id);
          ("title", `String title);
          ("text", `String text)]

let flist func l =
  let rec aux nl = function
  | h::t        -> aux ((func h)::nl) t
  | []          -> nl
  in `List (List.rev (aux [] l))

let content_list = flist content

let tag ((id:int), (subject:string)) =
    `Assoc [("id", `Int id);
            ("subject", `String subject)]

let tag_list = flist tag

let return (func:'a->Yj.json) param_name status param_func =
  `Assoc [("status", `Int status);
          (param_name, func param_func) ]

let detail = return content "content"

let contents = return content_list "contents"
