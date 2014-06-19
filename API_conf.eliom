(**
**   API Configuration file
*)

(** {6 API exception} *)
exception Pum_exc of int * string

(** {6 Return values} *)
let return_ok = 200
let return_created = 201
let return_no_content = 204
let return_not_found = 404
let return_internal_error = 500

(** {6 Error string functions / values} *)
let errstr_not_found str = "'" ^ str ^ "' is Not Found"
let errstr_not_objectid str = "'" ^ str ^ "' is not an valid object id."
let errstr_not_expected str = "'" ^ str ^ "' is not the expected value."
let errstr_exist str = "'" ^ str ^ "' already exist."
let errstr_internal_error = "Internal server error"

{shared{

(** {6 Tag types} *)
let link_tag = "LINK"
let content_tag = "CONTENT"

let link_tag_str = "Link"
let content_tag_str = "Content"

}}
