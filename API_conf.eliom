(*
   API Configuration file
 *)

(* DB configuration *)
let db_url = "127.0.0.1"
let db_name = "pumgrana"
let db_port = 27017

let contents_coll_name = "contents"
let tags_coll_name = "tags"
let links_coll_name = "links"

(* API exception *)
exception Pum_exc of int * string

(* Return values *)
let return_ok = 200
let return_created = 201
let return_no_content = 204
let return_not_found = 404
let return_internal_error = 500

(* Error string values *)
let errstr_not_found str = "'" ^ str ^ "' is Not Found"
let errstr_not_objectid str = "'" ^ str ^ "' is not an valid object id."
let errstr_not_expected str = "'" ^ str ^ "' is not the expected value."
let errstr_exist str = "'" ^ str ^ "' already exist."
let errstr_internal_error = "Internal server error"


(* Filter Values *)
let most_recent = 0
let most_used = 1
let most_view = 2

(* Tag types *)
let link_tag = "LINK"
let content_tag = "CONTENT"

let link_tag_str = "Link"
let content_tag_str = "Content"
