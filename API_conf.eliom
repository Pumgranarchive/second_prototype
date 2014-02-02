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

(* Return values *)
let return_fail = 500
let return_ok = 200
let return_no_content = 204

(* Filter Values *)
let most_recent = 0
let most_used = 1
let most_view = 2

(* Tag types *)
let link_tag = "LINK"
let content_tag = "CONTENT"

let link_tag_str = "Link"
let content_tag_str = "Content"
