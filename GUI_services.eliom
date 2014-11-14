(*
  GUI Services
  This module implement services of GUI
*)

(* Home service *)
let home =
  Eliom_service.App.service
    ~path:[]
    ~get_params:Eliom_parameter.unit
    ()

(* Contents list service *)
let contents =
  Eliom_service.Http.service
    ~path:["contents"]
    ~get_params:Eliom_parameter.(suffix (opt (string "s1")))
    ()

(* Contents list service with two parameters  *)
(* let contents_two = *)
(*   Eliom_service.Http.service *)
(*     ~path:["contents"] *)
(*     ~get_params:Eliom_parameter.(suffix (opt (string "filter") ** *)
(*                                            opt (string "research"))) *)
(*     () *)

(* Content detail view service *)
let content_detail =
  Eliom_service.Http.service
    ~path:["view"]
    ~get_params:Eliom_parameter.(suffix (string "content_uri"))
    ()

(* Content detail view service *)
let content_detail_by_platform =
  Eliom_service.Http.service
    ~path:["view"]
    ~get_params:Eliom_parameter.(suffix (string "plateform_name" **
                                           string "content_name"))
    ()

(* Update content detail *)
(* let content_update_service = *)
(*   Eliom_service.Http.service *)
(*     ~path:["content"; "update"] *)
(*     ~get_params:Eliom_parameter.(suffix (string "content_uri")) *)
(*     () *)

(* Insert content detail *)
(* let content_insert_service = *)
(*   Eliom_service.Http.service *)
(*     ~path:["content"; "insert"] *)
(*     ~get_params:Eliom_parameter.unit *)
(*     () *)

(* Insert link *)
(* let link_insert_service = *)
(*   Eliom_service.Http.service *)
(*     ~path:["link"; "insert"] *)
(*     ~get_params:Eliom_parameter.(suffix (opt (string "origin_uri") ** *)
(*                                            opt (string "target_uri"))) *)
(*     () *)

(* Update link *)
(* let link_update_service = *)
(*   Eliom_service.Http.service *)
(*     ~path:["link"; "update"] *)
(*     ~get_params:Eliom_parameter.(suffix (string "link_uri")) *)
(*     () *)
