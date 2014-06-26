(*
  GUI Services
  This module implement services of GUI
*)

(* Starting page *)
let starting_service =
  Eliom_service.App.service
    ~path:[]
    ~get_params:Eliom_parameter.unit
    ()

(* Home page without parameters *)
(* We need to do this because parameters does not allow to match
   to the source url.*)
let home_service_without =
  Eliom_service.Http.service
    ~path:["home"]
    ~get_params:Eliom_parameter.(suffix (opt (string "filter")))
    ()

(* Home page with parameters *)
let home_service =
  Eliom_service.Http.service
    ~path:["home"]
    ~get_params:Eliom_parameter.(suffix (opt (string "filter") **
                                           opt (list "tags" (string "uri"))))
    ()

(* Content detail *)
let content_detail_service =
  Eliom_service.Http.service
    ~path:["content"; "detail"]
    ~get_params:Eliom_parameter.(suffix (string "content_uri"))
    ()

(* Update content detail *)
let content_update_service =
  Eliom_service.Http.service
    ~path:["content"; "update"]
    ~get_params:Eliom_parameter.(suffix (string "content_uri"))
    ()

(* Insert content detail *)
let content_insert_service =
  Eliom_service.Http.service
    ~path:["content"; "insert"]
    ~get_params:Eliom_parameter.unit
    ()

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
