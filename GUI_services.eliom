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
                                           opt (list "tags" (string "id"))))
    ()

(* Content detail *)
let content_detail_service =
  Eliom_service.Http.service
    ~path:["content"; "detail"]
    ~get_params:Eliom_parameter.(suffix (string "content_id"))
    ()

(* Content detail *)
let content_update_service =
  Eliom_service.Http.service
    ~path:["content"; "update"]
    ~get_params:Eliom_parameter.(suffix (string "content_id"))
    ()
