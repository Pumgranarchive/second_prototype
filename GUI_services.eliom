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

(* Starting page with parameter *)
(* We need to do this because parameters does not allow to match
   to the source url.*)
let main_service =
  Eliom_service.Http.service
    ~path:[]
    ~get_params:Eliom_parameter.(suffix (opt (string "filter") **
                                           opt (list "tags" (string "id"))))
    ()

(* Content detail *)
let content_detail_service =
  Eliom_service.Http.service
    ~path:["content"]
    ~get_params:Eliom_parameter.(suffix (string "content_id"))
    ()
