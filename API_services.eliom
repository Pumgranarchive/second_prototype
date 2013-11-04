(*
** API Services
** This module registre services of API
*)

module Yj = Yojson.Safe

(* Tmp data *)
let zib_content = (2, "Zibaboue", "Un super texte")
let content_list = [zib_content; (3, "Dieudonne", "Revolution!")]

(* Services *)
let _ =
  Eliom_registration.String.register_service
    ~content_type:APIF.content_type
    ~path:["content"; "detail"]
    ~get_params:Eliom_parameter.(int "content_id")
    (fun content_id () ->
      Lwt.return (Yj.to_string (APIF.detail 42 zib_content),
                  APIF.content_type))

let _ =
  Eliom_registration.String.register_service
    ~content_type:APIF.content_type
    ~path:["content"; "detail_by_link"]
    ~get_params:Eliom_parameter.(int "link_id")
    (fun link_id () ->
      Lwt.return (Yj.to_string (APIF.detail 42 zib_content),
                  APIF.content_type))

let contents =
  Eliom_service.Unsafe.service
    ~path:["content"; "list_content"]
    ~get_params:Eliom_parameter.(opt (int "filter") **
                                   opt (list "tags" (int "id")))
    ()

let _ =
  Eliom_registration.String.register
    ~content_type:APIF.content_type
    ~service:contents
    (fun (filter, tags_id) () ->
      Lwt.return (Yj.to_string (APIF.contents 42 content_list),
                  APIF.content_type))
