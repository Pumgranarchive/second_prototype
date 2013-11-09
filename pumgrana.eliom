{shared{
  open Eliom_lib
  open Eliom_content
}}

module Pumgrana_app =
  Eliom_registration.App (
    struct
      let application_name = "pumgrana"
    end)

(* The main service is used to make some test *)
let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let () =
  Pumgrana_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"test_api"
           ~css:[["css";"test_api.css"]]
           Html5.F.(body [
             h2 [pcdata "Pumgrana"];
             p [a ~service:API_services.contents [pcdata "get link test"]
                   (Some 1, Some (["1"; "2"]))]
           ])))
