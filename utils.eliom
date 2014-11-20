(* module Bot = *)
(* struct *)

(*   let call str_uri = *)
(*     let path = "../pum_bot/pum_bot -n " in *)
(*     Lwt.async (fun () -> *)
(*       let cmd = Sys.command (path^" -n \""^str_uri^"\" > /dev/null 2>&1 &") in *)
(*       Lwt.return ()) *)

(* end *)

module Str =
struct

  include Str

  let regexps = List.map Str.regexp

  let contains regexp str =
    try ignore (Str.search_forward regexp str 0); true
    with Not_found -> false

  let exists regexps str =
    List.exists (fun r -> contains r str) regexps

end

module List =
struct

  include List

  let map_exc func list =
    let rec aux blist = function
      | []   -> blist
      | h::t ->
        try
          let e = func h in
          aux (e::blist) t
        with e ->
          (print_endline (Printexc.to_string e);
           aux blist t)
    in
    List.rev (aux [] list)

  let limit size l =
    let rec aux bl s = function
      | []   -> bl
      | h::t ->
        if s >= size then bl else
          aux (h::bl) (s + 1) t
    in
    List.rev (aux [] 0 l)

  let merge func l1 l2 =
    let add list e =
      if List.exists (func e) list
      then list
      else e::list
    in
    List.fold_left add l1 l2

end

module Lwt_list =
struct

  include Lwt_list

  let hd lwt_l =
    lwt l = lwt_l in
    Lwt.return (List.hd l)

  let filter f lwt_l =
    lwt l = lwt_l in
    Lwt.return (List.filter f l)

  let map f lwt_l =
    lwt l = lwt_l in
    Lwt.return (List.map f l)

  let wait x = x

  (** Map list (not Lwt) which catch all exceptions *)
  let map_exc func list =
    let rec aux blist = function
      | []   -> Lwt.return blist
      | h::t ->
        try_lwt
          let e = func h in
          aux (e::blist) t
        with e ->
          let () = print_endline (Printexc.to_string e) in
          aux blist t
    in
    lwt res = aux [] list in
    Lwt.return (List.rev res)

  (** Lwt Map list in synchronised manner which catch all exceptions *)
  let map_s_exc func list =
    let rec aux blist = function
      | []   -> Lwt.return blist
      | h::t ->
        try_lwt
          lwt e = func h in
          aux (e::blist) t
        with e ->
          let () = print_endline (Printexc.to_string e) in
          aux blist t
    in
    lwt res = aux [] list in
    Lwt.return (List.rev res)

  (** Lwt Iter list in synchronised manner which catch all exceptions *)
  let iter_s_exc func list =
    let rec aux = function
      | []   -> Lwt.return ()
      | h::t ->
        lwt () =
          try_lwt func h
          with e -> (print_endline (Printexc.to_string e); Lwt.return ())
        in
        aux t
    in
    aux list


end

module Opt =
struct

  let get_not_null default = function
    | None -> default
    | Some x -> x

end

{client{

open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

module Client =
  struct

    (** Remove all child from the given dom element.  *)
    let rec remove_all_child dom =
      let c = dom##firstChild in
      Js.Opt.iter c
        (fun c ->
          let () = Dom.removeChild dom c in
          remove_all_child dom)

    (** Append all div element of given list in the dom element. *)
    let rec append_all dom_of_elm dom = function
      | []          -> ()
      | block::tail ->
        let dom_block = dom_of_elm block in
        let () = Dom.appendChild dom dom_block in
        append_all dom_of_elm dom tail

    let get_research input =
        let dom_input = To_dom.of_input input in
        let value = Js.to_string (dom_input##value) in
        if String.length value > 0 then value else " "

    let refresh_list ~make_request ~elm_of_result dom_of_elm input div_list =
      let dom_input = To_dom.of_input input in
      let dom_list = To_dom.of_div div_list in
      let refresh_html () =
        let display result =
          let elms = elm_of_result result in
          let () = remove_all_child dom_list in
          append_all dom_of_elm dom_list elms
        in
        lwt answer = make_request () in
        Lwt.return (display answer)
      in
      Lwt.async (fun () ->
        Lwt_js_events.inputs dom_input
          (fun _ _ -> refresh_html ()))

  end

}}
