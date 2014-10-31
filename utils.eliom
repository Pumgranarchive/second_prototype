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

end

module Lwt_list =
struct

  include Lwt_list

  let wait x = x

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
