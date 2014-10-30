type 'a listenner = (Rdf_store.uri -> 'a Lwt.t)
type 'a t = (float * 'a) Ocsipersist.table * 'a listenner

(******************************************************************************
********************************** Utils **************************************
*******************************************************************************)

let max_length = 1000

(* 24 heures *)
let life_time = 60. *. 60. *. 24.

let calc_deadline () = (Unix.time ()) +. life_time

let pull table =
  let action k v is_removed =
    if is_removed
    then Lwt.return true
    else
      lwt () = Ocsipersist.remove table k in
      Lwt.return true
  in
  lwt _ = Ocsipersist.fold_table action table false in
  Lwt.return ()

let limit table =
  lwt length = Ocsipersist.length table in
  if length > max_length
  then pull table
  else Lwt.return ()

let refresh table sublistenner str_key =
  let key = Rdf_store.uri_of_string str_key in
  lwt new_data = sublistenner key in
  let deadline = calc_deadline () in
  print_endline "";
  Printf.printf "Ocsipersist.add: before: %s" str_key;
  print_endline "";
  lwt ()  = Ocsipersist.add table str_key (deadline, new_data) in
  print_endline "Ocsipersist.add: after\n";
  Lwt.return ()

let rec listenner table sublistenner str_key deadline () =
  let sleeping_time = deadline -. (Unix.time ()) in
  lwt () = if sleeping_time > 0.
    then Lwt_unix.sleep sleeping_time
    else Lwt.return ()
  in
  try
    lwt dl, data = Ocsipersist.find table str_key in
    let now = Unix.time () in
    lwt () = if dl <= now
      then refresh table sublistenner str_key
      else Lwt.return ()
    in
    let () = Lwt.async (listenner table sublistenner str_key deadline) in
    Lwt.return ()
  with Not_found -> Lwt.return ()

let new_deadline table sublistenner str_key =
  let deadline = calc_deadline () in
  let () = Lwt.async (listenner table sublistenner str_key deadline) in
  deadline

let assign_listenner table sublistenner =
  let assign str_key (deadline, data) =
    let () = Lwt.async (listenner table sublistenner str_key deadline) in
    Lwt.return ()
  in
  Ocsipersist.iter_table assign table

(******************************************************************************
******************************** Funtions *************************************
*******************************************************************************)

let make name sublistenner =
  print_endline "";
  Printf.printf "Make: before: %s" name;
  print_endline "";
  let table = Ocsipersist.open_table name  in
  lwt () = assign_listenner table sublistenner in
  print_endline "Make: after\n";
  Lwt.return (table, sublistenner)

let add (table, sublistenner) key data =
  print_endline "\n";
  let str_key = Rdf_store.string_of_uri key in
  Printf.printf "Add: enter : %s" str_key;
  print_endline "";
  let deadline = new_deadline table sublistenner str_key in
  lwt () = Ocsipersist.add table str_key (deadline, data) in
  lwt () = limit table in
  print_endline "Add: out\n";
  Lwt.return ()

let get (table, sublistenner) key =
  print_endline "\nGet: enter\n";
  let str_key = Rdf_store.string_of_uri key in
  lwt _, data = Ocsipersist.find table str_key in
  print_endline "Get: out\n";
  Lwt.return data

let exists pcash key =
  try_lwt
    lwt _ = get pcash key in
    Lwt.return true
  with Not_found -> Lwt.return false

let not_exists pcash key =
  try_lwt
    lwt _ = get pcash key in
    Lwt.return false
  with Not_found -> Lwt.return true
