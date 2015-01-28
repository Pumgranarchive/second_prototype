type 'a listenner = (Rdf_store.uri -> 'a Lwt.t)
type 'a t = (float * 'a) Ocsipersist.table * 'a listenner * (float * string) list ref

(******************************************************************************
********************************** Utils **************************************
*******************************************************************************)

let max_length = 600

(* 4 semaines *)
let life_time = 60. *. 60. *. 24. *. 7. *. 4.

(** Return now + life time  *)
let calc_deadline () = (Unix.time ()) +. life_time

module History =
struct

  (** Make history from table  *)
  let make table =
    let now = Unix.time () in
    let make key value history =
      Lwt.return ((now, key)::history)
    in
    lwt history = Ocsipersist.fold_table make table [] in
    Lwt.return (ref history)

  (** Update the access time on the given key  *)
  let access table history key =
    let open Utils.List in
    let now = Unix.time () in
    let remove_duplicate k new_history (time, key) =
      if (String.compare k key) == 0
      then new_history
      else new_history@@(time, key)
    in
    let cleaned_history = List.fold_left (remove_duplicate key) [] !history in
    history := cleaned_history@@(now, key);
    Lwt.return ()

  (** Pull the oldest access key to the table *)
  let pull table history =
    let (time, oldest) = List.hd !history in
    Lwt.async (fun () -> Ocsipersist.remove table oldest);
    history := List.tl !history;
    Lwt.return ()

end

(** Limit the table size  *)
let limit table history =
  lwt length = Ocsipersist.length table in
  if length > max_length
  then History.pull table history
  else Lwt.return ()

(** Refresh data of the given key *)
let refresh table sublistenner str_key =
  let key = Rdf_store.uri_of_string str_key in
  lwt new_data = sublistenner key in
  let deadline = calc_deadline () in
  lwt ()  = Ocsipersist.add table str_key (deadline, new_data) in
  Lwt.return deadline

(** Secure sleep *)
let sleep time =
  if time > 0.
  then Lwt_unix.sleep time
  else Lwt.return ()

(** Wait before launch refresh at end of life  *)
let rec listenner table sublistenner str_key deadline () =
  let sleeping_time = deadline -. (Unix.time ()) in
  lwt () = sleep sleeping_time in
  try_lwt
    let now = Unix.time () in
    lwt dl, data = Ocsipersist.find table str_key in
    lwt new_deadline = if dl <= now
      then refresh table sublistenner str_key
      else Lwt.return dl
    in
    let () = Lwt.async (listenner table sublistenner str_key new_deadline) in
    Lwt.return ()
  with _ -> Lwt.return ()

(** Add deadline and launch a listenner on the given key. *)
let new_deadline table sublistenner str_key =
  let deadline = calc_deadline () in
  let () = Lwt.async (listenner table sublistenner str_key deadline) in
  deadline

(** Launch listenner on all element of the table. *)
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
  let table = Ocsipersist.open_table name  in
  lwt () = assign_listenner table sublistenner in
  lwt history = History.make table in
  Lwt.return (table, sublistenner, history)

let add (table, sublistenner, history) key data =
  let str_key = Rdf_store.string_of_uri key in
  let deadline = new_deadline table sublistenner str_key in
  lwt () = Ocsipersist.add table str_key (deadline, data) in
  lwt () = History.access table history str_key in
  lwt () = limit table history in
  Lwt.return ()

let get (table, sublistenner, history) key =
  let str_key = Rdf_store.string_of_uri key in
  lwt _, data = Ocsipersist.find table str_key in
  lwt () = History.access table history str_key in
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
