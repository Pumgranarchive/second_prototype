(******************************************************************************
***************************** Initialisation **********************************
*******************************************************************************)

module OrderedUri =
  struct
    type t = Rdf_store.uri
    let compare = Rdf_store.compare_uri
  end

module Map = Map.Make(OrderedUri)

type 'a listenner = (Rdf_store.uri -> 'a Lwt.t)
type 'a t = (float * 'a) Map.t Ocsipersist.t * 'a listenner

let store = Ocsipersist.open_store "PumCash"

(******************************************************************************
********************************** Utils **************************************
*******************************************************************************)

let max_length = 1000

let life_time = 60. *. 60. *. 24.

let compare = OrderedUri.compare

let get_length map =
  Map.fold (fun k v n -> n + 1) map 0

let pull map =
  let select k = function
    | None    -> Some k
    | Some k' -> Some k'
  in
  match Map.fold (fun k v first -> select k first) map None with
  | Some k -> Map.remove k map
  | None   -> map

let calc_deadline () = (Unix.time ()) +. life_time

let refresh cash sublistenner key =
  let deadline = calc_deadline () in
  lwt new_data = sublistenner key in
  lwt cash_map = Ocsipersist.get cash in
  let embedded = (deadline, new_data) in
  let new_cash_map = Map.add key embedded cash_map in
  Ocsipersist.set cash new_cash_map

let rec listenner cash sublistenner key deadline () =
  let sleeping_time = deadline -. (Unix.time ()) in
  lwt () = if sleeping_time > 0.
    then Lwt_unix.sleep sleeping_time
    else Lwt.return ()
  in
  lwt cash_map = Ocsipersist.get cash in
  try
    let dl, data = Map.find key cash_map in
    let now = Unix.time () in
    lwt () = if dl <= now
      then refresh cash sublistenner key
      else Lwt.return ()
    in
    let () = Lwt.async (listenner cash sublistenner key deadline) in
    Lwt.return ()
  with Not_found -> Lwt.return ()

let new_deadline cash sublistenner key =
  let deadline = calc_deadline () in
  let () = Lwt.async (listenner cash sublistenner key deadline) in
  deadline

let assign_listenner cash sublistenner =
  let assign key (deadline, data) =
    Lwt.async (listenner cash sublistenner key deadline)
  in
  lwt cash_map = Ocsipersist.get cash in
  let () = Map.iter assign cash_map in
  Lwt.return ()

(******************************************************************************
******************************** Funtions *************************************
*******************************************************************************)

let make name sublistenner =
  let default () = Map.empty in
  lwt cash = Ocsipersist.make_persistent_lazy ~store ~name ~default in
  lwt () = assign_listenner cash sublistenner in
  Lwt.return (cash, sublistenner)

let add (cash, sublistenner) key data =
  lwt cash_map = Ocsipersist.get cash in
  let deadline = new_deadline cash sublistenner key in
  let embedded = (deadline, data) in
  let new_cash_map = Map.add key embedded cash_map in
  let length = get_length cash_map in
  let limited_cash_map =
    if length > max_length
    then pull new_cash_map
    else new_cash_map
  in
  Ocsipersist.set cash limited_cash_map

let exists (cash, sublistenner) key =
  lwt cash_map = Ocsipersist.get cash in
  Lwt.return (Map.exists (fun k v -> compare k key = 0) cash_map)

let not_exists (cash, sublistenner) key =
  lwt cash_map = Ocsipersist.get cash in
  Lwt.return (Map.for_all (fun k v -> compare k key != 0) cash_map)

let get (cash, sublistenner) key =
  lwt cash_map = Ocsipersist.get cash in
  let _, data = Map.find key cash_map in
  Lwt.return data
