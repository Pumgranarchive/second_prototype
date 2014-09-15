(******************************************************************************
***************************** Initialisation **********************************
*******************************************************************************)


module OrderedUri =
  struct
    type t = Rdf_store.uri
    let compare = Rdf_store.compare_uri
  end

module Map = Map.Make(OrderedUri)

let store =
  Ocsipersist.open_store "PumCash"

(******************************************************************************
********************************** Utils **************************************
*******************************************************************************)

let max_length = 1000

let life_time = 60. *. 60. *. 24. *. 7.

let compare = OrderedUri.compare

let new_cash name =
  let default () = Map.empty in
  Ocsipersist.make_persistent_lazy ~store ~name ~default

let get_length map =
  Map.fold (fun k v n -> n + 1) map 0

let pull map =
  let select k = function
    | None   -> Some k
    | Some v -> Some v
  in
  match Map.fold (fun k v first -> select k first) map None with
  | Some v -> Map.remove v map
  | None   -> map

let new_deadline cash key =
  let deadline = (Unix.time ()) +. life_time in
  let rec listenner () =
    lwt () = Lwt_unix.sleep (deadline -. (Unix.time ())) in
    lwt cash_map = Ocsipersist.get cash in
    let time, _ = Map.find key cash_map in
    let new_cash_map =
      if time <= (Unix.time ())
      then Map.remove key cash_map
      else (Lwt.async listenner; cash_map)
    in
    Ocsipersist.set cash new_cash_map
  in
  let () = Lwt.async listenner in
  deadline


(******************************************************************************
******************************** Funtions *************************************
*******************************************************************************)

let save cash key data =
  lwt cash_map = Ocsipersist.get cash in
  let deadline = new_deadline cash key in
  let embedded = (deadline, data) in
  let new_cash_map = Map.add key embedded cash_map in
  let length = get_length cash_map in
  let limited_cash_map =
    if length > max_length
    then pull new_cash_map
    else new_cash_map
  in
  Ocsipersist.set cash limited_cash_map

let exists cash key =
  lwt cash_map = Ocsipersist.get cash in
  Lwt.return (Map.exists (fun k v -> compare k key = 0) cash_map)

let not_exists cash key =
  lwt cash_map = Ocsipersist.get cash in
  Lwt.return (Map.for_all (fun k v -> compare k key != 0) cash_map)

let get cash key =
  lwt cash_map = Ocsipersist.get cash in
  let _, data = Map.find key cash_map in
  Lwt.return data
