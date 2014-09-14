module OrderedUri =
  struct
    type t = Rdf_store.uri
    let compare = Rdf_store.compare_uri
  end

module Map = Map.Make(OrderedUri)

let store =
  Ocsipersist.open_store "PumCash"

let new_cash name =
  let default () = Map.empty in
  Ocsipersist.make_persistent_lazy ~store ~name ~default

let save cash key data =
  lwt cash_map = Ocsipersist.get cash in
  let new_cash_map = Map.add key data cash_map in
  Ocsipersist.set cash new_cash_map

let compare = OrderedUri.compare

let exists cash key =
  lwt cash_map = Ocsipersist.get cash in
  Lwt.return (Map.exists (fun k v -> compare k key = 0) cash_map)

let not_exists cash key =
  lwt cash_map = Ocsipersist.get cash in
  Lwt.return (Map.for_all (fun k v -> compare k key != 0) cash_map)

let get cash key =
  lwt cash_map = Ocsipersist.get cash in
  Lwt.return (Map.find key cash_map)

(* module OrderedUri = *)
(*   struct *)
(*     type t = Rdf_store.uri *)
(*     let compare = Rdf_store.compare_uri *)
(*   end *)

(* module Map = Map.Make(OrderedUri) *)

(* let new_cash () = *)
(*   ref Map.empty *)

(* let save cash key data = *)
(*   cash := Map.add key data !cash *)

(* let compare = OrderedUri.compare *)

(* let exists cash key = *)
(*   Map.exists (fun k v -> compare k key = 0) !cash *)

(* let not_exists cash key = *)
(*   Map.for_all (fun k v -> compare k key != 0) !cash *)

(* let get cash key = *)
(*   Map.find key !cash *)
