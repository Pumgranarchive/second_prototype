module OrderedUri =
  struct
    type t = Rdf_store.uri
    let compare = Rdf_store.compare_uri
  end

module Map = Map.Make(OrderedUri)

let new_cash () =
  ref Map.empty

let save cash key data =
  cash := Map.add key data !cash

let compare = OrderedUri.compare

let exists cash key =
  Map.exists (fun k v -> compare k key = 0) !cash

let not_exists cash key =
  Map.for_all (fun k v -> compare k key != 0) !cash

let get cash key =
  Map.find key !cash
