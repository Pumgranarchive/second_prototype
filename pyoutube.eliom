module OrderedUri =
  struct
    type t = Rdf_store.uri
    let compare = Rdf_store.compare_uri
  end

module Map = Map.Make(OrderedUri)

let cash = Pcash.new_cash ()

let is_youtube_uri uri =
  let str_uri = Rdf_store.string_of_uri uri in
  try ignore (Youtube_http.get_id_from_url str_uri); true
  with _ -> false

let get_youtube_triple uris =
  let know_uris = List.filter (Pcash.exists cash) uris in
  let know_data = List.map (Pcash.get cash) know_uris in
  let unknow_uris = List.filter (Pcash.not_exists cash) uris in
  let id_of_uri uri = Youtube_http.get_id_from_url (Rdf_store.string_of_uri uri)
  in
  let ids = List.map id_of_uri unknow_uris in
  lwt videos =
      if List.length ids > 0
      then Youtube_http.get_videos_from_ids ids
      else Lwt.return []
  in
  let format (_, title, str_uri, summary, _) =
    let uri = Rdf_store.uri_of_string str_uri in
    let data = (uri, title, summary) in
    Pcash.save cash uri data;
    data
  in
  Lwt.return (know_data@(List.map format videos))

let get_youtube_detail uri =
  lwt results = get_youtube_triple [uri] in
  let (uri, title, summary) = List.hd results in
  lwt body = Preadability.get_readability_body uri in
  Lwt.return (uri, title, summary, body, true)
