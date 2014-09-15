lwt cash = Pcash.new_cash "Youtube"

let is_youtube_uri uri =
  let str_uri = Rdf_store.string_of_uri uri in
  try ignore (Youtube_http.get_video_id_from_url str_uri); true
  with _ -> false

let get_youtube_triple uris =
  lwt know_uris = Lwt_list.filter_p (Pcash.exists cash) uris in
  lwt know_data = Lwt_list.map_s (Pcash.get cash) know_uris in
  lwt unknow_uris = Lwt_list.filter_p (Pcash.not_exists cash) uris in
  let id_of_uri uri =
    Youtube_http.get_video_id_from_url (Rdf_store.string_of_uri uri)
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
    lwt () = Pcash.save cash uri data in
    Lwt.return data
  in
  lwt new_data = Lwt_list.map_s format videos in
  Lwt.return (know_data@new_data)

let get_youtube_detail uri =
  lwt results = get_youtube_triple [uri] in
  let (uri, title, summary) = List.hd results in
  lwt body = Preadability.get_readability_body uri in
  Lwt.return (uri, title, summary, body, true)
