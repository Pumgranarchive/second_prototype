let is_youtube_uri uri =
  let str_uri = Rdf_store.string_of_uri uri in
  try ignore (Youtube_http.get_video_id_from_url str_uri); true
  with _ -> false

let id_of_uri uri =
  Youtube_http.get_video_id_from_url (Rdf_store.string_of_uri uri)

let format (_, title, str_uri, summary, _) =
  let uri = Rdf_store.uri_of_string str_uri in
  uri, title, summary

let listenner uri =
  (* let str_uri = Rdf_store.string_of_uri uri in *)
  let id = id_of_uri uri in
  (* print_endline ""; *)
  (* Printf.printf "Youtube: Start: %s" str_uri; *)
  (* print_endline ""; *)
  lwt videos = Youtube_http.get_videos_from_ids [id] in
  (* Printf.printf "Youtube: Finished: %s" str_uri; *)
  (* print_endline "\n"; *)
  let new_data_list = List.map format videos in
  let new_data = List.hd new_data_list in
  Lwt.return new_data

lwt cash = Pcash.make "Youtube" listenner

let get_youtube_triple uris =
  lwt know_uris = Lwt_list.filter_p (Pcash.exists cash) uris in
  lwt unknow_uris = Lwt_list.filter_p (Pcash.not_exists cash) uris in
  let know_data = List.map (Pcash.get cash) know_uris in
  let ids = List.map id_of_uri unknow_uris in
  let add lwt_data =
    lwt data = lwt_data in
    let uri, title, summary = data in
    Pcash.add cash uri data
  in
  let lwt_format data = Lwt.return (format data) in
  lwt new_data =
      if List.length ids > 0
      then
        lwt videos = Youtube_http.get_videos_from_ids ids in
        let new_data = List.map lwt_format videos in
        lwt () = Lwt_list.iter_p add new_data in
        Lwt.return new_data
      else Lwt.return []
  in
  Lwt.return (know_data@new_data)

let get_youtube_detail uri =
  lwt results = get_youtube_triple [uri] in
  lwt uri, title, summary = List.hd results in
  lwt body = Preadability.get_readability_body uri in
  Lwt.return (uri, title, summary, body, true)
