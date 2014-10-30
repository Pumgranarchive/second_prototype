(******************************************************************************
***************************** Initialisation **********************************
*******************************************************************************)

module Yojson = Yojson.Basic

open Yojson.Util

let get_token () =
  let ic = open_in "token" in
  try
    let token = input_line ic in
    let () = close_in ic in
    token
  with e ->
    close_in_noerr ic;
    raise Not_found

let () = Readability.set_token (get_token ())

(******************************************************************************
********************************** Utils **************************************
*******************************************************************************)

let get_short_summary str =
  let length = String.length str in
  if length > 200
  then (String.sub str 0 200) ^ "..."
  else  str

let data_from_uri uri =
  let str_uri = Rdf_store.string_of_uri uri in
  let ruri = Rdf_uri.uri str_uri in
  lwt json =
      try_lwt Readability.get_parser ruri
      with e -> (print_endline (Printexc.to_string e); raise e)
  in
  let title = to_string (member "title" json) in
  let summary = get_short_summary (to_string (member "excerpt" json)) in
  lwt body =
      try_lwt Tidy.xhtml_of_html (to_string (member "content" json))
      with e -> Lwt.return ("<div><b>Tidy Error</b></div>")
  in
  Lwt.return (uri, title, summary, body, true)

lwt cash = Pcash.make "Readability" data_from_uri

(******************************************************************************
******************************** Funtions *************************************
*******************************************************************************)

let get_readability_data uris =
  let aux uri =
    lwt exist = Pcash.exists cash uri in
    if exist
    then Pcash.get cash uri
    else
      try_lwt
        (lwt data = data_from_uri uri in
         lwt () = Pcash.add cash uri data in
         Lwt.return data)
      with e ->
        (print_endline (Printexc.to_string e);
         let str_uri = Rdf_store.string_of_uri uri in
         Lwt.return (uri, str_uri, "Readability error", "", true))
  in
  List.map aux uris

let get_readability_detail uri =
  List.hd (get_readability_data [uri])

let get_readability_triple uris =
  let results = get_readability_data uris in
  let format data =
    lwt uri, title, summary, body, v_external = data in
    Lwt.return (uri, title, summary)
  in
  Lwt.return (List.map format results)

let get_readability_body uri =
  let result = List.hd (get_readability_data [uri]) in
  let format data =
    lwt uri, title, summary, body, v_external = data in
    Lwt.return body
  in
  format result
