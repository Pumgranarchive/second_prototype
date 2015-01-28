(******************************************************************************
***************************** Initialisation **********************************
*******************************************************************************)
module Yojson = Yojson.Basic

open Yojson
open Yojson.Util
open Utils

exception Preadability of string

(******************************************************************************
********************************** Utils **************************************
*******************************************************************************)

let get_token () =
  let ic = open_in "token" in
  try
    let token = input_line ic in
    let () = close_in ic in
    token
  with e ->
    close_in_noerr ic;
    raise Not_found

let () = Readability_http.set_token (get_token ())

let get_short_summary str =
  let length = String.length str in
  if length > 200
  then (String.sub str 0 200) ^ "..."
  else  str

let log suri exc detail =
  let title = Log.log "readability.error" suri exc detail in
  raise (Preadability title)

let data_from_uri uri =
  let str_uri = Rdf_store.string_of_uri uri in
  let ruri = Rdf_uri.uri str_uri in
  lwt json =
      try_lwt Readability_http.get_parser ruri
      with exc -> log str_uri exc ""
  in
  try_lwt
    let title = to_string (member "title" json) in
    let summary = get_short_summary (to_string (member "excerpt" json)) in
    let content = to_string (member "content" json) in
    lwt body = Tidy.xhtml_of_html content in
    Lwt.return (uri, title, summary, body, true)
  with exc -> log str_uri exc (pretty_to_string json)

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
      lwt data = data_from_uri uri in
      lwt () = Pcash.add cash uri data in
      Lwt.return data
  in
  Lwt_list.map_exc aux uris

let get_readability_detail uri =
  lwt list = get_readability_data [uri] in
  List.hd list

let get_readability_triple uris =
  lwt results = get_readability_data uris in
  let format data =
    lwt uri, title, summary, body, v_external = data in
    Lwt.return (uri, title, summary)
  in
  Lwt.return (List.map format results)

let get_readability_body uri =
  lwt list = get_readability_data [uri] in
  lwt uri, title, summary, body, v_external = List.hd list in
  Lwt.return body
