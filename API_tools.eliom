(*
** API tools
** This module provides some tools to help API implementation
*)

open Lwt

module Yojson = Yojson.Basic

(*** Static string for configuration / selection *)

(** content type return by API' services *)
(* let content_type = "application/json" *)

let uri_field = "uri"
let id_field = "_id"
let tagsid_field = "tags_uri"
let targetid_field = "target_uri"
let originid_field = "origin_uri"
let title_field = "title"
let summary_field = "summary"
let text_field = "text"
let body_field = "body"
let subject_field = "subject"
let type_field = "type"

let contents_ret_name = "contents"
let tags_ret_name = "tags"
let links_ret_name = "links"
let link_id_ret_name = "link_uri"
let content_ud_ret_name = "content_"
let content_id_ret_name = "content_uri"
let content_title_ret_name = "content_title"
let content_summary_ret_name = "content_summary"
let tagsid_ret_name = "tags_uri"
let linksid_ret_name = "links_uri"
let detail_ret_name = "links_uri"

(*** Cast tools *)

let json_of_ocsigen_string_stream input_type_opt ostream_opt =
  let _ = match input_type_opt with
    | None                                -> raise API_conf.(Pum_exc(return_not_found, "The content_type have to not be None."))
    | Some (("application", "json"), _)   -> ()
    | Some _                              -> raise API_conf.(Pum_exc(return_not_found, "The content_type have to be application/json."))
  in
  match ostream_opt with
  | None                -> raise API_conf.(Pum_exc(return_not_found, "The ocsigen stream have to not be None."))
  | Some ostream        ->
    let stream = Ocsigen_stream.get ostream in
    lwt str_json = Ocsigen_stream.string_of_stream 100000 stream in
    Lwt.return (Yojson.from_string str_json)

(*** Checking tools  *)

let check_empty_ocaml_list l original_value =
  if (l = [])
  then raise API_conf.(Pum_exc(return_not_found,
                               errstr_not_found original_value))
  else l

(*** Http tools  *)

let return_of_json yojson =
  yojson >>= (fun yj -> Lwt.return (Yojson.to_string yj, "application/json"))

let return_of_error str =
  str >>= (fun s -> Lwt.return (s, "application/json"))

(*** Manage return tools *)

(** Format the returned value *)
let format_ret ?param_name status ?error_str (param_value:Yojson.json) =
  let assoc_list = [] in
  let assoc_list_1 =
    match param_name with
    | None      -> assoc_list
    | Some name -> (name, param_value)::assoc_list
  in
  let assoc_list_2 =
    match error_str with
    | None      -> assoc_list_1
    | Some str  -> ("error", `String str)::assoc_list_1
  in
  `Assoc (("status", `Int status)::assoc_list_2)

(** [check_return func content_name]
    func: the function which return the result.
    content_name: the name of the content in the result.

    If the exception API_conf.Pum_exc (value, error_str) is fired,
    this data are use for the return.
    For any others exceptions, internal server error (500) is returned *)
let check_return ?(default_return=API_conf.return_ok) ?param_name func =
  let null_return () =
    Lwt.return (format_ret ?param_name API_conf.return_no_content `Null)
  in
  let error_return status error_str =
    Lwt.return (format_ret ?param_name status ~error_str `Null)
  in
  let valided_return ret =
    Lwt.return (format_ret ?param_name default_return (`List ret))
  in
  try_lwt
    lwt res = func () in
    match_lwt Lwt.return (res, param_name) with
    | `Null, Some _     -> null_return ()
    | `List [], _       -> null_return ()
    | `List ret, _      -> valided_return ret
    | ret, _            -> valided_return [ret]
  with
  | API_conf.Pum_exc (v, str)   ->
    begin
      print_endline ((string_of_int v) ^ ": " ^ str);
      error_return v str
    end
  | exc                         ->
    begin
      print_endline (Printexc.to_string exc);
      error_return API_conf.return_internal_error API_conf.errstr_internal_error
    end

let bad_request ?(error_value=API_conf.return_not_found) error_str =
  print_endline ((string_of_int error_value) ^ ": " ^ error_str);
  Lwt.return (Yojson.to_string (format_ret error_value ~error_str `Null))

let manage_bad_request aux =
  try_lwt
    aux ()
  with
  | API_conf.Pum_exc (_, str)   ->
    return_of_error (bad_request str)
  | exc                         ->
    print_endline (Printexc.to_string exc);
    return_of_error (bad_request ~error_value:API_conf.return_internal_error
                       API_conf.errstr_internal_error)
