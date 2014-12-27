(**
   {b API tools -
      This module provides some tools to help API implementation}
*)


(** {6 Selection string } *)


(** Value of the uri field in the database *)
val uri_field: string

(** Value of the id field in the database *)
val id_field: string

(** Value of the tags_id field in the database *)
val tagsid_field: string

(** Value of the targetid field in the database *)
val targetid_field: string

(** Value of the originid field in the database *)
val originid_field: string

(** Value of the title field in the database *)
val title_field: string

(** Value of the summary field in the database *)
val summary_field: string

(** Value of the text field in the database *)
val text_field: string

(** Value of the body field in the database *)
val body_field: string

(** Value of the subject field in the database *)
val subject_field: string

(** Value of the type field in the database *)
val type_field: string

(** External field name in the API json return *)
val external_field: string


(** {6 Configuration string } *)

(** Contents field name in the API json return *)
val contents_ret_name: string

(** Tags field name in the API json return *)
val tags_ret_name: string

(** Links field name in the API json return *)
val links_ret_name: string

(** Link_id field name in the API json return *)
val link_id_ret_name: string

(** Content_ field name in the API json return *)
val content_ud_ret_name: string

(** Content_id field name in the API json return *)
val content_id_ret_name: string

(** Content_title field name in the API json return *)
val content_title_ret_name: string

(** Content_summary field name in the API json return *)
val content_summary_ret_name: string

(** Tags_id field name in the API json return *)
val tagsid_ret_name: string

(** Links_id field name in the API json return *)
val linksid_ret_name: string

(** Detail field name in the API json return *)
val detail_ret_name: string

(** {6 Cast tools } *)

val json_of_ocsigen_string_stream:
  ((string * string) * (string * string) list) option->
  string Ocsigen_stream.t option ->
  Yojson.Basic.json Lwt.t

(** {6 Checking tools  } *)

(** If the list is empty, raise an not_found excepetion with the given value,
    else return the given list. *)
val check_empty_ocaml_list: 'a list -> string -> 'a list

(** {6 Http tools } *)

(** Transform Json of API return *)
val return_of_json : Yojson.Basic.json Lwt.t -> (string * string) Lwt.t

(** Transform Json of API error *)
val return_of_error : string Lwt.t -> (string * string) Lwt.t

(** {6 Manage return tools } *)

(** Format the returned value *)
val format_ret: ?param_name:string -> int -> ?error_str:string ->
  Yojson.Basic.json -> Yojson.Basic.json

(** [check_return ?default_return ?param_name func]
    default_return: specify the default return, It is return_ok by default.
    param_name: the name of the returned data.
    func: the function which return the result.

    If the exception API_conf.Pum_exc (error_value, error_str) is raised,
    the exception's data are use to build the return.
    For any others exceptions, internal server error (500) is returned *)
val check_return: ?default_return:int -> ?param_name:string ->
  (unit -> Yojson.Basic.json Lwt.t) -> Yojson.Basic.json Lwt.t

(** Use to directly return bad request in json string in register layout / unit *)
val bad_request: ?error_value:int -> string -> string Lwt.t

(** [manage_bad_request fun]
    call the [fun], catch the exception and write it in the returned string *)
val manage_bad_request: (unit -> (string * string) Lwt.t) ->
  (string * string) Lwt.t
