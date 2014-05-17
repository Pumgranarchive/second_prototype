{server{

(**
   {b API tools -
      This module provides some tools to help API implementation}
*)


(** {6 Selection string } *)


(** content type return by API' services *)
(* val content_type: string *)

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

(** Value of the subject field in the database *)
val subject_field: string

(** Value of the type field in the database *)
val type_field: string


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


(** {6 DB's collection } *)

(** Contents collection selection for Mongo request  *)
val contents_coll: Mongo.t

(** Tags collection selection for Mongo request  *)
val tags_coll: Mongo.t

(** Links collection selection for Mongo request  *)
val links_coll: Mongo.t


(** {6 Request format tools  } *)

(** Bson yes value using to select wanted field in return format *)
val yes_value: Bson.element

(** Wanted content return format *)
val content_format: Bson.t

(** Wanted tag return format *)
val tag_format: Bson.t

(** Wanted link return format *)
val link_format: Bson.t


(** {6 Getter tools  } *)

(** Save all id of given collection *)
val get_id_state: Mongo.t -> string list

(** Get new id in the given collection, in function of previous save state *)
val get_last_created_id: Mongo.t -> string list -> string list


(** {6 Cast tools } *)

(** Cast the given string to an object id in Bson manner *)
val objectid_of_string: string -> Bson.element

(** Convert string objectID from mongo in Hexa 12 char format
    into string in hex 24 char format *)
val string_of_id: string -> string

(** Convert string objectID from mongo in Hexa 12 char format
    into string in hex 24 char format *)
val string_of_objectid: Bson.element -> string

(** Cast single bson document to yojson *)
val yojson_of_bson: Bson.t -> Yojson.Safe.json

(** Cast list of bson document to yojson *)
val yojson_of_bson_list: Bson.t list -> Yojson.Safe.json

(** Cast mongreply document list to yojson *)
val yojson_of_mongoreply: MongoReply.t -> Yojson.Safe.json

val json_of_ocsigen_string_stream:
  ((string * string) * (string * string) list) option->
  string Ocsigen_stream.t option ->
  Yojson.Safe.json Lwt.t

(** {6 Checking tools  } *)

(** If the yojson is empty, raise an not_found excepetion with the given value,
    else, return the given yosjon. *)
val check_empty_yojson: Yojson.Safe.json -> string -> Yojson.Safe.json

(** If the list is empty, raise an not_found excepetion with the given value,
    else return the given list. *)
val check_empty_ocaml_list: 'a list -> string -> 'a list

(** If the reply is empty, raise an not_found excepetion with the given value,
    else return the reply. *)
val check_empty_bson: MongoReply.t -> string -> Bson.t list

(** Check if the given string_id {b exist} in the given collection,
    if not raise an not_found exception with the given id *)
val check_exist: Mongo.t -> string -> unit

(** Check if the given string_id {b not exist} in the given collection,
    if not raise an exist exception with given id *)
val check_not_exist: Mongo.t -> string -> Bson.element -> string -> unit


(** {6 Checking and cast tools  } *)

(** Check if the given string_id exist the the tags collection and
    cast it to ObjectID in Bson way. *)
val objectid_of_tagstr: string -> Bson.element


(** {6 Http tools } *)

val return_of_json : Yojson.Safe.json Lwt.t -> (string * string) Lwt.t

val return_of_error : string Lwt.t -> (string * string) Lwt.t

(** {6 Manage return tools } *)

(** Removing the value from text field *)
val link_format_ret: string list -> Yojson.Safe.json -> Yojson.Safe.json

(** Format the returned value *)
val format_ret: ?param_name:string -> int -> ?error_str:string ->
  Yojson.Safe.json -> Yojson.Safe.json

(** [check_return ?default_return ?param_name func]
    default_return: specify the default return, It is return_ok by default.
    param_name: the name of the returned data.
    func: the function which return the result.

    If the exception API_conf.Pum_exc (error_value, error_str) is raised,
    the exception's data are use to build the return.
    For any others exceptions, internal server error (500) is returned *)
val check_return: ?default_return:int -> ?param_name:string ->
  (unit -> Yojson.Safe.json Lwt.t) -> Yojson.Safe.json Lwt.t

(** Use to directly return bad request in json string in register layout / unit *)
val bad_request: ?error_value:int -> string -> string Lwt.t

(** [manage_bad_request fun]
    call the [fun], catch the exception and write it in the returned string *)
val manage_bad_request: (unit -> (string * string) Lwt.t) ->
  (string * string) Lwt.t

}}
