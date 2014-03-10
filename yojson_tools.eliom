(*
** Json tools
** This module help the translate between json and ocaml
 *)

exception Yojson_exc of string

(** get the string value from the given yojson *)
let get_string = function
  | `String str -> str
  | _           -> raise (Yojson_exc "It is not a yojson string")

(** get the value from the given yojson assoc *)
let get_assoc = function
  | `Assoc yj   -> yj
  | _           -> raise (Yojson_exc "It is not a yojson assoc")

(** get the value from the given yojson list *)
let get_list = function
  | `List yj    -> yj
  | _           -> raise (Yojson_exc "It is not a yojson list")

(** Get the first value of the given name from a yojson list  *)
let rec get_from_list name = function
  | (n, v)::tail when n = name  -> Some v
  | _::tail                     -> get_from_list name tail
  | []                          -> None

(** get all the values from the given yojson string list *)
let get_string_list string_list =
  try
    List.map get_string string_list
  with
  | Yojson_exc _        -> raise (Yojson_exc "It is not a string list")

(** map the given yojson with the given func *)
let map func = function
  | Some x      -> Some (func x)
  | None        -> None

(** Get insert content input data *)
let get_insert_content_data content_yojson =
  try
    let content_list = get_assoc content_yojson in
    let title = get_from_list "title" content_list in
    let summary = get_from_list "summary" content_list in
    let text = get_from_list "text" content_list in
    let tags_id = get_from_list "tags_id" content_list in
    match title, summary, text, tags_id with
    | Some tt, Some s, Some tx, td      ->
      get_string tt, get_string s, get_string tx,
      map (fun x -> get_string_list (get_list x)) td
    | _                                   ->
      raise API_conf.(Pum_exc (return_not_found, "title, summary and text are mandatory"))
  with
  | Yojson_exc _        ->
    raise API_conf.(Pum_exc (return_not_found, "Bad insert_content format"))

(** Get update content input data *)
let get_update_content_data content_yojson =
  try
    let content_list = get_assoc content_yojson in
    let content_id = get_from_list "content_id" content_list in
    let title = get_from_list "title" content_list in
    let summary = get_from_list "summary" content_list in
    let text = get_from_list "text" content_list in
    let tags_id = get_from_list "tags_id" content_list in
    match content_id, title, summary, text, tags_id with
    | Some id, tt, s, tx, td   ->
      get_string id, map get_string tt, map get_string s, map get_string tx,
      map (fun x -> get_string_list (get_list x)) td
    | _                        ->
      raise API_conf.(Pum_exc (return_not_found, "content_id is mandatory"))
  with
  | Yojson_exc _        ->
    raise API_conf.(Pum_exc (return_not_found, "Bad update_content format"))

(** Get delete contents input data *)
let get_delete_contents_data content_yojson =
  try
    let content_list = get_assoc content_yojson in
    let contents_id = get_from_list "contents_id" content_list in
    match contents_id with
    | Some id   -> get_string_list (get_list id)
    | _         ->
      raise API_conf.(Pum_exc (return_not_found, "contents_id is mandatory"))
  with
  | Yojson_exc _        ->
    raise API_conf.(Pum_exc (return_not_found, "Bad delete_contents format"))

(** Get insert tags input data *)
let get_insert_tags_data content_yojson =
  try
    let content_list = get_assoc content_yojson in
    let type_name = get_from_list "type_name" content_list in
    let id = get_from_list "id" content_list in
    let tags_subject = get_from_list "tags_subject" content_list in
    match type_name, id, tags_subject with
    | Some type_name, id, Some tags_subject     ->
      get_string type_name, map get_string id,
      get_string_list (get_list tags_subject)
    | _                                         ->
      raise API_conf.(Pum_exc (return_not_found, "type_name and tags_subjects are mandatory"))
  with
  | Yojson_exc _        ->
    raise API_conf.(Pum_exc (return_not_found, "Bad insert_tags format"))

(** Get delete tags input data *)
let get_delete_tags_data content_yojson =
  try
    let content_list = get_assoc content_yojson in
    let tags_id = get_from_list "tags_id" content_list in
    match tags_id with
    | Some id   -> get_string_list (get_list id)
    | _         ->
      raise API_conf.(Pum_exc (return_not_found, "tags_id is mandatory"))
  with
  | Yojson_exc _        ->
    raise API_conf.(Pum_exc (return_not_found, "Bad delete_tags format"))

(** Get insert links input data *)
let get_insert_links_data content_yojson =
  try
    let content_list = get_assoc content_yojson in
    let id_from = get_from_list "id_from" content_list in
    let ids_to = get_from_list "ids_to" content_list in
    let tags_id = get_from_list "tags_id" content_list in
    match id_from, ids_to, tags_id with
    | Some idf, Some idt, Some tid      ->
      get_string idf, get_string_list (get_list idt),
      List.map (fun x -> get_string_list (get_list x)) (get_list tid)
    | _         ->
      raise API_conf.(Pum_exc (return_not_found, "id_from, ids_to, tags_id are mandatory"))
  with
  | Yojson_exc _        ->
    raise API_conf.(Pum_exc (return_not_found, "Bad insert_links format"))

(** Get update link input data *)
let get_update_link_data content_yojson =
  try
    let content_list = get_assoc content_yojson in
    let link_id = get_from_list "link_id" content_list in
    let tags_id = get_from_list "tags_id" content_list in
    match link_id, tags_id with
    | Some id, Some tid ->
      get_string id, get_string_list (get_list tid)
    | _                 ->
      raise API_conf.(Pum_exc (return_not_found, "link_id and tags_id are mandatory"))
  with
  | Yojson_exc _        ->
    raise API_conf.(Pum_exc (return_not_found, "Bad update_link format"))

(** Get delete links input data *)
let get_delete_links_data content_yojson =
  try
    let content_list = get_assoc content_yojson in
    let links_id = get_from_list "links_id" content_list in
    match links_id with
    | Some id   -> get_string_list (get_list id)
    | _         ->
      raise API_conf.(Pum_exc (return_not_found, "links_id is mandatory"))
  with
  | Yojson_exc _        ->
    raise API_conf.(Pum_exc (return_not_found, "Bad delete_links format"))
