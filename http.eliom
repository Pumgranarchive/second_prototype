{shared{

open GUI_deserialize

}}

{server{

include Pumgrana_http

let get_content_detail content_uri =
  lwt (uri, title, summary, body, v_external) = get_content_detail content_uri in
  let ret = if v_external
    then External (Uri uri, title, summary, body)
    else Internal (id_of_uri uri, title, summary, body)
  in
  Lwt.return ret

let research_contents research =
  let format (uri, title, summary) = id_of_uri uri, title, summary in
  lwt ret = research_contents research in
  Lwt.return (List.map format ret)

let links_from_content content_uri =
  let format (id, uri, title, summary) = id, id_of_uri uri, title, summary in
  lwt ret = links_from_content content_uri in
  Lwt.return (List.map format ret)

let links_from_research content_uri research =
  let format (id, uri, title, summary) = id, id_of_uri uri, title, summary in
  lwt ret = links_from_research content_uri research in
  Lwt.return (List.map format ret)

}}

{client{

module Service =
struct

  open GUI_tools

  let prefix =
    let url = Js.to_string Dom_html.document##_URL in
    let http_length = Str.search_forward "://" url 0 + 3 in
    try String.sub url 0 (Str.search_forward "/" url http_length)
    with Not_found -> url

  let research_contents =
    Eliom_service.Http.external_service ~prefix
      ~path:["api"; "content"; "research"]
      ~get_params:Eliom_parameter.(suffix (opt (string "filter") **
                                             (string "research")))
      ()

  let get_tags_from_research =
    Eliom_service.Http.external_service ~prefix
      ~path:["api"; "tag"; "list_from_research"]
      ~get_params:Eliom_parameter.(suffix (string "research"))
      ()

  let get_links_from_research =
    Eliom_service.Http.external_service ~prefix
      ~path:["api"; "link"; "list_from_research"]
      ~get_params:Eliom_parameter.(suffix ((string "content_uri") **
                                              (string "research")))
      ()


end

let research_contents research =
  lwt json = Eliom_client.call_service
    ~service:Service.research_contents (None, research) ()
  in
  let yojson = Yojson.from_string json in
  Lwt.return (get_service_return get_short_content_list yojson)

let tags_from_research research =
  lwt json = Eliom_client.call_service
    ~service:Service.get_tags_from_research research ()
  in
  let yojson = Yojson.from_string json in
  Lwt.return (get_service_return get_tag_list yojson)

let links_from_research content_uri research =
  let str_uri = Rdf_store.string_of_uri content_uri in
  let encoded_uri = Rdf_store.uri_encode str_uri in
  lwt json = Eliom_client.call_service
    ~service:Service.get_links_from_research (encoded_uri, research) ()
  in
  let yojson = Yojson.from_string json in
  Lwt.return (get_service_return get_link_list yojson)

}}
