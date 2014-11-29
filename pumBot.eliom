let old = ref []

let filter_and_add uris =
  let aux uri =
    let exists = List.exists (fun x -> Ptype.compare_uri uri x = 0) !old in
    if not exists then old := uri::!old;
    not exists
  in
  List.filter aux uris

let launch uris =
  let uris = filter_and_add uris in
  let path = "../pum_bot/pum_bot" in
  let options = "-i 421" in
  let redirect = "> /dev/null 2>&1" in
  let bg = "&" in
  let string_of_uri uri = "\"" ^ Ptype.string_of_uri uri ^ "\"" in
  let str_uris = List.map string_of_uri uris in
  let concat_uris = String.concat " " str_uris in
  let cmd = String.concat " " [path; options; concat_uris; redirect; bg] in
  if List.length uris > 0
  then Lwt.async (fun () -> print_endline cmd; Lwt.return (ignore (Sys.command cmd)))
