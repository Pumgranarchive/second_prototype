(******************************************************************************
********************************** Utils **************************************
*******************************************************************************)

let output_line oc line =
  let line = line ^ "\n" in
  output oc line 0 (String.length line)

let options = [Open_wronly; Open_creat; Open_append; Open_text]
let right = 0o666

let datetime () =
  let open Unix in
  let tm = localtime (time ()) in
  (string_of_int tm.tm_mday) ^ "/" ^
  (string_of_int (tm.tm_mon + 1)) ^ "/" ^
  (string_of_int (tm.tm_year + 1900)) ^ " " ^
  (string_of_int tm.tm_hour) ^ ":" ^
  (string_of_int tm.tm_min) ^ ":" ^
  (string_of_int tm.tm_sec)

(******************************************************************************
******************************** Funtions *************************************
*******************************************************************************)

let log filename subject exc description =
  let now = datetime () in
  let title = now ^ " #ERROR on " ^ subject in
  let oc = open_out_gen options right filename in
  output_line oc ("\n" ^ title);
  output_line oc (Printexc.to_string exc);
  if (String.length description > 0) then output_line oc description;
  output_line oc "";
  close_out oc;
  title
