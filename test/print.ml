open Subtext

let string (s: string) = s

let s_option o = match o with
  | None -> "None"
  | Some(s) -> "Some(" ^ s ^ ")"

let note n = 
  "Note(key=" ^ (Note.Key.to_string (Note.key n))  ^ ", text=" ^ (Note.text n) ^ ")"

let note_list l = String.concat " " (List.map note l)

let reference_kind = function 
| Note.Reference.WikiLink -> "WikiLink"
| Note.Reference.SlashLink -> "SlashLink"

let reference_tuple (k, r, kd) = (Note.Key.to_string k) ^ ", " ^ r ^ ", " ^ (reference_kind kd) 

let reference_tuple_list l = List.map reference_tuple l |> String.concat " - "

let reference r = 
  let open Note.Reference in
  let kind = match r.kind with
   | SlashLink -> "SlashLink"
   | WikiLink -> "WikiLink"
  in 
    kind ^ "(repr=" ^ r.repr ^ ", name=" ^ (Note.Key.to_string r.key) ^ "))"

let reference_list l = String.concat " " (List.map reference l)

let action a = 
  let open Op.Command in
  match a with
  | RenameNote (o, n) -> "RenameNote(" ^ (Note.Key.to_string o) ^ ", " ^ (Note.Key.to_string n) ^ ")"
  | WriteNote (n, t) -> "WriteNote(" ^ (Note.Key.to_string n) ^ ", '" ^ t ^ "')"

let command c =
  let open Op.Command in
  let rec join c s = match c with
   | Stop -> s ^"\nStop"
   | Action (a, n) -> join n (s ^ "\nCommand(" ^ (action a) ^ ")")
  in
  join c ""

let batch_list l =
  String.concat " " (List.map (fun (a, b) -> (Note.Key.to_string a) ^ "->" ^ (Note.Key.to_string b)) l)

let note_key = Note.Key.to_string
