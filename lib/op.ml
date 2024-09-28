
module Command = struct
  type action = 
    | WriteNote of Note.Key.t * string
    | RenameNote of Note.Key.t * Note.Key.t

  type t = Stop | Action of action * t
end


let string_of_action =
  let open Note.Key in
  let open Command in
  function
  | (WriteNote (k, _)) -> "write note: " ^ (to_string k)
  | (RenameNote (o, n)) -> "rename note form " ^ (to_string o) ^ " to " ^ (to_string n)


module type Repository = sig
  type t
  val read_notes : t -> Note.t list
  val execute_exn : t -> Command.action -> unit
end


module MakeCommandRunner (R : Repository) = struct
  let rec run_exn r c = 
    let open Command in
    match c with
      | Stop -> ()
      | Action (a, s) -> 
          R.execute_exn r a;
          run_exn r s
end


module Rename = struct
  let command notes kold knew = 
    let open Command in
    let rename next =
      match (List.find_opt (fun n -> (Note.key n) = kold) notes) with
        | None ->  Stop
        | Some n -> Action ((RenameNote ((Note.key n), knew) ), next)
    in
    let rec update_chain notes next =
      match notes with 
      | [] -> next
      | h :: t ->
        let kcur = Note.key h in
        update_chain t
        (match (Note.Reference.replace (Note.text h) kold knew) with
          | None -> next
          | Some t -> let k = 
            if kcur = kold then knew else kcur in
              Action ((WriteNote (k, t) ), next)
        )
    in
    rename (update_chain notes Stop)

  module Batch = struct
    module Source = struct
      type t = 
        | All
        | StartsWith of string

      let of_string = function 
        | "." -> All
        | s -> StartsWith s
    end


    module Target = struct
      type t = 
        | RemovePrefix
        | AddPrefix of string

      let of_string = function 
        | "." -> RemovePrefix
        | s -> AddPrefix s
    end


    let parse_pattern s t nl =
      let add_prefix p k = (Note.Key.create (p ^ (Note.Key.to_string k))) in
      let remove_prefix p k =
        let s = Note.Key.to_string k in
        (Note.Key.create ((String.sub s (String.length p) ((String.length s) - (String.length p))))) in
      let keys = List.map Note.key nl in
      let open Source in
      let open Target in
      match ((Source.of_string s), (Target.of_string t)) with
        | (All, RemovePrefix) -> []
        | (All, AddPrefix p) -> keys 
          |> List.map (fun s -> (s, (add_prefix p s)))
        | (StartsWith b, AddPrefix p) -> keys
          |> List.filter (fun k -> String.starts_with ~prefix:b (Note.Key.to_string k))
          |> List.map (fun s -> (s, (add_prefix p s)))
        | (StartsWith b, RemovePrefix) -> keys
          |> List.filter (fun k -> String.starts_with ~prefix:b (Note.Key.to_string k))
          |> List.map (fun s -> (s, (remove_prefix b s)))
  end


  module Make (R : Repository) = struct
    module CR = MakeCommandRunner(R)

    let rename_note_exn r o n = 
      let nl = R.read_notes r in
      let c = command nl o n in
      CR.run_exn r c
      
    let rename_exn r o n = 
      rename_note_exn r (Note.Key.create o) (Note.Key.create n)
      
    let batch_rename_exn r s t = 
      let nl = R.read_notes r in
      Batch.parse_pattern s t nl
      |> List.iter (fun (o, n) -> rename_note_exn r o n)

  end
end
