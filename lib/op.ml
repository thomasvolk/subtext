
type action = 
  | WriteNote of Note.Slug.t * string
  | RenameNote of Note.Slug.t * Note.Slug.t

  
module Command = struct
  type 'a t = Stop | Action of 'a * 'a t
end


let string_of_action =
  let open Note.Slug in
  function
  | (WriteNote (k, _)) -> "write note: " ^ (to_string k)
  | (RenameNote (o, n)) -> "rename note form " ^ (to_string o) ^ " to " ^ (to_string n)


module type Repository = sig
  type t
  val read_notes : t -> Note.t list
  val execute : t -> action -> unit
end


module MakeCommandRunner (R : Repository) = struct
  let rec run r c = 
    let open Command in
    match c with
      | Stop -> ()
      | Action (a, s) -> 
          R.execute r a;
          run r s
end


module Rename = struct
  let command notes kold knew = 
    let open Command in
    let rename next =
      match (List.find_opt (fun n -> (Note.slug n) = kold) notes) with
        | None ->  Stop
        | Some n -> Action ((RenameNote ((Note.slug n), knew) ), next)
    in
    let rec update_chain notes next =
      match notes with 
      | [] -> next
      | h :: t ->
        let kcur = Note.slug h in
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
      let add_prefix p k = (Note.Slug.create (p ^ (Note.Slug.to_string k))) in
      let remove_prefix p k =
        let s = Note.Slug.to_string k in
        (Note.Slug.create ((String.sub s (String.length p) ((String.length s) - (String.length p))))) in
      let slugs = List.map Note.slug nl in
      let open Source in
      let open Target in
      match ((Source.of_string s), (Target.of_string t)) with
        | (All, RemovePrefix) -> []
        | (All, AddPrefix p) -> slugs 
          |> List.map (fun s -> (s, (add_prefix p s)))
        | (StartsWith b, AddPrefix p) -> slugs
          |> List.filter (fun k -> String.starts_with ~prefix:b (Note.Slug.to_string k))
          |> List.map (fun s -> (s, (add_prefix p s)))
        | (StartsWith b, RemovePrefix) -> slugs
          |> List.filter (fun k -> String.starts_with ~prefix:b (Note.Slug.to_string k))
          |> List.map (fun s -> (s, (remove_prefix b s)))
  end


  module Make (R : Repository) = struct
    module CR = MakeCommandRunner(R)

    let rename_note r o n = 
      let nl = R.read_notes r in
      let c = command nl o n in
      CR.run r c
      
    let rename r o n = 
      rename_note r (Note.Slug.create o) (Note.Slug.create n)
      
    let batch_rename r s t = 
      let nl = R.read_notes r in
      Batch.parse_pattern s t nl
      |> List.iter (fun (o, n) -> rename_note r o n)

  end
end


module DotGraph = struct

  let to_edge a b = "\"" ^ (Note.Slug.to_string a) ^ "\" -> \"" ^ (Note.Slug.to_string b) ^ "\";"

  let edges n = List.map (fun r -> to_edge (Note.slug n) (Note.Reference.to_slug r)) (Note.Reference.parse (Note.text n))

  let create name nl = let e = List.map edges nl |> List.flatten |> String.concat "\n" in 
     "digraph \"" ^ name ^ "\" {\n" ^ e ^ "\n}"
end
