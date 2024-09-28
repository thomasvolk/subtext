
module Key = struct
  type t = { name: string }

  let allowed_chars = "[A-Za-z0-9_\\-äöüÄÖÜß]" 

  let regex = Re.Perl.compile_pat ("^(" ^ allowed_chars ^ ")+(\\/" ^ allowed_chars ^ "+)*$")

  let is_valid name = List.length (Re.matches regex name) > 0

  let create n = { name = n }

  let to_string k = k.name
end


type t = {
  key: Key.t;
  text: string;
}


module Reference = struct
  type kind = 
    | SlashLink
    | WikiLink

  type t = { repr: string; key: Key.t; kind: kind }


  module SlashLink = struct 
    let regex = Re.Perl.compile_pat ("([\\s]+|^)(\\/" ^ Key.allowed_chars ^ "+)+")

    let to_key r = 
      let tr = String.trim r in
      String.sub tr 1 ((String.length tr) - 1)
      |> String.lowercase_ascii
      |> Key.create

    let to_repr t = "/" ^ (Key.to_string t)

    let find_all text =
      Re.all regex text
      |> List.map (fun m -> {
        repr = (String.trim (Re.Group.get m 0));
        key = to_key (Re.Group.get m 0);
        kind = SlashLink
      })
    
    let create repr key = { repr = repr; key = Key.create key; kind = SlashLink }
  end


  module WikiLink = struct 
    let regex = Re.Perl.compile_pat "\\[\\[([A-Za-z0-9\\/\\-\\s]+)\\]\\]"

    let to_key r = r
      |> String.trim
      |> Re.replace_string (Re.Perl.compile_pat "\\/{3,}") ~by:" "
      |> Re.split (Re.Perl.compile_pat "\\/\\/")
      |> List.map (fun s -> Re.replace_string (Re.Perl.compile_pat "\\/") ~by:" " s)
      |> List.map String.trim
      |> List.fold_left Filename.concat ""
      |> Re.replace_string (Re.Perl.compile_pat "[\\s]+") ~by:"-"
      |> String.lowercase_ascii
      |> Key.create

    let find_all text =
      Re.all regex text
      |> List.map (fun m -> 
        { repr = Re.Group.get m 0;
          key = to_key (Re.Group.get m 1); 
          kind = WikiLink
        })

    let to_repr t = "[[ " ^ ((Key.to_string t)
                          |> Re.replace_string (Re.Perl.compile_pat "\\/") ~by:"//"
                          |> Re.replace_string (Re.Perl.compile_pat "\\-") ~by:" "
                         ) ^ " ]]"

    let create repr key = { repr = repr; key = Key.create key; kind = WikiLink }
  end

  let find_all text = (SlashLink.find_all text) @ (WikiLink.find_all text)

  let to_repr name kind = match kind with 
    | SlashLink -> SlashLink.to_repr name
    | WikiLink -> WikiLink.to_repr name

  let replace text old_key new_key =
      let refs = find_all text
            |> List.filter (fun r -> r.key = old_key)
      in
      match refs with
        | [] -> None
        | rl -> Some (
          List.fold_left (fun t r -> Re.replace_string (Re.Perl.compile (Re.str r.repr)) ~by:(to_repr new_key r.kind) t) text rl
        )

end


let create k t = { key = Key.create k; text = t }

let key n = n.key 

let text n = n.text 
