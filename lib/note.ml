
module Key = struct
  type t = { name: string }

  let allowed_chars = "[\\p{L}\\p{M}\\d_\\-]"

  let regex = Re2.create_exn ("^(" ^ allowed_chars ^ ")+(\\/" ^ allowed_chars ^ "+)*$")

  let is_valid = Re2.matches regex

  let create n = match is_valid n with
  | true -> { name = n }
  | false -> raise (Exn.InterruptExecution ("invalid key: " ^ n))

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
    let regex = Re2.create_exn ("(\\s|^)(\\/" ^ Key.allowed_chars ^ "+)+(\\s|$)")

    let to_key r = 
      let tr = String.trim r in
      String.sub tr 1 ((String.length tr) - 1)
      |> String.lowercase_ascii
      |> Key.create

    let to_repr t = "/" ^ (Key.to_string t)

    let parse text =
      Re2.get_matches_exn regex text
      |> List.map (fun m -> {
        repr = (String.trim (Re2.Match.get_exn m ~sub:(`Index 0)));
        key = to_key (Re2.Match.get_exn m ~sub:(`Index 0));
        kind = SlashLink
      })
  end


  module WikiLink = struct 
    let regex = Re2.create_exn "\\[\\[([A-Za-z0-9\\/\\-\\s]+)\\]\\]"

    let to_key r = r
      |> String.trim
      |> Re2.replace_exn (Re2.create_exn "\\/{3,}") ~f:(fun _ -> " ")
      |> Re2.split (Re2.create_exn "\\/\\/")
      |> List.map (fun s -> Re2.replace_exn (Re2.create_exn "\\/") ~f:(fun _ -> " ") s)
      |> List.map String.trim
      |> List.fold_left Filename.concat ""
      |> Re2.replace_exn (Re2.create_exn "[\\s]+") ~f:(fun _ -> "-")
      |> String.lowercase_ascii
      |> Key.create

    let parse text =
      Re2.get_matches_exn regex text
      |> List.map (fun m -> {
        repr = Re2.Match.get_exn m ~sub:(`Index 0);
        key = to_key (Re2.Match.get_exn m ~sub:(`Index 1)); 
        kind = WikiLink
      })

    let to_repr t = "[[ " ^ ((Key.to_string t)
                          |> Re2.replace_exn (Re2.create_exn "\\/") ~f:(fun _ -> "//")
                          |> Re2.replace_exn (Re2.create_exn "\\-") ~f:(fun _ -> " ")
                         ) ^ " ]]"
  end

  let parse text = (SlashLink.parse text) @ (WikiLink.parse text)

  let to_repr name kind = match kind with 
    | SlashLink -> SlashLink.to_repr name
    | WikiLink -> WikiLink.to_repr name

  let to_tuple r = (r.key, r.repr, r.kind)

  let to_key r = r.key

  let replace text old_key new_key =
      let refs = parse text
            |> List.filter (fun r -> r.key = old_key)
      in
      match refs with
        | [] -> None
        | rl -> Some (
          List.fold_left (fun t r -> Re2.replace_exn (Re2.create_exn (Re2.escape r.repr)) ~f:(fun _ -> (to_repr new_key r.kind)) t) text rl
        )

end


let create k t = { key = Key.create k; text = t }

let key n = n.key 

let text n = n.text 
