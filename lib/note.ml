module Slug = struct
  type t = { name : string }

  let allowed_chars = "[\\p{L}\\p{M}\\d_\\-]"

  let regex =
    Re2.create_exn ("^(" ^ allowed_chars ^ ")+(\\/" ^ allowed_chars ^ "+)*$")

  let is_valid = Re2.matches regex

  let create n =
    match is_valid n with
    | true -> { name = n }
    | false -> raise (Exn.InterruptExecution ("invalid slug: " ^ n))

  let to_string k = k.name
end

type t = { slug : Slug.t; text : string }

module Reference = struct
  type kind = SlashLink | WikiLink
  type t = { repr : string; slug : Slug.t; kind : kind }

  module SlashLink = struct
    let regex =
      Re2.create_exn ("(\\s|^)(\\/" ^ Slug.allowed_chars ^ "+)+(\\s|$)")

    let to_slug r =
      let tr = String.trim r in
      String.sub tr 1 (String.length tr - 1)
      |> String.lowercase_ascii |> Slug.create

    let to_repr t = "/" ^ Slug.to_string t

    let parse text =
      Re2.get_matches_exn regex text
      |> List.map (fun m ->
             {
               repr = String.trim (Re2.Match.get_exn m ~sub:(`Index 0));
               slug = to_slug (Re2.Match.get_exn m ~sub:(`Index 0));
               kind = SlashLink;
             })
  end

  module WikiLink = struct
    let regex = Re2.create_exn "\\[\\[([A-Za-z0-9\\/\\-\\s]+)\\]\\]"

    let to_slug r =
      r |> String.trim
      |> Re2.replace_exn (Re2.create_exn "\\/{3,}") ~f:(fun _ -> " ")
      |> Re2.split (Re2.create_exn "\\/\\/")
      |> List.map (fun s ->
             Re2.replace_exn (Re2.create_exn "\\/") ~f:(fun _ -> " ") s)
      |> List.map String.trim
      |> List.fold_left Filename.concat ""
      |> Re2.replace_exn (Re2.create_exn "[\\s]+") ~f:(fun _ -> "-")
      |> String.lowercase_ascii |> Slug.create

    let parse text =
      Re2.get_matches_exn regex text
      |> List.map (fun m ->
             {
               repr = Re2.Match.get_exn m ~sub:(`Index 0);
               slug = to_slug (Re2.Match.get_exn m ~sub:(`Index 1));
               kind = WikiLink;
             })

    let to_repr t =
      "[[ "
      ^ (Slug.to_string t
        |> Re2.replace_exn (Re2.create_exn "\\/") ~f:(fun _ -> "//")
        |> Re2.replace_exn (Re2.create_exn "\\-") ~f:(fun _ -> " "))
      ^ " ]]"
  end

  let parse text = SlashLink.parse text @ WikiLink.parse text

  let to_repr name kind =
    match kind with
    | SlashLink -> SlashLink.to_repr name
    | WikiLink -> WikiLink.to_repr name

  let to_tuple r = (r.slug, r.repr, r.kind)
  let to_slug r = r.slug

  let replace text old_slug new_slug =
    let refs = parse text |> List.filter (fun r -> r.slug = old_slug) in
    match refs with
    | [] -> None
    | rl ->
        Some
          (List.fold_left
             (fun t r ->
               Re2.replace_exn
                 (Re2.create_exn (Re2.escape r.repr))
                 ~f:(fun _ -> to_repr new_slug r.kind)
                 t)
             text rl)
end

let create k t = { slug = Slug.create k; text = t }
let slug n = n.slug
let text n = n.text
