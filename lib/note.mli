
module Slug : sig
  type t = private { name: string }

  val allowed_chars : string

  val is_valid : string -> bool

  val create : string -> t

  val to_string : t -> string
end


type t = {
  slug: Slug.t;
  text: string;
}


val create : string -> string -> t

val slug : t -> Slug.t

val text : t -> string


module Reference : sig
  type kind = 
    | SlashLink
    | WikiLink

  type t = { repr: string; slug: Slug.t; kind: kind }

  val parse : string -> t list

  val to_slug : t -> Slug.t

  val to_tuple : t -> Slug.t * string * kind

  val to_repr : Slug.t -> kind -> string

  val replace : string -> Slug.t -> Slug.t -> string option

end
