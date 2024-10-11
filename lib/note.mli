
module Key : sig
  type t = private { name: string }

  val allowed_chars : string

  val is_valid : string -> bool

  val create : string -> t

  val to_string : t -> string
end


type t = {
  key: Key.t;
  text: string;
}


val create : string -> string -> t

val key : t -> Key.t

val text : t -> string


module Reference : sig
  type kind = 
    | SlashLink
    | WikiLink

  type t = { repr: string; key: Key.t; kind: kind }

  val parse : string -> t list

  val to_key : t -> Key.t

  val to_tuple : t -> Key.t * string * kind

  val to_repr : Key.t -> kind -> string

  val replace : string -> Key.t -> Key.t -> string option

end
