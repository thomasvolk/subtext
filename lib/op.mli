type action =
  | WriteNote of Note.Slug.t * string
  | RenameNote of Note.Slug.t * Note.Slug.t

module Command : sig
  type 'a t = Stop | Action of 'a * 'a t
end

module type Repository = sig
  type t

  val read_notes : t -> Note.t list
  val execute : t -> action -> unit
end

module MakeCommandRunner : functor (R : Repository) -> sig
  val run : R.t -> action Command.t -> unit
end

module Rename : sig
  module Make : functor (R : Repository) -> sig
    val rename : R.t -> string -> string -> unit
    val batch_rename : R.t -> string -> string -> unit
  end

  module Batch : sig
    val parse_pattern :
      string -> string -> Note.t list -> (Note.Slug.t * Note.Slug.t) list
  end

  val command : Note.t list -> Note.Slug.t -> Note.Slug.t -> action Command.t
end

val string_of_action : action -> string

module DotGraph : sig
  val create : string -> Note.t list -> string
end
