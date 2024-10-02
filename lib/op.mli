
module Command : sig
  type action = 
    | WriteNote of Note.Key.t * string
    | RenameNote of Note.Key.t * Note.Key.t

  type t = Stop | Action of action * t
end


module type Repository = sig
  type t

  val read_notes : t -> Note.t list

  val execute_exn : t -> Command.action -> unit
end


module MakeCommandRunner : functor (R : Repository) -> sig
  val run_exn : R.t -> Command.t -> unit
end


module Rename : sig
  module Make : functor (R : Repository) -> sig
    val rename_exn : R.t -> string -> string -> unit 

    val batch_rename_exn : R.t -> string -> string -> unit 
  end

  module Batch : sig
    val parse_pattern : string -> string -> Note.t list -> (Note.Key.t * Note.Key.t) list
  end

  val command : Note.t list -> Note.Key.t -> Note.Key.t -> Command.t
end

val string_of_action : Command.action -> string


module Graph : sig
  val create : Note.t list -> string
end
