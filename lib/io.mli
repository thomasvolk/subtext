open Op


module FileRepository : sig
  type t = { file_extension: string; base_dir: string; read_only: bool }
    
  val read_notes : t -> Note.t list

  val execute_exn : t -> Command.action -> unit

  val create : base_dir:string -> file_extension:string -> read_only:bool -> t
end


module LoggingFileRepository : sig
  type t = FileRepository.t
    
  val read_notes : t -> Note.t list

  val execute_exn : t -> Command.action -> unit

  val create : base_dir:string -> file_extension:string -> read_only:bool -> t
end
