
let write_file f c =
  let oc = open_out f in
  Printf.fprintf oc "%s" c;
  close_out oc


module FileRepository = struct
  type t = { file_extension: string; base_dir: string; read_only: bool }
    
  let is_directory =Sys_unix.is_directory_exn ~follow_symlinks:false

  let file_exists = Sys_unix.file_exists_exn ~follow_symlinks:true

  let split p = Str.split (Str.regexp Filename.dir_sep) p
                |> List.filter (fun l -> l != "")

  let traverse dir =
    let rec loop result = function
      (* by not following the symlinks we handle symlics pointing to a dir as file *)
      | f::tl when is_directory f ->
        Sys_unix.ls_dir f
        |> List.map (Filename.concat f)
        |> List.filter file_exists
        |> List.append tl
        |> loop result
      | f::tl -> loop (f::result) tl
      | [] -> result
    in
    (* after we finished the loop we have to filter out all symbolic links pointing to directories *)
    loop [] (dir::[]) |> List.filter (fun f -> not (Sys_unix.is_directory_exn ~follow_symlinks:true f))

  let read_file f =
    let ic = open_in f in 
    let length = in_channel_length ic in
    let content = really_input_string ic length in
    close_in ic;
    content

  type file_stat = 
    | Directory 
    | File 
    | NotFound

  let file_stat f = 
    if not (file_exists f) then NotFound
    else if is_directory f then Directory
    else File

  let mkdirs p perm =
    let rec mkdir b = function
      | [] -> ()
      | h::tl -> let d = Filename.concat b h in
                  match file_stat d with
                  | Directory -> mkdir d tl
                  | NotFound -> Sys.mkdir d perm; mkdir d tl
                  | File ->  raise (Exn.InterruptExecution ("can not create directory: " ^ p ^ " - " ^ d ^ " is a file")) 
    in
    mkdir "" (split p)

  let move_file s t = 
    if (file_exists t)
    then raise (Exn.InterruptExecution ("rename fails - file already exists: " ^ t))
    else
    let td = Filename.dirname t in
    mkdirs td 0o700;
    Sys.rename s t

  let chop_base_dir r p = 
     let cnt = List.length (split r.base_dir)
     in
     let rec loop c = function
       | _ :: tl when c > 0 -> loop (c - 1) tl
       | l -> List.fold_left Filename.concat "" l
     in
     loop cnt (split p)

  let note_from_file r f =
    Note.create (Filename.chop_extension (chop_base_dir r f)) (read_file f)

  let read_notes r =
    traverse r.base_dir 
      |> List.filter (fun f -> (Filename.extension f) = ("." ^ r.file_extension))
      |> List.map (note_from_file r)
      |> List.sort (fun n1 n2 -> compare (Note.key n1) (Note.key n2))

  let to_filename r k =
    Filename.concat r.base_dir ((Note.Key.to_string k) ^ "." ^ r.file_extension)

  let create ~base_dir:base_dir ~file_extension:file_extension ~read_only:read_only = 
    let file_ext = if (String.length file_extension) = 0
    then "subtext" else file_extension
    in
    { file_extension = file_ext; base_dir = base_dir; read_only  = read_only }

  let execute_action r a = 
    let open Op in
    match a with
      | WriteNote (n, t) -> write_file (to_filename r n) t
      | RenameNote (o, n) -> 
          let tp =  (to_filename r n) in
          move_file (to_filename r o) tp

  let execute r a = 
      if not r.read_only then
        execute_action r a
end

module LoggingFileRepository = struct
  type t = FileRepository.t

  let create ~base_dir:base_dir ~file_extension:file_extension ~read_only:read_only = 
    Format.printf "init repository: dir=%s, extension=%s, write=%b\n" base_dir file_extension (not read_only);
    FileRepository.create  ~base_dir:base_dir ~file_extension:file_extension ~read_only:read_only 

  let read_notes = FileRepository.read_notes

  let execute r a = 
    Format.printf "execute action: %s\n" (Op.string_of_action a);
    FileRepository.execute r a
end
