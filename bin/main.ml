open Subtext

let file_extension_flag =
  let open Command.Param in
  flag "-e" (optional_with_default "subtext" string) ~doc:" note file extension"

let base_dir_flag =
  let open Command.Param in
  flag "-d" (optional_with_default "." string) ~doc:" base dir for operation"

let dry_run_flag =
  let open Command.Param in
  flag "-n" no_arg ~doc:" dry run"

let graph =
  Command.basic ~summary:"graph command"
    (let%map_open.Command file_extension = file_extension_flag
     and base_dir = base_dir_flag
     and output_file = flag "-o" (optional string) ~doc:" output file"
     and name = anon ("graph name" %: string) in
     fun () ->
       let r =
         Io.FileRepository.create ~base_dir ~file_extension ~read_only:true
       in
       let nl = Io.FileRepository.read_notes r in
       let out = Op.DotGraph.create name nl in
       match output_file with
       | Some f -> Io.write_file f out
       | None -> print_endline out)

let batch_rename_command =
  Command.basic ~summary:"batch rename command"
    (let%map_open.Command file_extension = file_extension_flag
     and base_dir = base_dir_flag
     and dry_run = dry_run_flag
     and source = anon ("source pattern" %: string)
     and target = anon ("target pattern" %: string) in
     fun () ->
       let r =
         Io.LoggingFileRepository.create ~base_dir ~file_extension
           ~read_only:dry_run
       in
       let module ST = Op.Rename.Make (Io.LoggingFileRepository) in
       ST.batch_rename r source target)

let rename_command =
  Command.basic ~summary:"rename command"
    (let%map_open.Command file_extension = file_extension_flag
     and base_dir = base_dir_flag
     and dry_run = dry_run_flag
     and old_name = anon ("old_name" %: string)
     and new_name = anon ("new_name" %: string) in
     fun () ->
       let r =
         Io.LoggingFileRepository.create ~base_dir ~file_extension
           ~read_only:dry_run
       in
       let module ST = Op.Rename.Make (Io.LoggingFileRepository) in
       ST.rename r old_name new_name)

let main_command =
  Command.group ~summary:"subtext management tool"
    [
      ("rename", rename_command);
      ("batch-rename", batch_rename_command);
      ("graph", graph);
    ]

let () =
  try Command_unix.run ~version:"0.1" ~build_info:"subtext" main_command
  with Exn.InterruptExecution e ->
    prerr_endline ("ERROR: " ^ e);
    exit 1
