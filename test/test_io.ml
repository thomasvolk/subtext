open Subtext
open OUnit2

let tests base_dir =
  let ror = Io.FileRepository.create ~base_dir:base_dir ~file_extension:"subtext" ~read_only:true in
  let rwr = Io.FileRepository.create ~base_dir:base_dir ~file_extension:"subtext" ~read_only:false in
  let module ST = Op.Rename.Make(Subtext.Io.FileRepository) in
  "IO" >::: [
    "read notes" >:: (fun _ ->

      assert_equal ~printer:Print.note_list 
      [
        (Note.create "1"  "one /test /1 /2" );
        (Note.create "2"  "two /test /1 /2" );
        (Note.create "foo"  "This is the foo note see also [[ sub // bar ]]" );
        (Note.create "sub/bar" "This is the Bar note" );
        (Note.create "test" {|
# test

/foo

/image01.png

hello this is a [[wiki link]]

/sub/bar
|});
        (Note.create "wiki-link" "The wiki-link note" );
      ]
      (Io.FileRepository.read_notes ror);
    );

    "rename" >:: (fun _ ->
      let new_note_path = (Filename.concat base_dir "A/B/C/1.subtext") in
      ST.rename ror "1" "A/B/C/1";

      assert_bool "file A/B/C/1.subtext exists but dry-run mode was enabled" (not (Sys.file_exists new_note_path));

      ST.rename rwr "1" "A/B/C/1";

      assert_bool "file A/B/C/1.subtext does not exists" (Sys.file_exists new_note_path)
    );
    "rename to an existing file" >:: (fun _ ->
      let expected = (Subtext.Exn.InterruptExecution ("rename fails - file already exists: " ^ (Filename.concat base_dir ("foo" ^ ".subtext")))) in
      ST.rename ror "foo" "foo";

      assert_raises ~msg:"exception does not match" expected  (fun () -> 
        ST.rename rwr "foo" "foo"
      );
    );
]

let _ = run_test_tt_main (tests (Testdata.create "read_notes"))
