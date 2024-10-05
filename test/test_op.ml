open OUnit2
open Subtext

let tests =
  "Rename" >::: [
    "Rename.command" >:: (fun _ -> 
      let open Op in
      let open Note in

        assert_equal ~printer:Print.command 
          (
            Stop
          )
          (Rename.command [
            (Note.create "old" "  /old   /a  /b ") 
          ] (Key.create "not-found") (Key.create "new"));

        assert_equal ~printer:Print.command 
          (
            Stop
          )
          (Rename.command [
          ] (Key.create "old") (Key.create "new"));

        assert_equal ~printer:Print.command 
          (
            Action (RenameNote ( (Key.create "old"), (Key.create "new") ),
            Action (WriteNote ((Key.create "new"), "  /new   /a  /b " ),
            Stop))
          )
          (Rename.command [
            (Note.create "old" "  /old   /a  /b " )
          ] (Key.create "old") (Key.create "new"));

        assert_equal ~printer:Print.command 
          (
            Action (RenameNote ( (Key.create "old"), (Key.create "1-a/new") ),
            Action (WriteNote ( (Key.create "foo"), "  /1-a/new test [[ 1 a//new ]] " ),
            Action (WriteNote ( (Key.create "1-a/new"), "  /1-a/new   /a  /b " ),
            Stop)))
          )
          (Rename.command [
            (Note.create "old" "  /old   /a  /b " );
            (Note.create "foo" "  /old test [[ Old ]] " );
            (Note.create "bar" "  /foo  /new ... [[ Foo ]] " );
            (Note.create "baz" "  baz " )
          ] (Key.create "old") (Key.create "1-a/new"));
    );

    "parse_pattern" >:: (fun _ ->
      let nl = [
        Note.create "foo/1" "";
        Note.create "foo/2" ""
      ] in

      assert_equal ~printer:Print.batch_list
        [
          (Note.Key.create "foo/1", Note.Key.create "1");
          (Note.Key.create "foo/2", Note.Key.create "2");
        ]
        (Op.Rename.Batch.parse_pattern "foo/" "." nl)
    );

    "generate graph" >:: (fun _ ->
      let nl = [
        Note.create "a" " /a  /x ";
        Note.create "b" "   /a    /x";
        Note.create "c" "";
        Note.create "x" " /c  /a  ";
      ] in
      assert_equal ~printer:Print.string {|digraph test {
"a" -> "a";
"a" -> "x";
"b" -> "a";
"b" -> "x";
"x" -> "c";
"x" -> "a";
}|} (Op.DotGraph.create "test" nl)
      
    );
  ]

let _ = 
  run_test_tt_main tests
