open OUnit2
open Subtext
open Note

let tests =
  "Note" >::: [
    "is_valid_note_name" >:: (fun _ -> 
      let is_valid = Note.Key.is_valid in

      assert_bool "empty name must not be valid" (not (is_valid ""));

      assert_bool "starting with / must not be valid" (not (is_valid "/foo"));

      assert_bool "names like 'Ab_cd-98' must be valid" (is_valid "Ab_cd-98");

      assert_bool "names like 'Ab_cd-98/1/2/3' must be valid" (is_valid "Ab_cd-98/1/2/3");

      assert_bool "ending with / must not be valid" (not (is_valid "foo/"));

      assert_bool "names with spaces must not be valid" (not (is_valid "foo bar"));

      assert_bool "names like 'Ab_cd-98-äüö' must be valid" (is_valid "Ab_cd-98-äöü");

      assert_bool "Names with Japanese chars must be valid" (is_valid "グーゴルプレックス");

      assert_bool "Names with Arabic chars must be valid" (is_valid "الخط-العربي-جميل");

      assert_bool "Names with Icelandic chars must be valid" (is_valid "hæ-gaman-að-kynnast-þér");
    );

    "find_references" >:: (fun _ ->
      assert_equal ~printer:Print.reference_list
      [
        (Reference.SlashLink.create "/foo" "foo" );
        (Reference.WikiLink.create "[[ Foo bar ]]" "foo-bar" ) 
      ]
      (Reference.find_all "1 2 3 /foo 4 5 6 [[ Foo bar ]] [[ # invalid ]]  http://invalid.de ./invalid");

      assert_equal ~printer:Print.reference_list
      [
        (Reference.WikiLink.create "[[ Foo 1 // bar ]]" "foo-1/bar"  ) 
      ]
      (Reference.find_all " [[ Foo 1 // bar ]] ");
    );

    "WikiLink.normalize" >:: (fun _ ->
      let to_key t = Reference.WikiLink.to_key t in

      assert_equal ~printer:Print.note_key (Key.create "foo-1-a") (to_key "  foo-1-a   ");

      assert_equal ~printer:Print.note_key (Key.create "foo-1-a") (to_key "  Foo 1 A   ");

      assert_equal ~printer:Print.note_key (Key.create "foo/1-a") (to_key "  Foo // 1 A   ");

      assert_equal ~printer:Print.note_key (Key.create "foo-1-a") (to_key "  Foo /// 1 A   ");

      assert_equal ~printer:Print.note_key (Key.create "bar-1-a") (to_key "  Bar / 1 A   ");
    );

    "SlashLink.normalize" >:: (fun _ ->
      let to_key t = Reference.SlashLink.to_key t in

      assert_equal ~printer:Print.note_key (Key.create "foo-1-a") (to_key "  /fOo-1-A   ");
    );

    "replace" >:: (fun _ ->
      assert_equal ~printer:Print.s_option
        None 
        (Reference.replace "" (Key.create "foo") (Key.create "bar"));

      assert_equal ~printer:Print.s_option 
        (Some "/bar") 
        (Reference.replace "/foo" (Key.create "foo") (Key.create "bar"));

      assert_equal ~printer:Print.s_option 
        (Some"### /bar ###") 
        (Reference.replace "### /foo ###" (Key.create "foo") (Key.create "bar"));

      assert_equal ~printer:Print.s_option 
        (Some"### [[ bar//1//2 a ]] ###") 
        (Reference.replace "### [[ foo 1 ]] ###" (Key.create "foo-1") (Key.create "bar/1/2-a"));

      assert_equal ~printer:Print.s_option 
        None 
        (Reference.replace "/x12 /bar 7 /f" (Key.create "foo") (Key.create "bar"));
    );
  ]

let _ = run_test_tt_main tests 
