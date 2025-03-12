
let create_note base_dir name text =
  let oc = open_out (Filename.concat base_dir (name ^ ".subtext")) in
  Printf.fprintf oc "%s" text;
  close_out oc

let create_file base_dir name =
  create_note base_dir name name;
  let oc = open_out (Filename.concat base_dir name) in
  Printf.fprintf oc "%s" name;
  close_out oc

let create_base_dir name =
  let dir = (Printf.sprintf "%s-%f" name (Unix.time()) ) in
  Sys.mkdir dir 0o700;
  dir

let create_dir base_dir name =
  Sys.mkdir (Filename.concat base_dir name) 0o700


let create name =
  let base_dir = create_base_dir ("test_data_" ^ name) in
  let note = create_note base_dir in
  let file = create_file base_dir in
  let dir = create_dir base_dir in

  note "test" {|
# test

/foo

/image01.png

hello this is a [[wiki link]]

/sub/bar
|};
  note "foo" "This is the foo note see also [[ sub // bar ]]";
  note "wiki-link" "The wiki-link note";
  note "1" "one /test /1 /2";
  note "2" "two /test /1 /2";
  file "image01.png";
  dir "sub";
  note "sub/bar" "This is the Bar note";
  base_dir
