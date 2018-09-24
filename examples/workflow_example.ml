open Bistro_pack
open Shell_dsl

let echo msg file = cmd "echo" ~stdout:(dest // file) [ string msg ]

let dir : [`ABC] directory workflow =
  shell ~descr:"abc-dir" [
    mkdir_p dest ;
    echo "foo" "a" ;
    echo "bar" "b" ;
    echo "bazinga" "c" ;
  ]

let wc (f : text_file workflow) : text_file workflow =
  shell ~descr:"wc" [
    cmd "wc" ~stdout:dest [ dep f ] ;
  ]

let wc_dir =
  collection_map (glob dir) ~f:wc

let concat =
  shell ~descr:"concat" [
    cmd "cat" ~stdout:dest [ deps ~sep:" " wc_dir ]
  ]

let repo = Bistro_engine.Repo.[
    item ["src"] dir ;
    items ["wc"] wc_dir ;
    item ["concat"] concat ;
  ]

let () =
  Bistro_engine.Repo.build
    ~loggers:[console_logger ()]
    ~outdir:"delme" repo
