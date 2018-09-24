open Bistro
open Shell_dsl

let dir : [`ABC] directory workflow = shell [
    mkdir_p dest ;
    cmd "touch" [ dest // "a" ] ;
    cmd "touch" [ dest // "b" ] ;
    cmd "touch" [ dest // "c" ] ;
  ]

let wc (f : text_file workflow) : text_file workflow = shell [
    cmd "wc" ~stdout:dest [ dep f ] ;
  ]

let wc_dir =
  glob dir
  |> collection_map ~f:wc
