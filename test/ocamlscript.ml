#require "ppx_tools.metaquot"

open Core.Std
open Bistro.Std
open Bistro_bioinfo.Std
open Bistro.EDSL
open Bistro_app

let string_of_code c =
  let buf = Buffer.create 251 in
  let format = Format.formatter_of_buffer buf in
  Pprintast.structure format c ;
  Format.pp_print_flush format () ;
  Buffer.contents buf

let head_code = string_of_code [%str

open Core.Std

let head n fn dest =
  In_channel.read_lines fn
  |> Fn.flip List.take n
  |> Out_channel.write_lines dest

]

let head n file =
  workflow ~descr:"head" [
    Bistro.OCamlscript.(
      make
        ~findlib_deps:["core"]
        (app "head" [ arg (int 42) ; arg (quote (dep file)) ; arg (quote dest) ; ])
        head_code
    )
  ]


let () = local ~outdir:"out" [
    [ "_oasis2" ] %> head 2 (input "_oasis") ;
  ]
