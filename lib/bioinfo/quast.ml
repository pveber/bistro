open Core.Std
open Bistro.Std
open Bistro_bioinfo.Std
open Bistro.EDSL

type quast_output = [`quast_output] directory

let env = docker_image ~account:"pveber" ~name:"quast" ~tag:"4.3" ()

let quast ?reference ?labels fas =
  workflow ~descr:"quast" [
    cmd "quast.py" ~env [
      option (opt "-R" dep) reference ;
      option (opt "--labels" (list ~sep:"," string)) labels ;
      opt "--output-dir" (fun x -> seq [x ; string "/results"]) dest ;
      list ~sep:" " dep fas ;
    ]
  ]
