open Bistro
open Bistro.Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"quast" ~tag:"4.3" () ]

let quast ?reference ?labels fas =
  Workflow.shell ~descr:"quast" ~img [
    cmd "quast.py" [
      option (opt "-R" dep) reference ;
      option (opt "--labels" (list ~sep:"," string)) labels ;
      opt "--output-dir" (fun x -> seq [x ; string "/results"]) dest ;
      list ~sep:" " dep fas ;
    ]
  ]
