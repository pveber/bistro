open Core
open Bistro
open Bistro.Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"idba" ~tag:"1.1.3" () ]

let fq2fa ?filter input =
  let args = match input with
    | `Se fq -> dep fq
    | `Pe_merge (fq1, fq2) ->
      opt "--merge" Fn.id (seq ~sep:" " [dep fq1 ; dep fq2])
    | `Pe_paired fq ->
      opt "--paired" dep fq
  in
  Workflow.shell ~descr:"fq2fa" ~img [
    cmd "fq2fa" [
      option (flag string "--filter") filter ;
      args ;
      dest ;
    ]
  ]

let idba_ud ?(mem_spec = 10) fa =
  Workflow.shell ~img ~np:4 ~mem:(Workflow.int (mem_spec * 1024)) ~descr:"idba_ud" [
    mkdir_p dest ;
    cmd "idba_ud" [
      opt "--read" dep fa ;
      opt "--num_threads" Fn.id np ;
      opt "--out" Fn.id dest ;
    ]
  ]

let idba_ud_contigs x = Workflow.select x ["contig.fa"]
let idba_ud_scaffolds x = Workflow.select x ["scaffold.fa"]
