open Bistro.Std
open Bistro.EDSL

let env = docker_image ~account:"pveber" ~name:"fastqc" ~tag:"0.11.5" ()

type report = [`fastQC_report] directory

let run fq = workflow ~descr:"fastQC" [
    mkdir_p dest ;
    cmd "fastqc" ~env [
      seq ~sep:"" [ string "--outdir=" ; dest ] ;
      dep fq ;
    ] ;
  ]

let html_report =
  selector ["fastqc_report.html"]

let per_base_quality =
  selector ["Images" ; "per_base_quality.png"]

let per_base_sequence_content =
  selector ["Images" ; "per_base_sequence_content.png"]

