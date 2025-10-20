open Bistro
open Bistro.Shell_dsl

type report = [`fastQC] directory

let img = [ docker_image ~account:"pveber" ~name:"fastqc" ~tag:"0.11.8" () ]

module Cmd = struct
  let fastqc x = [
    mkdir_p dest ;
    cmd "fastqc" [
      seq ~sep:"" [ string "--outdir=" ; dest ] ;
      (
        match x with
        | `fq fq -> dep fq
        | `fq_gz fq_gz -> Bistro_unix.Cmd.gzdep fq_gz
      )
    ] ;
    and_list [
      cd dest ;
      cmd "unzip" [ string "*_fastqc.zip" ] ;
      cmd "mv" [ string "*_fastqc/*" ; string "." ]
    ] ;
  ]
end

let fastqc fq = Workflow.shell ~descr:"fastQC" ~img (Cmd.fastqc (`fq fq))

let fastqc_gz fq_gz = Workflow.shell ~descr:"fastQC" ~img (Cmd.fastqc (`fq_gz fq_gz))

let html_report x = Workflow.select x ["fastqc_report.html"]

let per_base_quality x =
  Workflow.select x ["Images" ; "per_base_quality.png"]

let per_base_sequence_content x =
  Workflow.select x ["Images" ; "per_base_sequence_content.png"]
