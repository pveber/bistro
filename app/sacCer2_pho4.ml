(** {:{http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE29506}GEO Series GSE29506} *)

open Core.Std
open Bistro.EDSL
open Bistro_bioinfo.Std


let chIP_pho4_noPi = List.map ~f:Sra.fetch_srr [ "SRR217304" ; "SRR217305" ]

let genome = Ucsc_gb.genome_sequence `sacCer2

(* SAMPLES AS FASTQ *)
let chIP_pho4_noPi_fq = List.map chIP_pho4_noPi ~f:Sra_toolkit.fastq_dump

(* MAPPING *)
let bowtie_index = Bowtie.bowtie_build genome
let chIP_pho4_noPi_sam = Bowtie.bowtie ~v:2 bowtie_index (`single_end chIP_pho4_noPi_fq)
let chIP_pho4_noPi_bam = Samtools.(indexed_bam_of_sam chIP_pho4_noPi_sam / indexed_bam_to_bam)

let chIP_pho4_noPi_macs2 = Macs2.callpeak chIP_pho4_noPi_bam

let () =
  Bistro_app.(
    local ~np:4 ~mem:(10 * 1024) [
      [ "output" ; "chIP_pho4_noPi_macs2.peaks" ] %> chIP_pho4_noPi_macs2
    ]
  )

(*
    Scheduler.pbs_backend ~queue

let spec =
  let open Command.Spec in
  empty
  +> flag "--pbs" (optional string) ~doc:"Name of a PBS queue"

let command =
  Command.basic
    ~summary:"Runs echo on a PBS cluster"
    spec
    main

let () = Command.run ~version:"0.0" command
*)
