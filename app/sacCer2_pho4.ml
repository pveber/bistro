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

let np = 4
let mem = 10 * 1024

let main queue workdir () =
  let backend = match queue with
    | None -> Bistro_engine.Scheduler.local_backend ~np ~mem
    | Some queue ->
      let workdir = Option.value ~default:(Sys.getcwd ()) workdir in
      Bistro_pbs.Backend.make ~queue ~workdir
  in
  Bistro_app.(
    with_backend backend [
      [ "output" ; "chIP_pho4_noPi_macs2.peaks" ] %> chIP_pho4_noPi_macs2
    ]
  )

let spec =
  let open Command.Spec in
  empty
  +> flag "--pbsqueue" (optional string) ~doc:"QUEUE Name of a PBS queue"
  +> flag "--nodedir"  (optional string) ~doc:"DIR (Preferably local) scratch directory on worker nodes"

let command =
  Command.basic
    ~summary:"Analysis of a ChIP-seq dataset"
    spec
    main

let () = Command.run ~version:"0.1" command
