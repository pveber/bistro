open Core_kernel.Std
open Bistro.Std
open Bistro.EDSL
open Types

type species = [
  | `homo_sapiens
  | `mus_musculus
]

let ucsc_reference_genome ~release ~species =
  match species with
  | `mus_musculus when 63 <= release && release <= 65 -> `mm9
  | `homo_sapiens when release = 71 -> `hg19
  | `homo_sapiens when release = 84 -> `hg38
  | _ -> failwith "unknown release for this species"

(* acronym of the lab where the species was sequenced *)
let lab_label_of_genome = function
  | `hg19 -> "GRCh37"
  | `hg38 -> "GRCh38"
  | `mm9 -> "NCBIM37"
  | `mm10 -> "GRCm38"

let string_of_species = function
  | `homo_sapiens -> "homo_sapiens"
  | `mus_musculus -> "mus_musculus"

let ucsc_chr_names_gtf gff =
  workflow ~descr:"ensembl.ucsc_chr_names_gtf" [
    pipe [
      cmd "gawk" [
        string "'{print \"chr\" $0}'" ;
        dep gff
      ] ;
      cmd "sed" [ string "'s/chrMT/chrM/g'" ] ~stdout:dest
    ]
  ]

let gff ?(chr_name = `ensembl) ~release ~species =
  let url =
    sprintf "ftp://ftp.ensembl.org/pub/release-%d/gtf/%s/%s.%s.%d.gtf.gz"
      release (string_of_species species)
      (String.capitalize (string_of_species species))
      (lab_label_of_genome (ucsc_reference_genome ~release ~species)) release
  in
  let gff = Unix_tools.(gunzip (wget url)) in
  match chr_name with
  | `ensembl -> gff
  | `ucsc -> ucsc_chr_names_gtf gff
