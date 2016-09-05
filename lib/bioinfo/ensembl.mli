open Bistro.Std
open Defs

type species = [
  | `homo_sapiens
  | `mus_musculus
]

val ucsc_reference_genome : release:int -> species:species -> Ucsc_gb.genome

val gff : ?chr_name : [`ensembl | `ucsc] -> release:int -> species:species -> gff workflow
