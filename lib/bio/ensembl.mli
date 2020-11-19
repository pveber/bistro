open Bistro

type species = [
  | `homo_sapiens
  | `mus_musculus
]

val ucsc_reference_genome : release:int -> species:species -> Ucsc_gb.genome

val gff : ?chr_name : [`ensembl | `ucsc] -> release:int -> species:species -> gff file
val gtf : ?chr_name : [`ensembl | `ucsc] -> release:int -> species:species -> gff file

val cdna : release:int -> species:species -> fasta gz file

val dna : release:int -> species:species -> fasta gz file
