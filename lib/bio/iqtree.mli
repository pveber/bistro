open Bistro
open Formats

type sequence_type = [
  | `DNA
  | `AA
  | `BIN
  | `MORPH
  | `CODON of int
  | `NT2AA
]

type dna_model = [
  | `JC69
  | `F81
  | `K80
  | `HKY85
  | `TN93
  | `TNe
  | `K81
  | `K81u
  | `TPM2
  | `TPM2u
  | `TPM3
  | `TPM3u
  | `TIM
  | `TIMe
  | `TIM2
  | `TIM2e
  | `TIM3
  | `TIM3e
  | `TVM
  | `TVMe
  | `SYM
  | `GTR
]

type protein_model = [
  | `BLOSUM62
  | `cpREV
  | `Dayhoff
  | `DCMut
  | `FLU
  | `HIVb
  | `HIVw
  | `JTT
  | `JTTDCMut
  | `LG
  | `mtART
  | `mtMAM
  | `mtREV
  | `mtZOA
  | `mtMet
  | `mtVer
  | `mtInv
  | `Poisson
  | `PMB
  | `rtREV
  | `VT
  | `WAG
]

type protein_mixture_model = [
  | `C10
  | `C20
  | `C30
  | `C40
  | `C50
  | `C60
  | `EX2
  | `EX3
  | `EHO
  | `UL2
  | `UL3
  | `EX_EHO
  | `LG4M
  | `LG4X
  | `CF4
]

type codon_model = [
  | `MG
  | `MGK
  | `MG1KTS
  | `MG1KTV
  | `MG2K
  | `GY
  | `GY1KTS
  | `GY1KTV
  | `GY2K
  | `ECMK07
  | `ECMrest
  | `ECMS05
]

type binary_model = [
  | `JC2
  | `GTR2
]

type freq_type = [
  | `F
  | `FO
  | `FQ
  | `F1x4
  | `F3x4
]

type rate_type = [
  | `I
  | `G of int option
  | `I_G
  | `R of int option
  | `I_R
]

type model_spec
val model_spec :
  ?freq_type:freq_type ->
  ?rate_type:rate_type ->
  [ dna_model | protein_model | protein_mixture_model | codon_model | binary_model ] ->
  model_spec

val iqtree :
  ?t:[`BIONJ | `RANDOM | `Tree of newick file] ->
  ?te:newick file ->
  ?o:string ->
  ?nt:int ->
  ?seed:int ->
  ?n:int ->
  ?m:model_spec ->
  ?z:newick file ->
  ?st:sequence_type ->
  ?spp:nexus file ->
  ?keep_ident:bool ->
  [`phylip of phylip file | `fasta of fasta file] ->
  [`iqtree] directory

val treefile : [`iqtree] directory -> newick file
val report :  [`iqtree] directory -> text file
