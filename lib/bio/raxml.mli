open Bistro
open Formats

type matrix = [
  | `DAYHOFF
  | `DCMUT
  | `JTT
  | `MTREV
  | `WAG
  | `RTREV
  | `CPREV
  | `VT
  | `BLOSUM62
  | `MTMAM
  | `LG
  | `MTART
  | `MTZOA
  | `PMB
  | `HIVB
  | `HIVW
  | `JTTDCMUT
  | `FLU
  | `STMTREV
  | `DUMMY
  | `DUMMY2
  | `AUTO
  | `LG4M
  | `LG4X
  | `PROT_FILE
  | `GTR_UNLINKED
  | `GTR
]

type bin_model = [
  | `BINCAT
  | `BINCATI
  | `ASC_BINCAT
  | `BINGAMMA
  | `ASC_BINGAMMA
  | `BINGAMMAI
]

type nucleotide_model = [
  | `GTRCAT
  | `GTRCATI
  | `ASC_GTRCAT
  | `GTRGAMMA
  | `ASC_GTRGAMMA
  | `GTRGAMMAI
]

type multi_state_model = [
  | `MULTICAT
  | `MULTICATI
  | `ASC_MULTICAT
  | `MULTIGAMMA
  | `ASC_MULTIGAMMA
  | `MULTIGAMMAI
]

type amino_acid_model = [
  | `PROTCAT of matrix
  | `ASC_PROTCAT of matrix
  | `PROTGAMMA of matrix
]

type model = [
  | bin_model
  | nucleotide_model
  | multi_state_model
  | amino_acid_model
  | `X of [ bin_model | nucleotide_model | multi_state_model | amino_acid_model ]
  | `F of amino_acid_model
]

val hpc_fasta :
  ?b:int ->
  ?c:int ->
  ?d:bool ->
  ?_D_:bool ->
  ?f:[< `A | `B | `C | `D | `E | `F | `G | `H | `I | `J | `N | `R | `S | `T | `V | `W | `a | `b | `c | `d | `e | `g | `h | `i | `j | `k | `m | `n | `o | `p | `q | `r | `s | `t | `u | `v | `w | `x | `y ] ->
  ?_F_:bool ->
  ?k:bool ->
  ?p:int ->
  ?t:newick file ->
  ?_T_:int ->
  ?no_bgfs:bool ->
  model ->
  fasta file ->
  [`raxmlHPC] directory

val hpc_phylip :
  ?b:int ->
  ?c:int ->
  ?d:bool ->
  ?_D_:bool ->
  ?f:[< `A | `B | `C | `D | `E | `F | `G | `H | `I | `J | `N | `R | `S | `T | `V | `W | `a | `b | `c | `d | `e | `g | `h | `i | `j | `k | `m | `n | `o | `p | `q | `r | `s | `t | `u | `v | `w | `x | `y ] ->
  ?_F_:bool ->
  ?k:bool ->
  ?p:int ->
  ?t:newick file ->
  ?_T_:int ->
  ?no_bgfs:bool ->
  model ->
  phylip file ->
  [`raxmlHPC] directory

val result : [`raxmlHPC] directory -> newick file
val best_tree : [`raxmlHPC] directory -> newick file
val distances : [`raxmlHPC] directory -> text file
