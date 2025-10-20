open Core
open Bistro
open Bistro.Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"raxml" ~tag:"8.2.9" () ]

let algorithm_token x =
  string (
    match x with
    | `a -> "a"
    | `A -> "A"
    | `b -> "b"
    | `B -> "B"
    | `c -> "c"
    | `C -> "C"
    | `d -> "d"
    | `D -> "D"
    | `e -> "e"
    | `E -> "E"
    | `F -> "F"
    | `g -> "g"
    | `G -> "G"
    | `h -> "h"
    | `H -> "H"
    | `i -> "i"
    | `I -> "I"
    | `j -> "j"
    | `J -> "J"
    | `k -> "k"
    | `m -> "m"
    | `n -> "n"
    | `N -> "N"
    | `o -> "o"
    | `p -> "p"
    | `q -> "q"
    | `r -> "r"
    | `R -> "R"
    | `s -> "s"
    | `S -> "S"
    | `t -> "t"
    | `T -> "T"
    | `u -> "u"
    | `v -> "v"
    | `V -> "V"
    | `w -> "w"
    | `W -> "W"
    | `x -> "x"
    | `y -> "y"
  )

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

let string_of_matrix = function
  | `DAYHOFF -> "DAYHOFF"
  | `DCMUT -> "DCMUT"
  | `JTT -> "JTT"
  | `MTREV -> "MTREV"
  | `WAG -> "WAG"
  | `RTREV -> "RTREV"
  | `CPREV -> "CPREV"
  | `VT -> "VT"
  | `BLOSUM62 -> "BLOSUM62"
  | `MTMAM -> "MTMAM"
  | `LG -> "LG"
  | `MTART -> "MTART"
  | `MTZOA -> "MTZOA"
  | `PMB -> "PMB"
  | `HIVB -> "HIVB"
  | `HIVW -> "HIVW"
  | `JTTDCMUT -> "JTTDCMUT"
  | `FLU -> "FLU"
  | `STMTREV -> "STMTREV"
  | `DUMMY -> "DUMMY"
  | `DUMMY2 -> "DUMMY2"
  | `AUTO -> "AUTO"
  | `LG4M -> "LG4M"
  | `LG4X -> "LG4X"
  | `PROT_FILE -> "PROT_FILE"
  | `GTR_UNLINKED -> "GTR_UNLINKED"
  | `GTR -> "GTR"

let string_of_model = function
  | `BINCAT -> "BINCAT"
  | `BINCATI -> "BINCATI"
  | `ASC_BINCAT -> "ASC_BINCAT"
  | `BINGAMMA -> "BINGAMMA"
  | `ASC_BINGAMMA -> "ASC_BINGAMMA"
  | `BINGAMMAI -> "BINGAMMAI"
  | `GTRCAT -> "GTRCAT"
  | `GTRCATI -> "GTRCATI"
  | `ASC_GTRCAT -> "ASC_GTRCAT"
  | `GTRGAMMA -> "GTRGAMMA"
  | `ASC_GTRGAMMA -> "ASC_GTRGAMMA"
  | `GTRGAMMAI -> "GTRGAMMAI"
  | `MULTICAT -> "MULTICAT"
  | `MULTICATI -> "MULTICATI"
  | `ASC_MULTICAT -> "ASC_MULTICAT"
  | `MULTIGAMMA -> "MULTIGAMMA"
  | `ASC_MULTIGAMMA -> "ASC_MULTIGAMMA"
  | `MULTIGAMMAI -> "MULTIGAMMAI"
  | `PROTCAT mat -> sprintf "PROTCAT%s" (string_of_matrix mat)
  | `ASC_PROTCAT mat -> sprintf "PROTCAT%s" (string_of_matrix mat)
  | `PROTGAMMA mat -> sprintf "PROTGAMMA%s" (string_of_matrix mat)

let model_token (x : model) =
  let s = match x with
    | #bin_model | #nucleotide_model | #multi_state_model | #amino_acid_model as x ->
      string_of_model x
    | `X m -> string_of_model m ^ "X"
    | `F m -> string_of_model m ^ "F"
  in
  string s

let suffix = "results"

let hpc
    ?b ?c ?d ?_D_ ?f ?_F_ ?k ?p ?t ?(_T_ = 1)
    ?no_bgfs
    model alignment =
  Workflow.shell ~descr:"raxmlHPC" ~np:_T_ ~img [
      and_list [
          mkdir_p dest ;
          cmd "raxmlHPC" [
              option (opt "-b" int) b ;
              option (opt "-c" int) c ;
              option (flag string "-d") d ;
              option (flag string "-D") _D_ ;
              option (opt "-f" algorithm_token) f ;
              option (flag string "-F") _F_ ;
              option (flag string "-k") k ;
              opt "-m" model_token model ;
              opt "-n" string suffix ;
              option (opt "-p" int) p ;
              opt "-s" dep alignment ;
              option (opt "-t" dep) t ;
              opt "-T" Fn.id np ;
              opt "-w" Fn.id dest ;
              option (flag string "--no-bfgs") no_bgfs ;
            ] ;
        ] ;
    ]

let hpc_fasta = hpc
let hpc_phylip = hpc

let result dir = Workflow.select dir ["RAxML_result." ^ suffix]
let best_tree dir = Workflow.select dir ["RAxML_bestTree." ^ suffix]
let distances dir = Workflow.select dir ["RAxML_distances." ^ suffix]
