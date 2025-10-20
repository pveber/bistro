open Core
open Bistro
open Bistro.Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"iqtree" ~tag:"1.6.12" () ]

let tree_token t =
  match t with
  | `Tree t -> dep t
  | `BIONJ -> string "BIONJ"
  | `RANDOM -> string "RANDOM"


type sequence_type = [
  | `DNA
  | `AA
  | `BIN
  | `MORPH
  | `CODON of int
  | `NT2AA
]

let sequence_type_token st =
  string (
    match st with
    | `AA -> "AA"
    | `BIN -> "BIN"
    | `MORPH -> "MORPH"
    | `CODON i -> sprintf "CODON%d" i
    | `NT2AA -> "NT2AA"
    | `DNA -> "DNA"
  )

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

let string_of_dna_model = function
  | `JC69 -> "JC69"
  | `F81 -> "F81"
  | `K80 -> "K80"
  | `HKY85 -> "HKY85"
  | `TN93 -> "TN93"
  | `TNe -> "TNe"
  | `K81 -> "K81"
  | `K81u -> "K81u"
  | `TPM2 -> "TPM2"
  | `TPM2u -> "TPM2u"
  | `TPM3 -> "TPM3"
  | `TPM3u -> "TPM3u"
  | `TIM -> "TIM"
  | `TIMe -> "TIMe"
  | `TIM2 -> "TIM2"
  | `TIM2e -> "TIM2e"
  | `TIM3 -> "TIM3"
  | `TIM3e -> "TIM3e"
  | `TVM -> "TVM"
  | `TVMe -> "TVMe"
  | `SYM -> "SYM"
  | `GTR -> "GTR"

let string_of_protein_model = function
  | `BLOSUM62 -> "BLOSUM62"
  | `cpREV -> "cpREV"
  | `Dayhoff -> "Dayhoff"
  | `DCMut -> "DCMut"
  | `FLU -> "FLU"
  | `HIVb -> "HIVb"
  | `HIVw -> "HIVw"
  | `JTT -> "JTT"
  | `JTTDCMut -> "JTTDCMut"
  | `LG -> "LG"
  | `mtART -> "mtART"
  | `mtMAM -> "mtMAM"
  | `mtREV -> "mtREV"
  | `mtZOA -> "mtZOA"
  | `mtMet -> "mtMet"
  | `mtVer -> "mtVer"
  | `mtInv -> "mtInv"
  | `Poisson -> "Poisson"
  | `PMB -> "PMB"
  | `rtREV -> "rtREV"
  | `VT -> "VT"
  | `WAG -> "WAG"

let string_of_protein_mixture_model = function
  | `C10 -> "C10"
  | `C20 -> "C20"
  | `C30 -> "C30"
  | `C40 -> "C40"
  | `C50 -> "C50"
  | `C60 -> "C60"
  | `EX2 -> "EX2"
  | `EX3 -> "EX3"
  | `EHO -> "EHO"
  | `UL2 -> "UL2"
  | `UL3 -> "UL3"
  | `EX_EHO -> "EX_EHO"
  | `LG4M -> "LG4M"
  | `LG4X -> "LG4X"
  | `CF4 -> "CF4"

let string_of_codon_model = function
  | `MG -> "MG"
  | `MGK -> "MGK"
  | `MG1KTS -> "MG1KTS"
  | `MG1KTV -> "MG1KTV"
  | `MG2K -> "MG2K"
  | `GY -> "GY"
  | `GY1KTS -> "GY1KTS"
  | `GY1KTV -> "GY1KTV"
  | `GY2K -> "GY2K"
  | `ECMK07 -> "ECMK07"
  | `ECMrest -> "ECMrest"
  | `ECMS05 -> "ECMS05"

let string_of_binary_model = function
  | `JC2 -> "JC2"
  | `GTR2 -> "GTR2"

let string_of_model = function
  | #dna_model as x -> string_of_dna_model x
  | #protein_model as x -> string_of_protein_model x
  | #protein_mixture_model as x -> string_of_protein_mixture_model x
  | #codon_model as x -> string_of_codon_model x
  | #binary_model as x -> string_of_binary_model x

let string_of_freq_type = function
  | `F -> "+F"
  | `FO -> "+FO"
  | `FQ -> "+FQ"
  | `F1x4 -> "+F1x4"
  | `F3x4 -> "+F3x4"

let string_of_rate_type = function
  | `I -> "+I"
  | `G cat -> sprintf "+G%s" (Option.value_map cat ~default:"" ~f:Int.to_string)
  | `I_G -> "+I+G"
  | `R cat -> sprintf "+R%s" (Option.value_map cat ~default:"" ~f:Int.to_string)
  | `I_R -> "+I+R"

let model_spec_token (model, freq_type, rate_type) =
  sprintf "%s%s%s"
    (string_of_model model)
    (Option.value_map ~default:"" freq_type ~f:string_of_freq_type)
    (Option.value_map ~default:"" rate_type ~f:string_of_rate_type)
  |> string

type model_spec =
  [ dna_model | protein_model | protein_mixture_model | codon_model | binary_model ]
  * freq_type option
  * rate_type option

let model_spec ?freq_type ?rate_type m = m, freq_type, rate_type

let iqtree ?t ?te ?o ?(nt = 1) ?seed ?n ?m ?z ?st ?spp ?keep_ident ali =
  let ali_dep = match ali with
    | `phylip f -> dep f
    | `fasta f -> dep f
  in
  Workflow.shell ~descr:"iqtree" ~np:nt ~img [
    mkdir_p dest ;
    and_list [
        cmd "/usr/local/bin/iqtree" [
            opt "-s" Fn.id ali_dep ;
            option (opt "-t" tree_token) t ;
            option (opt "-te" dep) te ;
            option (opt "-o" string) o ;
            opt "-pre" Fn.id (dest // "iqtree") ;
            opt "-nt" Fn.id np ;
            option (opt "-seed" int) seed ;
            option (opt "-n" int) n ;
            option (opt "-m" model_spec_token) m ;
            option (opt "-z" dep) z ;
            option (opt "-st" sequence_type_token) st ;
            option (opt "-spp" dep) spp ;
            option (flag string "-keep-ident") keep_ident ;
          ] ;
      ]
    ]

let treefile dir =
  Workflow.select dir ["iqtree.treefile"]

let report dir =
  Workflow.select dir ["iqtree.iqtree"]
