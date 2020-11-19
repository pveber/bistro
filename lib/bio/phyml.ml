open Core
open Bistro
open Bistro.Shell_dsl

let img = [ docker_image ~account:"carinerey" ~name:"phyml" ~tag:"v3.1" () ]

let token_of_datatype x =
  string (
    match x with
    | `NT -> "nt"
    | `AA -> "aa"
  )

let token_of_model m =
  string (
    match m with
    | `HKY85 -> "HKY85"
    | `JC69 -> "JC69"
    | `K80 -> "K80"
    | `F81 -> "F81"
    | `F84 -> "F84"
    | `TN93 -> "TN93"
    | `GTR -> "GTR"
    | `LG -> "LG"
    | `WAG -> "WAG"
    | `JTT -> "JTT"
    | `MtREV -> "MtREV"
    | `Dayhoff -> "Dayhoff"
    | `DCMut -> "DCMut"
    | `RtREV -> "RtREV"
    | `CpREV -> "CpREV"
    | `VT -> "VT"
    | `Blosum62 -> "Blosum62"
    | `MtMam -> "MtMam"
    | `MtArt -> "MtArt"
    | `HIVw -> "HIVw"
    | `HIVb -> "HIVb"
  )

let token_of_frequency_determination x =
  string (
    match x with
    | `Empirical -> "e"
    | `Model -> "m"
  )

let params ~branch_length ~substitution_rate ~tree_topology =
  string (
    if not branch_length && not substitution_rate && not tree_topology then
      "n"
    else
      let is_set x s =
        if x then s else ""
      in
      sprintf "%s%s%s" (is_set tree_topology "t") (is_set branch_length "l") (is_set substitution_rate "r")
  )

let tmp_ali_fn = "alignment"

let phyml
    ?bootstrap ?model ?datatype ?f ?rand_start ?n_rand_starts ?r_seed
    ?(branch_length = false) ?(substitution_rate = false) ?(tree_topology = false)
    tree alignment : [`phyml] directory =
  let tmp_ali = tmp // tmp_ali_fn in
  Workflow.shell ~descr:"phyml" ~img [
      and_list [
          cd tmp ;
          cmd "ln" [ string "-s" ; dep alignment ; tmp_ali ] ;
          cmd "phyml" [
              string "--quiet" ;
              option (opt "--bootstrap" int) bootstrap ;
              opt "--inputtree" dep tree;
              opt "-o" Fn.id (params ~branch_length ~substitution_rate ~tree_topology) ;
              opt "--input" Fn.id tmp_ali ;
              option (opt "-f" token_of_frequency_determination) f ;
              option (opt "--datatype" token_of_datatype) datatype ;
              option (opt "--model" token_of_model) model ;
              option (flag string "--rand_start") rand_start ;
              option (opt "--n_rand_starts" int) n_rand_starts ;
              option (opt "--r_seed" int) r_seed ;
            ];
          rm_rf tmp_ali ;
          mkdir_p dest ;
          mv (tmp // "*") dest ;
        ]
    ]

let tree dir =
  Workflow.select dir [tmp_ali_fn ^ "_phyml_tree.txt"]
