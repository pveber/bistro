(**
   Article: https://academic.oup.com/mbe/article/35/2/486/4644721
   Data: https://ndownloader.figshare.com/files/9473962
*)

open Core
open Bistro
open Bistro.Shell_dsl
open Bistro_utils

module Dataset = struct
  let to_string = function
    | `SongD1 -> "SongD1"

  let alignments d =
    Bistro_unix.wget "https://ndownloader.figshare.com/files/9473962"
    |> Bistro_unix.tar_xfj
    |> Fn.flip Workflow.select ["single-gene_alignments" ; to_string d ]
    |> Workflow.glob ~pattern:"*"

  let best_trees d =
    Bistro_unix.wget "https://ndownloader.figshare.com/files/9473953"
    |> Bistro_unix.tar_xfj
    |> Fn.flip Workflow.select ["single-gene_trees" ; to_string d ; "Best_observed"]
    |> Workflow.glob ~pattern:"*"

end

module Raxml = struct
  let img = [ docker_image ~account:"pveber" ~name:"raxml" ~tag:"8.2.9" () ]

  let hpc alignment =
    Workflow.shell ~descr:"raxmlhpc" ~np:4 ~img [
      cd tmp ;
      cmd "raxmlHPC" [
        opt "-T" Fun.id np ;
        string "-p 1 -m GTRGAMMA --no-bfgs" ;
        opt "-s" dep alignment ;
        string "-n NAME" ;
      ] ;
      mv (tmp // "RAxML_bestTree.NAME") dest ;
    ]
end

module Fasttree = struct
  let img = [ docker_image ~account:"pveber" ~name:"fasttree" ~tag:"2.1.10" () ]

  let fasttree fa =
    Workflow.shell ~descr:"fasttree" ~img [
      cmd "/usr/local/bin/FastTree" ~stdout:dest [
        string "-nt -gtr -gamma -spr 4 -mlacc 2 -slownni" ;
        dep fa ;
      ]
    ]
end

module IQTree = struct
  let img = [ docker_image ~account:"pveber" ~name:"iqtree" ~tag:"1.4.2" () ]

  let iqtree fa =
    let tmp_ali_fn = "data.fa" in
    let tmp_ali = tmp // tmp_ali_fn in
    Workflow.shell ~descr:"iqtree" ~img [
      cmd "ln" [ string "-s" ; dep fa ; tmp_ali ] ;
      cmd "/usr/local/bin/iqtree" [ (* iqtree save its output right next to its input, hence this mess *)
        string "-m GTR+G4" ;
        opt "-s" Fun.id tmp_ali ;
        string "-seed 1" ;
        opt "-nt" Fun.id np ;
      ] ;
      mv (tmp // (tmp_ali_fn ^ ".treefile")) dest ;
    ]
end

module PhyML = struct
  let img = [ docker_image ~account:"pveber" ~name:"phyml" ~tag:"3.3.20180129" () ]

  let phyml alignment =
    let tmp_ali_fn = "alignment" in
    let tmp_ali = tmp // tmp_ali_fn in
    Workflow.shell ~descr:"phyml" ~img [
      cd tmp ;
      cmd "ln" [ string "-s" ; dep alignment ; tmp_ali ] ;
      cmd "/usr/local/bin/phyml" [
        opt "-i" Fun.id tmp_ali ;
        string "--r_seed 1 -d nt -b 0 -m GTR -f e -c 4 -a e -s SPR --n_rand_starts 1 -o tlr -p --run_id ID" ;
      ] ;
      mv (tmp // (tmp_ali_fn ^ "*_phyml_tree_ID.txt")) dest ;
    ]
end

module Goalign = struct
  let img = [ docker_image ~account:"pveber" ~name:"goalign" ~tag:"0.2.9" () ]

  let phylip_of_fasta fa =
    Workflow.shell ~descr:"goalign.reformat" ~img [
      cmd "goalign" [
        string "reformat phylip" ;
        opt "-i" dep fa ;
        opt "-o" Fun.id dest ;
      ]
    ]
end

module Gotree = struct
  let img = [ docker_image ~account:"pveber" ~name:"gotree" ~tag:"0.2.10" () ]

  let compare_trees ~input ~reference =
    Workflow.shell ~descr:"gotree.compare" ~img [
      cmd "/usr/local/bin/gotree" ~stdout:dest [
        string "compare trees --binary" ;
        opt "-i" dep input ;
        opt "-c" dep reference ;
      ]
    ]
end


let tree_inference meth fa = match meth with
  | `Fasttree -> Fasttree.fasttree fa
  | `RAXML -> Raxml.hpc fa
  | `IQTree -> IQTree.iqtree fa
  | `PhyML -> PhyML.phyml (Goalign.phylip_of_fasta fa)

let inferred_trees d meth =
  Workflow.spawn (Dataset.alignments d) ~f:(tree_inference meth)

let comparisons d meth =
  Workflow.spawn2
    (inferred_trees d meth)
    (Dataset.best_trees d)
    ~f:(fun input reference -> Gotree.compare_trees ~input ~reference)

let concat results =
  let f = fun%workflow dest ->
    List.map [%eval Workflow.(spawn results ~f:path)] ~f:(fun fn ->
        In_channel.read_lines fn
        |> Fn.flip List.nth_exn 1
      )
    |> Out_channel.write_lines dest
  in
  Workflow.path_plugin f


let repo = Repo.[
    item ["concatenated_comps_fasttree"] (concat (comparisons `SongD1 `Fasttree)) ;
    (* items ["comps_fasttree"] ~prefix:"tree" (comparisons `SongD1 `Fasttree) ; *)
    (* FIXME: this is not possible anymore *)
  ]

let () = Repo.build_main ~loggers:[Console_logger.create ()] ~np:4 ~mem:(`GB 4) ~outdir:"res" repo
