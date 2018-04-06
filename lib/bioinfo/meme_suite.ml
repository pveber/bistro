open Core_kernel
open Bistro.EDSL


let env = docker_image ~account:"pveber" ~name:"meme" ~tag:"4.11.2" ()

let meme_chip ?meme_nmotifs ?meme_minw ?meme_maxw ?np fa =
  workflow ~descr:"meme-chip" ?np [
    cmd "meme-chip" ~env [
      option (opt "-meme-nmotifs" int) meme_nmotifs ;
      option (opt "-meme-minw" int) meme_minw ;
      option (opt "-meme-maxw" int) meme_maxw ;
      (*opt "-meme-p" ident Bistro.EDSL.np ;*)
      opt "--oc" ident dest ;
      dep fa ;
    ]
  ]

let fimo ?alpha ?bgfile ?max_stored_scores ?motif ?motif_pseudo ?qv_thresh ?thresh meme_motifs fa =
  workflow ~descr:"fimo" [
    cmd "fimo" ~env [
      option (opt "--aplha" float) alpha;
      option (opt "--bgfile" string) bgfile ;
      option (opt "--max-stored-scores" int) max_stored_scores ;
      option (opt "--motif" string) motif ;
      option (opt "--motif-pseudo" float) motif_pseudo ;
      option (flag string "--qv-thresh") qv_thresh ;
      option (opt "--thresh" float) thresh ;
      opt "--oc" ident dest ;
      dep meme_motifs ;
      dep fa;
    ]
  ]
