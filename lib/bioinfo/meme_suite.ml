open Core_kernel.Std
open Types
open Bistro.EDSL


let env = docker_image ~account:"pveber" ~name:"meme" ~tag:"4.11.2_1" ()

let meme_chip ?meme_nmotifs ?meme_minw ?meme_maxw ?np fa =
  workflow ~descr:"meme-chip" ?np [
    cmd "meme-chip" ~env [
      option (opt "-meme-nmotifs" int) meme_nmotifs ;
      option (opt "-meme-minw" int) meme_minw ;
      option (opt "-meme-maxw" int) meme_maxw ;
      opt "-meme-p" ident Bistro.EDSL.np ;
      dep fa ;
    ]
  ]
