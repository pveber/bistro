open Core.Std
open Types
open Bistro.EDSL_sh

let package = {
  Bistro.pkg_name = "meme" ;
  pkg_version = "4.11.1" ;
}

let meme_chip ?meme_nmotifs ?meme_minw ?meme_maxw fa =
  workflow ~pkgs:[package] ~descr:"meme_chip" ~np:8 [
    cmd "meme_chip" [
      option (opt "-meme-nmotifs" int) meme_nmotifs ;
      option (opt "-meme-minw" int) meme_minw ;
      option (opt "-meme-maxw" int) meme_maxw ;
      opt "-meme-p" ident np ;
      dep fa ;
    ]
  ]
