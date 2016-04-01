open Types
open Bistro.EDSL_sh

let package = {
  Bistro.pkg_name = "meme" ;
  pkg_version = "4.11.1" ;
}

let meme_chip ?meme_nmotifs ?meme_minw ?meme_maxw ?meme_p fa =
  workflow ~pkgs:[package] ~descr:"meme_chip" [
    cmd "meme_chip" [
      option (opt "-meme-nmotifs" int) meme_nmotifs ;
      option (opt "-meme-minw" int) meme_minw ;
      option (opt "-meme-maxw" int) meme_maxw ;
      option (opt "-meme-p" int) meme_p ;
      dep fa ;
    ]
  ]
