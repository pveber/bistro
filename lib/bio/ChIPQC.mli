open Bistro
open Formats

type 'a sample = {
  id : string ;
  tissue : string ;
  factor : string ;
  replicate : string ;
  bam : [`indexed_bam] directory ;
  peaks : (#bed3 as 'a) file ;
}

val run : 'a sample list -> [`ChIPQC] directory
(** Beware: doesn't work with only one sample (see
    https://support.bioconductor.org/p/84754/) *)
