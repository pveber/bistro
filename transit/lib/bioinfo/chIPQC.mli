open Bistro.Std
open Defs

type 'a sample = {
  id : string ;
  tissue : string ;
  factor : string ;
  replicate : string ;
  bam : bam workflow ;
  peaks : (#bed3 as 'a) workflow ;
}

type output = [`ChIPQC] directory

val run : 'a sample list -> output workflow
