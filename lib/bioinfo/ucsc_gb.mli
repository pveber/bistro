open Bistro.Std
open Defs

type twobit = ([`twobit], [`binary]) file

class type chrom_sizes = object
  inherit [ < header : [`no] ; .. > ] tsv
  method f1 : string
  method f2 : int
end

type bigBed = ([`bigBed], [`binary]) file

type bedGraph = ([`bedGraph], [`text]) file

type wig = ([`wig], [`text]) file

type bigWig = ([`bigWig], [`binary]) file

type genome = [ `dm3 | `hg18 | `hg19 | `hg38 | `mm8 | `mm9 | `mm10 | `sacCer2 ]
val string_of_genome : [< genome] -> string


(** {5 Dealing with genome sequences} *)
val chromosome_sequences : [< genome] -> [`ucsc_chromosome_sequences] directory workflow
val genome_sequence : [< genome] -> fasta workflow
val genome_2bit_sequence : [< genome] -> twobit workflow
val twoBitToFa : #bed4 workflow -> twobit workflow -> fasta workflow


(** {5 Chromosome size and clipping} *)
val fetchChromSizes : [< genome] -> chrom_sizes workflow
val bedClip : chrom_sizes workflow -> (#bed3 as 'a) workflow -> 'a workflow


(** {5 Conversion between annotation file formats} *)
(* val wig_of_bigWig : bigWig file -> wig file *)
(* val bigWig_of_wig : ?clip:bool -> [< genome] -> wig file -> bigWig file *)
val bedGraphToBigWig : [< genome] -> bedGraph workflow -> bigWig workflow

val bedToBigBed :
  [< genome] ->
  [ `bed3 of bed3 workflow | `bed5 of bed5 workflow ] ->
  bigBed workflow
(** {v bedToBigBed v} utility. Fails when given an empty BED file on
    input. *)

val bedToBigBed_failsafe :
  [< genome] ->
  [ `bed3 of bed3 workflow | `bed5 of bed5 workflow ] ->
  bigBed workflow
(** sam  as {! Ucsc_gb.bedToBigBed} but produces an empty file when
    given an empty BED on input. *)


(* val wg_encode_crg_mappability_36  : [`mm9 | `hg18 | `hg19] -> bigWig file *)
(* val wg_encode_crg_mappability_40  : [`mm9 | `hg18 | `hg19] -> bigWig file *)
(* val wg_encode_crg_mappability_50  : [`mm9 | `hg18 | `hg19] -> bigWig file *)
(* val wg_encode_crg_mappability_75  : [`mm9 | `hg18 | `hg19] -> bigWig file *)
(* val wg_encode_crg_mappability_100 : [`mm9 | `hg18 | `hg19] -> bigWig file *)


(* module Lift_over : sig *)
(*   open Fungen *)

(*   type chain_file = [`lift_over_chain] file *)
(*   val chain_file : org_from:[< genome] -> org_to:[< genome] -> chain_file *)

(*   (\** [conversion fp xs] returns a pair of location lists, mapped and *)
(*       unmapped locations. *\) *)
(*   val conversion : *)
(*     [`lift_over_chain] file_path -> *)
(*     Location.t Stream.t -> *)
(*     Location.t list * Location.t list *)

(*   (\** liftOver preserves {b more or less} the input BED: columns are *)
(*       conserved but fields may be changed (floats truncated to integers) *\) *)
(*   val bed_conversion : org_from:[< genome] -> org_to:[< genome] -> 'a Bed.file -> [`ucsc_lift_over of 'a] dir *)
(*   val mapped : [`ucsc_lift_over of 'a] dir -> 'a Bed.file *)
(*   val unmapped : [`ucsc_lift_over of 'a] dir -> 'a Bed.file *)
(* end *)
