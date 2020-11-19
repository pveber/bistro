open Bistro
open Formats

class type twobit = object
  method format : [`twobit]
  inherit binary_file
end

class type chrom_sizes = object
  inherit tsv
  method header : [`no]
  method f1 : string
  method f2 : int
end

class type bigBed = object
  method format : [`bigBed]
  inherit binary_file
end

class type bedGraph = object
  inherit bed3
  method f4 : float
end

class type wig = object
  method format : [`wig]
  inherit text
end

class type bigWig = object
  method format : [`bigWig]
  inherit binary_file
end

type genome = [ `dm3 | `droSim1 | `hg18 | `hg19 | `hg38 | `mm8 | `mm9 | `mm10 | `sacCer2 ]
val string_of_genome : [< genome] -> string
val genome_of_string : string -> genome option

(** {4 Dealing with genome sequences} *)
type chromosome_sequences = [`ucsc_chromosome_sequences] directory

val chromosome_sequence :
  [< genome] ->
  string ->
  fasta file
val chromosome_sequences : [< genome] -> [`ucsc_chromosome_sequences] directory
val genome_sequence : [< genome] -> fasta file
val genome_2bit_sequence : [< genome] -> twobit file
val twoBitToFa : twobit file -> #bed4 file -> fasta file
val faToTwoBit : fasta file -> twobit file

(** {4 Chromosome size and clipping} *)
val fetchChromSizes : [< genome] -> chrom_sizes file
val bedClip : chrom_sizes file -> (#bed3 as 'a) file -> 'a file


(** {4 Conversion between annotation file formats} *)
(* val wig_of_bigWig : bigWig file -> wig file *)
(* val bigWig_of_wig : ?clip:bool -> [< genome] -> wig file -> bigWig file *)
val bedGraphToBigWig : [< genome] -> bedGraph file -> bigWig file

val bedToBigBed :
  [< genome] ->
  [ `bed3 of bed3 file | `bed5 of bed5 file ] ->
  bigBed file
(** bedToBigBed utility. Fails when given an empty BED file on
    input. Note that the underlying bedToBigBed expects BED
    files with {i exactly} 3 or 5 columns. *)

val bedToBigBed_failsafe :
  [< genome] ->
  [ `bed3 of bed3 file | `bed5 of bed5 file ] ->
  bigBed file
(** sam  as {! Ucsc_gb.bedToBigBed} but produces an empty file when
    given an empty BED on input. *)


(* val wg_encode_crg_mappability_36  : [`mm9 | `hg18 | `hg19] -> bigWig file *)
(* val wg_encode_crg_mappability_40  : [`mm9 | `hg18 | `hg19] -> bigWig file *)
(* val wg_encode_crg_mappability_50  : [`mm9 | `hg18 | `hg19] -> bigWig file *)
(* val wg_encode_crg_mappability_75  : [`mm9 | `hg18 | `hg19] -> bigWig file *)
(* val wg_encode_crg_mappability_100 : [`mm9 | `hg18 | `hg19] -> bigWig file *)


module Lift_over : sig
  class type chain_file = object
    inherit regular_file_t
    method format : [`lift_over_chain_file]
  end
  type 'a output = [`ucsc_lift_over of 'a] directory

  val chain_file :
    org_from:[< genome] ->
    org_to:[< genome] ->
    chain_file file

  val bed :
    org_from:[< genome] ->
    org_to:[< genome] ->
    (* chain_file file -> *)
    (#bed3 as 'a) file ->
    'a output

  val mapped : 'a output -> 'a file
  val unmapped : 'a output -> 'a file
end
