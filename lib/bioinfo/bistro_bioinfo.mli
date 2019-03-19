open Bistro

class type bam = object
  inherit binary_file
  method format : [`bam]
end

class type bed3 = object
  inherit tsv
  method header : [`no]
  method f1 : string
  method f2 : int
  method f3 : int
end

class type bed4 = object
  inherit bed3
  method f4 : string
end

class type bed5 = object
  inherit bed4
  method f5 : int
end

class type bed6 = object
  inherit bed5
  method f6 : [ `Plus | `Minus | `Unknown ]
end

class type fasta = object
  inherit text_file
  method format : [`fasta]
end

class type fastq = object
  inherit text_file
  method format : [`fastq]
end

class type sanger_fastq = object
  inherit fastq
  method phred_encoding : [`sanger]
end

class type solexa_fastq = object
  inherit fastq
  method phred_encoding : [`solexa]
end

class type phred64_fastq = object
  inherit fastq
  method phred_encoding : [`phred64]
end

class type gff = object
  inherit tsv
  method header : [`no]
  method f1 : string
  method f2 : string
  method f3 : string
  method f4 : int
  method f5 : int
  method f6 : float
  method f7 : [`Plus | `Minus]
  method f8 : [`frame0 | `frame1 | `frame2]
  method f9 : string
end

class type gff2 = object
  inherit gff
  method version : [`v2]
end

class type gff3 = object
  inherit gff
  method version : [`v3]
end

class type indexed_bam = object
  inherit directory
  method contents : [`indexed_bam]
end

class type sam = object
  inherit text_file
  method format : [`sam]
end

class type sra = object
  inherit binary_file
  method format : [`sra]
end

(** {3 File_formats} *)

module Bed : sig
  val keep3 : #bed3 pworkflow -> bed3 pworkflow
  val keep4 : #bed4 pworkflow -> bed4 pworkflow
  val keep5 : #bed5 pworkflow -> bed5 pworkflow
  val keep6 : #bed6 pworkflow -> bed6 pworkflow
end

module Fastq : sig
  type _ format =
    | Sanger  : sanger_fastq format
    | Solexa  : solexa_fastq format
    | Phred64 : phred64_fastq format
  (* val to_sanger : 'a format -> < fastq ; phred_encoding : 'a ; .. > pworkflow -> sanger_fastq pworkflow *)

  val concat : (#fastq as 'a) pworkflow list -> 'a pworkflow
  val head : int -> (#fastq as 'a) pworkflow -> 'a pworkflow
end

(** {3 Genome databases} *)

module Ucsc_gb : sig

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
    inherit text_file
  end

  class type bigWig = object
    method format : [`bigWig]
    inherit binary_file
  end

  type genome = [ `dm3 | `droSim1 | `hg18 | `hg19 | `hg38 | `mm8 | `mm9 | `mm10 | `sacCer2 ]
  val string_of_genome : [< genome] -> string
  val genome_of_string : string -> genome option

  (** {4 Dealing with genome sequences} *)
  class type chromosome_sequences = object
    inherit directory
    method contents : [`ucsc_chromosome_sequences]
  end

  val chromosome_sequence :
    [< genome] ->
    string ->
    fasta pworkflow
  val chromosome_sequences : [< genome] -> chromosome_sequences pworkflow
  val genome_sequence : [< genome] -> fasta pworkflow
  val genome_2bit_sequence : [< genome] -> twobit pworkflow
  val twoBitToFa : twobit pworkflow -> #bed4 pworkflow -> fasta pworkflow


  (** {4 Chromosome size and clipping} *)
  val fetchChromSizes : [< genome] -> chrom_sizes pworkflow
  val bedClip : chrom_sizes pworkflow -> (#bed3 as 'a) pworkflow -> 'a pworkflow


  (** {4 Conversion between annotation file formats} *)
  (* val wig_of_bigWig : bigWig file -> wig file *)
  (* val bigWig_of_wig : ?clip:bool -> [< genome] -> wig file -> bigWig file *)
  val bedGraphToBigWig : [< genome] -> bedGraph pworkflow -> bigWig pworkflow

  val bedToBigBed :
    [< genome] ->
    [ `bed3 of bed3 pworkflow | `bed5 of bed5 pworkflow ] ->
    bigBed pworkflow
  (** bedToBigBed utility. Fails when given an empty BED file on
      input. Note that the underlying bedToBigBed expects BED
      files with {i exactly} 3 or 5 columns. *)

  val bedToBigBed_failsafe :
    [< genome] ->
    [ `bed3 of bed3 pworkflow | `bed5 of bed5 pworkflow ] ->
    bigBed pworkflow
  (** sam  as {! Ucsc_gb.bedToBigBed} but produces an empty file when
      given an empty BED on input. *)


  (* val wg_encode_crg_mappability_36  : [`mm9 | `hg18 | `hg19] -> bigWig file *)
  (* val wg_encode_crg_mappability_40  : [`mm9 | `hg18 | `hg19] -> bigWig file *)
  (* val wg_encode_crg_mappability_50  : [`mm9 | `hg18 | `hg19] -> bigWig file *)
  (* val wg_encode_crg_mappability_75  : [`mm9 | `hg18 | `hg19] -> bigWig file *)
  (* val wg_encode_crg_mappability_100 : [`mm9 | `hg18 | `hg19] -> bigWig file *)


  module Lift_over : sig
    class type chain_file = object
      inherit file
      method format : [`lift_over_chain_file]
    end
    class type ['a] output = object
      inherit directory
      method format : [`ucsc_lift_over of 'a]
    end

    val chain_file :
      org_from:[< genome] ->
      org_to:[< genome] ->
      chain_file pworkflow

    val bed :
      org_from:[< genome] ->
      org_to:[< genome] ->
      (* chain_file pworkflow -> *)
      (#bed3 as 'a) pworkflow ->
      'a output pworkflow

    val mapped : 'a output pworkflow -> 'a pworkflow
    val unmapped : 'a output pworkflow -> 'a pworkflow
  end
end

module Ensembl : sig

  type species = [
    | `homo_sapiens
    | `mus_musculus
  ]

  val ucsc_reference_genome : release:int -> species:species -> Ucsc_gb.genome

  val gff : ?chr_name : [`ensembl | `ucsc] -> release:int -> species:species -> gff pworkflow
  val gtf : ?chr_name : [`ensembl | `ucsc] -> release:int -> species:species -> gff pworkflow

  val cdna : release:int -> species:species -> fasta gz pworkflow
end

(** {3 NGS utilities} *)

module Bedtools : sig
  val img : Shell_dsl.container_image list

  type 'a input

  val bed : #bed3 input
  val gff : gff input


  module Cmd : sig
    val slop :
      ?strand:bool ->
      ?header:bool ->
      mode:[
        | `both of int
        | `left of int
        | `right of int
        | `both_pct of float
        | `left_pct of float
        | `right_pct of float
      ] ->
      'a pworkflow ->
      Ucsc_gb.chrom_sizes pworkflow ->
      Bistro.Shell_dsl.command
  end


  val slop :
    ?strand:bool ->
    ?header:bool ->
    mode:[
      | `both of int
      | `left of int
      | `right of int
      | `both_pct of float
      | `left_pct of float
      | `right_pct of float
    ] ->
    'a input ->
    'a pworkflow ->
    Ucsc_gb.chrom_sizes pworkflow ->
    'a pworkflow


  val intersect :
    ?ubam:bool ->
    ?wa:bool ->
    ?wb:bool ->
    ?loj:bool ->
    ?wo:bool ->
    ?wao:bool ->
    ?u:bool ->
    ?c:bool ->
    ?v:bool ->
    ?f:float ->
    ?_F:float ->
    ?r:bool ->
    ?e:bool ->
    ?s:bool ->
    ?_S:bool ->
    ?split:bool ->
    ?sorted:bool ->
    ?g:Ucsc_gb.chrom_sizes pworkflow ->
    ?header:bool ->
    ?filenames:bool ->
    ?sortout:bool ->
    'a input ->
    'a pworkflow ->
    #bed3 pworkflow list ->
    'a pworkflow

  val bamtobed :
    ?bed12:bool ->
    ?split:bool ->
    ?splitD:bool ->
    ?ed:bool ->
    ?tag:bool ->
    ?cigar:bool ->
    bam pworkflow ->
    #bed6 pworkflow

end


module Deeptools : sig

  type 'a signal_format
  val bigwig : Ucsc_gb.bigWig signal_format
  val bedgraph : Ucsc_gb.bedGraph signal_format

  type 'a img_format
  val png : png img_format
  val pdf : pdf img_format
  val svg : svg img_format

  val bamcoverage :
    ?scalefactor:float ->
    ?filterrnastrand: [ `forward | `reverse ] ->
    ?binsize:int ->
    ?blacklist:#bed3 pworkflow ->
    ?threads:int ->
    ?normalizeUsing:[`RPKM | `CPM | `BPM | `RPGC] ->
    ?ignorefornormalization:string list ->
    ?skipnoncoveredregions:bool ->
    ?smoothlength:int ->
    ?extendreads:int ->
    ?ignoreduplicates:bool ->
    ?minmappingquality:int ->
    ?centerreads:bool ->
    ?samflaginclude:int ->
    ?samflagexclude:int ->
    ?minfragmentlength:int ->
    ?maxfragmentlength:int ->
    'a signal_format ->
    indexed_bam pworkflow ->
    'a pworkflow


  val bamcompare :
    ?scalefactormethod : [ `readcount | `ses ] ->
    ?samplelength:int ->
    ?numberofsamples:int ->
    ?scalefactor:float ->
    ?ratio: [ `log2 | `ratio | `subtract | `add | `mean | `reciprocal_ratio | `first | `second ] ->
    ?pseudocount:int ->
    ?binsize:int ->
    ?region:string ->
    ?blacklist:#bed3 pworkflow ->
    ?threads:int ->
    ?normalizeUsing:[`RPKM | `CPM | `BPM | `RPGC] ->
    ?ignorefornormalization:string list ->
    ?skipnoncoveredregions:bool ->
    ?smoothlength:int ->
    ?extendreads:int ->
    ?ignoreduplicates:bool ->
    ?minmappingquality:int ->
    ?centerreads:bool ->
    ?samflaginclude:int ->
    ?samflagexclude:int ->
    ?minfragmentlength:int ->
    ?maxfragmentlength:int ->
    'a signal_format ->
    indexed_bam pworkflow ->
    indexed_bam pworkflow ->
    'a pworkflow


  val bigwigcompare :
    ?scalefactor:float ->
    ?ratio: [ `log2 | `ratio | `subtract | `add | `mean | `reciprocal_ratio | `first | `second ] ->
    ?pseudocount:int ->
    ?binsize:int ->
    ?region:string ->
    ?blacklist:#bed3 pworkflow ->
    ?threads:int ->
    'a signal_format ->
    Ucsc_gb.bigWig pworkflow ->
    Ucsc_gb.bigWig pworkflow ->
    'a pworkflow

  class type compressed_numpy_array = object
    inherit binary_file
    method format : [`compressed_numpy_array]
  end

  val multibamsummary_bins :
    ?binsize:int ->
    ?distancebetweenbins:int ->
    ?region:string ->
    ?blacklist:#bed3 pworkflow ->
    ?threads:int ->
    ?outrawcounts:bool ->
    ?extendreads:int ->
    ?ignoreduplicates:bool ->
    ?minmappingquality:int ->
    ?centerreads:bool ->
    ?samflaginclude:int ->
    ?samflagexclude:int ->
    ?minfragmentlength:int ->
    ?maxfragmentlength:int ->
    indexed_bam pworkflow list ->
    compressed_numpy_array pworkflow


  val multibamsummary_bed :
    ?region:string ->
    ?blacklist:#bed3 pworkflow ->
    ?threads:int ->
    ?outrawcounts:bool ->
    ?extendreads:int ->
    ?ignoreduplicates:bool ->
    ?minmappingquality:int ->
    ?centerreads:bool ->
    ?samflaginclude:int ->
    ?samflagexclude:int ->
    ?minfragmentlength:int ->
    ?maxfragmentlength:int ->
    ?metagene:bool ->
    ?transcriptid:bool ->
    ?exonid:bool ->
    ?transcriptiddesignator:bool->
    #bed3 pworkflow ->
    indexed_bam pworkflow list ->
    compressed_numpy_array pworkflow

  class type deeptools_matrix = object
    inherit binary_file
    method format : [`deeptools_matrix]
  end

  val computeMatrix_reference_point :
    ?referencePoint:[`TSS | `TES | `center] ->
    ?upstream:int ->
    ?downstream:int ->
    ?nanAfterEnd:bool ->
    ?binSize:int ->
    ?sortRegions:[`descend | `ascend | `no | `keep] ->
    ?sortUsing:[`mean | `median | `max | `min | `sum | `region_length] ->
    ?sortUsingSamples:int list ->
    ?averageTypeBins:[`mean | `median | `min | `max | `std | `sum] ->
    ?missingDataAsZero:bool ->
    ?skipZeros:bool ->
    ?minThreshold:float ->
    ?maxThreshold:float ->
    ?blackList:#bed3 pworkflow ->
    ?scale:float ->
    ?numberOfProcessors:int ->
    regions:#bed3 pworkflow list ->
    scores:Ucsc_gb.bigWig pworkflow list ->
    unit ->
    deeptools_matrix gz pworkflow

  val plotHeatmap :
    ?dpi:int ->
    ?kmeans:int ->
    ?hclust:int ->
    ?sortRegions:[`descend | `ascend | `no] ->
    ?sortUsing:[`mean | `median | `max | `min | `sum | `region_length] ->
    ?sortUsingSamples:int list ->
    ?averageTypeSummaryPlot:[`mean | `median | `min | `max | `std | `sum] ->
    ?missingDataColor:string ->
    ?colorMap:string ->
    ?alpha:float ->
    ?colorList:string list ->
    ?colorNumber:int ->
    ?zMin:float list ->
    ?zMax:float list ->
    ?heatmapHeight:float ->
    ?heatmapWidth:float ->
    ?whatToShow:[`plot_heatmap_and_colorbar | `plot_and_heatmap | `heatmap_only | `heatmap_and_colorbar] ->
    ?boxAroundHeatmaps:bool ->
    ?xAxisLabel:string ->
    ?startLabel:string ->
    ?endLabel:string ->
    ?refPointLabel:string ->
    ?regionsLabel:string list ->
    ?samplesLabel:string list ->
    ?plotTitle:string ->
    ?yAxisLabel:string ->
    ?yMin:float list ->
    ?yMax:float list ->
    ?legendLocation:[`best | `upper_right | `upper_left | `upper_center | `lower_left | `lower_right | `lower_center | `center | `center_left | `center_right | `none] ->
    ?perGroup:bool ->
    'a img_format ->
    deeptools_matrix gz pworkflow ->
    'a pworkflow

  val plotCorrelation :
    ?skipZeros:bool ->
    ?labels:string list ->
    ?plotTitle:string ->
    ?removeOutliers:bool ->
    ?colorMap:string ->
    ?plotNumbers:bool ->
    ?log1p:bool ->
    corMethod:[`spearman | `pearson] ->
    whatToPlot:[`heatmap | `scatterplot] ->
    'a img_format ->
    compressed_numpy_array pworkflow ->
    directory pworkflow

  (* val plotProfile : *)
  (*   ?dpi:int -> *)
  (*   ?kmeans:int -> *)
  (*   ?hclust:int -> *)
  (*   ?averageType:[`mean | `median | `min | `max | `std | `sum] -> *)
  (*   ?plotHeight:float -> (\** in cm *\) *)
  (*   ?plotWidth:float -> *)
  (*   ?plotType:[`lines | `fill | `se | `std | `overlapped_lines | `heatmap] -> *)
  (*   ?colors:string list -> *)
  (*   ?numPlotsPerRow:int -> *)
  (*   ?startLabel:string -> *)
  (*   ?endLabel:string -> *)
  (*   ?refPointLabel:string -> *)
  (*   ?regionsLabel:string list -> *)
  (*   ?samplesLabel:string list -> *)
  (*   ?plotTitle:string -> *)
  (*   ?yAxisLabel:string -> *)
  (*   ?yMin:int list -> *)
  (*   ?yMax:int list -> *)
  (*   ?legendLocation:[`best | `upper_right | `upper_left | `upper_center | `lower_left | `lower_right | `lower_center | `center | `center_left | `center_right | `none] -> *)
  (*   ?perGroup:bool -> *)
  (*   'a output -> *)
  (*   deeptools_matrix gz pworkflow -> *)
  (*   'a pworkflow *)
end

module Htseq : sig

  class type count_tsv = object
    inherit tsv
    method header : [`no]
    method f1 : string
    method f2 : int
  end

  val count :
    ?order:[`name | `position] ->
    ?mode:[`union | `intersection_strict | `intersection_nonempty] ->
    ?stranded:[` yes | `no | `reverse] ->
    ?feature_type:string ->
    ?minaqual:int ->
    ?idattribute:string ->
    [`sam of sam pworkflow | `bam of bam pworkflow] ->
    gff pworkflow ->
    count_tsv pworkflow
end


module Samtools : sig

  type 'a format

  val bam : bam format
  val sam : sam format

  val sort :
    ?on:[`name | `position] ->
    bam pworkflow -> bam pworkflow
  val indexed_bam_of_sam : sam pworkflow -> indexed_bam pworkflow
  val indexed_bam_of_bam : bam pworkflow -> indexed_bam pworkflow
  val indexed_bam_to_bam : indexed_bam pworkflow -> bam pworkflow
  val bam_of_sam : sam pworkflow -> bam pworkflow
  val sam_of_bam : bam pworkflow -> sam pworkflow

  (* val rmdup : ?single_end_mode:bool -> bam pworkflow -> bam pworkflow *)

  val view :
    output:'o format ->
    (* ?_1:bool ->
     * ?u:bool -> *)
    ?h:bool ->
    ?_H:bool ->
    (* ?c:bool -> *)
    (* ?_L: #bed3 pworkflow -> *)
    ?q:int ->
    (* ?m:int ->
     * ?f:int ->
     * ?_F:int ->
     * ?_B:bool ->
     * ?s:float -> *)
    < file_kind : [`regular] ;
      format : [< `bam | `sam] ; .. > pworkflow ->
    'o pworkflow
end

module Picardtools : sig
  val img : Shell_dsl.container_image list

  val markduplicates :
    ?remove_duplicates:bool ->
    [`indexed_bam] dworkflow ->
    [`picard_markduplicates] dworkflow

  val reads :
    [`picard_markduplicates] dworkflow ->
    bam pworkflow

  val sort_bam_by_name :
    bam pworkflow ->
    bam pworkflow
end

module Sra_toolkit : sig
  val img : Shell_dsl.container_image list

  val fastq_dump :
    [`id of string | `idw of string workflow | `file of sra pworkflow] ->
    sanger_fastq pworkflow

  val fastq_dump_gz :
    [`id of string | `file of sra pworkflow] ->
    sanger_fastq gz pworkflow

  val fastq_dump_pe : sra pworkflow -> sanger_fastq pworkflow * sanger_fastq pworkflow

  val fastq_dump_pe_gz :
    [`id of string | `file of sra pworkflow] ->
    sanger_fastq gz pworkflow * sanger_fastq gz pworkflow

  val fastq_dump_to_fasta : sra pworkflow -> fasta pworkflow

end

(** http://subread.sourceforge.net/ *)
module Subread : sig
  class type count_table = object
    inherit tsv
    method header : [`no]
    method f1 : string
    method f2 : string
    method f3 : int
    method f4 : int
    method f5 : [`Plus | `Minus]
    method f6 : int
    method f7 : int
  end

  val featureCounts :
    ?feature_type:string ->
    ?attribute_type:string ->
    ?strandness:[`Unstranded | `Stranded | `Reversely_stranded] ->
    ?q:int ->
    ?nthreads:int ->
    gff pworkflow ->
    < format : [< `bam | `sam] ; .. > pworkflow -> (*FIXME: handle paired-hand, just add other file next to the other*)
    [`featureCounts] dworkflow

  val featureCounts_tsv : [`featureCounts] dworkflow -> count_table pworkflow
  val featureCounts_htseq_tsv : [`featureCounts] dworkflow -> Htseq.count_tsv pworkflow
  val featureCounts_summary : [`featureCounts] dworkflow -> text_file pworkflow
end

(** {3 NGS quality} *)

module ChIPQC : sig
  type 'a sample = {
    id : string ;
    tissue : string ;
    factor : string ;
    replicate : string ;
    bam : indexed_bam pworkflow ;
    peaks : (#bed3 as 'a) pworkflow ;
  }

  class type output = object
    inherit directory
    method contents : [`ChIPQC]
  end

  val run : 'a sample list -> output pworkflow
end


module FastQC : sig

  class type report = object
    inherit directory
    method contents : [`fastQC_report]
  end

  val run : #fastq pworkflow -> report pworkflow
  val html_report : report pworkflow -> html pworkflow
  val per_base_quality : report pworkflow -> png pworkflow
  val per_base_sequence_content : report pworkflow -> png pworkflow
end

module Fastq_screen : sig
  class type output = object
    inherit directory
    method contents : [`fastq_screen]
  end

  val fastq_screen :
    ?bowtie2_opts:string ->
    ?filter: [ `Not_map | `Uniquely | `Multi_maps | `Maps | `Not_map_or_Uniquely | `Not_map_or_Multi_maps | `Ignore ] list ->
    ?illumina:bool ->
    ?nohits:bool ->
    ?pass:int ->
    ?subset:int ->
    ?tag:bool ->
    ?threads:int ->
    ?top: [ `top1 of int | `top2 of int * int ] ->
    ?lightweight:bool ->
    #fastq pworkflow ->
    (string * fasta pworkflow) list ->
    output pworkflow

  val html_report : output pworkflow -> html pworkflow

end

(** {3 NGS aligners} *)

module Bowtie : sig

  class type index = object
    method contents : [`bowtie_index]
    inherit directory
  end

  val bowtie_build :
    ?packed:bool ->
    ?color:bool  ->
    fasta pworkflow -> index pworkflow

  val bowtie :
    ?l:int -> ?e:int -> ?m:int ->
    ?fastq_format:'a Fastq.format ->
    ?n:int -> ?v:int ->
    ?maxins:int ->
    index pworkflow ->
    [ `single_end of 'a pworkflow list
    | `paired_end of 'a pworkflow list * 'a pworkflow list ] ->
    sam pworkflow
end

module Bowtie2 : sig

  class type index = object
    method contents : [`bowtie2_index]
    inherit directory
  end

  val bowtie2_build :
    ?large_index:bool ->
    ?noauto:bool ->
    ?packed:bool ->
    ?bmax:int ->
    ?bmaxdivn:int ->
    ?dcv:int ->
    ?nodc:bool ->
    ?noref:bool ->
    ?justref:bool ->
    ?offrate:int ->
    ?ftabchars:int ->
    ?seed:int ->
    ?cutoff:int ->
    fasta pworkflow ->
    index pworkflow

  val bowtie2 :
    ?skip:int ->
    ?qupto:int ->
    ?trim5:int ->
    ?trim3:int ->
    ?preset:[`very_fast | `fast | `sensitive | `very_sensitive] ->
    ?_N:int ->
    ?_L:int ->
    ?ignore_quals:bool ->
    ?mode:[ `end_to_end | `local ] ->
    ?a:bool ->
    ?k:int ->
    ?_D:int ->
    ?_R:int ->
    ?minins:int ->
    ?maxins:int ->
    ?orientation:[`fr | `ff | `rf] ->
    ?no_mixed:bool ->
    ?no_discordant:bool ->
    ?dovetail:bool ->
    ?no_contain:bool ->
    ?no_overlap:bool ->
    ?no_unal:bool ->
    ?seed:int ->
    ?fastq_format:'a Fastq.format ->
    index pworkflow ->
    [ `single_end of 'a pworkflow list
    | `paired_end of 'a pworkflow list * 'a pworkflow list ] ->
    sam pworkflow
end


module Tophat : sig
  class type output = object
    inherit directory
    method contents : [`tophat]
  end

  val tophat1 :
    ?color:bool ->
    Bowtie.index pworkflow ->
    [ `single_end of #fastq pworkflow list
    | `paired_end of (#fastq as 'a) pworkflow list * 'a pworkflow list ] ->
    output pworkflow

  val tophat2 :
    Bowtie2.index pworkflow ->
    [ `single_end of #fastq pworkflow list
    | `paired_end of (#fastq as 'a) pworkflow list * 'a pworkflow list ] ->
    output pworkflow

  val accepted_hits : output pworkflow -> bam pworkflow
  val junctions : output pworkflow -> bed6 pworkflow
end

module Hisat2 : sig
  val img : Shell_dsl.container_image list

  val hisat2_build :
    ?large_index:bool ->
    ?noauto:bool ->
    ?packed:bool ->
    ?bmax:int ->
    ?bmaxdivn:int ->
    ?dcv:int ->
    ?nodc:bool ->
    ?noref:bool ->
    ?justref:bool ->
    ?offrate:int ->
    ?ftabchars:int ->
    ?seed:int ->
    ?cutoff:int ->
    fasta pworkflow ->
    [`hisat2_index] dworkflow


  val hisat2 :
    ?skip:int ->
    ?qupto:int ->
    ?trim5:int ->
    ?trim3:int ->
    ?fastq_format:'a Fastq.format ->
    ?k:int ->
    ?minins:int ->
    ?maxins:int ->
    ?orientation:[`fr | `ff | `rf] ->
    ?no_mixed:bool ->
    ?no_discordant:bool ->
    ?seed:int ->
    [`hisat2_index] dworkflow ->
    [ `single_end of sanger_fastq pworkflow list
    | `paired_end of sanger_fastq pworkflow list * sanger_fastq pworkflow list ] ->
    sam pworkflow
end

module Kallisto : sig
  class type index = object
    inherit binary_file
    method format : [`kallisto_index]
  end

  class type abundance_table = object
    inherit tsv
    method f1 : [`target_id] * string
    method f2 : [`length] * int
    method f3 : [`eff_length] * int
    method f4 : [`est_counts] * float
    method f5 : [`tpm] * float
  end

  val img : Shell_dsl.container_image list
  val index : fasta pworkflow list -> index pworkflow
  val quant :
    ?bootstrap_samples:int ->
    ?threads:int ->
    ?fragment_length:float ->
    ?sd:float ->
    index pworkflow ->
    fq1:[`fq of sanger_fastq pworkflow | `fq_gz of sanger_fastq gz pworkflow] ->
    ?fq2:[`fq of sanger_fastq pworkflow | `fq_gz of sanger_fastq gz pworkflow] ->
    unit ->
    [`kallisto_output] dworkflow

  val abundance : [`kallisto_output] dworkflow -> abundance_table pworkflow

  val merge_eff_counts :
    sample_ids:string list ->
    kallisto_outputs:abundance_table pworkflow list ->
    tsv pworkflow

  val merge_tpms :
    sample_ids:string list ->
    kallisto_outputs:abundance_table pworkflow list ->
    tsv pworkflow
end

(** {3 Genome assembly} *)

module Spades : sig
  val spades :
    ?single_cell:bool ->
    ?iontorrent:bool ->
    ?pe:sanger_fastq pworkflow list * sanger_fastq pworkflow list ->
    ?threads:int ->
    ?memory:int ->
    unit ->
    [`spades] dworkflow

  val contigs : [`spades] dworkflow -> fasta pworkflow
  val scaffolds : [`spades] dworkflow -> fasta pworkflow
end

module Idba : sig
  val fq2fa :
    ?filter:bool ->
    [ `Se of sanger_fastq pworkflow
    | `Pe_merge of sanger_fastq pworkflow * sanger_fastq pworkflow
    | `Pe_paired of sanger_fastq pworkflow ] ->
    fasta pworkflow

  val idba_ud : ?mem_spec:int -> fasta pworkflow -> [`idba] dworkflow

  val idba_ud_contigs : [`idba] dworkflow -> fasta pworkflow
  val idba_ud_scaffolds : [`idba] dworkflow -> fasta pworkflow
end

module Cisa : sig
  val merge :
    ?min_length:int ->
    (string * fasta pworkflow) list -> fasta pworkflow

  val cisa :
    genome_size:int ->
    fasta pworkflow ->
    fasta pworkflow
end

module Quast : sig
  val quast :
    ?reference:fasta pworkflow ->
    ?labels:string list ->
    fasta pworkflow list ->
    [`quast] dworkflow
end

(** {3 Differential analysis} *)

module DESeq2 : sig

  val img : Shell_dsl.container_image list

  class type table = object
    inherit tsv
    method header : [`yes]
  end

  type output =
    <
      comparison_summary : table pworkflow ;
      comparisons : ((string * string * string) * table pworkflow) list ;
      effect_table : table pworkflow ;
      normalized_counts : table pworkflow ;
      sample_clustering : svg pworkflow ;
      sample_pca : svg pworkflow ;
      directory : directory pworkflow
    >

  val main_effects :
    string list ->
    (string list * #Htseq.count_tsv pworkflow) list ->
    output
end

(** {3 Peak callers } *)

module Macs : sig
  type gsize = [`hs | `mm | `ce | `dm | `gsize of int]
  type keep_dup = [ `all | `auto | `int of int ]

  type _ format

  class type output = object
    inherit directory
    method contents : [`macs]
  end

  val sam : sam format
  val bam : bam format

  val run :
    ?control: 'a pworkflow list ->
    ?petdist:int ->
    ?gsize:gsize ->
    ?tsize:int ->
    ?bw:int ->
    ?pvalue:float ->
    ?mfold:int * int ->
    ?nolambda:bool ->
    ?slocal:int ->
    ?llocal:int ->
    ?on_auto:bool ->
    ?nomodel:bool ->
    ?shiftsize:int ->
    ?keep_dup:keep_dup ->
    ?to_large:bool ->
    ?wig:bool ->
    ?bdg:bool ->
    ?single_profile:bool ->
    ?space:int ->
    ?call_subpeaks:bool ->
    ?diag:bool ->
    ?fe_min:int ->
    ?fe_max:int ->
    ?fe_step:int ->
    'a format ->
    'a pworkflow list ->
    output pworkflow

  class type peaks_xls = object
    inherit bed3
    method f4 : int
    method f5 : int
    method f6 : int
    method f7 : float
    method f8 : float
    method f9 : float
  end

  val peaks_xls : output pworkflow -> peaks_xls pworkflow

  class type narrow_peaks = object
    inherit bed5
    method f6 : string
    method f7 : float
    method f8 : float
    method f9 : float
    method f10 : int
  end

  val narrow_peaks :
    output pworkflow -> narrow_peaks pworkflow

  class type peak_summits = object
    inherit bed4
    method f5 : float
  end

  val peak_summits :
    output pworkflow -> peak_summits pworkflow
end

module Macs2 : sig

  val pileup :
    ?extsize:int ->
    ?both_direction:bool ->
    bam pworkflow -> Ucsc_gb.bedGraph pworkflow

  type gsize = [`hs | `mm | `ce | `dm | `gsize of int]
  type keep_dup = [ `all | `auto | `int of int ]

  type _ format

  val sam : sam format
  val bam : bam format

  class type output = object
    inherit directory
    method contents : [`macs2]
  end

  class type narrow_output = object
    inherit output
    method peak_type : [`narrow]
  end

  class type broad_output = object
    inherit output
    method peak_type : [`broad]
  end

  val callpeak :
    ?pvalue:float ->
    ?qvalue:float ->
    ?gsize:gsize ->
    ?call_summits:bool ->
    ?fix_bimodal:bool ->
    ?mfold:int * int ->
    ?extsize:int ->
    ?nomodel:bool ->
    ?bdg:bool ->
    ?control:'a pworkflow list ->
    ?keep_dup:keep_dup ->
    'a format ->
    'a pworkflow list ->
    narrow_output pworkflow

  class type peaks_xls = object
    inherit bed3
    method f4 : int
    method f5 : int
    method f6 : int
    method f7 : float
    method f8 : float
    method f9 : float
  end

  val peaks_xls : #output pworkflow -> peaks_xls pworkflow

  class type narrow_peaks = object
    inherit bed5
    method f6 : string
    method f7 : float
    method f8 : float
    method f9 : float
    method f10 : int
  end

  val narrow_peaks : narrow_output pworkflow -> narrow_peaks pworkflow

  class type peak_summits = object
    inherit bed4
    method f5 : float
  end

  val peak_summits : #output pworkflow -> peak_summits pworkflow

  val callpeak_broad :
    ?pvalue:float ->
    ?qvalue:float ->
    ?gsize:gsize ->
    ?call_summits:bool ->
    ?fix_bimodal:bool ->
    ?mfold:int * int ->
    ?extsize:int ->
    ?nomodel:bool ->
    ?bdg:bool ->
    ?control:'a pworkflow list ->
    ?keep_dup:keep_dup ->
    'a format ->
    'a pworkflow list ->
    broad_output pworkflow

  class type broad_peaks = object
    inherit bed5
    method f6 : string
    method f7 : float
    method f8 : float
    method f9 : float
  end

  val broad_peaks : broad_output pworkflow -> broad_peaks pworkflow
end


module Meme_suite : sig
  class type meme_chip_output = object
    inherit directory
    method contents : [`meme_chip_output]
  end

  val meme :
    ?nmotifs:int ->
    ?minw:int ->
    ?maxw:int ->
    ?revcomp:bool ->
    ?maxsize:int ->
    ?alphabet:[`dna | `rna | `protein] ->
    (* ?threads:int -> *)
    fasta pworkflow ->
    directory pworkflow

  val meme_chip :
    ?meme_nmotifs:int ->
    ?meme_minw:int ->
    ?meme_maxw:int ->
    (* ?np:int -> *)
    fasta pworkflow ->
    meme_chip_output pworkflow

  val fimo :
    ?alpha: float ->
    ?bgfile: string ->
    ?max_stored_scores: int ->
    ?motif: string ->
    ?motif_pseudo: float ->
    ?qv_thresh:bool ->
    ?thresh: float ->
    meme_chip_output pworkflow ->
    fasta pworkflow ->
    directory pworkflow
end

module Prokka : sig
  val run :
    ?prefix:string ->
    ?addgenes:bool ->
    ?locustag:string ->
    ?increment:int ->
    ?gffver:string ->
    ?compliant:bool ->
    ?centre:string ->
    ?genus:string ->
    ?species:string ->
    ?strain:string ->
    ?plasmid:string ->
    ?kingdom:string ->
    ?gcode:int ->
    ?gram: [ `Plus | `Minus ] ->
    ?usegenus:bool ->
    ?proteins:string ->
    ?hmms:string ->
    ?metagenome:bool ->
    ?rawproduct:bool ->
    ?fast:bool ->
    ?threads:int ->
    ?mincontiglen:int ->
    ?evalue:float ->
    ?rfam:bool ->
    ?norrna:bool ->
    ?notrna:bool ->
    ?rnammer:bool ->
    fasta pworkflow ->
    directory pworkflow

end

module Srst2 : sig

  val run_gen_cmd :
    ?mlst_db:fasta pworkflow ->
    ?mlst_delimiter:string ->
    ?mlst_definitions:fasta pworkflow ->
    ?mlst_max_mismatch:int ->
    ?gene_db:fasta pworkflow list ->
    ?no_gene_details:bool ->
    ?gene_max_mismatch:int ->
    ?min_coverage:int ->
    ?max_divergence:int ->
    ?min_depth:int ->
    ?min_edge_depth:int ->
    ?prob_err:float ->
    ?truncation_score_tolerance:int ->
    ?other:string ->
    ?max_unaligned_overlap:int ->
    ?mapq:int ->
    ?baseq:int ->
    ?samtools_args:string ->
    ?report_new_consensus:bool ->
    ?report_all_consensus:bool ->
    string ->
    Shell_dsl.template list ->
    Shell_dsl.command


  val run_se :
    ?mlst_db:fasta pworkflow ->
    ?mlst_delimiter:string ->
    ?mlst_definitions:fasta pworkflow ->
    ?mlst_max_mismatch:int ->
    ?gene_db:fasta pworkflow list ->
    ?no_gene_details:bool ->
    ?gene_max_mismatch:int ->
    ?min_coverage:int ->
    ?max_divergence:int ->
    ?min_depth:int ->
    ?min_edge_depth:int ->
    ?prob_err:float ->
    ?truncation_score_tolerance:int ->
    ?other:string ->
    ?max_unaligned_overlap:int ->
    ?mapq:int ->
    ?baseq:int ->
    ?samtools_args:string ->
    ?report_new_consensus:bool ->
    ?report_all_consensus:bool ->
    ?threads:int ->
    #fastq pworkflow list ->
    directory pworkflow


  val run_pe :
    ?mlst_db:fasta pworkflow ->
    ?mlst_delimiter:string ->
    ?mlst_definitions:fasta pworkflow ->
    ?mlst_max_mismatch:int ->
    ?gene_db:fasta pworkflow list ->
    ?no_gene_details:bool ->
    ?gene_max_mismatch:int ->
    ?min_coverage:int ->
    ?max_divergence:int ->
    ?min_depth:int ->
    ?min_edge_depth:int ->
    ?prob_err:float ->
    ?truncation_score_tolerance:int ->
    ?other:string ->
    ?max_unaligned_overlap:int ->
    ?mapq:int ->
    ?baseq:int ->
    ?samtools_args:string ->
    ?report_new_consensus:bool ->
    ?report_all_consensus:bool ->
    ?threads:int ->
    #fastq pworkflow list ->
    directory pworkflow
end
