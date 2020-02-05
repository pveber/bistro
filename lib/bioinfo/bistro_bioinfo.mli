open Bistro

module SE_or_PE : sig
  type 'a t =
    | Single_end of 'a
    | Paired_end of 'a * 'a

  val map : 'a t -> f:('a -> 'b) -> 'b t
end

(** {3 File_formats} *)

module Bed : sig
  val keep3 : #bed3 file -> bed3 file
  val keep4 : #bed4 file -> bed4 file
  val keep5 : #bed5 file -> bed5 file
  val keep6 : #bed6 file -> bed6 file
end

module Fastq : sig
  type _ format =
    | Sanger  : sanger_fastq format
    | Solexa  : solexa_fastq format
    | Phred64 : phred64_fastq format
  (* val to_sanger : 'a format -> < fastq ; phred_encoding : 'a ; .. > file -> sanger_fastq file *)

  val concat : (#fastq as 'a) file list -> 'a file
  val head : int -> (#fastq as 'a) file -> 'a file
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
  val chromosome_sequence :
    [< genome] ->
    string ->
    fasta file
  val chromosome_sequences : [< genome] -> [`ucsc_chromosome_sequences] directory
  val genome_sequence : [< genome] -> fasta file
  val genome_2bit_sequence : [< genome] -> twobit file
  val twoBitToFa : twobit file -> #bed4 file -> fasta file


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

    val chain_file :
      org_from:[< genome] ->
      org_to:[< genome] ->
      chain_file file

    val bed :
      org_from:[< genome] ->
      org_to:[< genome] ->
      (* chain_file file -> *)
      (#bed3 as 'a) file ->
      [`ucsc_lift_over of 'a] directory

    val mapped : [`ucsc_lift_over of 'a] directory -> 'a file
    val unmapped : [`ucsc_lift_over of 'a] directory -> 'a file
  end
end

module Ensembl : sig

  type species = [
    | `homo_sapiens
    | `mus_musculus
  ]

  val ucsc_reference_genome : release:int -> species:species -> Ucsc_gb.genome

  val gff : ?chr_name : [`ensembl | `ucsc] -> release:int -> species:species -> gff file
  val gtf : ?chr_name : [`ensembl | `ucsc] -> release:int -> species:species -> gff file

  val cdna : release:int -> species:species -> fasta gz file
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
      'a file ->
      Ucsc_gb.chrom_sizes file ->
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
    'a file ->
    Ucsc_gb.chrom_sizes file ->
    'a file


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
    ?g:Ucsc_gb.chrom_sizes file ->
    ?header:bool ->
    ?filenames:bool ->
    ?sortout:bool ->
    'a input ->
    'a file ->
    #bed3 file list ->
    'a file

  val bamtobed :
    ?bed12:bool ->
    ?split:bool ->
    ?splitD:bool ->
    ?ed:bool ->
    ?tag:bool ->
    ?cigar:bool ->
    bam file ->
    #bed6 file

  val closest :
    ?strand:[`same | `opposite] ->
    ?io:bool ->
    ?iu:bool ->
    ?id:bool ->
    ?fu:bool ->
    ?fd:bool ->
    ?ties:[`all | `first | `last] ->
    ?mdb:[`each | `all] ->
    ?k:int ->
    ?header:bool ->
    'a input ->
    'a file ->
    #bed3 file list ->
    'a file
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
    ?blacklist:#bed3 file ->
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
    [`indexed_bam] directory ->
    'a file


  val bamcompare :
    ?scalefactormethod : [ `readcount | `ses ] ->
    ?samplelength:int ->
    ?numberofsamples:int ->
    ?scalefactor:float ->
    ?ratio: [ `log2 | `ratio | `subtract | `add | `mean | `reciprocal_ratio | `first | `second ] ->
    ?pseudocount:int ->
    ?binsize:int ->
    ?region:string ->
    ?blacklist:#bed3 file ->
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
    [`indexed_bam] directory ->
    [`indexed_bam] directory ->
    'a file


  val bigwigcompare :
    ?scalefactor:float ->
    ?ratio: [ `log2 | `ratio | `subtract | `add | `mean | `reciprocal_ratio | `first | `second ] ->
    ?pseudocount:int ->
    ?binsize:int ->
    ?region:string ->
    ?blacklist:#bed3 file ->
    ?threads:int ->
    'a signal_format ->
    Ucsc_gb.bigWig file ->
    Ucsc_gb.bigWig file ->
    'a file

  class type compressed_numpy_array = object
    inherit binary_file
    method format : [`compressed_numpy_array]
  end

  val multibamsummary_bins :
    ?binsize:int ->
    ?distancebetweenbins:int ->
    ?region:string ->
    ?blacklist:#bed3 file ->
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
    [`indexed_bam] directory list ->
    compressed_numpy_array file


  val multibamsummary_bed :
    ?region:string ->
    ?blacklist:#bed3 file ->
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
    #bed3 file ->
    [`indexed_bam] directory list ->
    compressed_numpy_array file

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
    ?blackList:#bed3 file ->
    ?scale:float ->
    ?numberOfProcessors:int ->
    regions:#bed3 file list ->
    scores:Ucsc_gb.bigWig file list ->
    unit ->
    deeptools_matrix gz file

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
    deeptools_matrix gz file ->
    'a file

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
    compressed_numpy_array file ->
    'a file

  val plotProfile :
    ?dpi:int ->
    ?kmeans:int ->
    ?hclust:int ->
    ?averageType:[`mean | `median | `min | `max | `std | `sum] ->
    ?plotHeight:float -> (** in cm *)
    ?plotWidth:float ->
    ?plotType:[`lines | `fill | `se | `std | `overlapped_lines | `heatmap] ->
    ?colors:string list ->
    ?numPlotsPerRow:int ->
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
    deeptools_matrix gz file ->
    'a file

  val plotEnrichment :
    ?labels:string list ->
    ?regionLabels:string list ->
    ?plotTitle:string ->
    ?variableScales:bool ->
    ?plotHeight:float ->
    ?plotWidth:float ->
    ?colors:string list ->
    ?numPlotsPerRow:int ->
    ?alpha:float ->
    ?offset:int ->
    ?blackList:#bed3 file ->
    ?numberOfProcessors:int ->
    bams:bam file list ->
    beds:#bed3 file list ->
    'a img_format ->
    'a file
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
    [`sam of sam file | `bam of bam file] ->
    gff file ->
    count_tsv file
end


module Samtools : sig

  type 'a format

  val bam : bam format
  val sam : sam format

  val sort :
    ?on:[`name | `position] ->
    bam file -> bam file
  val indexed_bam_of_sam : sam file -> [`indexed_bam] directory
  val indexed_bam_of_bam : bam file -> [`indexed_bam] directory
  val indexed_bam_to_bam : [`indexed_bam] directory -> bam file
  val bam_of_sam : sam file -> bam file
  val sam_of_bam : bam file -> sam file

  (* val rmdup : ?single_end_mode:bool -> bam file -> bam file *)

  val view :
    output:'o format ->
    (* ?_1:bool ->
     * ?u:bool -> *)
    ?h:bool ->
    ?_H:bool ->
    (* ?c:bool -> *)
    (* ?_L: #bed3 file -> *)
    ?q:int ->
    (* ?m:int ->
     * ?f:int ->
     * ?_F:int ->
     * ?_B:bool ->
     * ?s:float -> *)
    < file_kind : [`regular] ;
      format : [< `bam | `sam] ; .. > file ->
    'o file

  val faidx :
    fasta file -> [`indexed_fasta] directory

  val fasta_of_indexed_fasta :
    [`indexed_fasta] directory -> fasta file
end

module Picardtools : sig
  val img : Shell_dsl.container_image list

  val markduplicates :
    ?remove_duplicates:bool ->
    [`indexed_bam] directory ->
    [`picard_markduplicates] directory

  val reads :
    [`picard_markduplicates] directory ->
    bam file

  val sort_bam_by_name :
    bam file ->
    bam file
end

module Sra_toolkit : sig
  val img : Shell_dsl.container_image list

  val fastq_dump :
    [`id of string | `idw of string workflow | `file of sra file] ->
    sanger_fastq file

  val fastq_dump_gz :
    [`id of string | `file of sra file] ->
    sanger_fastq gz file

  val fastq_dump_pe : sra file -> sanger_fastq file * sanger_fastq file

  val fastq_dump_pe_gz :
    [`id of string | `file of sra file] ->
    sanger_fastq gz file * sanger_fastq gz file

  val fastq_dump_to_fasta : sra file -> fasta file

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
    gff file ->
    < format : [< `bam | `sam] ; .. > file -> (*FIXME: handle paired-hand, just add other file next to the other*)
    [`featureCounts] directory

  val featureCounts_tsv : [`featureCounts] directory -> count_table file
  val featureCounts_htseq_tsv : [`featureCounts] directory -> Htseq.count_tsv file
  val featureCounts_summary : [`featureCounts] directory -> text file
end

(** {3 NGS quality} *)

module ChIPQC : sig
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
end


module FastQC : sig
  val run : #fastq file -> [`fastQC] directory
  val html_report : [`fastQC] directory -> html file
  val per_base_quality : [`fastQC] directory -> png file
  val per_base_sequence_content : [`fastQC] directory -> png file
end

module Fastq_screen : sig
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
    #fastq file ->
    (string * fasta file) list ->
    [`fastq_screen] directory

  val html_report : [`fastq_screen] directory -> html file

end

(** {3 NGS aligners} *)

module Bowtie : sig
  val bowtie_build :
    ?packed:bool ->
    ?color:bool  ->
    fasta file -> [`bowtie_index] directory

  val bowtie :
    ?l:int -> ?e:int -> ?m:int ->
    ?fastq_format:'a Fastq.format ->
    ?n:int -> ?v:int ->
    ?maxins:int ->
    [`bowtie_index] directory ->
    'a file list SE_or_PE.t ->
    sam file
end

module Bowtie2 : sig
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
    fasta file ->
    [`bowtie2_index] directory

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
    [`bowtie2_index] directory ->
    'a file list SE_or_PE.t ->
    sam file
end


module Tophat : sig
  val tophat1 :
    ?color:bool ->
    [`bowtie_index] directory ->
    #fastq file list SE_or_PE.t ->
    [`tophat] directory

  val tophat2 :
    [`bowtie_index] directory ->
    #fastq file list SE_or_PE.t ->
    [`tophat] directory

  val accepted_hits : [`tophat] directory -> bam file
  val junctions : [`tophat] directory -> bed6 file
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
    fasta file ->
    [`hisat2_index] directory


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
    [`hisat2_index] directory ->
    sanger_fastq file list SE_or_PE.t ->
    sam file
end

module Star : sig
  val genomeGenerate : fasta file -> [`star_index] directory

  val alignReads :
    ?max_mem:[`GB of int] ->
    ?outFilterMismatchNmax:int ->
    ?outFilterMultimapNmax:int ->
    ?outSAMstrandField:[`None | `intronMotif] ->
    ?alignIntronMax:int ->
    [`star_index] directory ->
    sanger_fastq file SE_or_PE.t ->
    bam file
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
  val index : fasta file list -> index file
  val quant :
    ?bias:bool ->
    ?bootstrap_samples:int ->
    ?fr_stranded:bool ->
    ?rf_stranded:bool ->
    ?threads:int ->
    ?fragment_length:float ->
    ?sd:float ->
    index file ->
    fq1:[`fq of sanger_fastq file | `fq_gz of sanger_fastq gz file] ->
    ?fq2:[`fq of sanger_fastq file | `fq_gz of sanger_fastq gz file] ->
    unit ->
    [`kallisto_output] directory

  val abundance : [`kallisto_output] directory -> abundance_table file

  val merge_eff_counts :
    sample_ids:string list ->
    kallisto_outputs:abundance_table file list ->
    tsv file

  val merge_tpms :
    sample_ids:string list ->
    kallisto_outputs:abundance_table file list ->
    tsv file
end

(** {3 Genome assembly} *)

module Spades : sig
  val spades :
    ?single_cell:bool ->
    ?iontorrent:bool ->
    ?pe:sanger_fastq file list * sanger_fastq file list ->
    ?threads:int ->
    ?memory:int ->
    unit ->
    [`spades] directory

  val contigs : [`spades] directory -> fasta file
  val scaffolds : [`spades] directory -> fasta file
end

module Idba : sig
  val fq2fa :
    ?filter:bool ->
    [ `Se of sanger_fastq file
    | `Pe_merge of sanger_fastq file * sanger_fastq file
    | `Pe_paired of sanger_fastq file ] ->
    fasta file

  val idba_ud : ?mem_spec:int -> fasta file -> [`idba] directory

  val idba_ud_contigs : [`idba] directory -> fasta file
  val idba_ud_scaffolds : [`idba] directory -> fasta file
end

module Cisa : sig
  val merge :
    ?min_length:int ->
    (string * fasta file) list -> fasta file

  val cisa :
    genome_size:int ->
    fasta file ->
    fasta file
end

module Quast : sig
  val quast :
    ?reference:fasta file ->
    ?labels:string list ->
    fasta file list ->
    [`quast] directory
end

module Busco : sig
  val img : Shell_dsl.container_image list

  type db = [
    | `bacteria
    | `proteobacteria
    | `rhizobiales
    | `betaproteobacteria
    | `gammaproteobacteria
    | `enterobacteriales
    | `deltaepsilonsub
    | `actinobacteria
    | `cyanobacteria
    | `firmicutes
    | `clostridia
    | `lactobacillales
    | `bacillales
    | `bacteroidetes
    | `spirochaetes
    | `tenericutes
    | `eukaryota
    | `fungi
    | `microsporidia
    | `dikarya
    | `ascomycota
    | `pezizomycotina
    | `eurotiomycetes
    | `sordariomyceta
    | `saccharomyceta
    | `saccharomycetales
    | `basidiomycota
    | `metazoa
    | `nematoda
    | `arthropoda
    | `insecta
    | `endopterygota
    | `hymenoptera
    | `diptera
    | `vertebrata
    | `actinopterygii
    | `tetrapoda
    | `aves
    | `mammalia
    | `euarchontoglires
    | `laurasiatheria
    | `embryophyta
    | `protists_ensembl
    | `alveolata_stramenophiles_ensembl
  ]

  val busco :
    ?evalue:float ->
    ?limit:int ->
    ?tarzip:bool ->
    threads:int ->
    mode:[`genome | `transcriptome | `proteins] ->
    db:db ->
    fasta file ->
    [`busco] directory
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
      comparison_summary : table file ;
      comparisons : ((string * string * string) * table file) list ;
      effect_table : table file ;
      normalized_counts : table file ;
      sample_clustering : svg file ;
      sample_pca : svg file ;
      directory : [`deseq2] directory ;
    >

  val main_effects :
    string list ->
    (string list * #Htseq.count_tsv file) list ->
    output
end

(** {3 Peak callers } *)

module Macs : sig
  type gsize = [`hs | `mm | `ce | `dm | `gsize of int]
  type keep_dup = [ `all | `auto | `int of int ]

  type _ format

  val sam : sam format
  val bam : bam format

  val run :
    ?control: 'a file list ->
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
    'a file list ->
    [`macs] directory

  class type peaks_xls = object
    inherit bed3
    method f4 : int
    method f5 : int
    method f6 : int
    method f7 : float
    method f8 : float
    method f9 : float
  end

  val peaks_xls : [`macs] directory -> peaks_xls file

  class type narrow_peaks = object
    inherit bed5
    method f6 : string
    method f7 : float
    method f8 : float
    method f9 : float
    method f10 : int
  end

  val narrow_peaks :
    [`macs] directory -> narrow_peaks file

  class type peak_summits = object
    inherit bed4
    method f5 : float
  end

  val peak_summits :
    [`macs] directory -> peak_summits file
end

module Macs2 : sig
  val pileup :
    ?extsize:int ->
    ?both_direction:bool ->
    bam file -> Ucsc_gb.bedGraph file

  type gsize = [`hs | `mm | `ce | `dm | `gsize of int]
  type keep_dup = [ `all | `auto | `int of int ]

  type _ format

  val sam : sam format
  val bam : bam format

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
    ?control:'a file list ->
    ?keep_dup:keep_dup ->
    'a format ->
    'a file list ->
    [`macs2_narrow] directory

  class type peaks_xls = object
    inherit bed3
    method f4 : int
    method f5 : int
    method f6 : int
    method f7 : float
    method f8 : float
    method f9 : float
  end

  val peaks_xls : [< `macs2_narrow | `macs2_broad] directory -> peaks_xls file

  class type narrow_peaks = object
    inherit bed5
    method f6 : string
    method f7 : float
    method f8 : float
    method f9 : float
    method f10 : int
  end

  val narrow_peaks : [`macs2_narrow] directory -> narrow_peaks file

  class type peak_summits = object
    inherit bed4
    method f5 : float
  end

  val peak_summits : [< `macs2_narrow | `macs2_broad] directory -> peak_summits file

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
    ?control:'a file list ->
    ?keep_dup:keep_dup ->
    'a format ->
    'a file list ->
    [`macs2_broad] directory

  class type broad_peaks = object
    inherit bed5
    method f6 : string
    method f7 : float
    method f8 : float
    method f9 : float
  end

  val broad_peaks : [`macs2_broad] directory -> broad_peaks file
end

module Idr : sig
  type 'a format

  val narrowPeak : Macs2.narrow_peaks format
  val broadPeak : Macs2.broad_peaks format
  val bed : bed3 format
  val gff : gff format

  type 'a output = [`idr_output of 'a]

  val idr :
    input_file_type:'a format ->
    ?idr_threshold:float ->
    ?soft_idr_threshold:float ->
    ?peak_merge_method:[ `sum | `avg | `min | `max] ->
    ?rank:[ `signal | `pvalue | `qvalue ] ->
    ?random_seed:int ->
    ?peak_list:'a file ->
    'a file ->
    'a file ->
    'a output directory

  val items : 'a output directory -> 'a file
  val figure : _ output directory -> png file
end

module Meme_suite : sig
  val meme :
    ?nmotifs:int ->
    ?minw:int ->
    ?maxw:int ->
    ?revcomp:bool ->
    ?maxsize:int ->
    ?alphabet:[`dna | `rna | `protein] ->
    (* ?threads:int -> *)
    fasta file ->
    [`meme] directory

  val meme_logo :
    [`meme] directory ->
    ?rc:bool ->
    int ->
    png file

  val meme_chip :
    ?meme_nmotifs:int ->
    ?meme_minw:int ->
    ?meme_maxw:int ->
    (* ?np:int -> *)
    fasta file ->
    [`meme_chip] directory

  (** http://meme-suite.org/doc/fimo.html?man_type=web *)
  val fimo :
    ?alpha: float ->
    ?bgfile:text file ->
    ?max_stored_scores: int ->
    ?max_strand:bool ->
    ?motif:string ->
    ?motif_pseudo:float ->
    ?no_qvalue:bool ->
    ?norc:bool ->
    ?parse_genomic_coord:bool ->
    ?prior_dist:text file ->
    ?psp:text file ->
    ?qv_thresh:bool ->
    ?thresh: float ->
    [`meme] directory ->
    fasta file ->
    [`fimo] directory
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
    fasta file ->
    [`prokka] directory

end

module Srst2 : sig

  val run_gen_cmd :
    ?mlst_db:fasta file ->
    ?mlst_delimiter:string ->
    ?mlst_definitions:fasta file ->
    ?mlst_max_mismatch:int ->
    ?gene_db:fasta file list ->
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
    ?mlst_db:fasta file ->
    ?mlst_delimiter:string ->
    ?mlst_definitions:fasta file ->
    ?mlst_max_mismatch:int ->
    ?gene_db:fasta file list ->
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
    #fastq file list ->
    [`srst2] directory


  val run_pe :
    ?mlst_db:fasta file ->
    ?mlst_delimiter:string ->
    ?mlst_definitions:fasta file ->
    ?mlst_max_mismatch:int ->
    ?gene_db:fasta file list ->
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
    #fastq file list ->
    [`srst2] directory
end
