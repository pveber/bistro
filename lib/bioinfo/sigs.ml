module type S = sig
  include module type of File_formats
  open Bistro
  open Bistro_unix

  type 'a workflow
  type shell_command
  type docker_image

  module Bed : sig
    val keep3 : #bed3 workflow -> bed3 workflow
    val keep4 : #bed4 workflow -> bed4 workflow
    val keep5 : #bed5 workflow -> bed5 workflow
    val keep6 : #bed6 workflow -> bed6 workflow
  end

  module Fastq : sig
    type _ format =
      | Sanger  : [`sanger] format
      | Solexa  : [`solexa] format
      | Phred64 : [`phred64] format

    val to_sanger : 'a format -> 'a fastq workflow -> [`sanger] fastq workflow

    val concat : 'a fastq workflow list -> 'a fastq workflow
    val head : int -> 'a fastq workflow -> 'a fastq workflow
  end

  module Ucsc_gb : sig
    type genome = [ `dm3 | `droSim1 | `hg18 | `hg19 | `hg38 | `mm8 | `mm9 | `mm10 | `sacCer2 ]
    val string_of_genome : [< genome] -> string
    val genome_of_string : string -> genome option

    (** {5 Dealing with genome sequences} *)
    val chromosome_sequence :
      [< genome] ->
      string ->
      fasta workflow
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
        input. Note that the underlying {v bedToBigBed v} expects BED
        files with {i exactly} 3 or 5 columns. *)

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


    module Lift_over : sig
      class type chain_file = object
        inherit file
        method format : [`lift_over_chain_file]
      end
      val chain_file :
        org_from:[< genome] ->
        org_to:[< genome] ->
        chain_file workflow

      val bed :
        org_from:[< genome] ->
        org_to:[< genome] ->
        (* chain_file workflow -> *)
        (#bed3 as 'a) workflow ->
        [`ucsc_lift_over of 'a] directory workflow

      val mapped : [`ucsc_lift_over of 'a] directory workflow -> 'a workflow
      val unmapped : [`ucsc_lift_over of 'a] directory workflow -> 'a workflow
    end
  end

  module Ensembl : sig
    type species = [
      | `homo_sapiens
      | `mus_musculus
    ]

    val ucsc_reference_genome : release:int -> species:species -> Ucsc_gb.genome

    val gff : ?chr_name : [`ensembl | `ucsc] -> release:int -> species:species -> gff workflow
    val gtf : ?chr_name : [`ensembl | `ucsc] -> release:int -> species:species -> gff workflow
  end

  module Sra : sig
    val input : string -> sra workflow

    val fetch_srr : string -> sra workflow
  end

  module Bedtools : sig
    val env : docker_image

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
        'a workflow ->
        chrom_sizes workflow ->
        shell_command
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
      'a workflow ->
      chrom_sizes workflow ->
      'a workflow


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
      ?g:chrom_sizes workflow ->
      ?header:bool ->
      ?filenames:bool ->
      ?sortout:bool ->
      'a input ->
      'a workflow ->
      #bed3 workflow list ->
      'a workflow

    val bamtobed :
      ?bed12:bool ->
      ?split:bool ->
      ?splitD:bool ->
      ?ed:bool ->
      ?tag:bool ->
      ?cigar:bool ->
      bam workflow ->
      #bed6 workflow
  end

  module Samtools : sig
    type 'a format

    val bam : bam format
    val sam : sam format

    val sort :
      ?on:[`name | `position] ->
      bam workflow -> bam workflow
    val indexed_bam_of_sam : sam workflow -> indexed_bam workflow
    val indexed_bam_of_bam : bam workflow -> indexed_bam workflow
    val indexed_bam_to_bam : [`indexed_bam] directory workflow -> bam workflow
    val bam_of_sam : sam workflow -> bam workflow
    val sam_of_bam : bam workflow -> sam workflow

    (* val rmdup : ?single_end_mode:bool -> bam workflow -> bam workflow *)

    val view :
      output:'o format ->
      (* ?_1:bool ->
       * ?u:bool -> *)
      ?h:bool ->
      ?_H:bool ->
      (* ?c:bool -> *)
      (* ?_L: #bed3 workflow -> *)
      ?q:int ->
      (* ?m:int ->
       * ?f:int ->
       * ?_F:int ->
       * ?_B:bool ->
       * ?s:float -> *)
      < kind : [`file] ;
        format : [< `bam | `sam] ; .. > workflow ->
      'o workflow
  end

  module Sra_toolkit : sig
    val env : docker_image

    val fastq_dump : sra workflow -> [`sanger] fastq workflow

    val fastq_dump_gz :
      [`id of string | `file of sra workflow] ->
      [`sanger] fastq gz workflow

    val fastq_dump_pe : sra workflow -> [`sanger] fastq workflow * [`sanger] fastq workflow

    val fastq_dump_pe_gz :
      [`id of string | `file of sra workflow] ->
      [`sanger] fastq gz workflow * [`sanger] fastq gz workflow

    val fastq_dump_to_fasta : sra workflow -> fasta workflow
  end

  module Deeptools : sig
    type 'a signal_format
    val bigwig : bigWig signal_format
    val bedgraph : bedGraph signal_format

    type 'a img_format
    val png : png img_format
    val pdf : pdf img_format
    val svg : svg img_format

    val bamcoverage :
      ?scalefactor:float ->
      ?filterrnastrand: [ `forward | `reverse ] ->
      ?binsize:int ->
      ?blacklist:#bed3 workflow ->
      ?threads:int ->
      ?normalizeto1x:int ->
      ?normalizeusingrpkm:bool ->
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
      [ `indexed_bam ] directory workflow ->
      'a workflow


    val bamcompare :
      ?scalefactormethod : [ `readcount | `ses ] ->
      ?samplelength:int ->
      ?numberofsamples:int ->
      ?scalefactor:float ->
      ?ratio: [ `log2 | `ratio | `subtract | `add | `mean | `reciprocal_ratio | `first | `second ] ->
      ?pseudocount:int ->
      ?binsize:int ->
      ?region:string ->
      ?blacklist:#bed3 workflow ->
      ?threads:int ->
      ?normalizeto1x:int ->
      ?normalizeusingrpkm:bool ->
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
      [ `indexed_bam ] directory workflow ->
      [ `indexed_bam ] directory workflow ->
      'a workflow


    val bigwigcompare :
      ?scalefactor:float ->
      ?ratio: [ `log2 | `ratio | `subtract | `add | `mean | `reciprocal_ratio | `first | `second ] ->
      ?pseudocount:int ->
      ?binsize:int ->
      ?region:string ->
      ?blacklist:#bed3 workflow ->
      ?threads:int ->
      'a signal_format ->
      bigWig workflow ->
      bigWig workflow ->
      'a workflow

    class type compressed_numpy_array = object
      inherit binary_file
      method format : [`compressed_numpy_array]
    end

    val multibamsummary_bins :
      ?binsize:int ->
      ?distancebetweenbins:int ->
      ?region:string ->
      ?blacklist:#bed3 workflow ->
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
      indexed_bam workflow list ->
      compressed_numpy_array workflow


    val multibamsummary_bed :
      ?region:string ->
      ?blacklist:#bed3 workflow ->
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
      #bed3 workflow ->
      indexed_bam workflow list ->
      compressed_numpy_array workflow

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
      ?blackList:#bed3 workflow ->
      ?scale:float ->
      ?numberOfProcessors:int ->
      regions:#bed3 workflow list ->
      scores:bigWig workflow list ->
      unit ->
      deeptools_matrix gz workflow

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
      deeptools_matrix gz workflow ->
      'a workflow

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
      compressed_numpy_array workflow ->
      _ directory workflow

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
    (*   deeptools_matrix gz workflow -> *)
    (*   'a workflow *)
  end

  module FastQC : sig
    type report = [`fastQC_report] directory

    val run : 'a fastq workflow -> report workflow
    val html_report : report workflow -> html workflow
    val per_base_quality : report workflow -> png workflow
    val per_base_sequence_content : report workflow -> png workflow
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
      'a fastq workflow ->
      (string * fasta workflow) list ->
      [ `fastq_screen ] directory workflow

    val html_report : [`fastq_screen] directory workflow -> html workflow
  end

  module Bowtie : sig
    type index = [`bowtie_index] directory

    val bowtie_build :
      ?packed:bool ->
      ?color:bool  ->
      fasta workflow -> index workflow

    val bowtie :
      ?l:int -> ?e:int -> ?m:int ->
      ?fastq_format:'a Fastq.format ->
      ?n:int -> ?v:int ->
      ?maxins:int ->
      index workflow ->
      [ `single_end of 'a fastq workflow list
      | `paired_end of 'a fastq workflow list * 'a fastq workflow list ] ->
      sam workflow
  end

  module Bowtie2 : sig
    type index = [`bowtie2_index] directory

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
      fasta workflow ->
      index workflow

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
      index workflow ->
      [ `single_end of 'a fastq workflow list
      | `paired_end of 'a fastq workflow list * 'a fastq workflow list ] ->
      sam workflow
  end

  module ChIPQC : sig
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
  end

  module Macs : sig
    type gsize = [`hs | `mm | `ce | `dm | `gsize of int]
    type keep_dup = [ `all | `auto | `int of int ]

    type _ format

    val sam : sam format
    val bam : bam format

    val run :
      ?control: 'a workflow list ->
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
      'a workflow list ->
      [`macs_output] directory workflow

    class type peaks_xls = object
      inherit bed3
      method f4 : int
      method f5 : int
      method f6 : int
      method f7 : float
      method f8 : float
      method f9 : float
    end

    val peaks_xls :
      [`macs_output] directory workflow ->
      peaks_xls workflow

    class type narrow_peaks = object
      inherit bed5
      method f6 : string
      method f7 : float
      method f8 : float
      method f9 : float
      method f10 : int
    end

    val narrow_peaks :
      [`macs_output] directory workflow ->
      narrow_peaks workflow

    class type peak_summits = object
      inherit bed4
      method f5 : float
    end

    val peak_summits :
      [`macs_output] directory workflow ->
      peak_summits workflow
  end

  module Macs2 : sig
    val pileup :
      ?extsize:int ->
      ?both_direction:bool ->
      bam workflow -> bedGraph workflow

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
      ?control:'a workflow list ->
      ?keep_dup:keep_dup ->
      'a format ->
      'a workflow list ->
      [`macs2_callpeak_output] directory workflow

    class type peaks_xls = object
      inherit bed3
      method f4 : int
      method f5 : int
      method f6 : int
      method f7 : float
      method f8 : float
      method f9 : float
    end

    val peaks_xls :
      [`macs2_callpeak_output] directory workflow ->
      peaks_xls workflow

    class type narrow_peaks = object
      inherit bed5
      method f6 : string
      method f7 : float
      method f8 : float
      method f9 : float
      method f10 : int
    end

    val narrow_peaks :
      [`macs2_callpeak_output] directory workflow ->
      narrow_peaks workflow

    class type peak_summits = object
      inherit bed4
      method f5 : float
    end

    val peak_summits :
      [`macs2_callpeak_output] directory workflow ->
      peak_summits workflow

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
      ?control:'a workflow list ->
      ?keep_dup:keep_dup ->
      'a format ->
      'a workflow list ->
      [`macs2_callpeak_broad_output] directory workflow

    class type broad_peaks = object
      inherit bed5
      method f6 : string
      method f7 : float
      method f8 : float
      method f9 : float
    end

    val broad_peaks :
      [`macs2_callpeak_broad_output] directory workflow ->
      broad_peaks workflow
  end

  module Meme_suite : sig
    val meme_chip :
      ?meme_nmotifs:int ->
      ?meme_minw:int ->
      ?meme_maxw:int ->
      ?np:int ->
      fasta workflow ->
      [`meme_chip_output] directory workflow

    val fimo :
      ?alpha: float ->
      ?bgfile: string ->
      ?max_stored_scores: int ->
      ?motif: string ->
      ?motif_pseudo: float ->
      ?qv_thresh:bool ->
      ?thresh: float ->
      [`meme_chip_output] directory workflow ->
      fasta workflow ->
      [`fimo_output] directory workflow
  end

  module Tophat : sig
    val tophat1 :
      ?color:bool ->
      Bowtie.index workflow ->
      [ `single_end of 'a fastq workflow list
      | `paired_end of 'a fastq workflow list * 'a fastq workflow list ] ->
      [`tophat_output] directory workflow

    val tophat2 :
      Bowtie2.index workflow ->
      [ `single_end of 'a fastq workflow list
      | `paired_end of 'a fastq workflow list * 'a fastq workflow list ] ->
      [`tophat_output] directory workflow

    val accepted_hits : [`tophat_output] directory workflow -> bam workflow
    val junctions : [`tophat_output] directory workflow -> bed6 workflow
  end

  module Htseq : sig
    class type count_tsv = object
      inherit tsv
      method header : [`none]
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
      [`sam of sam workflow | `bam of bam workflow] ->
      gff workflow ->
      count_tsv workflow
  end

  module DESeq2 : sig
    val env : docker_image

    class type table = object
      inherit tsv
      method header : [`yes]
    end

    type output =
      <
        comparison_summary : table workflow ;
        comparisons : ((string * string * string) * table workflow) list ;
        effect_table : table workflow ;
        normalized_counts : table workflow ;
        sample_clustering : svg workflow ;
        sample_pca : svg workflow ;
        directory : [ `deseq2_output ] directory workflow
      >

    val main_effects :
      string list ->
      (string list * Htseq.count_tsv workflow) list ->
      output
  end

  module Spades : sig
    val spades :
      ?single_cell:bool ->
      ?iontorrent:bool ->
      ?pe:[`sanger] fastq workflow list * [`sanger] fastq workflow list ->
      ?threads:int ->
      ?memory:int ->
      unit ->
      [`spades] directory workflow

    val contigs : [`spades] directory workflow -> fasta workflow
    val scaffolds : [`spades] directory workflow -> fasta workflow
  end

  module Srst2 : sig
    val run_se :
      ?mlst_db:fasta workflow ->
      ?mlst_delimiter:string ->
      ?mlst_definitions:fasta workflow ->
      ?mlst_max_mismatch:int ->
      ?gene_db:fasta workflow list ->
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
      'a fastq workflow list ->
      [ `srst2 ] directory workflow


    val run_pe :
      ?mlst_db:fasta workflow ->
      ?mlst_delimiter:string ->
      ?mlst_definitions:fasta workflow ->
      ?mlst_max_mismatch:int ->
      ?gene_db:fasta workflow list ->
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
      'a fastq workflow list ->
      [ `srst2 ] directory workflow
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
      fasta workflow ->
      [ `prokka ] directory workflow
  end

end
