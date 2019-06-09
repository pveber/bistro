open Printf
open Base
open Base.Poly
open Bistro
open Bistro.Shell_dsl
open Stdio

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

module Bed = struct
  let keep ~n bed =
    if n < 1 then raise (Invalid_argument "Bed.keep") ;
    Workflow.shell ~descr:"bed.keep" [
      cmd "cut" ~stdout:dest [
        string (sprintf "-f 1-%d" n) ;
        dep bed ;
      ]
    ]

  let keep3 x = keep ~n:3 x

  let keep4 x = keep ~n:4 x

  let keep5 x = keep ~n:5 x

  let keep6 x = keep ~n:6 x
end

module Fastq = struct

  type _ format =
    | Sanger  : sanger_fastq format
    | Solexa  : solexa_fastq format
    | Phred64 : phred64_fastq format

  let concat = function
    | [] -> raise (Invalid_argument "fastq concat: empty list")
    | x :: [] -> x
    | fqs ->
      Workflow.shell ~descr:"fastq.concat" [
        cmd "cat" ~stdout:dest [ list dep ~sep:" " fqs ]
      ]

  let head n fq =
    Workflow.shell ~descr:"fastq.head" [
      cmd "head" ~stdout:dest [
        opt "-n" int (n * 4) ;
        dep fq ;
      ]
    ]
end

module Bedtools = struct
  let img = [ docker_image ~account:"pveber" ~name:"bedtools" ~tag:"2.21.0" () ]

  let bedtools ?stdout subcmd args =
    cmd "bedtools" ?stdout ~img (string subcmd :: args)

  type 'a input = Bed | Gff

  let bed = Bed
  let gff = Gff

  module Cmd = struct
    let slop_args ?strand ?header ~mode = [
      option (flag string "-s") strand ;
      option (flag string "-header") header ;
      seq (
        match mode with
        | `both n      -> [ opt "-b" int n ]
        | `left n      -> [ opt "-l" int n ]
        | `right n     -> [ opt "-r" int n ]
        | `left_pct p  -> [ opt "-l" float p ; string "-pct" ]
        | `right_pct p -> [ opt "-l" float p ; string "-pct" ]
        | `both_pct p  -> [ opt "-b" float p ; string "-pct" ]
      )
    ]

    let slop ?strand ?header ~mode input chrom_size =
      bedtools "slop" ~stdout:dest [
        seq (slop_args ?strand ?header ~mode) ;
        opt "-i" dep input ;
        opt "-g" dep chrom_size ;
      ]
  end

  let slop ?strand ?header ~mode _ input chrom_size =
    Workflow.shell ~descr:"bedtools.slop" [
      Cmd.slop ?strand ?header ~mode input chrom_size
    ]

  let intersect ?ubam ?wa ?wb ?loj ?wo ?wao ?u ?c ?v ?f ?_F ?r ?e ?s ?_S
      ?split ?sorted ?g ?header ?filenames ?sortout _ file files =
    Workflow.shell ~descr:"bedtools.intersect" [
      cmd "bedtools intersect" ~img ~stdout:dest [
        option (flag string "-ubam") ubam ;
        option (flag string "-wa") wa ;
        option (flag string "-wb") wb ;
        option (flag string "-loj") loj ;
        option (flag string "-wo") wo ;
        option (flag string "-wao") wao ;
        option (flag string "-u") u ;
        option (flag string "-c") c ;
        option (flag string "-v") v ;
        option (opt "-f" float) f ;
        option (opt "-F" float) _F ;
        option (flag string "-r") r ;
        option (flag string "-e") e ;
        option (flag string "-s") s ;
        option (flag string "-S") _S ;
        option (flag string "-split") split ;
        option (flag string "-sorted") sorted ;
        option (opt "-g" dep) g ;
        option (flag string "-header") header ;
        option (flag string "-filenames") filenames ;
        option (flag string "-sortout") sortout ;
        opt "-a" dep file ;
        opt "-b" (list dep ~sep:" ") files ;
      ]
    ]

  let closest ?strand ?io ?iu ?id ?fu ?fd ?ties ?mdb ?k ?header _ query beds =
    Workflow.shell ~descr:"bedtools.intersect" [
      cmd "bedtools.closest" ~img ~stdout:dest [
        option ((function `same -> "-s" | `opposite -> "-S") % string) strand ;
        option (flag string "-io") io ;
        option (flag string "-iu") iu ;
        option (flag string "-id") id ;
        option (flag string "-fu") fu ;
        option (flag string "-fd") fd ;
        option (opt "-t" ((function `all -> "all" | `first -> "first" | `last -> "last") % string)) ties ;
        option (opt "-mdb" ((function `each -> "each" | `all -> "all") % string)) mdb ;
        option (opt "-k" int) k ;
        option (flag string "-header") header ;
        opt "-a" dep query ;
        opt "-b" (list dep ~sep:" ") beds ;
      ]
    ]

  let bamtobed ?bed12 ?split ?splitD ?ed ?tag ?cigar bam =
    Workflow.shell ~descr:"bedtools.bamtobed" ~mem:(Workflow.int  (3 * 1024)) ~np:8 [
      cmd "bedtools bamtobed" ~stdout:dest ~img [
        option (flag string "-bed12") bed12 ;
        option (flag string "-split") split ;
        option (flag string "-splitD") splitD ;
        option (flag string "-ed") ed ;
        option (flag string "-tag") tag ;
        option (flag string "-cigar") cigar ;
        opt "-i" dep bam ;
      ]
    ]

end


module Samtools = struct
  type 'a format = Bam | Sam

  let bam = Bam
  let sam = Sam


  let img = [ docker_image ~account:"pveber" ~name:"samtools" ~tag:"1.3.1" () ]

  let samtools subcmd args =
    cmd "samtools" ~img (string subcmd :: args)

  let sam_of_bam bam =
    Workflow.shell ~descr:"samtools.sam_of_bam" [
      samtools "view" [
        opt "-o" Fn.id dest ;
        dep bam ;
      ]
    ]

  let bam_of_sam sam =
    Workflow.shell ~descr:"samtools.bam_of_sam" [
      samtools "view" [
        string "-S -b" ;
        opt "-o" Fn.id dest ;
        dep sam ;
      ]
    ]

  let indexed_bam_of_sam sam =
    Workflow.shell ~descr:"samtools.indexed_bam_of_sam" [
      mkdir_p dest ;
      samtools "view" [
        string "-S -b" ;
        opt "-o" (fun () -> dest // "temp.bam") () ;
        dep sam ;
      ] ;
      samtools "sort" [
        dest // "temp.bam" ;
        opt "-o" Fn.id (dest // "reads.bam") ;
      ] ;
      samtools "index" [ dest // "reads.bam" ] ;
      rm_rf (dest // "temp.bam") ;
    ]

  let sort ?on:order bam =
    Workflow.shell ~descr:"samtools.sort" [
      samtools "sort" [
        option (fun o -> flag string "-n" (o = `name)) order ;
        dep bam ;
        opt "-o" Fn.id dest ;
      ] ;
    ]

  let indexed_bam_of_bam bam =
    Workflow.shell ~descr:"samtools.indexed_bam_of_bam" [
      mkdir_p dest ;
      samtools "sort" [
        dep bam ;
        opt "-o" Fn.id (dest // "reads.bam") ;
      ] ;
      samtools "index" [ dest // "reads.bam" ] ;
    ]

  let indexed_bam_to_bam x = Workflow.select x ["reads.bam"]

  let output_format_expr = function
    | Bam -> string "-b"
    | Sam -> string ""


  let view ~output (* ?_1 ?u *) ?h ?_H (* ?c ?_L *) ?q (* ?m ?f ?_F ?_B ?s *) file =
    Workflow.shell ~descr:"samtools.view" [
      cmd "samtools view" ~img [
        output_format_expr output ;
        (* option (flag string "-1") _1 ; *)
        (* option (flag string "-u") u ; *)
        option (flag string "-h") h ;
        option (flag string "-H") _H ;
        (* option (flag string "-c") c ; *)
        (* option (opt "-L" dep) _L ; *)
        option (opt "-q" int) q ;
        (* option (opt "-m" int) m ; *)
        (* option (opt "-f" int) f ; *)
        (* option (opt "-F" int) _F ; *)
        (* option (flag string "-B") _B ; *)
        (* option (opt "-s" float) s ; *)
        dep file ;
        opt "-o" Fn.id dest ;
      ]
    ]
end

module Bowtie2 = struct

  class type index = object
    method contents : [`bowtie2_index]
    inherit directory
  end

  let img = [ docker_image ~account:"pveber" ~name:"bowtie2" ~tag:"2.3.3" () ]

  (* memory bound correspond to storing a human index in memory, following bowtie manual *)
  let bowtie2_build ?large_index ?noauto ?packed ?bmax ?bmaxdivn ?dcv ?nodc ?noref ?justref ?offrate ?ftabchars ?seed ?cutoff fa =
    Workflow.shell ~descr:"bowtie2_build" ~np:8 ~mem:(Workflow.int (3 * 1024)) [
      mkdir_p dest ;
      cmd "bowtie2-build" ~img [
        option (flag string "--large-index") large_index ;
        option (flag string "--no-auto") noauto ;
        option (flag string "--packed") packed ;
        option (flag string "--nodc") nodc ;
        option (flag string "--noref") noref ;
        option (flag string "--justref") justref ;
        option (opt "--bmax" int) bmax ;
        option (opt "--bmaxdivn" int) bmaxdivn ;
        option (opt "--dcv" int) dcv ;
        option (opt "--offrate" int) offrate ;
        option (opt "--ftabchars" int) ftabchars ;
        opt "--threads" Fn.id np ;
        option (opt "--seed" int) seed ;
        option (opt "--cutoff" int) cutoff ;
        opt "-f" dep fa ;
        seq [ dest ; string "/index" ]
      ]
    ]

  let qual_option (type s) x = match (x : s Fastq.format) with
    | Fastq.Solexa  -> "--solexa-quals"
    | Fastq.Sanger -> "--phred33-quals"
    | Fastq. Phred64 -> "--phred64-quals"

  let flag_of_preset mode preset =
    let flag = match preset with
      | `very_fast -> "--very-fast"
      | `fast -> "--fast"
      | `sensitive -> "--sensitive"
      | `very_sensitive -> "--very-sensitive"
    in
    if mode = `local then flag ^ "-local" else flag

  let flag_of_mode = function
    | `end_to_end -> "--end-to-end"
    | `local -> "--local"

  let flag_of_orientation = function
    | `fr -> "--fr"
    | `rf -> "--rf"
    | `ff -> "--ff"

  (* memory bound correspond to storing a human index in memory, following bowtie manual *)
  let bowtie2
      ?skip ?qupto ?trim5 ?trim3 ?preset
      ?_N ?_L ?ignore_quals ?(mode = `end_to_end)
      ?a ?k ?_D ?_R ?minins ?maxins ?orientation
      ?no_mixed ?no_discordant ?dovetail ?no_contain ?no_overlap
      ?no_unal ?seed
      ?fastq_format index fqs =

    let args = match fqs with
      | `single_end fqs ->
        opt "-U" (list dep ~sep:",") fqs
      | `paired_end (fqs1, fqs2) ->
        seq [
          opt "-1" (list dep ~sep:",") fqs1 ;
          string " " ;
          opt "-2" (list dep ~sep:",") fqs2
        ]
    in
    Workflow.shell ~descr:"bowtie2" ~mem:(Workflow.int (3 * 1024)) ~np:8 [
      cmd "bowtie2" ~img [
        option (opt "--skip" int) skip ;
        option (opt "--qupto" int) qupto ;
        option (opt "--trim5" int) trim5 ;
        option (opt "--trim3" int) trim3 ;
        option ((flag_of_preset mode) % string) preset ;
        option (opt "-N" int) _N ;
        option (opt "-L" int) _L ;
        option (flag string "--ignore-quals") ignore_quals ;
        (flag_of_mode % string) mode ;
        option (flag string "-a") a ;
        option (opt "-k" int) k ;
        option (opt "-D" int) _D ;
        option (opt "-R" int) _R ;
        option (opt "--minins" int) minins ;
        option (opt "--maxins" int) maxins ;
        option (flag_of_orientation % string) orientation ;
        option (flag string "--no-mixed") no_mixed  ;
        option (flag string "--no-discordant") no_discordant  ;
        option (flag string "--dovetail") dovetail ;
        option (flag string "--no-contain") no_contain ;
        option (flag string "--no-overlap") no_overlap ;
        option (flag string "--no-unal") no_unal ;
        opt "--threads" Fn.id np ;
        option (opt "--seed" int) seed ;
        option (opt "-q" (qual_option % string)) fastq_format ;
        opt "-x" (fun index -> seq [dep index ; string "/index"]) index ;
        args ;
        opt "-S" Fn.id dest ;
      ]
    ]
end

module Bowtie = struct


  class type index = object
    method contents : [`bowtie_index]
    inherit directory
  end

  let img = [ docker_image ~account:"pveber" ~name:"bowtie" ~tag:"1.1.2" () ]

  (* memory bound correspond to storing a human index in memory, following bowtie manual *)
  let bowtie_build ?packed ?color fa =
    Workflow.shell ~descr:"bowtie_build" ~mem:(Workflow.int (3 * 1024)) [
      mkdir_p dest ;
      cmd "bowtie-build" ~img [
        option (flag string "-a -p") packed ;
        option (flag string "--color") color ;
        opt "-f" dep fa ;
        seq [ dest ; string "/index" ]
      ]
    ]

  let qual_option (type s) x = match (x : s Fastq.format) with
    | Fastq.Solexa  -> "--solexa-quals"
    | Fastq.Sanger -> "--phred33-quals"
    | Fastq.Phred64 -> "--phred64-quals"

  let bowtie ?l ?e ?m ?fastq_format ?n ?v ?maxins index fastq_files =
    let args = match fastq_files with
      | `single_end fqs -> list dep ~sep:"," fqs
      | `paired_end (fqs1, fqs2) ->
        seq [
          opt "-1" (list dep ~sep:",") fqs1 ;
          string " " ;
          opt "-2" (list dep ~sep:",") fqs2
        ]
    in
    Workflow.shell ~descr:"bowtie" ~mem:(Workflow.int (3 * 1024)) ~np:8 [
      cmd "bowtie" ~img [
        string "-S" ;
        option (opt "-n" int) n ;
        option (opt "-l" int) l ;
        option (opt "-e" int) e ;
        option (opt "-m" int) m ;
        option (opt "-v" int) v ;
        option (opt "-q" (qual_option % string)) fastq_format ;
        opt "-p" Fn.id np ;
        option (opt "--maxins" int) maxins ;
        seq [dep index ; string "/index"] ;
        args ;
        dest ;
      ]
    ]
end

module ChIPQC = struct

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

  let img = [ docker_image ~account:"pveber" ~name:"bioconductor" ~tag:"3.8" () ]

  let sample_sheet samples =
    let header = string "SampleID,Tissue,Factor,Replicate,bamReads,Peaks" in
    let line { id ; tissue ; factor ; replicate ; bam ; peaks } =
      seq ~sep:"," [ string id ; string tissue ; string factor ; string replicate ; dep (Samtools.indexed_bam_to_bam bam) ; dep peaks ]
    in
    seq ~sep:"\n" (header :: List.map ~f:line samples)

  let rscript sample_sheet =
    seq ~sep:"\n" [
      string "library(ChIPQC)" ;
      seq ~sep:"" [
        string {|samples = read.csv("|} ;
        file_dump sample_sheet ;
        string {|")|} ;
      ] ;
      string "experiment = ChIPQC(samples)" ;
      seq ~sep:"" [
        string {|ChIPQCreport(experiment,facet=F,reportFolder="|} ;
        dest ;
        string {|")|} ;
      ]
    ]

  let run samples =
    Workflow.shell ~descr:"ChIPQC" [
      cmd "Rscript" ~img [ file_dump (rscript (sample_sheet samples)) ] ;
    ]

end

module Deeptools = struct

  let img = [ docker_image ~account:"pveber" ~name:"deeptools" ~tag:"3.1.3" () ]

  type 'a signal_format = [ `bigWig | `bedGraph ]
  type 'a img_format = [ `png | `pdf | ` svg ]

  class type compressed_numpy_array = object
    inherit binary_file
    method format : [`compressed_numpy_array]
  end

  let bigwig = `bigWig
  let bedgraph = `bedGraph
  let png = `png
  let pdf = `pdf
  let svg = `svg

  let ext_of_format = function
    | `png -> "png"
    | `pdf -> "pdf"
    | `svg -> "svg"

  let file_format_expr = function
    | `bigWig -> string "bigwig"
    | `bedGraph -> string "bedgraph"

  let filterRNAstrand_expr = function
    | `forward -> string "forward"
    | `reverse -> string "reverse"

  let scalefactormethod_expr = function
    | `readcount -> string "readCount"
    | `ses -> string "SES"

  let ratio_expr = function
    | `log2 -> string "log2"
    | `ratio -> string "ratio"
    | `subtract -> string "subtract"
    | `add -> string "add"
    | `mean -> string "mean"
    | `reciprocal_ratio -> string "reciprocal_ratio"
    | `first -> string "first"
    | `second -> string "second"

  let slist f x = list ~sep:" " f x

  let dep_list xs = slist dep xs

  let normalization_method_expr = function
    | `RPKM -> string "RPKM"
    | `CPM -> string "CPM"
    | `BPM -> string "BPM"
    | `RPGC -> string "RPGC"

  let bam_gen_cmd ?outfileformat ?scalefactor ?blacklist
      ?centerreads ?normalizeUsing ?ignorefornormalization ?skipnoncoveredregions
      ?smoothlength ?extendreads ?ignoreduplicates ?minmappingquality
      ?samflaginclude ?samflagexclude ?minfragmentlength ?maxfragmentlength
      cmd_name other_args =
    cmd cmd_name ~img (
      List.append [
        option (opt "--outFileFormat" file_format_expr) outfileformat ;
        option (opt "--scaleFactor" float) scalefactor ;
        option (opt "--blackListFileName" dep) blacklist ;
        option (opt "--normalizeUsing" normalization_method_expr) normalizeUsing ;
        option (opt "--ignoreForNormalization" (list string ~sep:" ")) ignorefornormalization ;
        option (flag string "--skipNonCoveredRegions") skipnoncoveredregions ;
        option (opt "--smoothLength" int) smoothlength ;
        option (opt "--extendReads" int) extendreads ;
        option (flag string "--ignoreDuplicates") ignoreduplicates ;
        option (opt "--minMappingQuality" int) minmappingquality ;
        option (flag string "--centerReads") centerreads ;
        option (opt "--samFlagInclude" int) samflaginclude ;
        option (opt "--samFlagExclude" int) samflagexclude ;
        option (opt "--minFragmentLength" int) minfragmentlength ;
        option (opt "--maxfragmentLength" int) maxfragmentlength ;
      ]
        other_args
    )

  let bamcoverage ?scalefactor ?filterrnastrand ?binsize ?blacklist
      ?(threads = 1) ?normalizeUsing ?ignorefornormalization
      ?skipnoncoveredregions ?smoothlength ?extendreads ?ignoreduplicates
      ?minmappingquality ?centerreads ?samflaginclude ?samflagexclude
      ?minfragmentlength ?maxfragmentlength outfileformat indexed_bam =
    Workflow.shell ~descr:"bamcoverage" ~np:threads [
      bam_gen_cmd "bamCoverage" ?scalefactor ?blacklist
        ?normalizeUsing ?ignorefornormalization
        ?skipnoncoveredregions ?smoothlength ?extendreads ?ignoreduplicates
        ?minmappingquality ?centerreads ?samflaginclude ?samflagexclude
        ?minfragmentlength ?maxfragmentlength [
        option (opt "--filterRNAstrand" filterRNAstrand_expr) filterrnastrand ;
        option (opt "--binSize" int) binsize ;
        opt "--numberOfProcessors" Fn.id np ;
        opt "--bam" Fn.id (dep (Samtools.indexed_bam_to_bam indexed_bam)) ;
        opt "--outFileName" Fn.id dest ;
        opt "--outFileFormat" file_format_expr outfileformat ;
      ]
    ]

  let bamcompare ?scalefactormethod ?samplelength ?numberofsamples
      ?scalefactor ?ratio ?pseudocount ?binsize ?region ?blacklist ?(threads = 1)
      ?normalizeUsing ?ignorefornormalization ?skipnoncoveredregions
      ?smoothlength ?extendreads ?ignoreduplicates ?minmappingquality
      ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
      ?maxfragmentlength outfileformat indexed_bam1 indexed_bam2 =
    Workflow.shell ~descr:"bamcompare" ~np:threads [
      bam_gen_cmd "bamCompare"
        ?scalefactor ?blacklist
        ?normalizeUsing ?ignorefornormalization ?skipnoncoveredregions
        ?smoothlength ?extendreads ?ignoreduplicates ?minmappingquality
        ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
        ?maxfragmentlength [
        option (opt "--scaleFactorMethod" scalefactormethod_expr) scalefactormethod ;
        option (opt "--sampleLength" int) samplelength ;
        option (opt "--numberOfSamples" int) numberofsamples ;
        option (opt "--ratio" ratio_expr) ratio ;
        option (opt "--pseudocount" int) pseudocount ;
        option (opt "--binSize" int) binsize ;
        option (opt "--region" string) region ;
        opt "--numberOfProcessors" Fn.id np ;
        opt "--bamfile1" Fn.id (dep (Samtools.indexed_bam_to_bam indexed_bam1)) ;
        opt "--bamfile2" Fn.id (dep (Samtools.indexed_bam_to_bam indexed_bam2)) ;
        opt "--outFileName" Fn.id dest ;
        opt "--outFileFormat" file_format_expr outfileformat ;
      ]
    ]


  let bigwigcompare ?scalefactor ?ratio ?pseudocount ?binsize
      ?region ?blacklist ?(threads = 1)
      outfileformat bigwig1 bigwig2 =
    Workflow.shell ~descr:"bigwigcompare" ~np:threads [
      cmd "bigwigCompare" ~img [
        option (opt "--scaleFactor" float) scalefactor ;
        option (opt "--ratio" ratio_expr) ratio ;
        option (opt "--pseudocount" int) pseudocount ;
        option (opt "--binSize" int) binsize ;
        option (opt "--region" string) region ;
        option (opt "--blackListFileName" dep) blacklist ;
        opt "--numberOfProcessors" Fn.id np ;
        opt "--bigwig1" dep bigwig1 ;
        opt "--bigwig2" dep bigwig2 ;
        opt "--outFileName" Fn.id dest ;
        opt "--outFileFormat" file_format_expr outfileformat ;
      ]
    ]


  let multibamsum_gen_cmd ?outrawcounts ?extendreads ?ignoreduplicates
      ?minmappingquality ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
      ?maxfragmentlength ?blacklist ?region cmd_name other_args =
    cmd cmd_name ~img (
      List.append [
        option (opt "--region" string) region ;
        option (flag string "--outRawCounts") outrawcounts ;
        option (opt "--extendReads" int) extendreads ;
        option (flag string "--ignoreDuplicates") ignoreduplicates ;
        option (opt "--minMappingQuality" int) minmappingquality ;
        option (flag string "--centerReads") centerreads ;
        option (opt "--samFlagInclude" int) samflaginclude ;
        option (opt "--samFlagExclude" int) samflagexclude ;
        option (opt "--minFragmentLength" int) minfragmentlength ;
        option (opt "--maxfragmentLength" int) maxfragmentlength ;
        option (opt "--blackListFileName" dep) blacklist ;
      ]
        other_args
    )


  let multibamsummary_bins ?binsize ?distancebetweenbins ?region ?blacklist
      ?(threads = 1) ?outrawcounts ?extendreads ?ignoreduplicates ?minmappingquality
      ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
      ?maxfragmentlength indexed_bams =
    Workflow.shell ~descr:"multibamsummary_bins" ~np:threads [
      multibamsum_gen_cmd "multiBamSummary bins"
        ?region ?blacklist
        ?outrawcounts ?extendreads ?ignoreduplicates ?minmappingquality
        ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
        ?maxfragmentlength [
        option (opt "--binSize" int) binsize ;
        option (opt "--distanceBetweenBins" int) distancebetweenbins ;
        opt "--numberOfProcessors" Fn.id np ;
        opt "--bamfiles" (list (fun bam -> dep (Samtools.indexed_bam_to_bam bam)) ~sep:" ") indexed_bams ;
        opt "--outFileName" Fn.id dest ;
      ]
    ]


  let multibamsummary_bed ?region ?blacklist ?(threads = 1)
      ?outrawcounts ?extendreads ?ignoreduplicates ?minmappingquality
      ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
      ?maxfragmentlength ?metagene ?transcriptid ?exonid ?transcriptiddesignator bed
      indexed_bams =
    Workflow.shell ~descr:"multibamsummary_bed" ~np:threads [
      multibamsum_gen_cmd "multiBamSummary BED-file"
        ?region ?blacklist
        ?outrawcounts ?extendreads ?ignoreduplicates ?minmappingquality
        ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
        ?maxfragmentlength [
        option (flag string "--metagene") metagene ;
        option (flag string "--transcriptID") transcriptid ;
        option (flag string "--exonID") exonid ;
        option (flag string "--transcript_id_designator") transcriptiddesignator ;
        opt "--numberOfProcessors" Fn.id np ;
        string "--BED" ;
        (dep bed) ;
        opt "--bamfiles"
          (list (fun bam -> dep (Samtools.indexed_bam_to_bam bam)) ~sep:" ")
          indexed_bams ;
        opt "--outFileName" Fn.id dest ;
      ]
    ]

  let reference_point_enum x =
    (match x with
     | `TES -> "TES"
     | `TSS -> "TSS"
     | `center -> "center")
    |> string

  let sort_regions_enum x =
    (match x with
     | `no -> "no"
     | `ascend -> "ascend"
     | `descend -> "descend"
     | `keep -> "keep")
    |> string

  let sort_using_enum x =
    (match x with
     | `max -> "max"
     | `min -> "min"
     | `mean -> "mean"
     | `median -> "median"
     | `region_length -> "region_length"
     | `sum -> "sum")
    |> string

  let average_type_bins_enum x =
    (match x with
     | `max -> "max"
     | `min -> "min"
     | `mean -> "mean"
     | `median -> "median"
     | `std -> "std"
     | `sum -> "sum"
    )
    |> string

  let what_to_show_enum x =
    (match x with
     | `plot_heatmap_and_colorbar -> "plot, heatmap and colorbar"
     | `plot_and_heatmap -> "plot and heatmap"
     | `heatmap_only -> "heatmap only"
     | `heatmap_and_colorbar -> "heatmap and colorbar"
    )
    |> string


  let legend_location_enum x=
    string @@ match x with
    | `best-> "best"
    | `upper_right-> "upper_right"
    | `upper_left-> "upper_left"
    | `upper_center-> "upper_center"
    | `lower_left-> "lower_left"
    | `lower_right-> "lower_right"
    | `lower_center-> "lower_center"
    | `center-> "center"
    | `center_left-> "center_left"
    | `center_right-> "center_right"
    | `none -> "none"


  class type deeptools_matrix = object
    inherit binary_file
    method format : [`deeptools_matrix]
  end

  let computeMatrix_reference_point
      ?referencePoint ?upstream ?downstream ?nanAfterEnd
      ?binSize ?sortRegions ?sortUsing ?sortUsingSamples
      ?averageTypeBins ?missingDataAsZero ?skipZeros
      ?minThreshold ?maxThreshold ?blackList ?scale
      ?(numberOfProcessors = 1) ~regions ~scores () =
    Workflow.shell ~descr:"deeptools.computeMatrix_reference_point" ~np:numberOfProcessors [
      cmd "computeMatrix" ~img [
        string "reference-point" ;
        option (opt "--referencePoint" reference_point_enum) referencePoint ;
        option (opt "--upstream" int) upstream ;
        option (opt "--downstream" int) downstream ;
        option (flag string "--nanAfterEnd") nanAfterEnd ;
        option (opt "--binSize" int) binSize ;
        option (opt "--sortRegions" sort_regions_enum) sortRegions ;
        option (opt "--sortUsing" sort_using_enum) sortUsing ;
        option (opt "--sortUsingSamples" (slist int) ) sortUsingSamples ;
        option (opt "--averageTypeBins" average_type_bins_enum) averageTypeBins ;
        option (flag string "--missingDataAsZero") missingDataAsZero ;
        option (flag string "--skipZeros") skipZeros ;
        option (opt "--minThreshold" float) minThreshold ;
        option (opt "--maxThreshold" float) maxThreshold ;
        option (opt "--blackListFileName" dep) blackList ;
        option (opt "--sc" float) scale ;
        opt "--numberOfProcessors" Fn.id np ;
        opt "--outFileName" Fn.id dest ;
        opt "--regionsFileName" dep_list regions ;
        opt "--scoreFileName" dep_list scores ;
      ]
    ]

  let plotHeatmap
      ?dpi ?kmeans ?hclust ?sortRegions ?sortUsing ?sortUsingSamples
      ?averageTypeSummaryPlot ?missingDataColor ?colorMap ?alpha
      ?colorList ?colorNumber ?zMin ?zMax ?heatmapHeight ?heatmapWidth
      ?whatToShow ?boxAroundHeatmaps ?xAxisLabel ?startLabel
      ?endLabel ?refPointLabel ?regionsLabel ?samplesLabel ?plotTitle
      ?yAxisLabel ?yMin ?yMax ?legendLocation ?perGroup
      output_format matrix =
    let tmp_file = tmp // ("file." ^ ext_of_format output_format) in
    Workflow.shell ~descr:"deeptools.plotHeatmap" [
      cmd "plotHeatmap" ~img [
        option (opt "--dpi" int) dpi ;
        option (opt "--kmeans" int) kmeans ;
        option (opt "--hclust" int) hclust ;
        option (opt "--sortRegions" sort_regions_enum) sortRegions ;
        option (opt "--sortUsing" sort_using_enum) sortUsing ;
        option (opt "--sortUsingSamples" (slist int)) sortUsingSamples ;
        option (opt "--averageTypeSummaryPlot" average_type_bins_enum) averageTypeSummaryPlot ;
        option (opt "--missingDataColor" string) missingDataColor ;
        option (opt "--colorMap" string) colorMap ;
        option (opt "--alpha" float) alpha ;
        option (opt "--colorList" (slist string)) colorList ;
        option (opt "--colorNumber" int) colorNumber ;
        option (opt "--zMin" (slist float)) zMin ;
        option (opt "--zMax" (slist float)) zMax ;
        option (opt "--heatmapHeight" float) heatmapHeight ;
        option (opt "--heatmapWidth" float) heatmapWidth ;
        option (opt "--whatToShow" what_to_show_enum) whatToShow ;
        option (opt "--boxAroundHeatmaps" (fun b -> string (if b then "yes" else "no"))) boxAroundHeatmaps ;
        option (opt "--xAxisLabel" string) xAxisLabel ;
        option (opt "--startLabel" string) startLabel  ;
        option (opt "--endLabel" string) endLabel  ;
        option (opt "--refPointLabel" string) refPointLabel  ;
        option (opt "--regionsLabel" (slist string)) regionsLabel ;
        option (opt "--samplesLabel" (slist string)) samplesLabel ;
        option (opt "--plotTitle" (string % quote ~using:'\'')) plotTitle ;
        option (opt "--yAxisLabel" string) yAxisLabel ;
        option (opt "--yMin" (slist float)) yMin ;
        option (opt "--yMax" (slist float)) yMax ;
        option (opt "--legendLocation" legend_location_enum) legendLocation ;
        option (flag string "--perGroup") perGroup ;
        opt "--matrixFile" dep matrix ;
        opt "--outFileName" Fn.id tmp_file ;
      ] ;
      mv tmp_file dest ;
    ]

  let corMethod_enum x =
    (match x with
     | `spearman -> "spearman"
     | `pearson -> "pearson"
    )
    |> string

  let whatToPlot_enum x =
    string @@ match x with
    | `heatmap -> "heatmap"
    | `scatterplot -> "scatterplot"
    | `lines -> "lines"
    | `fill -> "fill"
    | `se -> "se"
    | `std -> "std"
    | `overlapped_lines -> "overlapped_lines"

  let plotCorrelation
      ?skipZeros ?labels ?plotTitle ?removeOutliers
      ?colorMap ?plotNumbers ?log1p
      ~corMethod ~whatToPlot output_format corData
    =
    Workflow.shell ~descr:"deeptools.plotCorrelation" [
      cmd "plotCorrelation" ~img [
        opt "--corData" dep corData ;
        opt "--corMethod" corMethod_enum corMethod ;
        opt "--whatToPlot" whatToPlot_enum whatToPlot ;
        opt "--plotFile" Fn.id dest ;
        opt "--plotFileFormat" string (ext_of_format output_format) ;
        option (flag string "--skipZeros") skipZeros ;
        option (opt "--labels" (list ~sep:" " string)) labels ;
        option (opt "--plotTitle" (string % quote ~using:'\'')) plotTitle ;
        option (flag string "--removeOutliers") removeOutliers ;
        option (opt "--colorMap" string) colorMap ;
        option (flag string "--plotNumbers") plotNumbers ;
        option (flag string "--log1p") log1p ;
      ] ;
    ]

  let plotProfile
      ?dpi ?kmeans ?hclust ?averageType ?plotHeight
      ?plotWidth ?plotType ?colors ?numPlotsPerRow
      ?startLabel ?endLabel ?refPointLabel ?regionsLabel
      ?samplesLabel ?plotTitle ?yAxisLabel ?yMin ?yMax
      ?legendLocation ?perGroup
      output_format matrix
    =
    Workflow.shell ~descr:"deeptools.plotProfile" [
      cmd "plotProfile" ~img [
        option (opt "--dpi" int) dpi ;
        option (opt "--kmeans" int) kmeans ;
        option (opt "--hclust" int) hclust ;
        option (opt "--averageType" average_type_bins_enum) averageType ;
        option (opt "--plotHeight" float) plotHeight ;
        option (opt "--plotWidth" float) plotWidth ;
        option (opt "--plotType" whatToPlot_enum) plotType ;
        option (opt "--colors" (list ~sep:" " string)) colors ;
        option (opt "--numPlotsPerRow" int) numPlotsPerRow ;
        option (opt "--startLabel" (string % quote ~using:'"')) startLabel ;
        option (opt "--endLabel" (string % quote ~using:'"')) endLabel ;
        option (opt "--refPointLabel" (string % quote ~using:'"')) refPointLabel ;
        option (opt "--regionsLabel" (list ~sep:" " (string % quote ~using:'"'))) regionsLabel ;
        option (opt "--samplesLabel" (list ~sep:" " (string % quote ~using:'"'))) samplesLabel ;
        option (opt "--plotTitle" (string % quote ~using:'"')) plotTitle ;
        option (opt "--yAxisLabel" (string % quote ~using:'"')) yAxisLabel ;
        option (opt "--yMin" (list ~sep:" " float)) yMin ;
        option (opt "--yMax" (list ~sep:" " float)) yMax ;
        option (opt "--legendLocation" legend_location_enum) legendLocation ;
        option (flag string "--perGroup") perGroup ;
        opt "--plotFileFormat" string (ext_of_format output_format) ;
        opt "--outFileName" Fn.id dest ;
        opt "--matrixFile" dep matrix ;
      ]
    ]

  let plotEnrichment
      ?labels ?regionLabels ?plotTitle ?variableScales ?plotHeight
      ?plotWidth ?colors ?numPlotsPerRow ?alpha ?offset ?blackList
      ?(numberOfProcessors = 1) ~bams ~beds output_format
    =
    Workflow.shell ~np:numberOfProcessors ~descr:"deeptools.plotEnrichment" [
      cmd "plotEnrichment" ~img [
        option (opt "--labels" (list ~sep:" " (string % quote ~using:'"'))) labels ;
        option (opt "--regionLabels" (list ~sep:" " (string % quote ~using:'"'))) regionLabels ;
        option (opt "--plotTitle" (string % quote ~using:'"')) plotTitle ;
        option (flag string "--variableScales") variableScales ;
        option (opt "--plotHeight" float) plotHeight ;
        option (opt "--plotWidth" float) plotWidth ;
        option (opt "--colors" (list ~sep:" " string)) colors ;
        option (opt "--numPlotsPerRow" int) numPlotsPerRow ;
        option (opt "--alpha" float) alpha ;
        option (opt "--offset" int) offset ;
        option (opt "--blackListFileName" dep) blackList ;
        opt "--numberOfProcessors" Fn.id np ;
        opt "--bamfiles" (list ~sep:" " dep) bams ;
        opt "--BED" (list ~sep:" " dep) beds ;
        opt "--plotFile" Fn.id dest ;
        opt "--plotFileFormat" string (ext_of_format output_format) ;
      ]
    ]
end

module DESeq2 = struct
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
      directory : directory pworkflow ;
    >

  let img = [ docker_image ~account:"pveber" ~name:"bioconductor" ~tag:"3.3" () ]

  let wrapper_script = {|
library(DESeq2)
library(gplots)
library(RColorBrewer)

### DATA PROCESSING
loadCounts <- function(sample_files) {
    loadFile <- function(fn) {
        d <- read.table(fn,header=F,sep='\t')
        d[!grepl("^__",d[,1]), 2] #remove HTSEQ sum counts
    }
    sapply(sample_files,loadFile)
}

loadIds <- function(sample_files) {
    d <- read.table(sample_files[1],header=F,sep='\t')
    d[!grepl("^__",d[,1]), 1]
}

differentialAnalysis <- function(counts, description) {
    DESeq(DESeqDataSetFromMatrix(countData=counts,
                                 colData=description,
                                 design=as.formula(paste("~", paste(colnames(description),collapse=" + ")))),
          fitType='local')
}

my.summary.results <- function(object) {
    alpha <- 0.1
    notallzero <- sum(object$baseMean > 0)
    up <- sum(object$padj < alpha & object$log2FoldChange > 0,
              na.rm = TRUE)
    down <- sum(object$padj < alpha & object$log2FoldChange <
                    0, na.rm = TRUE)
    filt <- sum(!is.na(object$pvalue) & is.na(object$padj))
    outlier <- sum(object$baseMean > 0 & is.na(object$pvalue))
    c(notallzero, up, down, filt, outlier)
}

### OUTPUT
outputForAllComparisons <- function(description, outdir, factor_names, ids, dds) {
    recap <- data.frame(gene = ids)
    stats <- data.frame(comparison = character(0),
                        expressed = integer(0),
                        up = integer(0),
                        down = integer(0),
                        filt = integer(0),
                        outlier = integer(0), stringsAsFactors=F)
    for(f in factor_names) {
        l <- unique(description[,f])
        for(i in 1:length(l))
            for(j in if(i+1 > length(l)) c() else (i + 1):length(l)) {
                label <- paste0(f,"_",l[i],"_",l[j])

                res <- results(dds,contrast=c(f,as.character(l[i]),as.character(l[j])))

                fn <- paste0(outdir,"/results_",label,".tsv")
                write.table(cbind(data.frame(id = ids), res),file=fn,row.names=F,sep='\t',quote=F)

                recap[,paste0(label,"_l2fc")] <- res[,"log2FoldChange"]
                recap[,paste0(label,"_padj")] <- res[,"padj"]

                stats[dim(stats)[1]+1,] <- c(label,my.summary.results(res))

                svg(paste0(outdir,"/MA_plot_",label,".svg"), width=7, height=3.5)
                plotMA(res,main=label)
                dev.off()
            }
    }

    write.table(recap, file = paste0(outdir,"/recap.tsv"),row.names=F,sep='\t',quote=F)
    write.table(stats, file = paste0(outdir,"/summary.tsv"),row.names=F,sep='\t',quote=F)
}


generalPlots <- function(description, outdir, factor_names, ids, dds) {
    rld <- rlog(dds)
    rldMat <- assay(rld)
    rldDist <- dist(t(rldMat))
    mat <- as.matrix(rldDist)
    rownames(mat) <- colnames(mat) <- apply(description, 1, paste, collapse = ":")
    hc <- hclust(rldDist)
    hmcol <- colorRampPalette(brewer.pal(9,"GnBu"))(100)

    svg(paste0(outdir,"/sample_clustering.svg"))
    heatmap.2(mat, Rowv = as.dendrogram(hc), symm = TRUE, trace = "none", col = rev(hmcol), margin = c(13,13))
    dev.off()

    svg(paste0(outdir,"/sample_pca.svg"))
    print(plotPCA(rld, intgroup = factor_names))
    dev.off()

    counts <- cbind(ids,as.data.frame(counts(dds,normalized=T)))
    write.table(counts, file = paste0(outdir,"/normalized_counts.tsv"),row.names=F,sep='\t',quote=F,col.names=F)
}

main <- function(outdir, factor_names, sample_files, conditions) {
    description <- as.data.frame(conditions)
    colnames(description) <- factor_names
    rownames(description) <- NULL
    ids <- loadIds(sample_files)
    counts <- loadCounts(sample_files)
    dds <- differentialAnalysis(counts, description)
    system(paste("mkdir -p", outdir))
    outputForAllComparisons(description, outdir, factor_names, ids, dds)
    generalPlots(description, outdir, factor_names, ids, dds)
}
|}

  let app fn args =
    seq ~sep:"" [
      string fn ; string "(" ;
      seq ~sep:"," args ;
      string ")" ;
    ]

  let app' fn args =
    let arg (k, v) = seq ~sep:"=" [ string k ; v ] in
    seq ~sep:"" [
      string fn ; string "(" ;
      list arg ~sep:"," args ;
      string ")" ;
    ]

  let script factors samples =
    seq ~sep:"\n" [
      string wrapper_script ;
      app "main" [
        quote dest ~using:'"' ;
        app "c" (List.map ~f:(string % quote ~using:'"') factors) ;
        app "c" (List.map ~f:(snd % dep % quote ~using:'"') samples) ;
        app' "matrix" [
          "data", app "c" (List.map ~f:fst samples
                           |> List.concat
                           |> List.map ~f:(string % quote ~using:'"')) ;
          "ncol", int (List.length factors) ;
          "byrow", string "T" ;
        ]
      ]
    ]


  let wrapper factors samples =
    Workflow.shell ~descr:"deseq2.wrapper" [
      cmd "Rscript" ~img [ file_dump (script factors samples) ] ;
    ]

(*
   remove duplicates *and* keep original order
   not tail-recursive and quadratic complexity
*)
  let unique xs =
    let rec aux seen = function
      | [] -> []
      | h :: t ->
        if List.mem seen h ~equal:( = ) then
          aux seen t
        else
          h :: aux (h :: seen) t
    in
    aux [] xs

  let rec fold_2sets xs ~init ~f =
    match xs with
    | [] -> init
    | h :: t ->
      let next = List.fold_left t ~init ~f:(fun accu g -> f accu h g) in
      fold_2sets t ~init:next ~f

  let factor_levels factor_names conditions =
    List.fold_right
      conditions
      ~init:(List.map factor_names ~f:(fun _ -> []))
      ~f:(fun cond accu -> List.map2_exn cond accu ~f:(fun c cs -> c :: cs))
    |> List.map ~f:unique

  let comparisons factor_names conditions =
    let factor_levels = factor_levels factor_names conditions in
    List.map2_exn factor_names factor_levels ~f:(fun name levels ->
        fold_2sets levels ~init:[] ~f:(fun accu l1 l2 ->
            (name, l1, l2) :: accu
          )
        |> List.rev
      )
    |> List.concat


  let main_effects factors samples =
    let o = wrapper factors samples in
    let sel p = Workflow.select o p in
    object
      method sample_clustering = sel [ "sample_clustering.svg" ]
      method sample_pca = sel [ "sample_pca.svg" ]
      method normalized_counts = sel [ "normalized_counts.tsv" ]
      method comparison_summary = sel [ "summary.tsv" ]
      method effect_table = sel [ "recap.tsv" ]
      method comparisons =
        let conditions = List.map samples ~f:fst in
        List.map (comparisons factors conditions) ~f:(fun ((name, l1, l2) as comp) ->
            comp, sel [ sprintf "results_%s_%s_%s.tsv" name l1 l2 ]
          )
      method directory = o
    end
end

module Ensembl = struct
  type species = [
    | `homo_sapiens
    | `mus_musculus
  ]

  let ucsc_reference_genome ~release ~species =
    match species with
    | `mus_musculus when 63 <= release && release <= 65 -> `mm9
    | `mus_musculus when 81 <= release -> `mm10
    | `homo_sapiens when release = 71 -> `hg19
    | `homo_sapiens when 84 <= release && release <= 87 -> `hg38
    | _ -> failwith "Ensembl.ucsc_reference_genome: unknown release for this species"

  (* acronym of the lab where the species was sequenced *)
  let lab_label_of_genome = function
    | `hg19 -> "GRCh37"
    | `hg38 -> "GRCh38"
    | `mm9 -> "NCBIM37"
    | `mm10 -> "GRCm38"

  let string_of_species = function
    | `homo_sapiens -> "homo_sapiens"
    | `mus_musculus -> "mus_musculus"

  let ucsc_chr_names_gtf gff =
    Workflow.shell ~descr:"ensembl.ucsc_chr_names_gtf" [
      pipe [
        cmd "awk" [
          string "'{print \"chr\" $0}'" ;
          dep gff
        ] ;
        cmd "sed" [ string "'s/chrMT/chrM/g'" ] ;
        cmd "sed" [ string "'s/chr#/#/g'" ] ~stdout:dest
      ]
    ]

  let gff ?(chr_name = `ensembl) ~release ~species =
    let url =
      sprintf "ftp://ftp.ensembl.org/pub/release-%d/gff3/%s/%s.%s.%d.gff3.gz"
        release (string_of_species species)
        (String.capitalize (string_of_species species))
        (lab_label_of_genome (ucsc_reference_genome ~release ~species)) release
    in
    let gff = Bistro_unix.(gunzip (wget url)) in
    match chr_name with
    | `ensembl -> gff
    | `ucsc -> ucsc_chr_names_gtf gff


  let gtf ?(chr_name = `ensembl) ~release ~species =
    let url =
      sprintf "ftp://ftp.ensembl.org/pub/release-%d/gtf/%s/%s.%s.%d.gtf.gz"
        release (string_of_species species)
        (String.capitalize (string_of_species species))
        (lab_label_of_genome (ucsc_reference_genome ~release ~species)) release
    in
    let f = match chr_name with
      | `ensembl -> Fn.id
      | `ucsc -> ucsc_chr_names_gtf
    in
    f @@ Bistro_unix.(gunzip (wget url))

  let cdna ~release ~species =
    let url = sprintf "ftp://ftp.ensembl.org/pub/release-%d/fasta/%s/cdna/%s.%s.cdna.all.fa.gz"
        release (string_of_species species)
        (String.capitalize (string_of_species species))
        (lab_label_of_genome (ucsc_reference_genome ~release ~species))
    in
    Bistro_unix.wget url
end

module FastQC = struct
  let img = [ docker_image ~account:"pveber" ~name:"fastqc" ~tag:"0.11.5" () ]

  class type report = object
    inherit directory
    method contents : [`fastQC_report]
  end

  let run fq = Workflow.shell ~descr:"fastQC" [
      mkdir_p dest ;
      cmd "fastqc" ~img [
        seq ~sep:"" [ string "--outdir=" ; dest ] ;
        dep fq ;
      ] ;
      and_list [
        cd dest ;
        cmd "unzip" [ string "*_fastqc.zip" ] ;
        cmd "mv" [ string "*_fastqc/*" ; string "." ]
      ] ;
    ]

  let html_report x = Workflow.select x ["fastqc_report.html"]

  let per_base_quality x =
    Workflow.select x ["Images" ; "per_base_quality.png"]

  let per_base_sequence_content x =
    Workflow.select x ["Images" ; "per_base_sequence_content.png"]
end

module Fastq_screen = struct
  class type output = object
    inherit directory
    method contents : [`fastq_screen]
  end

  let img = [ docker_image ~account:"pveber" ~name:"fastq-screen" ~tag:"0.11.1" () ]

  let rec filter_expr res = function
      [] -> string res
    | h :: t ->
      let res = match h with
        | `Not_map -> res ^ "0"
        | `Uniquely -> res ^ "1"
        | `Multi_maps -> res ^ "2"
        | `Maps -> res ^ "3"
        | `Not_map_or_Uniquely -> res ^ "4"
        | `Not_map_or_Multi_maps -> res ^ "5"
        | `Ignore -> res ^ "-"
      in
      filter_expr res t

  let top_expr = function
    | `top1 x -> string (Int.to_string x)
    | `top2 (x, y) -> string (Int.to_string x ^ "," ^ Int.to_string y)

  let configuration genomes =
    let database_lines = List.map genomes ~f:(fun (name, fa) ->
        let index = Bowtie2.bowtie2_build fa in
        seq ~sep:"\t" [
          string "DATABASE" ;
          string name ;
          dep index // "index"
        ]
      )
    in
    seq ~sep:"\n" database_lines

  let fastq_screen ?bowtie2_opts ?filter ?illumina ?nohits ?pass ?subset
      ?tag ?(threads = 1) ?top ?(lightweight = true) fq genomes =
    Workflow.shell ~descr:"fastq_screen" ~np:threads ~mem:(Workflow.int (3 * 1024)) [
      mkdir_p dest ;
      cmd "fastq_screen" ~img [
        string "--aligner bowtie2" ;
        option (opt "--bowtie2" string) bowtie2_opts ;
        option (opt "--filter" (filter_expr "")) filter ;
        option (flag string "--illumina1_3") illumina ;
        option (flag string "--nohits") nohits ;
        option (opt "--pass" int) pass ;
        option (opt "--subset" int) subset ;
        option (flag string "--tag") tag ;
        opt "--threads" Fn.id np ;
        option (opt "--top" top_expr) top ;
        dep fq ;
        string "--conf" ; file_dump (configuration genomes) ;
        opt "--outdir" Fn.id dest ;
      ] ;
      if lightweight then rm_rf ( dest // "*.fastq" )
      else cmd "" [] ;
      mv ( dest // "*_screen.html"  ) ( dest // "report_screen.html") ;
    ]

  let html_report x = Workflow.select x [ "report_screen.html" ]
end

module Htseq = struct
  let img = [ docker_image ~account:"pveber" ~name:"htseq" ~tag:"0.6.1" () ]

  class type count_tsv = object
    inherit tsv
    method header : [`no]
    method f1 : string
    method f2 : int
  end

  let string_of_mode = function
    | `union -> "union"
    | `intersection_strict -> "intersection-strict"
    | `intersection_nonempty -> "intersection-nonempty"

  let string_of_strandedness = function
    | `yes -> "yes"
    | `no -> "no"
    | `reverse -> "reverse"

  let string_of_order = function
    | `name -> "name"
    | `position -> "pos"

  let count ?order ?mode ?stranded ?feature_type ?minaqual ?idattribute alns gff =
    let format, input = match alns with
      | `sam sam -> "sam", dep sam
      | `bam bam -> "bam", dep bam
    in
    Workflow.shell ~descr:"htseq-count" [
      cmd "htseq-count" ~img ~stdout:dest [
        opt "-f" string format ;
        option (opt "-m" (string_of_mode % string)) mode ;
        option (opt "-r" (string_of_order % string)) order ;
        option (opt "-s" (string_of_strandedness % string)) stranded ;
        option (opt "-t" string) feature_type ;
        option (opt "-a" int) minaqual ;
        option (opt "-i" string) idattribute ;
        input ;
        dep gff ;
      ]
    ]
end

module Macs2 = struct
  let img = [ docker_image ~account:"pveber" ~name:"macs2" ~tag:"2.1.1" () ]

  let macs2 subcmd opts =
    cmd "macs2" ~img (string subcmd :: opts)

  let pileup ?extsize ?both_direction bam =
    Workflow.shell ~descr:"macs2.pileup" [
      macs2 "pileup" [
        opt "-i" dep bam ;
        opt "-o" Fn.id dest ;
        option (flag string "-B") both_direction ;
        option (opt "--extsize" int) extsize ;
      ]
    ]

  type _ format =
    | Sam
    | Bam

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

  let sam = Sam
  let bam = Bam

  let opt_of_format = function
    | Sam -> "SAM"
    | Bam -> "BAM"

  type gsize = [`hs | `mm | `ce | `dm | `gsize of int]

  let gsize_expr = function
    | `hs -> string "hs"
    | `mm -> string "mm"
    | `dm -> string "dm"
    | `ce -> string "ce"
    | `gsize n -> int n

  let name = "macs2"

  type keep_dup = [ `all | `auto | `int of int ]

  let keep_dup_expr = function
    | `all -> string "all"
    | `auto -> string "auto"
    | `int n -> int n

  let callpeak_gen
      ?broad ?pvalue ?qvalue ?gsize ?call_summits
      ?fix_bimodal ?mfold ?extsize ?nomodel ?bdg ?control ?keep_dup format treatment =
    Workflow.shell ~descr:"macs2.callpeak" [
      macs2 "callpeak" [
        option (flag string "--broad") broad ;
        opt "--outdir" Fn.id dest ;
        opt "--name" string name ;
        opt "--format" (fun x -> x |> opt_of_format |> string) format ;
        option (opt "--pvalue" float) pvalue ;
        option (opt "--qvalue" float) qvalue ;
        option (opt "--gsize" gsize_expr) gsize ;
        option (flag string "--bdg") bdg ;
        option (flag string "--call-summits") call_summits ;
        option (opt "--mfold" (fun (i, j) -> seq ~sep:" " [int i ; int j])) mfold ;
        option (opt "--extsize" int) extsize ;
        option (flag string "--nomodel") nomodel ;
        option (flag string "--fix-bimodal") fix_bimodal ;
        option (opt "--keep-dup" keep_dup_expr) keep_dup ;
        option (opt "--control" (list ~sep:" " dep)) control ;
        opt "--treatment" (list ~sep:" " dep) treatment ;
      ]
    ]

  let callpeak
      ?pvalue ?qvalue ?gsize ?call_summits
      ?fix_bimodal ?mfold ?extsize ?nomodel ?bdg ?control ?keep_dup format treatment
    =
    callpeak_gen ~broad:false ?pvalue ?qvalue ?gsize ?call_summits
      ?fix_bimodal ?mfold ?extsize ?nomodel ?bdg ?control ?keep_dup format treatment


  class type peaks_xls = object
    inherit bed3
    method f4 : int
    method f5 : int
    method f6 : int
    method f7 : float
    method f8 : float
    method f9 : float
  end

  let peaks_xls x = Workflow.select x [ name ^ "_peaks.xls" ]

  class type narrow_peaks = object
    inherit bed5
    method f6 : string
    method f7 : float
    method f8 : float
    method f9 : float
    method f10 : int
  end

  let narrow_peaks x =
    Workflow.select x [ name ^ "_peaks.narrowPeak" ]

  class type peak_summits = object
    inherit bed4
    method f5 : float
  end

  let peak_summits x = Workflow.select x [ name ^ "_summits.bed" ]

  let callpeak_broad
      ?pvalue ?qvalue ?gsize ?call_summits
      ?fix_bimodal ?mfold ?extsize ?nomodel ?bdg ?control ?keep_dup format treatment
    =
    callpeak_gen ~broad:true ?pvalue ?qvalue ?gsize ?call_summits
      ?fix_bimodal ?mfold ?extsize ?nomodel ?bdg ?control ?keep_dup format treatment

  class type broad_peaks = object
    inherit bed5
    method f6 : string
    method f7 : float
    method f8 : float
    method f9 : float
  end

  let broad_peaks x =
    Workflow.select x [ name ^ "_peaks.broadPeak" ]
end

module Macs = struct

  let img = [ docker_image ~account:"pveber" ~name:"macs" ~tag:"1.4.2" () ]


  type _ format =
    | Sam
    | Bam

  class type output = object
    inherit directory
    method contents : [`macs]
  end

  let sam = Sam
  let bam = Bam

  let opt_of_format = function
    | Sam -> "SAM"
    | Bam -> "BAM"

  type gsize = [ `hs | `mm | `ce | `dm | `gsize of int ]

  let gsize_expr = function
    | `hs -> string "hs"
    | `mm -> string "mm"
    | `dm -> string "dm"
    | `ce -> string "ce"
    | `gsize n -> int n

  type keep_dup = [ `all | `auto | `int of int ]

  let keep_dup_expr = function
    | `all -> string "all"
    | `auto -> string "auto"
    | `int n -> int n

  let name = "macs"

  let run ?control ?petdist ?gsize ?tsize ?bw ?pvalue ?mfold ?nolambda
      ?slocal ?llocal ?on_auto ?nomodel ?shiftsize ?keep_dup
      ?to_large ?wig ?bdg ?single_profile ?space ?call_subpeaks
      ?diag ?fe_min ?fe_max ?fe_step format treatment =
    Workflow.shell ~descr:"macs" ~mem:(Workflow.int (3 * 1024)) ~np:8  [
      mkdir_p dest ;
      cmd "macs14" ~img [
        option (opt "--control" (list ~sep:"," dep)) control ;
        opt "--name" seq [ dest ; string "/" ; string name ] ;
        opt "--format" (fun x -> x |> opt_of_format |> string) format ;
        option (opt "--petdist" int) petdist ;
        option (opt "--gsize" gsize_expr) gsize ;
        option (opt "--tsize" int) tsize ;
        option (opt "--bw" int) bw ;
        option (opt "--pvalue" float) pvalue ;
        option (opt "--mfold" (fun (i, j) -> seq ~sep:"," [int i ; int j])) mfold ;
        option (flag string "--nolambda") nolambda ;
        option (opt "--slocal" int) slocal ;
        option (opt "--llocal" int) llocal ;
        option (flag string "--on-auto") on_auto ;
        option (flag string "--nomodel") nomodel ;
        option (opt "--shiftsize" int) shiftsize ;
        option (opt "--keep-dup" keep_dup_expr) keep_dup ;
        option (flag string "--to-large") to_large ;
        option (flag string "--wig") wig ;
        option (flag string "--bdg") bdg ;
        option (flag string "--single-profile") single_profile ;
        option (opt "--space" int) space ;
        option (flag string "--call-subpeaks") call_subpeaks ;
        option (flag string "--diag") diag ;
        option (opt "--fe-min" int) fe_min ;
        option (opt "--fe-max" int) fe_max ;
        option (opt "--fe-step" int) fe_step ;
        opt "--treatment" (list ~sep:"," dep) treatment ;
        Fn.id dest ;
      ]
    ]

  class type peaks_xls = object
    inherit bed3
    method f4 : int
    method f5 : int
    method f6 : int
    method f7 : float
    method f8 : float
    method f9 : float
  end

  let peaks_xls x = Workflow.select x [ name ^ "_peaks.xls" ]

  class type narrow_peaks = object
    inherit bed5
    method f6 : string
    method f7 : float
    method f8 : float
    method f9 : float
    method f10 : int
  end

  let narrow_peaks x =
    Workflow.select x [ name ^ "_peaks.narrowPeak" ]

  class type peak_summits = object
    inherit bed4
    method f5 : float
  end

  let peak_summits x = Workflow.select x [ name ^ "_summits.bed" ]
end

module Meme_suite = struct

  let img = [ docker_image ~account:"pveber" ~name:"meme" ~tag:"4.11.2" () ]

  let meme_chip ?meme_nmotifs ?meme_minw ?meme_maxw (* ?np:threads *) fa =
    Workflow.shell ~descr:"meme-chip" (* ?np:threads *) [
      cmd "meme-chip" ~img [
        option (opt "-meme-nmotifs" int) meme_nmotifs ;
        option (opt "-meme-minw" int) meme_minw ;
        option (opt "-meme-maxw" int) meme_maxw ;
        (* opt "-meme-p" Fn.id np ; *)(* this is disabled due to mpirun refusing to run as root under docker *)
        opt "--oc" Fn.id dest ;
        dep fa ;
      ]
    ]

  let string_of_alphabet = function
    | `dna -> "dna"
    | `rna -> "rna"
    | `protein -> "protein"

  let meme_alphabet_opt x =
    string ("-" ^ string_of_alphabet x)

  let meme ?nmotifs ?minw ?maxw ?revcomp ?maxsize ?alphabet (* ?threads *) fa =
    Workflow.shell ~descr:"meme" (* ?np:threads *) [
      cmd "meme" ~img [
        option (opt "-nmotifs" int) nmotifs ;
        option (opt "-minw" int) minw ;
        option (opt "-maxw" int) maxw ;
        option meme_alphabet_opt alphabet ;
        option (flag string "-revcomp") revcomp ;
        option (opt "-maxsize" int) maxsize ;
        (* opt "-p" Fn.id np ; *) (* this is disabled due to mpirun refusing to run as root under docker *)
        opt "-oc" Fn.id dest ;
        dep fa ;
      ]
    ]

  let meme_logo dir ?(rc = false) n =
    Workflow.select dir [ sprintf "logo%s%d.png" (if rc then "" else "_rc") n ]

  let fimo
      ?alpha ?bgfile ?max_stored_scores ?max_strand ?motif ?motif_pseudo
      ?no_qvalue ?norc ?parse_genomic_coord ?prior_dist ?psp
      ?qv_thresh ?thresh meme_motifs seqs =
    Bistro.Workflow.shell ~descr:"meme_suite.fimo"  [
      cmd "fimo" ~img [
        option (opt "--alpha" float) alpha ;
        option (opt "--bgfile" dep) bgfile ;
        option (opt "--max-stored-scores" int) max_stored_scores ;
        option (flag string "--max-strand") max_strand ;
        option (opt "--motif" string) motif ;
        option (opt "--motif-pseudo" float) motif_pseudo ;
        option (flag string "--no-qvalue") no_qvalue ;
        option (flag string "--norc") norc ;
        option (flag string "--parse-genomic-coord") parse_genomic_coord ;
        option (opt "--prior-dist" dep) prior_dist ;
        option (opt "--psp" dep) psp ;
        option (flag string "--qv-thresh") qv_thresh ;
        option (opt "--thresh" float) thresh ;
        opt "--oc" Fn.id dest ;
        dep meme_motifs ;
        dep seqs ;
      ]
    ]
end

module Prokka = struct
  let img = [ docker_image ~account:"pveber" ~name:"prokka" ~tag:"1.12" () ]

  let gram_expr = function
    | `Plus -> string "+"
    | `Minus -> string "-"

  let run ?prefix ?addgenes ?locustag ?increment ?gffver ?compliant
      ?centre ?genus ?species ?strain ?plasmid ?kingdom ?gcode ?gram
      ?usegenus ?proteins ?hmms ?metagenome ?rawproduct ?fast ?(threads = 1)
      ?mincontiglen ?evalue ?rfam ?norrna ?notrna ?rnammer fa =
    Workflow.shell ~descr:"prokka" ~np:threads ~mem:(Workflow.int (3 * 1024)) [
      mkdir_p dest ;
      cmd "prokka" ~img [
        string "--force" ;
        option (opt "--prefix" string) prefix ;
        option (flag string "--addgenes") addgenes ;
        option (opt "--locustag" string) locustag ;
        option (opt "--increment" int) increment ;
        option (opt "--gffver" string) gffver ;
        option (flag string "--compliant") compliant ;
        option (opt "--centre" string) centre ;
        option (opt "--genus" string) genus ;
        option (opt "--species" string) species ;
        option (opt "--strain" string) strain ;
        option (opt "--plasmid" string) plasmid ;
        option (opt "--kingdom" string) kingdom ;
        option (opt "--gcode" int) gcode ;
        option (opt "--gram" gram_expr) gram ;
        option (flag string "--usegenus") usegenus ;
        option (opt "--proteins" string) proteins ;
        option (opt "--hmms" string) hmms ;
        option (flag string "--metagenome") metagenome ;
        option (flag string "--rawproduct") rawproduct ;
        option (flag string "--fast") fast ;
        opt "--cpus" Fn.id np ;
        option (opt "--mincontiglen" int) mincontiglen ;
        option (opt "--evalue" float) evalue ;
        option (flag string "--rfam") rfam ;
        option (flag string "--norrna") norrna ;
        option (flag string "--notrna") notrna ;
        option (flag string "--rnammer") rnammer ;
        opt "--outdir" Fn.id dest ;
        dep fa ;
      ] ;
    ]
end

module Sra_toolkit = struct

  let img = [ docker_image ~account:"pveber" ~name:"sra-toolkit" ~tag:"2.8.0" () ]

  let sra_of_input = function
    | `id id -> string id
    | `idw w -> string_dep w
    | `file w -> dep w

  let fastq_dump sra =
    Workflow.shell ~descr:"sratoolkit.fastq_dump" [
      cmd ~img "fastq-dump" [ string "-Z" ; sra_of_input sra ] ~stdout:dest
    ]

  let fastq_dump_gz input =
    let sra = sra_of_input input in
    Workflow.shell ~descr:"sratoolkit.fastq_dump" [
      cmd ~img "fastq-dump" [ string "--gzip" ; string "-Z" ; sra ] ~stdout:dest
    ]

  let fastq_dump_pe sra =
    let dir =
      Workflow.shell ~descr:"sratoolkit.fastq_dump" [
        mkdir_p dest ;
        cmd ~img "fastq-dump" [
          opt "-O" Fn.id dest ;
          string "--split-files" ;
          dep sra
        ] ;
        mv (dest // "*_1.fastq") (dest // "reads_1.fastq") ;
        mv (dest // "*_2.fastq") (dest // "reads_2.fastq") ;
      ]
    in
    Workflow.select dir ["reads_1.fastq"],
    Workflow.select dir ["reads_2.fastq"]


  let fastq_dump_pe_gz input =
    let sra = sra_of_input input in
    let dir =
      Workflow.shell ~descr:"sratoolkit.fastq_dump" [
        mkdir_p dest ;
        cmd ~img "fastq-dump" [
          opt "-O" Fn.id dest ;
          string "--split-files" ;
          string "--gzip" ;
          sra ;
        ] ;
        mv (dest // "*_1.fastq*") (dest // "reads_1.fq.gz") ;
        mv (dest // "*_2.fastq*") (dest // "reads_2.fq.gz") ;
      ]
    in
    Workflow.select dir ["reads_1.fq.gz"],
    Workflow.select dir ["reads_2.fq.gz"]

  let fastq_dump_to_fasta sra =
    Workflow.shell ~descr:"sratoolkit.fastq_dump" [
      cmd ~img "fastq-dump" [
        string "-Z" ;
        string "--fasta" ;
        dep sra
      ] ~stdout:dest
    ]
end

module Srst2 = struct

  let img = [ docker_image ~account:"pveber" ~name:"srst2" ~tag:"0.2.0" () ]


  let run_gen_cmd ?mlst_db ?mlst_delimiter ?mlst_definitions
      ?mlst_max_mismatch ?gene_db ?no_gene_details ?gene_max_mismatch
      ?min_coverage ?max_divergence ?min_depth ?min_edge_depth ?prob_err
      ?truncation_score_tolerance ?other ?max_unaligned_overlap ?mapq
      ?baseq ?samtools_args ?report_new_consensus
      ?report_all_consensus cmd_name other_args =
    cmd cmd_name ~img (
      List.append [
        option (opt "--mlst_db" dep) mlst_db ;
        option (opt "--mlst_delimiter" string) mlst_delimiter ;
        option (opt "--mlst_definitions" dep) mlst_definitions ;
        option (opt "--mlst_max_mismatch" int) mlst_max_mismatch ;
        option (opt "--gene_db" (list ~sep:" " dep)) gene_db ;
        option (flag string "--no_gene_details") no_gene_details ;
        option (opt "--gene_max_mismatch" int) gene_max_mismatch ;
        option (opt "--min_coverage" int) min_coverage ;
        option (opt "--max_divergence" int) max_divergence ;
        option (opt "--min_depth" int) min_depth ;
        option (opt "--min_edge_depth" int) min_edge_depth ;
        option (opt "--prob_err" float) prob_err ;
        option (opt "--truncation_score_tolerance" int) truncation_score_tolerance ;
        option (opt "--other" string) other ;
        option (opt "--max_unaligned_overlap" int) max_unaligned_overlap ;
        option (opt "--mapq" int) mapq ;
        option (opt "--baseq" int) baseq ;
        option (opt "--samtools_args" string) samtools_args ;
        option (flag string "--report_new_consensus") report_new_consensus ;
        option (flag string "--report_all_consensus") report_all_consensus ;
      ]
        other_args
    )

  let run_se ?mlst_db ?mlst_delimiter ?mlst_definitions
      ?mlst_max_mismatch ?gene_db ?no_gene_details ?gene_max_mismatch
      ?min_coverage ?max_divergence ?min_depth ?min_edge_depth ?prob_err
      ?truncation_score_tolerance ?other ?max_unaligned_overlap ?mapq
      ?baseq ?samtools_args ?report_new_consensus
      ?report_all_consensus ?(threads = 1) fq =
    Workflow.shell ~descr:"srst2" ~np:threads ~mem:(Workflow.int (3 * 1024)) [
      mkdir_p dest ;
      run_gen_cmd "srst2" ?mlst_db ?mlst_delimiter ?mlst_definitions
        ?mlst_max_mismatch ?gene_db ?no_gene_details ?gene_max_mismatch
        ?min_coverage ?max_divergence ?min_depth ?min_edge_depth ?prob_err
        ?truncation_score_tolerance ?other ?max_unaligned_overlap ?mapq
        ?baseq ?samtools_args ?report_new_consensus
        ?report_all_consensus [
        opt "--threads" Fn.id np ;
        opt "--input_se" (list ~sep:" " dep) fq ;
        opt "--output" Fn.id dest ;
      ] ;
    ]

  let run_pe ?mlst_db ?mlst_delimiter ?mlst_definitions
      ?mlst_max_mismatch ?gene_db ?no_gene_details ?gene_max_mismatch
      ?min_coverage ?max_divergence ?min_depth ?min_edge_depth ?prob_err
      ?truncation_score_tolerance ?other ?max_unaligned_overlap ?mapq
      ?baseq ?samtools_args ?report_new_consensus
      ?report_all_consensus ?(threads = 1) fq =
    Workflow.shell ~descr:"srst2" ~np:threads ~mem:(Workflow.int (3 * 1024)) [
      mkdir_p dest ;
      run_gen_cmd "srst2" ?mlst_db ?mlst_delimiter ?mlst_definitions
        ?mlst_max_mismatch ?gene_db ?no_gene_details ?gene_max_mismatch
        ?min_coverage ?max_divergence ?min_depth ?min_edge_depth ?prob_err
        ?truncation_score_tolerance ?other ?max_unaligned_overlap ?mapq
        ?baseq ?samtools_args ?report_new_consensus
        ?report_all_consensus [
        opt "--threads" Fn.id np ;
        opt "--input_pe" (list ~sep:" " dep) fq ;
        opt "--output" Fn.id dest ;
      ] ;
    ]
end

module Tophat = struct
  let img = [ docker_image ~account:"pveber" ~name:"tophat" ~tag:"2.1.1" () ]

  class type output = object
    inherit directory
    method contents : [`tophat]
  end

  let tophat1 ?color index fqs =
    let args = match fqs with
      | `single_end fqs -> list dep ~sep:"," fqs
      | `paired_end (fqs1, fqs2) ->
        seq [
          list dep ~sep:"," fqs1 ;
          string " " ;
          list dep ~sep:"," fqs2
        ]
    in
    Workflow.shell ~np:8 ~mem:(Workflow.int (4 * 1024)) ~descr:"tophat" [
      cmd ~img "tophat" [
        string "--bowtie1" ;
        opt "--num-threads" Fn.id np ;
        option (flag string "--color") color ;
        opt "--output-dir" Fn.id dest ;
        seq [ dep index ; string "/index" ] ;
        args
      ]
    ]

  let tophat2 index fqs =
    let args = match fqs with
      | `single_end fqs -> list dep ~sep:"," fqs
      | `paired_end (fqs1, fqs2) ->
        seq [
          list dep ~sep:"," fqs1 ;
          string " " ;
          list dep ~sep:"," fqs2
        ]
    in
    Workflow.shell ~np:8 ~mem:(Workflow.int (4 * 1024)) ~descr:"tophat2" [
      cmd ~img "tophat2" [
        opt "--num-threads" Fn.id np ;
        opt "--output-dir" Fn.id dest ;
        seq [ dep index ; string "/index" ] ;
        args
      ]
    ]

  let accepted_hits x = Workflow.select x ["accepted_hits.bam"]

  let junctions x = Workflow.select x ["junctions.bed"]
end

module Ucsc_gb = struct

  type genome = [ `dm3 | `droSim1 | `hg18 | `hg19 | `hg38 | `mm8 | `mm9 | `mm10 | `sacCer2 ]

  let string_of_genome = function
    | `dm3 -> "dm3"
    | `droSim1 -> "droSim1"
    | `hg18 -> "hg18"
    | `hg19 -> "hg19"
    | `hg38 -> "hg38"
    | `mm8 -> "mm8"
    | `mm9 -> "mm9"
    | `mm10 -> "mm10"
    | `sacCer2 -> "sacCer2"

  let genome_of_string = function
    | "dm3" -> Some `dm3
    | "droSim1" -> Some `droSim1
    | "hg18" -> Some `hg18
    | "hg19" -> Some `hg19
    | "hg38" -> Some `hg38
    | "mm8" -> Some `mm8
    | "mm9" -> Some `mm9
    | "mm10" -> Some `mm10
    | "sacCer2" -> Some `sacCer2
    | _ -> None

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

  class type chromosome_sequences = object
    inherit directory
    method contents : [`ucsc_chromosome_sequences]
  end

  let img = [ docker_image ~account:"pveber" ~name:"ucsc-kent" ~tag:"330" () ]


  (** {5 Dealing with genome sequences} *)

  let chromosome_sequence org chr =
    let org = string_of_genome org in
    let url =
      sprintf
        "ftp://hgdownload.cse.ucsc.edu/goldenPath/%s/chromosomes/%s.fa.gz"
        org chr
    in
    let descr = sprintf "ucsc_gb.chromosome_sequence(%s,%s)" org chr in
    Workflow.shell ~descr [
      Bistro_unix.Cmd.wget ~dest:(tmp // "seq.fa.gz") (Workflow.string url) ;
      cmd "gunzip" [ tmp // "seq.fa.gz" ] ;
      cmd "mv" [ tmp // "seq.fa.gz" ; dest ] ;
    ]

  let chromosome_sequences org =
    let org = string_of_genome org in
    let url = sprintf "ftp://hgdownload.cse.ucsc.edu/goldenPath/%s/chromosomes/*" org in
    Workflow.shell ~descr:(sprintf "ucsc_gb.chromosome_sequences(%s)" org) [
      mkdir_p dest ;
      cd dest ;
      Bistro_unix.Cmd.wget (Workflow.string url) ;
      cmd "gunzip" [ string "*.gz" ]
    ]

  let genome_sequence org =
    let chr_seqs = chromosome_sequences org in
    Workflow.shell ~descr:"ucsc_gb.genome_sequence" [
      cmd "bash" [
        opt "-c" string "'shopt -s nullglob ; cat $0/{chr?.fa,chr??.fa,chr???.fa,chr????.fa} > $1'" ;
        dep chr_seqs ;
        dest
      ]
    ]

  (* UGLY hack due to twoBitToFa: this tool requires that the 2bit
     sequence should be put in a file with extension 2bit. So I'm forced
     to create first a directory and then to select the unique file in it...*)
  let genome_2bit_sequence_dir org =
    let org = string_of_genome org in
    let url = sprintf "ftp://hgdownload.cse.ucsc.edu/goldenPath/%s/bigZips/%s.2bit" org org in
    Workflow.shell ~descr:(sprintf "ucsc_gb.2bit_sequence(%s)" org) [
      mkdir dest ;
      cd dest ;
      Bistro_unix.Cmd.wget (Workflow.string url) ;
    ]

  let genome_2bit_sequence org =
    Workflow.select (genome_2bit_sequence_dir org) [ (string_of_genome org) ^ ".2bit" ]

  (* (\* let wg_encode_crg_mappability n org = *\) *)
  (* (\*   let url = sp "ftp://hgdownload.cse.ucsc.edu/gbdb/%s/bbi/wgEncodeCrgMapabilityAlign%dmer.bigWig" (string_of_genome org) n in *\) *)
  (* (\*   Guizmin_unix.wget (Workflow.string url) *\) *)

  (* (\* let wg_encode_crg_mappability_36 org = wg_encode_crg_mappability 36 org *\) *)
  (* (\* let wg_encode_crg_mappability_40 org = wg_encode_crg_mappability 40 org *\) *)
  (* (\* let wg_encode_crg_mappability_50 org = wg_encode_crg_mappability 50 org *\) *)
  (* (\* let wg_encode_crg_mappability_75 org = wg_encode_crg_mappability 75 org *\) *)
  (* (\* let wg_encode_crg_mappability_100 org = wg_encode_crg_mappability 100 org *\) *)

  let twoBitToFa twobits bed =
    Workflow.shell ~descr:"ucsc_gb.twoBitToFa" [
      cmd ~img "twoBitToFa" [
        opt' "-bed" dep bed ;
        dep twobits ;
        dest
      ]
    ]

  (* let fasta_of_bed org bed = *)
  (*   twoBitToFa bed (genome_2bit_sequence org) *)

  (* (\*   f2 *\) *)
  (* (\*     "guizmin.bioinfo.ucsc.fasta_of_bed[1]" [] *\) *)
  (* (\*     seq2b bed *\) *)
  (* (\*     (fun env (File seq2b) (File bed) path -> *\) *)
  (* (\*       twoBitToFa ~positions:(`bed bed) ~seq2b ~fa:path) *\) *)

  (* (\* let fetch_sequences (File seq2b) locations = *\) *)
  (* (\*   let open Core_kernel in *\) *)
  (* (\*   Core_extended.Sys_utils.with_tmp ~pre:"gzm" ~suf:".seqList" (fun seqList -> *\) *)
  (* (\*     Core_extended.Sys_utils.with_tmp ~pre:"gzm" ~suf:".fa" (fun fa -> *\) *)
  (* (\*       (\\* Write locations to a file *\\) *\) *)
  (* (\*       List.map locations Fungen.Location.to_string *\) *)
  (* (\*       |> Out_channel.write_lines seqList ; *\) *)

  (* (\*       (\\* run twoBitToFa *\\) *\) *)
  (* (\*       twoBitToFa ~positions:(`seqList seqList) ~seq2b ~fa ; *\) *)

  (* (\*       (\\* Parse the fasta file *\\) *\) *)
  (* (\*       In_channel.with_file fa ~f:(fun ic -> *\) *)
  (* (\*         Biocaml.fasta (in_channel_to_char_seq_item_stream_exn ic) *\) *)
  (* (\*         /@ (fun x -> x.Biocaml.fasta sequence) *\) *)
  (* (\*         |> Stream.to_list *\) *)
  (* (\*       ) *\) *)
  (* (\*     ) *\) *)
  (* (\*   ) *\) *)

  (** {5 Chromosome size and clipping} *)

  let fetchChromSizes org =
    Workflow.shell ~descr:"ucsc_gb.fetchChromSizes" [
      cmd "fetchChromSizes" ~img ~stdout:dest [
        string (string_of_genome org) ;
      ]
    ]

  let bedClip org bed =
    Workflow.shell ~descr:"ucsc_gb.bedClip" [
      cmd "bedClip -verbose=2" ~img [
        dep bed ;
        dep org ;
        dest ;
      ]
    ]




  (** {5 Conversion between annotation file formats} *)

  (* (\* let wig_of_bigWig bigWig = *\) *)
  (* (\*   f1 *\) *)
  (* (\*     "guizmin.bioinfo.ucsc.wig_of_bigWig[r1]" [] *\) *)
  (* (\*     bigWig *\) *)
  (* (\*     ( *\) *)
  (* (\*       fun env (File bigWig) path -> *\) *)
  (* (\* 	env.bash [ *\) *)
  (* (\*           sp "bigWigToWig %s %s" bigWig path *\) *)
  (* (\* 	] *\) *)
  (* (\*     ) *\) *)

  (* (\* let bigWig_of_wig ?(clip = false) org wig = *\) *)
  (* (\*   let chrom_info = chrom_info org in *\) *)
  (* (\*   f2 *\) *)
  (* (\*     "guizmin.bioinfo.ucsc.bigWig_of_wig[r1]" [] *\) *)
  (* (\*     chrom_info wig *\) *)
  (* (\*     (fun env (File chrom_info) (File wig) path -> *\) *)
  (* (\*       let clip = if clip then "-clip" else "" in *\) *)
  (* (\*       env.sh "wigToBigWig %s %s %s %s" clip wig chrom_info path) *\) *)

  let bedGraphToBigWig org bg =
    let tmp = seq [ tmp ; string "/sorted.bedGraph" ] in
    Workflow.shell ~descr:"bedGraphToBigWig" [
      cmd "sort" ~stdout:tmp [
        string "-k1,1" ;
        string "-k2,2n" ;
        dep bg ;
      ] ;
      cmd "bedGraphToBigWig" ~img [
        tmp ;
        dep (fetchChromSizes org) ;
        dest ;
      ]
    ]

  let bedToBigBed_command org bed =
    let tmp = seq [ tmp ; string "/sorted.bed" ] in
    let sort =
      cmd "sort" ~stdout:tmp [
        string "-k1,1" ;
        string "-k2,2n" ;
        dep bed ;
      ] in
    let bedToBigBed =
      cmd "bedToBigBed" ~img [
        tmp ;
        dep (fetchChromSizes org) ;
        dest ;
      ]
    in
    [ sort ; bedToBigBed ]

  let bedToBigBed org =
    let f bed =
      Workflow.shell
        ~descr:"ucsc_gb.bedToBigBed"
        (bedToBigBed_command org bed)
    in
    function
    | `bed3 bed -> f bed
    | `bed5 bed -> f bed

  (* implements the following algorithm
     if bed is empty
     then touch target
     else bedToBigBed (sort bed)
  *)
  let bedToBigBed_failsafe org =
    let f bed =
      let test = cmd "test" [ string "! -s" ; dep bed ] in
      let touch = cmd "touch" [ dest ] in
      let cmd = or_list [
          and_list [ test ; touch ] ;
          and_list (bedToBigBed_command org bed) ;
        ] in
      Workflow.shell [ cmd ]
    in
    function
    | `bed3 bed -> f bed
    | `bed5 bed -> f bed


  module Lift_over = struct
    class type chain_file = object
      inherit file
      method format : [`lift_over_chain_file]
    end
    class type ['a] output = object
      inherit directory
      method format : [`ucsc_lift_over of 'a]
    end

    let chain_file ~org_from ~org_to =
      let org_from = string_of_genome org_from
      and org_to = string_of_genome org_to in
      let url =
        sprintf
          "ftp://hgdownload.cse.ucsc.edu/goldenPath/%s/liftOver/%sTo%s.over.chain.gz"
          org_from org_from (String.capitalize org_to)
      in
      Bistro_unix.(gunzip (wget url))

    let bed ~org_from ~org_to bed =
      let chain_file = chain_file ~org_from ~org_to in
      Workflow.shell ~descr:"ucsc.liftOver" [
        mkdir_p dest ;
        cmd "liftOver" ~img [
          dep bed ;
          dep chain_file ;
          dest // "mapped.bed" ;
          dest // "unmapped.bed" ;
        ] ;
      ]

    let mapped x = Workflow.select x ["mapped.bed"]
    let unmapped x = Workflow.select x ["unmapped.bed"]
  end
end


module Subread = struct

  let img = [ docker_image ~account:"pveber" ~name:"subread" ~tag:"1.6.3" () ]

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

  let strandness_token = function
    | `Unstranded -> int 0
    | `Stranded -> int 1
    | `Reversely_stranded -> int 2

  let featureCounts
      ?feature_type ?attribute_type ?strandness
      ?q ?nthreads
      gff mapped_reads =
    Workflow.shell ~descr:"featureCounts" ~np:(Option.value ~default:1 nthreads) [
      mkdir_p dest ;
      cmd "featureCounts" ~img [
        option (opt "-t" string) feature_type ;
        option (opt "-g" string) attribute_type ;
        option (opt "-s" strandness_token) strandness ;
        option (opt "-Q" int) q ;
        option (opt "-T" (fun _ -> np)) nthreads ;
        opt "-a" dep gff ;
        opt "-o" Fn.id (dest // "counts.tsv") ;
        dep mapped_reads ;
      ]
    ]

  let featureCounts_tsv o = Workflow.select o ["counts.tsv"]
  let featureCounts_htseq_tsv o =
    Workflow.shell ~descr:"featureCounts_htseq_tsv" [
      pipe [
        cmd "sed" [ quote ~using:'\'' (string "1,2d") ; dep (featureCounts_tsv o) ] ;
        cmd "awk" ~stdout:dest [
          quote ~using:'\'' (string "{print $1,$7}") ;
          string "OFS='\t'" ;
        ]
      ]
    ]
  let featureCounts_summary o = Workflow.select o ["counts.tsv.summary"]
end

module Kallisto = struct
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

  let img = [ docker_image ~account:"pveber" ~name:"kallisto" ~tag:"0.43.0" () ]

  let index fas =
    Workflow.shell ~descr:"kallisto-index" [
      cmd "kallisto index" ~img [
        opt "-i" Fn.id dest ;
        list ~sep:" " dep fas ;
      ]
    ]

  let fq_input = function
    | `fq_gz x -> Bistro_unix.Cmd.psgunzip x
    | `fq x -> dep x

  let quant ?bootstrap_samples ?threads ?fragment_length ?sd idx ~fq1 ?fq2 () =
    Workflow.shell ~descr:"kallisto-quant" ?np:threads [
      cmd "kallisto quant" ~img [
        opt "-i" dep idx ;
        opt "-o" Fn.id dest ;
        opt "-t" Fn.id np ;
        option (opt "-b" int) bootstrap_samples ;
        fq_input fq1 ;
        option fq_input fq2 ;
        option (opt "-l" float) fragment_length ;
        option (opt "-s" float) sd ;
        string (
          match fq2 with
          | None -> "--single"
          | Some _ -> ""
        ) ;
      ]
    ]

  let abundance x =
    Workflow.select x [ "abundance.tsv" ]

  let%pworkflow merge_eff_counts ~sample_ids ~kallisto_outputs =

    let parse_eff_counts fn =
      In_channel.read_lines fn
      |> Fn.flip List.drop 1
      |> List.map ~f:(fun l ->
          String.split ~on:'\t' l
          |> Fn.flip List.nth_exn 3
        )
    in
    let parse_names fn =
      In_channel.read_lines fn
      |> Fn.flip List.drop 1
      |> List.map ~f:(fun l ->
          String.split ~on:'\t' l
          |> List.hd_exn
        )
    in

    let names = parse_names [%path List.hd_exn kallisto_outputs] in
    let counts  = List.map [%eval Workflow.(eval_paths kallisto_outputs)] ~f:parse_eff_counts in

    let table = List.transpose_exn (names :: counts) in

    let lines =
      ("transcript" :: [%param sample_ids]) :: table
      |> List.map ~f:(String.concat ~sep:"\t")
    in

    Out_channel.write_lines [%dest] lines


  let%pworkflow merge_tpms ~sample_ids ~kallisto_outputs =

    let parse_tpms fn =
      In_channel.read_lines fn
      |> Fn.flip List.drop 1
      |> List.map ~f:(fun l ->
          String.split ~on:'\t' l
          |> Fn.flip List.nth_exn 4
        )
    in
    let parse_names fn =
      In_channel.read_lines fn
      |> Fn.flip List.drop 1
      |> List.map ~f:(fun l ->
          String.split ~on:'\t' l
          |> List.hd_exn
        )
    in

    let names = parse_names [%eval Workflow.eval_path (List.hd_exn kallisto_outputs)] in
    let tpms  = List.map [%eval Workflow.(eval_paths kallisto_outputs)] ~f:parse_tpms in

    let table = List.transpose_exn (names :: tpms) in

    let lines =
      ("transcript" :: [%param sample_ids]) :: table
      |> List.map ~f:(String.concat ~sep:"\t")
    in

    Out_channel.write_lines [%dest] lines
end

module Spades = struct
  let img = [ docker_image ~account:"pveber" ~name:"spades" ~tag:"3.9.1" () ]

  (* spades insists on reading file extensions~ *)
  let renamings (ones, twos) =
    let f side i x =
      let id = sprintf "pe%d-%d" (i + 1) side in
      let new_name = seq ~sep:"/" [ tmp ; string (id ^ ".fq") ] in
      let opt = opt (sprintf "--pe%d-%d" (i + 1) side) Fn.id new_name in
      let cmd = cmd "ln" [ string "-s" ; dep x ; new_name ] in
      opt, cmd
    in
    let r = List.mapi ones ~f:(f 1) @ List.mapi twos ~f:(f 2) in
    let args = seq ~sep:" " (List.map r ~f:fst) in
    let cmds = List.map r ~f:snd in
    Some args, cmds

  let spades
      ?single_cell ?iontorrent
      ?pe
      ?(threads = 4)
      ?(memory = 10)
      ()
    =
    let pe_args, ln_commands = match pe with
      | None -> None, []
      | Some files -> renamings files
    in
    Workflow.shell ~np:threads ~mem:(Workflow.int (memory * 1024)) ~descr:"spades" [
      mkdir_p tmp ;
      mkdir_p dest ;
      within_container img (
        and_list (
          ln_commands @ [
            cmd "spades.py" ~img [
              option (flag string "--sc") single_cell ;
              option (flag string "--iontorrent") iontorrent ;
              opt "--threads" Fn.id np ;
              opt "--memory" Fn.id (seq [ string "$((" ; mem ; string " / 1024))" ]) ;
              option Fn.id pe_args ;
              opt "-o" Fn.id dest ;
            ]
          ]
        )
      )
    ]

  let contigs x = Workflow.select x ["contigs.fasta"]
  let scaffolds x = Workflow.select x ["scaffolds.fasta"]
end

module Idba = struct
  let img = [ docker_image ~account:"pveber" ~name:"idba" ~tag:"1.1.3" () ]

  let fq2fa ?filter input =
    let args = match input with
      | `Se fq -> dep fq
      | `Pe_merge (fq1, fq2) ->
        opt "--merge" Fn.id (seq ~sep:" " [dep fq1 ; dep fq2])
      | `Pe_paired fq ->
        opt "--paired" dep fq
    in
    Workflow.shell ~descr:"fq2fa" [
      cmd "fq2fa" ~img [
        option (flag string "--filter") filter ;
        args ;
        dest ;
      ]
    ]

  let idba_ud ?(mem_spec = 10) fa =
    Workflow.shell ~np:4 ~mem:(Workflow.int (mem_spec * 1024)) ~descr:"idba_ud" [
      mkdir_p dest ;
      cmd "idba_ud" ~img [
        opt "--read" dep fa ;
        opt "--num_threads" Fn.id np ;
        opt "--out" Fn.id dest ;
      ]
    ]

  let idba_ud_contigs x = Workflow.select x ["contig.fa"]
  let idba_ud_scaffolds x = Workflow.select x ["scaffold.fa"]
end

module Cisa = struct
  let img = [ docker_image ~account:"pveber" ~name:"cisa" ~tag:"20140304" () ]

  let merge ?(min_length = 100) xs =
    let config_line (label, fa) =
      [
        string "data=" ; dep fa ;
        string ",title=" ; string label ;
      ]
    in
    let config_file = file_dump (
        List.intersperse ~sep:[string "\n"] (
          [ string "count=" ; int (List.length xs) ]
          :: List.map xs ~f:config_line
          @
          [string "Master_file=" ; dest]
          :: [string "min_length=" ; int min_length]
          :: []
        )
        |> List.concat
        |> seq ~sep:""
      )
    in
    Workflow.shell ~descr:"cisa.Merge" [
      mkdir_p tmp ;
      cmd "Merge.py" ~img [ config_file ] ;
    ]

  let cisa ~genome_size contigs =
    let ( := ) var expr = seq ~sep:"" [string var ; string "=" ; expr ] in
    let script = file_dump (
        seq ~sep:"\n" [
          "TMP" := tmp // "output" ;
          "GENOMESIZE" := int genome_size ;
          "CONTIGS" := dep contigs ;
          "DEST" := dest ;
          string {|
NUCMER=`which nucmer`
CISA=$(dirname $(readlink -f $(which CISA.py)))
MAKEBLASTDB=`which makeblastdb`
BLASTN=`which blastn`
CONFIG=$TMP/cisa.config

mkdir -p $TMP
mkdir -p $TMP/CISA1
cd $TMP

cat > $CONFIG <<__HEREDOC__
genome=$GENOMESIZE
infile=$CONTIGS
outfile=$DEST
nucmer=$NUCMER
R2_Gap=0.95
CISA=$CISA
makeblastdb=$MAKEBLASTDB
blastn=$BLASTN
__HEREDOC__

yes | CISA.py $CONFIG
|}
        ]
      )
    in
    Workflow.shell ~descr:"cisa" [
      mkdir_p tmp ;
      cmd "bash" ~img [ script ] ;
    ]
end

module Quast = struct
  let img = [ docker_image ~account:"pveber" ~name:"quast" ~tag:"4.3" () ]

  let quast ?reference ?labels fas =
    Workflow.shell ~descr:"quast" [
      cmd "quast.py" ~img [
        option (opt "-R" dep) reference ;
        option (opt "--labels" (list ~sep:"," string)) labels ;
        opt "--output-dir" (fun x -> seq [x ; string "/results"]) dest ;
        list ~sep:" " dep fas ;
      ]
    ]
end

module Hisat2 = struct
  let img = [ docker_image ~account:"pveber" ~name:"hisat2" ~tag:"2.1.0" () ]

  let hisat2_build ?large_index ?noauto ?packed ?bmax ?bmaxdivn ?dcv ?nodc ?noref ?justref ?offrate ?ftabchars ?seed ?cutoff fa =
    Workflow.shell ~descr:"hisat2-build" ~mem:(Workflow.int (8 * 1024)) ~np:8 [
      mkdir_p dest ;
      cmd "hisat2-build" ~img [
        option (flag string "--large-index") large_index ;
        option (flag string "--no-auto") noauto ;
        option (flag string "--packed") packed ;
        option (flag string "--nodc") nodc ;
        option (flag string "--noref") noref ;
        option (flag string "--justref") justref ;
        option (opt "--bmax" int) bmax ;
        option (opt "--bmaxdivn" int) bmaxdivn ;
        option (opt "--dcv" int) dcv ;
        option (opt "--offrate" int) offrate ;
        option (opt "--ftabchars" int) ftabchars ;
        opt "--threads" Fn.id np ;
        option (opt "--seed" int) seed ;
        option (opt "--cutoff" int) cutoff ;
        opt "-f" dep fa ;
        seq [ dest ; string "/index" ]
      ]
    ]

  let qual_option (type s) x = match (x : s Fastq.format) with
    | Fastq.Solexa  -> "--solexa-quals"
    | Fastq.Sanger -> "--phred33-quals"
    | Fastq. Phred64 -> "--phred64-quals"

  let flag_of_orientation = function
    | `fr -> "--fr"
    | `rf -> "--rf"
    | `ff -> "--ff"

  let hisat2
      ?skip ?qupto ?trim5 ?trim3 ?fastq_format
      ?k
      ?minins ?maxins ?orientation ?no_mixed ?no_discordant
      ?seed
      index
      fqs
    =
    let args = match fqs with
      | `single_end fqs ->
        opt "-U" (list dep ~sep:",") fqs
      | `paired_end (fqs1, fqs2) ->
        seq [
          opt "-1" (list dep ~sep:",") fqs1 ;
          string " " ;
          opt "-2" (list dep ~sep:",") fqs2
        ]
    in
    Workflow.shell ~descr:"hisat2" ~mem:(Workflow.int (4 * 1024)) ~np:8 [
      cmd "hisat2" ~img [
        option (opt "--skip" int) skip ;
        option (opt "--qupto" int) qupto ;
        option (opt "--trim5" int) trim5 ;
        option (opt "--trim3" int) trim3 ;
        option (opt "-k" int) k ;
        option (opt "--minins" int) minins ;
        option (opt "--maxins" int) maxins ;
        option (flag_of_orientation % string) orientation ;
        option (flag string "--no-mixed") no_mixed  ;
        option (flag string "--no-discordant") no_discordant  ;
        opt "--threads" Fn.id np ;
        option (opt "--seed" int) seed ;
        option (opt "-q" (qual_option % string)) fastq_format ;

        opt "-x" (fun index -> seq [dep index ; string "/index"]) index ;
        args ;
        opt "-S" Fn.id dest ;
      ]
    ]
end

module Picardtools = struct
  let img = [ docker_image ~account:"pveber" ~name:"picard-tools" ~tag:"2.8.1" () ]

  let arg k v =
    seq ~sep:"" [ string k ; string "=" ; v ]

  let markduplicates ?remove_duplicates indexed_bam =
    Workflow.shell ~descr:"picard.markduplicates" ~mem:(Workflow.int (3 * 1024)) [
      mkdir_p dest ;
      cmd "PicardCommandLine" ~img [
        string "MarkDuplicates" ;
        arg "INPUT" (dep @@ Samtools.indexed_bam_to_bam indexed_bam) ;
        arg "OUTPUT" (dest // "reads.bam") ;
        arg "METRICS_FILE" (dest // "dup_qc") ;
        string "VALIDATION_STRINGENCY=LENIENT" ;
        string "ASSUME_SORT_ORDER=coordinate" ;
        option (Printf.sprintf "REMOVE_DUPLICATES=%b" % string) remove_duplicates ;
      ]
    ]

  let reads x = Workflow.select x ["reads.bam"]

  let sort_bam_by_name bam =
    Workflow.shell ~descr:"picard.sort_bam_by_name" ~mem:(Workflow.int (1 * 1024)) [
      cmd "PicardCommandLine" ~img [
        string "SortSam" ;
        arg "INPUT" (dep bam) ;
        arg "OUTPUT" dest ;
        arg "SORT_ORDER" (string "queryname") ;
      ]
    ]
end

module Idr = struct
  type 'a format = NarrowPeak | BroadPeak | Bed | Gff

  let narrowPeak = NarrowPeak
  let broadPeak = BroadPeak
  let bed = Bed
  let gff = Gff

  type 'a output = [`idr_output of 'a]

  let string_of_file_format = function
    | NarrowPeak -> "narrowPeak"
    | BroadPeak -> "broadPeak"
    | Bed -> "bed"
    | Gff -> "gff"

  let file_format x = string (string_of_file_format x)

  let string_of_merge_method = function
    | `sum -> "sum"
    | `avg -> "avg"
    | `min -> "min"
    | `max -> "max"

  let merge_method x = string (string_of_merge_method x)

  let token_of_rank r =
    string (
      match r with
      | `signal -> "signal.value"
      | `pvalue -> "p.value"
      | `qvalue -> "q.value"
    )

  let img = [ docker_image ~account:"pveber" ~name:"idr" ~tag:"2.0.3" () ]

  let idr
      ~input_file_type ?idr_threshold ?soft_idr_threshold
      ?peak_merge_method ?rank ?random_seed ?peak_list
      sample1 sample2 =
    Workflow.shell ~descr:"Idr.idr" [
      mkdir_p dest ;
      cmd "idr" ~img [
        opt "--input-file-type" file_format input_file_type ;
        opt "--output-file" (fun x -> x) (dest // "items.tsv") ;
        option (opt "--idr-threshold" float) idr_threshold ;
        option (opt "--soft-idr-threshold" float) soft_idr_threshold ;
        option (opt "--peak-merge-method" merge_method) peak_merge_method ;
        option (opt "--rank" token_of_rank) rank ;
        option (opt "--random-seed" int) random_seed ;
        option (opt "--peak-list" dep) peak_list ;
        string "--plot" ;
        opt "--samples" (list ~sep:" " dep) [ sample1 ; sample2 ] ;
      ]
    ]

  let items x = Workflow.select x [ "items.tsv" ]
  let figure x = Workflow.select x [ "items.tsv.png" ]
end

module Star = struct
  let img = [ docker_image ~account:"flemoine" ~name:"star" () ]

  let mem_in_bytes = seq ~sep:" " [string "$((" ; mem ; string " * 1024 * 1024))$"]

  let genomeGenerate fa =
    Workflow.shell ~descr:"star.index" ~np:8 ~mem:(Workflow.int (30 * 1024)) [
      mkdir_p dest ;
      cmd "STAR" ~img [
        opt "--runThreadN" Fn.id np ;
        opt "--runMode" string "genomeGenerate" ;
        opt "--genomeDir" Fn.id dest ;
        opt "--genomeFastaFiles" dep fa ;
        opt "--limitGenomeGenerateRAM" Fn.id mem_in_bytes ;
      ]
    ]

  let fq_args = function
    | `paired_end (fq1, fq2) ->
      [dep fq1 ; dep fq2]
    | `single_end fq -> [ dep fq ]

  let samStrandField = function
    | `None -> string "None"
    | `intronMotif -> string "intronMotif"


  let sorted_mapped_reads x = Workflow.select x ["sorted.bam"]

  let alignReads ?(max_mem = `GB 8)
      ?outFilterMismatchNmax
      ?outFilterMultimapNmax
      ?outSAMstrandField
      ?alignIntronMax
      idx fqs =
    let `GB max_mem = max_mem in
    Workflow.shell ~descr:"star.map" ~np:8 ~mem:(Workflow.int (max_mem * 1024)) [
      mkdir_p dest ;
      cmd "STAR" ~stdout:(dest // "sorted.bam") ~img [
        opt "--outFileNamePrefix" Fn.id (dest // "star") ;
        opt "--runThreadN" Fn.id np ;
        option (opt "--outSAMstrandField" samStrandField) outSAMstrandField ;
        option (opt "--outFilterMismatchNmax" int) outFilterMismatchNmax ;
        option (opt "--outFilterMultimapNmax" int) outFilterMultimapNmax ;
        opt "--genomeDir" dep idx ;
        opt "--readFilesIn" Fn.id (seq ~sep:" " (fq_args fqs)) ;
        opt "--outSAMunmapped" string "None" ;
        opt "--outStd" string "SAM" ;
        opt "--genomeLoad" string "NoSharedMemory" ;
        option (opt "--alignIntronMax" int) alignIntronMax ;
        (* opt "--limitBAMsortRAM" Fn.id mem_in_bytes ; *)
      ]
    ]
    |> sorted_mapped_reads
end
