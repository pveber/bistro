open Core_kernel.Std
open Bistro.Std
open Bistro.EDSL

let env = docker_image ~account:"pveber" ~name:"deeptools" ~tag:"2.4.1" ()

type 'a output = [ `bigWig | `bedGraph ]

class type compressed_numpy_array = object
  inherit binary_file
  method format : [`compressed_numpy_array]
end

let bigwig = `bigWig
let bedgraph = `bedGraph

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


(*Transform a number from French format into an US format.*)
let normalizeto1x_expr nbr =
  let nbr_s = string_of_int nbr in
  let lgth = String.length nbr_s in
  let rec compute acc idx nbr_s = match idx with
    | 0 -> string acc
    | l ->
      if l <= 3 then
        string (String.sub nbr_s 0 l ^ acc)
      else
        let acc = "," ^ String.sub nbr_s (l - 3) 3 ^ acc in
        compute acc (l-3) nbr_s
  in
  compute "" lgth nbr_s


let bam_gen_cmd ?outfileformat ?scalefactor ?blacklistfilename ?normalizeto1x
    ?centerreads ?normalizeusingrpkm ?ignorefornormalization ?skipnoncoveredregions
    ?smoothlength ?extendreads ?ignoreduplicates ?minmappingquality
    ?samflaginclude ?samflagexclude ?minfragmentlength ?maxfragmentlength
    cmd_name other_args =
  cmd cmd_name ~env (
    List.append [
      option (opt "--outFileFormat" file_format_expr) outfileformat ;
      option (opt "--scaleFactor" float) scalefactor ;
      option (opt "--blackListFileName" string) blacklistfilename ;
      option (opt "--normalizeTo1x" normalizeto1x_expr) normalizeto1x ;
      option (flag string "--normalizeUsingRPKM") normalizeusingrpkm ;
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

let ( /// ) x (Bistro.Selector s) = x // Bistro.Path.to_string s

let bamcoverage ?scalefactor ?filterrnastrand ?binsize ?blacklistfilename
    ?(threads = 1) ?normalizeto1x ?normalizeusingrpkm ?ignorefornormalization
    ?skipnoncoveredregions ?smoothlength ?extendreads ?ignoreduplicates
    ?minmappingquality ?centerreads ?samflaginclude ?samflagexclude
    ?minfragmentlength ?maxfragmentlength outfileformat indexed_bam =
  workflow ~descr:"bamcoverage" ~np:threads ~mem:(3 * 1024) [
    bam_gen_cmd "bamCoverage" [
      option (opt "--filterRNAstrand" filterRNAstrand_expr) filterrnastrand ;
      option (opt "--binSize" int) binsize ;
      opt "--numberOfProcessors" ident np ;
      opt "--bam" ident (dep indexed_bam /// Samtools.indexed_bam_to_bam) ;
      opt "--outFileName" ident dest ;
      opt "--outFileFormat" file_format_expr outfileformat ;
    ]
  ]

let bamcompare ?scalefactormethod ?samplelength ?numberofsamples
    ?scalefactor ?ratio ?pseudocount ?binsize ?region ?blacklistfilename ?(threads = 1)
    ?normalizeto1x ?normalizeusingrpkm ?ignorefornormalization ?skipnoncoveredregions
    ?smoothlength ?extendreads ?ignoreduplicates ?minmappingquality
    ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
    ?maxfragmentlength outfileformat indexed_bam1 indexed_bam2 =
  workflow ~descr:"bamcompare" ~np:threads ~mem:(3 * 1024) [
    bam_gen_cmd "bamCompare" [
      option (opt "--scaleFactorMethod" scalefactormethod_expr) scalefactormethod ;
      option (opt "--sampleLength" int) samplelength ;
      option (opt "--numberOfSamples" int) numberofsamples ;
      option (opt "--ratio" ratio_expr) ratio ;
      option (opt "--pseudocount" int) pseudocount ;
      option (opt "--binSize" int) binsize ;
      option (opt "--region" string) region ;
      opt "--numberOfProcessors" ident np ;
      opt "--bamfile1" ident (dep indexed_bam1 /// Samtools.indexed_bam_to_bam) ;
      opt "--bamfile2" ident (dep indexed_bam2 /// Samtools.indexed_bam_to_bam) ;
      opt "--outFileName" ident dest ;
      opt "--outFileFormat" file_format_expr outfileformat ;
    ]
  ]


let bigwigcompare ?scalefactor ?ratio ?pseudocount ?binsize
    ?region ?blacklistfilename ?(threads = 1)
    outfileformat bigwig1 bigwig2 =
  workflow ~descr:"bigwigcompare" ~np:threads ~mem:(3 * 1024) [
    cmd "bigwigCompare" ~env [
      option (opt "--scaleFactor" string) scalefactor ;
      option (opt "--ratio" ratio_expr) ratio ;
      option (opt "--pseudocount" int) pseudocount ;
      option (opt "--binSize" int) binsize ;
      option (opt "--region" string) region ;
      option (opt "--blackListFileName" dep) blacklistfilename ;
      opt "--numberOfProcessors" ident np ;
      opt "--bigwig1" dep bigwig1 ;
      opt "--bigwig2" dep bigwig2 ;
      opt "--outFileName" ident dest ;
      opt "--outFileFormat" file_format_expr outfileformat ;
    ]
  ]


let multibamsum_gen_cmd ?(threads = 1) ?outrawcounts ?extendreads ?ignoreduplicates
    ?minmappingquality ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
    ?maxfragmentlength ?blacklistfilename ?region cmd_name other_args =
  cmd cmd_name ~env (
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
      option (opt "--blackListFileName" dep) blacklistfilename ;
    ]
      other_args
  )


let multibamsummary_bins ?binsize ?distancebetweenbins ?region ?blacklistfilename
    ?(threads = 1) ?outrawcounts ?extendreads ?ignoreduplicates ?minmappingquality
    ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
    ?maxfragmentlength indexed_bams =
  workflow ~descr:"multibamsummary_bins" ~np:threads ~mem:(3 * 1024) [
    multibamsum_gen_cmd "multiBamSummary bins" [
      option (opt "--binSize" int) binsize ;
      option (opt "--distanceBetweenBins" int) distancebetweenbins ;
      opt "--numberOfProcessors" ident np ;
      opt "--bamfiles" (list (fun bam -> dep bam /// Samtools.indexed_bam_to_bam) ~sep:" ") indexed_bams ;
      opt "--outFileName" ident dest ;
    ]
  ]


let multibamsummary_bed ?region ?blacklistfilename ?(threads = 1)
    ?outrawcounts ?extendreads ?ignoreduplicates ?minmappingquality
    ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
    ?maxfragmentlength ?metagene ?transcriptid ?exonid ?transcriptiddesignator bed
    indexed_bams =
  workflow ~descr:"multibamsummary_bed" ~np:threads ~mem:(3 * 1024) [
    multibamsum_gen_cmd "multiBamSummary BED-file" [
      option (flag string "--metagene") metagene ;
      option (flag string "--transcriptID") transcriptid ;
      option (flag string "--exonID") exonid ;
      option (flag string "--transcript_id_designator") transcriptiddesignator ;
      opt "--numberOfProcessors" ident np ;
      string "--BED" ; (dep bed) ;
      opt "--bamfiles" (list (fun bam -> dep bam /// Samtools.indexed_bam_to_bam) ~sep:" ") indexed_bams ;
      opt "--outFileName" ident dest ;
    ]
  ]
