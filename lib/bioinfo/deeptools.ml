open Core_kernel.Std
open Bistro.EDSL

let env = docker_image ~account:"pveber" ~name:"deeptools" ~tag:"2.4.1" ()

type 'a output = [ `bigWig | `bedGraph ]

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

let string_rev s =
  let lgth = String.length s in
  String.init lgth (fun c -> s.[lgth -1 -c])

let normalizeto1x_expr nbr =
  let nbr_s = string_rev (string_of_int nbr) in
  let lgth = String.length nbr_s in
  let rec compute acc idx nbr_s lgth = match idx with
    | e when (idx = lgth) -> string (string_rev acc)
    | l ->
      if lgth - l <= 3 then
        let acc = acc ^ String.sub nbr_s l (lgth-l) in
        compute acc lgth nbr_s lgth
      else
        let acc = acc ^ String.sub nbr_s l 3 ^ "," in
        compute acc (l+3) nbr_s lgth
  in
  compute "" 0 nbr_s lgth


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


let bamcoverage ?outfileformat ?scalefactor ?filterrnastrand ?binsize ?blacklistfilename
    ?(threads = 1) ?normalizeto1x ?normalizeusingrpkm ?ignorefornormalization
    ?skipnoncoveredregions ?smoothlength ?extendreads ?ignoreduplicates
    ?minmappingquality ?centerreads ?samflaginclude ?samflagexclude
    ?minfragmentlength ?maxfragmentlength bam =
  workflow ~descr:"bamcoverage" ~np:threads ~mem:(3 * 1024) [
    bam_gen_cmd "bamCoverage" [
      option (opt "--filterRNAstrand" filterRNAstrand_expr) filterrnastrand ;
      option (opt "--binSize" int) binsize ;
      opt "--numberOfProcessors" ident np ;
      string "--bam" ; (dep bam) // "reads.bam"  ;
      opt "--outFileName" ident dest ;
    ]
  ]

let bamcompare ?outfileformat ?scalefactormethod ?samplelength ?numberofsamples
    ?scalefactor ?ratio ?pseudocount ?binsize ?region ?blacklistfilename ?(threads = 1)
    ?normalizeto1x ?normalizeusingrpkm ?ignorefornormalization ?skipnoncoveredregions
    ?smoothlength ?extendreads ?ignoreduplicates ?minmappingquality
    ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
    ?maxfragmentlength bam1 bam2 =
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
      string "--bamfile1" ; (dep bam1) // "reads.bam"  ;
      string "--bamfile2" ; (dep bam2) // "reads.bam"  ;
      opt "--outFileName" ident dest ;
    ]
  ]


let bigwigcompare ?outfileformat ?scalefactor ?ratio ?pseudocount ?binsize
    ?region ?blacklistfilename ?(threads = 1)
     bigwig1 bigwig2 =
  workflow ~descr:"bigwigcompare" ~np:threads ~mem:(3 * 1024) [
    cmd "bigwigCompare" ~env [
      option (opt "--outFileFormat" file_format_expr) outfileformat ;
      option (opt "--scaleFactor" string) scalefactor ;
      option (opt "--ratio" ratio_expr) ratio ;
      option (opt "--pseudocount" int) pseudocount ;
      option (opt "--binSize" int) binsize ;
      option (opt "--region" string) region ;
      option (opt "--blackListFileName" dep) blacklistfilename ;
      opt "--numberOfProcessors" ident np ;
      string "--bigwig1" ; (dep bigwig1) // "reads.bw"  ;
      string "--bigwig2" ; (dep bigwig2) // "reads.bw"  ;
      opt "--outFileName" ident dest ;
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
    ?maxfragmentlength bam =
  workflow ~descr:"multibamsummary_bins" ~np:threads ~mem:(3 * 1024) [
    multibamsum_gen_cmd "multiBamSummary bins" [
      option (opt "--binSize" int) binsize ;
      option (opt "--distanceBetweenBins" int) distancebetweenbins ;
      opt "--numberOfProcessors" ident np ;
      opt "--bamfiles" (list dep ~sep:" ") bam ;
      opt "--outFileName" ident dest ;
    ]
  ]


let multibamsummary_bed ?region ?blacklistfilename ?(threads = 1)
    ?outrawcounts ?extendreads ?ignoreduplicates ?minmappingquality
    ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
    ?maxfragmentlength ?metagene ?transcriptid ?exonid ?transcriptiddesignator bed bam =
  workflow ~descr:"multibamsummary_bed" ~np:threads ~mem:(3 * 1024) [
    multibamsum_gen_cmd "multiBamSummary BED-file" [
      option (flag string "--metagene") metagene ;
      option (flag string "--transcriptID") transcriptid ;
      option (flag string "--exonID") exonid ;
      option (flag string "--transcript_id_designator") transcriptiddesignator ;
      opt "--numberOfProcessors" ident np ;
      string "--BED" ; (dep bed) ;
      opt "--bamfiles" (list dep ~sep:" ") bam ;
      opt "--outFileName" ident dest ;
    ]
  ]
