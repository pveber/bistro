open Core_kernel.Std
open Bistro.Std
open Bistro.EDSL

let env = docker_image ~account:"pveber" ~name:"deeptools" ~tag:"2.4.1" ()

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

(*Transform a number from French format into an US format.*)
let normalizeto1x_expr nbr =
  let nbr_s = string_of_int nbr in
  let lgth = String.length nbr_s in
  let rec compute acc idx nbr_s = match idx with
    | 0 -> string acc
    | l ->
      if l <= 3 then
        string (String.sub nbr_s ~pos:0 ~len:l ^ acc)
      else
        let acc = "," ^ String.sub nbr_s ~pos:(l - 3) ~len:3 ^ acc in
        compute acc (l-3) nbr_s
  in
  compute "" lgth nbr_s


let bam_gen_cmd ?outfileformat ?scalefactor ?blacklist ?normalizeto1x
    ?centerreads ?normalizeusingrpkm ?ignorefornormalization ?skipnoncoveredregions
    ?smoothlength ?extendreads ?ignoreduplicates ?minmappingquality
    ?samflaginclude ?samflagexclude ?minfragmentlength ?maxfragmentlength
    cmd_name other_args =
  cmd cmd_name ~env (
    List.append [
      option (opt "--outFileFormat" file_format_expr) outfileformat ;
      option (opt "--scaleFactor" float) scalefactor ;
      option (opt "--blackListFileName" dep) blacklist ;
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

let bamcoverage ?scalefactor ?filterrnastrand ?binsize ?blacklist
    ?(threads = 1) ?normalizeto1x ?normalizeusingrpkm ?ignorefornormalization
    ?skipnoncoveredregions ?smoothlength ?extendreads ?ignoreduplicates
    ?minmappingquality ?centerreads ?samflaginclude ?samflagexclude
    ?minfragmentlength ?maxfragmentlength outfileformat indexed_bam =
  workflow ~descr:"bamcoverage" ~np:threads [
    bam_gen_cmd "bamCoverage" ?scalefactor ?blacklist
    ?normalizeto1x ?normalizeusingrpkm ?ignorefornormalization
    ?skipnoncoveredregions ?smoothlength ?extendreads ?ignoreduplicates
    ?minmappingquality ?centerreads ?samflaginclude ?samflagexclude
    ?minfragmentlength ?maxfragmentlength [
      option (opt "--filterRNAstrand" filterRNAstrand_expr) filterrnastrand ;
      option (opt "--binSize" int) binsize ;
      opt "--numberOfProcessors" ident np ;
      opt "--bam" ident (dep indexed_bam /// Samtools.indexed_bam_to_bam) ;
      opt "--outFileName" ident dest ;
      opt "--outFileFormat" file_format_expr outfileformat ;
    ]
  ]

let bamcompare ?scalefactormethod ?samplelength ?numberofsamples
    ?scalefactor ?ratio ?pseudocount ?binsize ?region ?blacklist ?(threads = 1)
    ?normalizeto1x ?normalizeusingrpkm ?ignorefornormalization ?skipnoncoveredregions
    ?smoothlength ?extendreads ?ignoreduplicates ?minmappingquality
    ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
    ?maxfragmentlength outfileformat indexed_bam1 indexed_bam2 =
  workflow ~descr:"bamcompare" ~np:threads [
    bam_gen_cmd "bamCompare"
      ?scalefactor ?blacklist
      ?normalizeto1x ?normalizeusingrpkm ?ignorefornormalization ?skipnoncoveredregions
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
      opt "--numberOfProcessors" ident np ;
      opt "--bamfile1" ident (dep indexed_bam1 /// Samtools.indexed_bam_to_bam) ;
      opt "--bamfile2" ident (dep indexed_bam2 /// Samtools.indexed_bam_to_bam) ;
      opt "--outFileName" ident dest ;
      opt "--outFileFormat" file_format_expr outfileformat ;
    ]
  ]


let bigwigcompare ?scalefactor ?ratio ?pseudocount ?binsize
    ?region ?blacklist ?(threads = 1)
    outfileformat bigwig1 bigwig2 =
  workflow ~descr:"bigwigcompare" ~np:threads [
    cmd "bigwigCompare" ~env [
      option (opt "--scaleFactor" float) scalefactor ;
      option (opt "--ratio" ratio_expr) ratio ;
      option (opt "--pseudocount" int) pseudocount ;
      option (opt "--binSize" int) binsize ;
      option (opt "--region" string) region ;
      option (opt "--blackListFileName" dep) blacklist ;
      opt "--numberOfProcessors" ident np ;
      opt "--bigwig1" dep bigwig1 ;
      opt "--bigwig2" dep bigwig2 ;
      opt "--outFileName" ident dest ;
      opt "--outFileFormat" file_format_expr outfileformat ;
    ]
  ]


let multibamsum_gen_cmd ?outrawcounts ?extendreads ?ignoreduplicates
    ?minmappingquality ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
    ?maxfragmentlength ?blacklist ?region cmd_name other_args =
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
      option (opt "--blackListFileName" dep) blacklist ;
    ]
      other_args
  )


let multibamsummary_bins ?binsize ?distancebetweenbins ?region ?blacklist
    ?(threads = 1) ?outrawcounts ?extendreads ?ignoreduplicates ?minmappingquality
    ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
    ?maxfragmentlength indexed_bams =
  workflow ~descr:"multibamsummary_bins" ~np:threads [
    multibamsum_gen_cmd "multiBamSummary bins"
      ?region ?blacklist
      ?outrawcounts ?extendreads ?ignoreduplicates ?minmappingquality
      ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
      ?maxfragmentlength [
      option (opt "--binSize" int) binsize ;
      option (opt "--distanceBetweenBins" int) distancebetweenbins ;
      opt "--numberOfProcessors" ident np ;
      opt "--bamfiles" (list (fun bam -> dep bam /// Samtools.indexed_bam_to_bam) ~sep:" ") indexed_bams ;
      opt "--outFileName" ident dest ;
    ]
  ]


let multibamsummary_bed ?region ?blacklist ?(threads = 1)
    ?outrawcounts ?extendreads ?ignoreduplicates ?minmappingquality
    ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
    ?maxfragmentlength ?metagene ?transcriptid ?exonid ?transcriptiddesignator bed
    indexed_bams =
  workflow ~descr:"multibamsummary_bed" ~np:threads [
    multibamsum_gen_cmd "multiBamSummary BED-file"
      ?region ?blacklist
      ?outrawcounts ?extendreads ?ignoreduplicates ?minmappingquality
      ?centerreads ?samflaginclude ?samflagexclude ?minfragmentlength
      ?maxfragmentlength [
      option (flag string "--metagene") metagene ;
      option (flag string "--transcriptID") transcriptid ;
      option (flag string "--exonID") exonid ;
      option (flag string "--transcript_id_designator") transcriptiddesignator ;
      opt "--numberOfProcessors" ident np ;
      string "--BED" ;
      (dep bed) ;
      opt "--bamfiles"
        (list (fun bam -> dep bam /// Samtools.indexed_bam_to_bam) ~sep:" ")
        indexed_bams ;
      opt "--outFileName" ident dest ;
    ]
  ]

let reference_point_enum =
  Fn.compose string (function
      | `TES -> "TES"
      | `TSS -> "TSS"
      | `center -> "center"
    )

let sort_regions_enum x =
  Fn.compose string (function
      | `no -> "no"
      | `ascend -> "ascend"
      | `descend -> "descend"
      | `keep -> "keep"
    ) x

let sort_using_enum =
  Fn.compose string (function
      | `max -> "max"
      | `min -> "min"
      | `mean -> "mean"
      | `median -> "median"
      | `region_length -> "region_length"
      | `sum -> "sum"
    )

let average_type_bins_enum =
  Fn.compose string (function
      | `max -> "max"
      | `min -> "min"
      | `mean -> "mean"
      | `median -> "median"
      | `std -> "std"
      | `sum -> "sum"
    )

let what_to_show_enum =
  Fn.compose string (function
      | `plot_heatmap_and_colorbar -> "plot, heatmap and colorbar"
      | `plot_and_heatmap -> "plot and heatmap"
      | `heatmap_only -> "heatmap only"
      | `heatmap_and_colorbar -> "heatmap and colorbar"
    )


let legend_location_enum =
  Fn.compose string (function
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
    )



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
  workflow ~descr:"deeptools.computeMatrix_reference_point" ~np:numberOfProcessors [
    cmd "computeMatrix" ~env [
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
  workflow ~descr:"deeptools.plotHeatmap" [
    cmd "plotHeatmap" ~env [
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

let corMethod_enum =
  Fn.compose string (function
      | `spearman -> "spearman"
      | `pearson -> "pearson"
    )

let whatToPlot_enum =
  Fn.compose string (function
      | `heatmap -> "heatmap"
      | `scatterplot -> "scatterplot"
    )

let plotCorrelation
    ?skipZeros ?labels ?plotTitle ?removeOutliers
    ?colorMap ?plotNumbers ?log1p
    ~corMethod ~whatToPlot output_format corData
  =
  workflow  ~descr:"deeptools.plotCorrelation" [
    cmd "plotCorrelation" ~env [
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
