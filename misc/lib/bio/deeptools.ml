open Core
open Bistro
open Bistro.Shell_dsl

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
  cmd cmd_name (
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
  Workflow.shell ~descr:"bamcoverage" ~img ~np:threads [
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
  Workflow.shell ~descr:"bamcompare" ~img ~np:threads [
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
  Workflow.shell ~descr:"bigwigcompare" ~img ~np:threads [
    cmd "bigwigCompare" [
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
  cmd cmd_name (
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
  Workflow.shell ~descr:"multibamsummary_bins" ~img ~np:threads [
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
  Workflow.shell ~descr:"multibamsummary_bed" ~img ~np:threads [
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
      opt "--BED" dep bed ;
      opt "--bamfiles"
        (list (fun bam -> dep (Samtools.indexed_bam_to_bam bam)) ~sep:" ")
        indexed_bams ;
      opt "--outFileName" Fn.id dest ;
    ]
  ]

let multibigwigsummary_bed
    ?labels ?chromosomesToSkip ?region ?blacklist ?(threads = 1)
    ?metagene ?transcriptid ?exonid ?transcriptiddesignator
    bed bigwigs =
  let inner =
    Workflow.shell ~descr:"multibigwigsummary_bed" ~img ~np:threads [
      mkdir_p dest ;
      cmd "multiBigwigSummary BED-file" [
        option (opt "--labels" (list string ~sep:" ")) labels ;
        option (opt "--chromosomesToSkip" (list string ~sep:" ")) chromosomesToSkip ;
        option (opt "--region" string) region ;
        option (opt "--blackListFileName" dep) blacklist ;
        opt "--outRawCounts" Fn.id (dest // "summary.tsv") ;
        option (flag string "--metagene") metagene ;
        option (flag string "--transcriptID") transcriptid ;
        option (flag string "--exonID") exonid ;
        option (flag string "--transcript_id_designator") transcriptiddesignator ;
        opt "--BED" dep bed ;
        opt "--bwfiles" (list dep ~sep:" ") bigwigs ;
        opt "--outFileName" Fn.id (dest // "summary.npy.gz") ;
      ]
    ]
  in
  let f x = Workflow.select inner [x] in
  f "summary.npy.gz", f "summary.tsv"

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
  Workflow.shell ~descr:"deeptools.computeMatrix_reference_point" ~img ~np:numberOfProcessors [
    cmd "computeMatrix" [
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
  Workflow.shell ~descr:"deeptools.plotHeatmap" ~img [
    cmd "plotHeatmap" [
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
  Workflow.shell ~descr:"deeptools.plotCorrelation" ~img [
    cmd "plotCorrelation" [
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
  Workflow.shell ~descr:"deeptools.plotProfile" ~img [
    cmd "plotProfile" [
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
  Workflow.shell ~np:numberOfProcessors ~img ~descr:"deeptools.plotEnrichment" [
    cmd "plotEnrichment" [
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

let plotFingerprint
    ?extendReads ?ignoreDuplicates ?minMappingQuality ?centerReads
    ?samFlagInclude ?samFlagExclude ?minFragmentLength ?maxFragmentLength
    ?labels ?binSize ?numberOfSamples ?plotTitle ?skipZeros ?region
    ?blackList ?(numberOfProcessors = 1)
    output_format bams
  =
  Workflow.shell ~descr:"deeptools.plotFingerprint" ~img ~np:numberOfProcessors [
    cmd "plotFingerprint" [
      option (flag string "--extendReads") extendReads ;
      option (flag string "--ignoreDuplicates") ignoreDuplicates ;
      option (opt "--minMappingQuality" int) minMappingQuality ;
      option (flag string "--centerReads") centerReads ;
      option (opt "--samFlagInclude" int) samFlagInclude ;
      option (opt "--samFlagExclude" int) samFlagExclude ;
      option (opt "--minFragmentLength" int) minFragmentLength ;
      option (opt "--maxFragmentLength" int) maxFragmentLength ;
      option (opt "--blackListFileName" dep) blackList ;
      opt "--numberOfProcessors" Fn.id np ;
      opt "--bamfiles" (list ~sep:" " (fun x -> dep (Samtools.indexed_bam_to_bam x))) bams ;
      opt "--plotFile" Fn.id dest ;
      opt "--plotFileFormat" string (ext_of_format output_format) ;
      option (opt "--labels" (list ~sep:" " (string % quote ~using:'"'))) labels ;
      option (opt "--plotTitle" (string % quote ~using:'"')) plotTitle ;
      option (flag string "--skipZeros") skipZeros ;
      option (opt "--region" string) region ;
      opt "--numberOfProcessors" Fn.id np ;
      option (opt "--binSize" int) binSize ;
      option (opt "--numberOfSamples" int) numberOfSamples ;
    ]
  ]
