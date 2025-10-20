open Bistro
open Formats

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

val multibigwigsummary_bed :
  ?labels:string list ->
  ?chromosomesToSkip:string list ->
  ?region:string ->
  ?blacklist:#bed3 file ->
  ?threads:int ->
  ?metagene:bool ->
  ?transcriptid:bool ->
  ?exonid:bool ->
  ?transcriptiddesignator:bool->
  #bed3 file ->
  Ucsc_gb.bigWig file list ->
  compressed_numpy_array file * tsv file

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
  ?plotHeight:float ->
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
(** [plotHeight] and [plotWidth] are given in cm *)

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

val plotFingerprint :
  ?extendReads:bool ->
  ?ignoreDuplicates:bool ->
  ?minMappingQuality:int ->
  ?centerReads:bool ->
  ?samFlagInclude:int ->
  ?samFlagExclude:int ->
  ?minFragmentLength:int ->
  ?maxFragmentLength:int ->
  ?labels:string list ->
  ?binSize:int ->
  ?numberOfSamples:int ->
  ?plotTitle:string ->
  ?skipZeros:bool ->
  ?region:string ->
  ?blackList:#bed3 file ->
  ?numberOfProcessors:int ->
  'a img_format ->
  [`indexed_bam] directory list ->
  'a file
