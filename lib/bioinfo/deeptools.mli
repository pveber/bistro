open Bistro.Std
open Defs

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
  ?blacklistfilename: [ `bed | `gtf ] workflow ->
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
  ?scalefactor:string ->
  ?ratio: [ `log2 | `ratio | `subtract | `add | `mean | `reciprocal_ratio | `first | `second ] ->
  ?pseudocount:int ->
  ?binsize:int ->
  ?region:string ->
  ?blacklistfilename: [ `bed | `gtf ] workflow ->
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
  ?scalefactor:string ->
  ?ratio: [ `log2 | `ratio | `subtract | `add | `mean | `reciprocal_ratio | `first | `second ] ->
  ?pseudocount:int ->
  ?binsize:int ->
  ?region:string ->
  ?blacklistfilename: [ `bed | `gtf ] workflow ->
  ?threads:int ->
  'a signal_format ->
  Ucsc_gb.bigWig workflow ->
  Ucsc_gb.bigWig workflow ->
  'a workflow

class type compressed_numpy_array = object
  inherit binary_file
  method format : [`compressed_numpy_array]
end

val multibamsummary_bins :
  ?binsize:int ->
  ?distancebetweenbins:int ->
  ?region:string ->
  ?blacklistfilename: [ `bed | `gtf ] workflow ->
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
  ?blacklistfilename: [ `bed | `gtf ] workflow ->
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
  scores:Ucsc_gb.bigWig workflow list ->
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
