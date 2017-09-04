open Bistro.Std
open Defs

type 'a output

val bigwig : Ucsc_gb.bigWig output
val bedgraph : Ucsc_gb.bedGraph output

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
  'a output ->
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
  'a output ->
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
  'a output ->
  [ `bigwig ] workflow ->
  [ `bigwig ] workflow ->
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
