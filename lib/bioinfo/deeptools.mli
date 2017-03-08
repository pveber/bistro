open Bistro.Std
open Bistro_bioinfo


type 'a output

val bigwig : Ucsc_gb.bigWig output
val bedgraph : Ucsc_gb.bedGraph output

val bamcoverage :
  ?outfileformat: 'a output ->
  ?scalefactor:float ->
  ?filterrnastrand: [ `forward | `reverse ] ->
  ?binsize:int ->
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
  [ `indexed_bam ] directory workflow ->
  'a workflow


val bamcompare :
  ?outfileformat: 'a output ->
  ?scalefactormethod : [ `readcount | `ses ] ->
  ?samplelength:int ->
  ?numberofsamples:int ->
  ?scalefactor:string ->
  ?ratio: [ `log2 | `ratio | `subtract | `add | `mean | `reciprocal_ratio | `first | `second ] ->
  ?pseudocount:int ->
  ?binsize:int ->
  ?region:string ->
  ?blacklistfilename:string ->
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
  [ `indexed_bam ] directory workflow ->
  [ `indexed_bam ] directory workflow ->
  'a workflow


val bigwigcompare :
  ?outfileformat: 'a output ->
  ?scalefactor:string ->
  ?ratio: [ `log2 | `ratio | `subtract | `add | `mean | `reciprocal_ratio | `first | `second ] ->
  ?pseudocount:int ->
  ?binsize:int ->
  ?region:string ->
  ?blacklistfilename: [ `bed | `gtf ] workflow ->
  ?threads:int ->
  [ `bigwig ] directory workflow ->
  [ `bigwig ] directory workflow ->
  'a workflow


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
  [ `indexed_bam ] directory workflow list ->
  'a workflow


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
  [ `bed ] workflow ->
  [ `indexed_bam ] directory workflow list ->
  'a workflow
