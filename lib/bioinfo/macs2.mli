open Bistro.Std
open Defs

val pileup :
  ?extsize:int ->
  ?both_direction:bool ->
  bam workflow -> Ucsc_gb.bedGraph workflow

type gsize = [`hs | `mm | `ce | `dm | `gsize of int]

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
  ([`macs2_callpeak_output], peaks_xls) selector

class type narrow_peaks = object
  inherit bed5
  method f6 : string
  method f7 : float
  method f8 : float
  method f9 : float
  method f10 : int
end

val narrow_peaks :
  ([`macs2_callpeak_output], narrow_peaks) selector

class type peak_summits = object
  inherit bed4
  method f5 : float
end

val peak_summits :
  ([`macs2_callpeak_output], peak_summits) selector
