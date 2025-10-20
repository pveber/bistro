open Bistro
open Formats

val pileup :
  ?extsize:int ->
  ?both_direction:bool ->
  bam file -> Ucsc_gb.bedGraph file

type gsize = [`hs | `mm | `ce | `dm | `gsize of int]
type keep_dup = [ `all | `auto | `int of int ]

type _ format

val sam : sam format
val bam : bam format
val bampe : bam format

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
  ?control:'a file list ->
  ?keep_dup:keep_dup ->
  'a format ->
  'a file list ->
  [`macs2_narrow] directory

class type peaks_xls = object
  inherit bed3
  method f4 : int
  method f5 : int
  method f6 : int
  method f7 : float
  method f8 : float
  method f9 : float
end

val peaks_xls : [< `macs2_narrow | `macs2_broad] directory -> peaks_xls file

class type narrow_peaks = object
  inherit bed5
  method f6 : string
  method f7 : float
  method f8 : float
  method f9 : float
  method f10 : int
end

val narrow_peaks : [`macs2_narrow] directory -> narrow_peaks file

class type peak_summits = object
  inherit bed4
  method f5 : float
end

val peak_summits : [< `macs2_narrow | `macs2_broad] directory -> peak_summits file

val callpeak_broad :
  ?pvalue:float ->
  ?qvalue:float ->
  ?gsize:gsize ->
  ?call_summits:bool ->
  ?fix_bimodal:bool ->
  ?mfold:int * int ->
  ?extsize:int ->
  ?nomodel:bool ->
  ?bdg:bool ->
  ?control:'a file list ->
  ?keep_dup:keep_dup ->
  'a format ->
  'a file list ->
  [`macs2_broad] directory

class type broad_peaks = object
  inherit bed5
  method f6 : string
  method f7 : float
  method f8 : float
  method f9 : float
end

val broad_peaks : [`macs2_broad] directory -> broad_peaks file
