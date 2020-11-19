open Bistro

type gsize = [`hs | `mm | `ce | `dm | `gsize of int]
type keep_dup = [ `all | `auto | `int of int ]

type _ format

val sam : sam format
val bam : bam format

val run :
  ?control: 'a file list ->
  ?petdist:int ->
  ?gsize:gsize ->
  ?tsize:int ->
  ?bw:int ->
  ?pvalue:float ->
  ?mfold:int * int ->
  ?nolambda:bool ->
  ?slocal:int ->
  ?llocal:int ->
  ?on_auto:bool ->
  ?nomodel:bool ->
  ?shiftsize:int ->
  ?keep_dup:keep_dup ->
  ?to_large:bool ->
  ?wig:bool ->
  ?bdg:bool ->
  ?single_profile:bool ->
  ?space:int ->
  ?call_subpeaks:bool ->
  ?diag:bool ->
  ?fe_min:int ->
  ?fe_max:int ->
  ?fe_step:int ->
  'a format ->
  'a file list ->
  [`macs2] directory

class type peaks_xls = object
  inherit bed3
  method f4 : int
  method f5 : int
  method f6 : int
  method f7 : float
  method f8 : float
  method f9 : float
end

val peaks_xls : [`macs2] directory -> peaks_xls file

class type narrow_peaks = object
  inherit bed5
  method f6 : string
  method f7 : float
  method f8 : float
  method f9 : float
  method f10 : int
end

val narrow_peaks :
  [`macs2] directory -> narrow_peaks file

class type peak_summits = object
  inherit bed4
  method f5 : float
end

val peak_summits :
  [`macs2] directory -> peak_summits file
