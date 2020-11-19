open Bistro
open Formats

type 'a format

val narrowPeak : Macs2.narrow_peaks format
val broadPeak : Macs2.broad_peaks format
val bed : bed3 format
val gff : gff format

type 'a output = [`idr_output of 'a]

val idr :
  input_file_type:'a format ->
  ?idr_threshold:float ->
  ?soft_idr_threshold:float ->
  ?peak_merge_method:[ `sum | `avg | `min | `max] ->
  ?rank:[ `signal | `pvalue | `qvalue ] ->
  ?random_seed:int ->
  ?peak_list:'a file ->
  'a file ->
  'a file ->
  'a output directory

val items : 'a output directory -> 'a file
val figure : _ output directory -> png file
