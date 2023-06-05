open Bistro
open Formats

val img : container_image list

type 'a input

val bed : #bed3 input
val gff : gff input


module Cmd : sig
  val slop :
    ?strand:bool ->
    ?header:bool ->
    mode:[
      | `both of int
      | `left of int
      | `right of int
      | `both_pct of float
      | `left_pct of float
      | `right_pct of float
    ] ->
    'a file ->
    Ucsc_gb.chrom_sizes file ->
    Bistro.Shell_dsl.command
end


val slop :
  ?strand:bool ->
  ?header:bool ->
  mode:[
    | `both of int
    | `left of int
    | `right of int
    | `both_pct of float
    | `left_pct of float
    | `right_pct of float
  ] ->
  'a input ->
  'a file ->
  Ucsc_gb.chrom_sizes file ->
  'a file


val intersect :
  ?ubam:bool ->
  ?wa:bool ->
  ?wb:bool ->
  ?loj:bool ->
  ?wo:bool ->
  ?wao:bool ->
  ?u:bool ->
  ?c:bool ->
  ?v:bool ->
  ?f:float ->
  ?_F:float ->
  ?r:bool ->
  ?e:bool ->
  ?s:bool ->
  ?_S:bool ->
  ?split:bool ->
  ?sorted:bool ->
  ?g:Ucsc_gb.chrom_sizes file ->
  ?header:bool ->
  ?filenames:bool ->
  ?sortout:bool ->
  'a input ->
  'a file ->
  #bed3 file list ->
  'a file

val bamtobed :
  ?bed12:bool ->
  ?split:bool ->
  ?splitD:bool ->
  ?ed:bool ->
  ?tag:bool ->
  ?cigar:bool ->
  bam file ->
  #bed6 file

val closest :
  ?strand:[`same | `opposite] ->
  ?io:bool ->
  ?iu:bool ->
  ?id:bool ->
  ?fu:bool ->
  ?fd:bool ->
  ?ties:[`all | `first | `last] ->
  ?mdb:[`each | `all] ->
  ?k:int ->
  ?header:bool ->
  'a input ->
  'a file ->
  #bed3 file list ->
  'a file

val merge :
  ?s:bool ->
  ?_S:[ `minus | `plus ] ->
  ?d:int ->
  ?c:int list ->
  ?o:[ `absmin
     | `collapse
     | `count
     | `count_distinct
     | `distinct
     | `max
     | `mean
     | `median
     | `min
     | `sum ] list ->
  #bed3 file list ->
  #bed3 file

val unionbedg :
  ?header:bool ->
  ?names:string list ->
  ?empty:bool ->
  ?genome_chrom_size:text file ->
  ?filler:string ->
  Ucsc_gb.bedGraph file list ->
  #bed3 file
