open Bistro
open Formats

class type index = object
  inherit binary_file
  method format : [`kallisto_index]
end

class type abundance_table = object
  inherit tsv
  method f1 : [`target_id] * string
  method f2 : [`length] * int
  method f3 : [`eff_length] * int
  method f4 : [`est_counts] * float
  method f5 : [`tpm] * float
end

val img : container_image list
val index : fasta file list -> index file
val quant :
  ?bias:bool ->
  ?bootstrap_samples:int ->
  ?fr_stranded:bool ->
  ?rf_stranded:bool ->
  ?threads:int ->
  ?fragment_length:float ->
  ?sd:float ->
  index file ->
  fq1:[`fq of fastq file | `fq_gz of fastq gz file] ->
  ?fq2:[`fq of fastq file | `fq_gz of fastq gz file] ->
  unit ->
  [`kallisto] directory

val abundance : [`kallisto] directory -> abundance_table file

val merge_eff_counts :
  sample_ids:string list ->
  kallisto_outputs:abundance_table file list ->
  tsv file

val merge_tpms :
  sample_ids:string list ->
  kallisto_outputs:abundance_table file list ->
  tsv file
