open Bistro
open Biotk
open Formats

type t =
  | Fq of fastq file SE_or_PE.t
  | Fq_gz of fastq gz file SE_or_PE.t

val is_single_end : t -> bool

val plain_se : fastq file -> t
val plain_pe : fastq file -> fastq file -> t
val compressed_se : fastq gz file -> t
val compressed_pe : fastq gz file -> fastq gz file -> t

val dep : t -> Shell_dsl.template SE_or_PE.t

val explode : t list ->
  (fastq file list * fastq file list * fastq file list)
  * (fastq gz file list * fastq gz file list * fastq gz file list)

type source =
  | Fastq_url of string SE_or_PE.t
  | Fastq_gz_url of string SE_or_PE.t
  | SRA_dataset of { srr_id : string ;
                     library_type : [`single_end | `paired_end] }

val fastq_of_source : source -> fastq file SE_or_PE.t
val fastq_gz_of_source : source -> fastq gz file SE_or_PE.t
val of_source : source -> t

module type Data = sig
  type t
  val source : t -> source List1.t
end

module Make(Data : Data) : sig
  val fastq : Data.t -> fastq file SE_or_PE.t List1.t
  val fastq_gz : Data.t -> fastq gz file SE_or_PE.t List1.t
  val fastq_samples : Data.t -> t List1.t
  val fastqc : Data.t -> FastQC.report SE_or_PE.t List1.t
end
