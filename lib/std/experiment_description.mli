open Bistro.Stdtype t = statement list
and statement =
| Factor of factor
| sam le of sample
| Model of model
| Project of string
and factor = {
  factor_name : string
}
and sample = {
  sample_id : string ;
  sample_data : sample_data ;
  sample_exp : experiment ;
  sample_model : string ;
  sample_condition : (string * string) list ;
}
and sample_data = [
| `short_read_data of short_read_data
]
and experiment = [
| `whole_cell_extract
| `TF_ChIP of string
| `EM_ChIP of string
| `FAIRE
| `mRNA
]
and short_read_data = [
| `fastq of
    [ `sanger | `solexa | `phred64 ] *
    string list se_or_pe
| `sra of
    [`single_end | `paired_end] *
    [ `SRR of string list | `file of string list ]
]
and model = {
  model_id : string ;
  model_genome : genome option ;
  model_annotation : annotation option ;
}
and genome = [
| `ucsc of ucsc_genome
| `fasta of string
]
and annotation = [
  | `ensembl of ensembl_species * int
  | `gff of string
]
and 'a se_or_pe = [
  | `single_end of 'a
  | `paired_end of 'a * 'a
]
and ucsc_genome = [ `dm3 | `hg18 | `hg19 | `mm8 | `mm9 | `mm10 | `sacCer2 ]
and ensembl_species = [
  | `homo_sapiens
  | `mus_musculus
]with sexp

val load : string -> t
val save : t -> string -> unit

val se_or_pe_map : 'a se_or_pe -> f:('a -> 'b) -> 'b se_or_pe

type error = [
  | `undeclared of [`factor | `model | `sample] * string
  | `multiple_declaration of [`factor | `model | `sample] * string
  | `missing_project_description
  | `more_than_one_project_description of string list
  | `missing_factor_in_sample of string * string
  | `repeated_factor_in_sample of string * string
]
with sexp

val check : t -> error list
val error_msg : error -> string

