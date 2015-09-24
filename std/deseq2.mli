open Types

type output = [`deseq2_output] directory

val wrapper : string list -> (string list * Htseq.count_tsv workflow) list -> output workflow
val index_of_wrapper_output : output workflow -> html workflow
