open Bistro.Std

class type table = object
  inherit tsv
  method header : [`yes]
end

type output =
  <
    comparison_summary : table workflow ;
    comparisons : ((string * string * string) * table workflow) list ;
    effect_table : table workflow ;
    normalized_counts : table workflow ;
    sample_clustering : svg workflow ;
    sample_pca : svg workflow ;
    directory : [ `deseq2_output ] directory workflow
  >

val main_effects :
  string list ->
  (string list * Htseq.count_tsv workflow) list ->
  output
