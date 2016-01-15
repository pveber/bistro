open Types

class type table = object
  inherit [ < header : [`yes] ; .. > ] tsv
end

type output =
  <
    comparison_summary : table workflow ;
    comparisons : ((string * string * string) * table workflow) list ;
    effect_table : table workflow ;
    normalized_counts : table workflow ;
    sample_clustering : svg workflow ;
    sample_pca : svg workflow ;
  >

val main_effects :
  string list ->
  (string list * Htseq.count_tsv workflow) list ->
  output
