open Bistro

val img : container_image list

class type table = object
  inherit tsv
  method header : [`yes]
end

type output =
  <
    comparison_summary : table file ;
    comparisons : ((string * string * string) * table file) list ;
    effect_table : table file ;
    normalized_counts : table file ;
    sample_clustering : svg file ;
    sample_pca : svg file ;
    directory : [`DESeq2] directory ;
  >

val main_effects :
  string list ->
  (string list * #Htseq.count_tsv file) list ->
  output
