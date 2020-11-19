open Bistro
open Formats

val phyml :
  ?bootstrap:int ->
  ?model:[< `Blosum62
         | `CpREV
         | `DCMut
         | `Dayhoff
         | `F81
         | `F84
         | `GTR
         | `HIVb
         | `HIVw
         | `HKY85
         | `JC69
         | `JTT
         | `K80
         | `LG
         | `MtArt
         | `MtMam
         | `MtREV
         | `RtREV
         | `TN93
         | `VT
         | `WAG ] ->
  ?datatype:[< `AA | `NT ] ->
  ?f:[< `Empirical | `Model ] ->
  ?rand_start:bool ->
  ?n_rand_starts:int ->
  ?r_seed:int ->
  ?branch_length:bool ->
  ?substitution_rate:bool ->
  ?tree_topology:bool ->
  newick file ->
  phylip file ->
  [`phyml] directory

val tree : [`phyml] directory -> newick file
