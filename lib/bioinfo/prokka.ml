open Core_kernel.Std
open Bistro.EDSL


let env = docker_image ~account:"pveber" ~name:"srst2" ~tag:"1.12" ()

let gram_expr = function
  | `Plus -> string "+"
  | `Minus -> string "-"

let run ?prefix ?addgenes ?locustag ?increment ?gffver ?compliant
    ?centre ?genus ?species ?strain ?plasmid ?kingdom ?gcode ?gram
    ?usegenus ?proteins ?hmms ?metagenome ?rawproduct ?fast ?(threads = 1)
    ?mincontiglen ?evalue ?rfam ?norrna ?notrna ?rnammer fa =
  workflow ~descr:"prokka" ~np:threads ~mem:(3 * 1024) [
    mkdir_p dest ;
    cmd "prokka" ~env [
      string "--force" ;
      option (opt "--prefix" string) prefix ;
      option (flag string "--addgenes") addgenes ;
      option (opt "--locustag" string) locustag ;
      option (opt "--increment" int) increment ;
      option (opt "--gffver" string) gffver ;
      option (flag string "--compliant") compliant ;
      option (opt "--centre" string) centre ;
      option (opt "--genus" string) genus ;
      option (opt "--species" string) species ;
      option (opt "--strain" string) strain ;
      option (opt "--plasmid" string) plasmid ;
      option (opt "--kingdom" string) kingdom ;
      option (opt "--gcode" int) gcode ;
      option (opt "--gram" gram_expr) gram ;
      option (flag string "--usegenus") usegenus ;
      option (opt "--proteins" string) proteins ;
      option (opt "--hmms" string) hmms ;
      option (flag string "--metagenome") metagenome ;
      option (flag string "--rawproduct") rawproduct ;
      option (flag string "--fast") fast ;
      opt "--cpus" ident np ;
      option (opt "--mincontiglen" int) mincontiglen ;
      option (opt "--evalue" float) evalue ;
      option (flag string "--rfam") rfam ;
      option (flag string "--norrna") norrna ;
      option (flag string "--notrna") notrna ;
      option (flag string "--rnammer") rnammer ;
      opt "--outdir" ident dest ;
      dep fa ;
    ] ;
  ]
