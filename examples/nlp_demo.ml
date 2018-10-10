open Core_kernel
open Bistro_pack
open Shell_dsl

let wikipedia_query q : text_file workflow =
  let url = "https://en.wikipedia.org/api/rest_v1/page/summary/" ^ q in
  shell ~descr:"wikipedia_query" [
    pipe [
      cmd "curl" [
        quote ~using:'\'' (string url) ;
      ] ;
      cmd "sed" ~stdout:dest [ string {|-n 's/.*"extract":"\(.*\)","extract_html.*/\1/p'|} ] ;
    ]
  ]

let env = docker_image ~account:"pveber" ~name:"stanford-parser" ~tag:"3.9.1" ()

class type stanford_parser_deps = object
  inherit text_file
  method format : [`stanford_parser_deps]
end

let stanford_parser (x : text_file workflow) : stanford_parser_deps workflow =
  shell ~descr:"stanford_parser" [
      cmd ~env "lexparser.sh" ~stdout:dest [ dep x ]
    ]

let sentences_of_stanford_deps (x : stanford_parser_deps workflow) : stanford_parser_deps collection =
  shell ~descr:"sentences_of_stanford_deps" [
    mkdir_p dest ;
    cmd "csplit" [
      string "--quiet --elide-empty-files --suppress-matched" ;
      opt "-f" ident (dest // "sentence") ;
      dep x ;
      string "'/^$/' '{*}'"
    ] ;
  ]
  |> glob ~pattern:"*"

let dependensee (x : stanford_parser_deps workflow) : png workflow =
  shell ~descr:"stanford_dependensee" [
    cmd "java" ~env [
      opt "-cp" string "/usr/bin/DependenSee.2.0.5.jar:/usr/bin/stanford-parser.jar:/usr/bin/stanford-parser-3.3.0-models.jar" ;
      string "com.chaoticity.dependensee.Main" ;
      opt "-t" dep x ;
      dest ;
    ]
  ]

let definition_analysis w =
  let text = wikipedia_query w in
  let deps = stanford_parser text in
  let deps_graphs =
    collection_map (sentences_of_stanford_deps deps) ~f:dependensee
    |> collect_in_directory ~ext:"png"
  in
  Bistro_pack.Repo.[
    item [ "definition.txt" ] text ;
    item [ "deps" ] deps ;
    item [ "deps_graphs" ] deps_graphs ;
  ]
  |> Repo.shift w

let () =
  let open Repo in
  [ "Protein" ; "Cell_(biology)" ]
  |> List.map ~f:definition_analysis
  |> List.concat
  |> build ~np:2 ~outdir:"res" ~loggers:[console_logger ()]
