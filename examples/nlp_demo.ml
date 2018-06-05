open Bistro

let wikipedia_query q : text_file workflow =
  let url = "https://en.wikipedia.org/api/rest_v1/page/summary/" ^ q in
  shell ~descr:"wikipedia_query" Shell_dsl.[
    pipe [
      cmd "curl" [
        quote ~using:'\'' (string url) ;
      ] ;
      cmd "sed" ~stdout:dest [ string {|-n 's/.*"extract":"\([^"]*\)".*/\1/p'|} ] ;
    ]
  ]

let () =
  let open Repo in
  build ~outdir:"res" [
    item [ "text" ; "protein.txt" ] (wikipedia_query "Protein") ;
  ]
