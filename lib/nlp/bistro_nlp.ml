open Bistro
open Shell_dsl

let wikipedia_summary q : text_file path workflow =
  let url = "https://en.wikipedia.org/api/rest_v1/page/summary/" ^ q in
  Workflow.shell ~descr:"nlp.wikipedia_summary" [
    pipe [
      cmd "curl" [
        quote ~using:'\'' (string url) ;
      ] ;
      cmd "sed" ~stdout:dest [ string {|-n 's/.*"extract":"\(.*\)","extract_html.*/\1/p'|} ] ;
    ]
  ]

module Stanford_parser = struct
  let env = docker_image ~account:"pveber" ~name:"stanford-parser" ~tag:"3.9.1" ()

  class type deps = object
    inherit text_file
    method format : [`stanford_parser_deps]
  end

  let lexparser (x : text_file path workflow) : deps path workflow =
    Workflow.shell ~descr:"stanford_parser" [
      cmd ~env "lexparser.sh" ~stdout:dest [ dep x ]
    ]
end
