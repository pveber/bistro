open Bistro

val wikipedia_summary :
  string ->
  text_file path workflow

module Stanford_parser : sig
  val env : Shell_dsl.docker_image

  class type deps = object
    inherit text_file
    method format : [`stanford_parser_deps]
  end

  val lexparser :
    text_file path workflow ->
    deps path workflow

  val dependensee :
    deps path workflow ->
    png path workflow
end

