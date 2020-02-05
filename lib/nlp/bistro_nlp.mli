open Bistro

val wikipedia_summary : string -> text file

module Stanford_parser : sig
  val img : Shell_dsl.container_image list

  class type deps = object
    inherit text
    method format : [`stanford_parser_deps]
  end

  val lexparser :
    text file ->
    deps file

  val dependensee : deps file -> png file
end

