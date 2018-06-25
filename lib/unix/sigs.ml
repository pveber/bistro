open Bistro_base.File_formats

module type S = sig
  type 'a workflow
  type shell_command
  type template

  include module type of File_formats

  module Unix_tools : sig
    module Cmd : sig
      val wget :
        ?no_check_certificate:bool ->
        ?user:string ->
        ?password:string ->
        ?dest:template ->
        string -> shell_command
    end

    val wget :
      ?descr_url:string ->
      ?no_check_certificate:bool ->
      ?user:string ->
      ?password:string ->
      string -> #file workflow
    val gunzip : 'a gz workflow -> 'a workflow
    val bunzip2 : 'a bz2 workflow -> 'a workflow
    val unzip : 'a zip workflow -> 'a workflow
    val tar_xfz :
      ?strip_components:int ->
      'a tar gz workflow ->
      'a workflow
    val crlf2lf : (#text_file as 'a) workflow -> 'a workflow
  end
end
