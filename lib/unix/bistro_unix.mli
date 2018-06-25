module File_formats = File_formats
open File_formats
open Bistro


module Cmd : sig
  val wget :
    ?no_check_certificate:bool ->
    ?user:string ->
    ?password:string ->
    ?dest:Shell_dsl.template ->
    string -> Shell_dsl.command
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
