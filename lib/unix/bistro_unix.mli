open Bistro

module Cmd : sig
  val wget :
    ?no_check_certificate:bool ->
    ?user:string ->
    ?password:string ->
    ?dest:Shell_dsl.template ->
    string workflow -> Shell_dsl.command

  val psgunzip : _ gz pworkflow -> Shell_dsl.template
end

val wget :
  ?descr_url:string ->
  ?no_check_certificate:bool ->
  ?user:string ->
  ?password:string ->
  string -> #file pworkflow

val wget_dyn :
  ?descr_url:string ->
  ?no_check_certificate:bool ->
  ?user:string ->
  ?password:string ->
  string workflow -> #file pworkflow

val gunzip : 'a gz pworkflow -> 'a pworkflow
val bunzip2 : 'a bz2 pworkflow -> 'a pworkflow
val unzip : 'a zip pworkflow -> 'a pworkflow
val tar_xfz :
  ?strip_components:int ->
  'a tar gz pworkflow ->
  'a pworkflow
val tar_xfj :
  ?strip_components:int ->
  'a tar bz2 pworkflow ->
  'a pworkflow
val crlf2lf : (#text_file as 'a) pworkflow -> 'a pworkflow

val head :
  n:int ->
  (#text_file as 'a) pworkflow ->
  'a pworkflow
