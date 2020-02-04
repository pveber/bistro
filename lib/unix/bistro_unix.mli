open Bistro

module Cmd : sig
  val wget :
    ?no_check_certificate:bool ->
    ?user_agent:string ->
    ?user:string ->
    ?password:string ->
    ?dest:Shell_dsl.template ->
    string workflow -> Shell_dsl.command

  val gzdep : _ gz pworkflow -> Shell_dsl.template
  (** Process-substitution construct. Use it to pass a gzipped file
     to a command that expects a decompressed file *)

  val gzdest : Shell_dsl.template
  (** Use [gzdest] instead of [dest] to produce a gzipped file from
     the output of a command. *)
end

val wget :
  ?descr_url:string ->
  ?no_check_certificate:bool ->
  ?user_agent:string ->
  ?user:string ->
  ?password:string ->
  string -> #file pworkflow

val wget_dyn :
  ?descr_url:string ->
  ?no_check_certificate:bool ->
  ?user_agent:string ->
  ?user:string ->
  ?password:string ->
  string workflow -> #file pworkflow

val gzip : (#file as 'a) pworkflow -> 'a gz pworkflow
val gunzip : 'a gz pworkflow -> 'a pworkflow
val bunzip2 : 'a bz2 pworkflow -> 'a pworkflow
val unzip : 'a zip pworkflow -> 'a pworkflow
val tar_xf :
  ?strip_components:int ->
  'a tar pworkflow ->
  'a pworkflow
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
