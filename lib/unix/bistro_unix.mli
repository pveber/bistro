open Bistro

module Cmd : sig
  val wget :
    ?no_check_certificate:bool ->
    ?user_agent:string ->
    ?user:string ->
    ?password:string ->
    ?dest:Shell_dsl.template ->
    string workflow -> Shell_dsl.command

  val gzdep : _ gz file -> Shell_dsl.template
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
  string -> _ file

val wget_dyn :
  ?descr_url:string ->
  ?no_check_certificate:bool ->
  ?user_agent:string ->
  ?user:string ->
  ?password:string ->
  string workflow -> _ file

val gzip : 'a file -> 'a gz file
val gunzip : 'a gz file -> 'a file
val bunzip2 : 'a bz2 file -> 'a file
val unzip : 'a zip file -> 'a path workflow
val tar_xf :
  ?strip_components:int ->
  'a tar file ->
  'a path workflow
val tar_xfz :
  ?strip_components:int ->
  'a tar gz file ->
  'a path workflow
val tar_xfj :
  ?strip_components:int ->
  'a tar bz2 file ->
  'a path workflow
val crlf2lf : (#text as 'a) file -> 'a file

val head :
  n:int ->
  (#text as 'a) file ->
  'a file
