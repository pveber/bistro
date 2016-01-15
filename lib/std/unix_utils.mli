open Types

val wget :
  ?descr_url:string ->
  ?no_check_certificate:bool ->
  string -> (_,_) #file workflow
val gunzip : 'a gz workflow -> 'a workflow
val bunzip2 : 'a bz2 workflow -> 'a workflow
val unzip : 'a zip workflow -> 'a workflow
val tar_xfz : 'a tgz workflow -> 'a workflow
val crlf2lf : (_,[`text]) file workflow -> (_,[`text]) file workflow
