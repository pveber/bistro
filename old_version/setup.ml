(* setup.ml generated for the first time by OASIS v0.3.1 *)

(* OASIS_START *)
(* DO NOT EDIT (digest: a8ec13643733e93ac1ad6216802516bc) *)
let () =
  try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
  with Not_found -> ();;
#use "topfind";;
#require "oasis.dynrun";;
open OASISDynRun;;
(* OASIS_STOP *)
let () = setup ();;
