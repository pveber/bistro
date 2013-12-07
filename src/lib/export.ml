open Core.Std
open Workflow

let fprintf fmt = Printf.kfprintf (fun oc -> output_char oc '\n') fmt

let rec to_script_aux path oc = function
  | Input _ as i ->
    fprintf oc "test -e %s || (echo 'Missing input file %s, stopping' ; exit)" (path i) (path i) ;
  | Select (dir, p) ->
    fprintf oc "test -e %s || (echo 'Missing generated file %s, stopping' ; exit)" (path dir) (path dir) ;
    fprintf oc "test -e %s/%s || (echo 'Missing file %s in dir %s, stopping' ; exit)" (path dir) p (path dir) p ;
  | Rule r as x ->
    List.iter r.deps (to_script_aux path oc) ;
    List.iter r.cmds (fun cmd ->
      let tokens = exec_cmd (path x) path cmd in
      let line = String.concat ~sep:" " tokens in
      fprintf oc "%s" line
    )


let to_script ~cache_dir (w : _ t) oc =
  fprintf oc "mkdir -p %s" cache_dir ;
  to_script_aux (Workflow.path ~cache_dir) oc (w : _ t :> u)
