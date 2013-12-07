open Core.Std
open Workflow

let fprintf fmt = Printf.kfprintf (fun oc -> output_char oc '\n') fmt

let script_calls_of_workflow path oc = function
  | Input _ as i ->
    fprintf oc "test -e %s || (echo 'Missing input file %s, stopping' ; exit)" (path i) (path i) ;
  | Select (dir, p) ->
    fprintf oc "test -e %s || (echo 'Missing generated file %s, stopping' ; exit)" (path dir) (path dir) ;
    fprintf oc "test -e %s/%s || (echo 'Missing file %s in dir %s, stopping' ; exit)" (path dir) p (path dir) p
  | Rule r as x ->
    List.iter r.cmds (fun cmd ->
      let tokens = exec_cmd (path x) path cmd in
      let line = String.concat ~sep:" " tokens in
      fprintf oc "%s" line
    )


let to_script ~cache_dir (w : _ t) oc =
  let path = Workflow.path ~cache_dir in
  fprintf oc "mkdir -p %s" cache_dir ;
  depth_first_traversal
    ~init:()
    ~f:(fun w () -> script_calls_of_workflow path oc w)
    w
