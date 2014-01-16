open Core.Std
open Bistro_workflow

let fprintf fmt = Printf.kfprintf (fun oc -> output_char oc '\n') fmt

let script_calls_of_workflow ~path ~tmp oc = function
  | Input _ as i ->
    fprintf oc "test -e %s || (echo 'Missing input file %s, stopping' ; exit)" (path i) (path i) ;
  | Select (dir, p) ->
    fprintf oc "test -e %s || (echo 'Missing generated file %s, stopping' ; exit)" (path dir) (path dir) ;
    fprintf oc "test -e %s/%s || (echo 'Missing file %s in dir %s, stopping' ; exit)" (path dir) p (path dir) p
  | Rule r as x ->
    fprintf oc "rm -rf %s && mkdir -p %s" (tmp x) (tmp x) ;
    List.iter r.cmds (fun cmd ->
      let line = exec_cmd ~dest:(path x) ~tmp:(tmp x) path cmd in
      fprintf oc "%s" line
    ) ;
    fprintf oc "rm -rf %s" (tmp x)


let to_script db (w : _ t) oc =
  let path x = Bistro_db.path db (x :> u) in
  let tmp x = Bistro_db.tmp_path db (x :> u) in
  fprintf oc "mkdir -p %s" (Bistro_db.cache_dir db) ;
  depth_first_traversal
    ~init:()
    ~f:(fun w () -> script_calls_of_workflow ~path ~tmp oc w)
    w
