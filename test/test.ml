open OUnit
open Core.Std
open Bistro

module Scheduler = Bistro_engine.Scheduler
module Db = Bistro_engine.Db

let db = Db.init_exn "_bistro"
let scheduler = Scheduler.make ~np:2 ~mem:1024 db

let build x = Lwt_unix.run (Scheduler.build scheduler x)

(* let add x y = *)
(*   let open Bistro.Term in *)
(*   Bistro.workflow ( *)
(*     prim "add" (fun x y _ -> x + y) *)
(*     $ int x *)
(*     $ int y *)
(*   ) *)

(* let test_add () = assert_bool "test 1 + 1 = 2" (eval (add 1 1) = 2) *)

(* let print_int i = *)
(*   let open Bistro.Term in *)
(*   Bistro.path_workflow ( *)
(*     prim "print_int" (fun i o _ -> Out_channel.write_lines o [ string_of_int i ]) *)
(*     $ workflow i *)
(*   ) *)

(* let test_print_int () = *)
(*   let Bistro.Path fn = eval (print_int (add 1 1)) in *)
(*   assert_equal ~printer:ident "2\n" (In_channel.read_all fn) *)

(* let wc x = *)
(*   let open Bistro.Term in *)
(*   Bistro.workflow ( *)
(*     prim "wc" (fun (Bistro.Path p) _ -> List.length (In_channel.read_lines p)) *)
(*     $ workflow x *)
(*   ) *)

(* let test_input () = *)
(*   assert_raises *)
(*     ~msg:"Eval an input of a non-existent file should raise" *)
(*     (Failure "File aze348753485 is declared as an input of a workflow but does not exist.") *)
(*     (fun () -> eval (Bistro.input "aze348753485")) ; *)
(*   assert_equal ~printer:string_of_int 43 (eval (wc (Bistro.input "_oasis"))) *)

(* let seq i j = *)
(*   let open Bistro.Term in *)
(*   Bistro.path_workflow ( *)
(*     prim "seq" (fun i j output env -> env.Bistro.out *)
(*                    "seq %d %d > %s" i j output ; env.Bistro.shf "seq %d %d > %s" i j output) *)
(*     $ int i *)
(*     $ int j *)
(*   ) *)

(* let test_shell_cmd () = *)
(*   assert_equal ~printer:string_of_int 10 (eval (wc (seq 1 10))) *)

(* let tests = [ *)
(*   "Simple value workflow" >:: test_add ; *)
(*   "Simple path workflow depending on value workflow" >:: test_print_int ; *)
(*   "Input workflows" >:: test_input ; *)
(*   "Shell commands" >:: test_shell_cmd ; *)
(* ] *)

(* let () = *)
(*   ignore(OUnit.run_test_tt_main ("Bistro tests" >::: tests)); *)
