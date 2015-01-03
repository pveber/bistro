open OUnit
open Core.Std

module Config = struct
  let db_path = "_bistro"
  let np = 2
  let mem = 1024
end

module E = Bistro.Engine(Config)

let eval x = Lwt_unix.run (E.eval x)

let add x y =
  let open Bistro.Term in
  Bistro.workflow (
    prim "add" (fun x y _ -> x + y)
    $ int x
    $ int y
  )

let test_add () = assert_bool "test 1 + 1 = 2" (eval (add 1 1) = 2)

let print_int i =
  let open Bistro.Term in
  Bistro.path_workflow (
    prim "print_int" (fun i o _ -> Out_channel.write_lines o [ string_of_int i ])
    $ workflow i
  )

let test_print_int () =
  let Bistro.Path fn = eval (print_int (add 1 1)) in
  assert_equal ~printer:ident "2\n" (In_channel.read_all fn)

let tests = [
  "Simple value workflow" >:: test_add ;
  "Simple path workflow depending on value workflow" >:: test_print_int ;
]

let () =
  ignore(OUnit.run_test_tt_main ("Bistro tests" >::: tests));
