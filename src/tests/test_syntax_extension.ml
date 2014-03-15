open OUnit

let assert_equal = assert_equal ~printer:(fun x -> x)
let string_of_script s =
  Bistro_workflow.Script.to_string ~dest:"#DEST" ~tmp:"#TMP" Bistro_workflow.digest s

let test_for_loops () =
  assert_equal ~msg:"Loop without sep"
    "123" (string_of_script <:script<#! i <- [1;2;3]#[#i:i#]>>) ;
  assert_equal ~msg:"Loop with sep"
    "1#2#3" (string_of_script <:script<#! i <- [1;2;3]#[#i:i#][\#]>>)

let tests = "SYNTAX EXTENSION" >::: [
  "For loops" >:: test_for_loops;
]
