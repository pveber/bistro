open Core_kernel

type template = Workflow.template

let dest = [ Template.DEST ]
let tmp = [ Template.TMP ]
let np = [ Template.NP ]
let mem = [ Template.MEM ]

let string s = [ Template.S s ]
let int i = string (string_of_int i)
let float f = string (Float.to_string f)
let dep w = [ Template.D Workflow.(dep (pureW w)) ]

let quote ?using:(c = '"') e =
  let quote_symbol = Template.S (Char.to_string c) in
  quote_symbol :: e @ [ quote_symbol ]

let option f = function
  | None -> []
  | Some x -> f x

let list f ?(sep = ",") l =
  List.map l ~f
  |> List.intersperse ~sep:(string sep)
  |> List.concat

let seq ?sep xs =
  let format = match sep with
    | None -> ident
    | Some sep -> List.intersperse ~sep:(string sep)
  in
  List.concat (format xs)

let enum dic x = string (List.Assoc.find_exn ~equal:( = ) dic x)

let file_dump contents = [ Template.F contents ] (* FIXME: should check that there is no file_dump in contents *)
