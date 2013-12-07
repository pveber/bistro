open Core.Std

type path = string

type workflow =
| Input of path
| Rule of rule
| Select of workflow * path
and rule = {
  cmds : cmd list ;
  deps : workflow list ;
}
and cmd =
| A : string -> cmd (* atom *)
| W : workflow -> cmd (* workflow *)
| S : cmd list -> cmd
| Q : cmd list -> cmd (* inside a quotation, nothing is quoted *)
| D : cmd (* destination *)
| N : cmd (* nothing *)
and 'a t = workflow

(* let quote = sprintf "'%s'" *)

(* let string_of_cmd x = *)
(*   let b = Buffer.create 1024 in *)
(*   let rec aux = function *)
(*     | A s | P s -> *)
(*       Buffer.add_string b (quote s) ; *)
(*       Buffer.add_char b ' ' *)
(*     |  *)

let deps_of_cmd x =
  let rec aux = function
    | A _ | D | N -> []
    | S s | Q s -> (
      List.map s ~f:aux
      |> List.fold_left ~init:[] ~f:( @ )
    )
    | W w -> [ w ]
  in
  List.dedup (aux x)

let digest x =
  Digest.to_hex (Digest.string (Marshal.to_string x []))

let input x = Input x
let make cmds = Rule {
  cmds = cmds ;
  deps = (
    List.map cmds ~f:deps_of_cmd
    |> List.fold_left ~init:[] ~f:( @ )
    |> List.dedup
  )
}

let select dir path = Select (dir, path)

let depends_on wflw dep = match wflw with
  | Rule r ->
    if List.mem r.deps dep
    then wflw
    else Rule {
      r with deps = dep :: r.deps
    }
  | Input _ | Select _ -> wflw
