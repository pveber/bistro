open Core.Std

type path = string

type 'a t = u
and u =
| Input of path
| Rule of rule
| Select of u * path
and rule = {
  cmds : cmd list ;
  deps : u list ;
  np : int ;
  mem : int ;
  timeout : duration ;
}
and duration = [`minute | `hour | `day | `week | `month]
and cmd =
| S : string -> cmd
| I : int -> cmd
| F : float -> cmd
| W : u -> cmd (* workflow *)
| L : cmd list -> cmd
| Q : cmd -> cmd (* inside a quotation, nothing is quoted *)
| D : cmd (* destination *)
| E : cmd (* empty word *)

let digest x =
  Digest.to_hex (Digest.string (Marshal.to_string x []))

let quote = sprintf "'%s'"

let exec_cmd dest path x =
  let rec aux = function
    | S s -> [ s ]
    | I i -> [ string_of_int i ]
    | F f -> [ Float.to_string f ]
    | W w -> [ path w ]
    | L l ->
      List.fold_right (List.map l aux) ~f:( @ ) ~init:[]
    | Q q -> [ quote (aux_quotation q) ]
    | D -> [ dest ]
    | E -> []
  and aux_quotation = function
    | S s -> s
    | I i -> string_of_int i
    | F f -> Float.to_string f
    | W w -> path w
    | L s ->
      String.concat ~sep:"" (List.map s aux_quotation)
    | Q q -> aux_quotation q
    | D -> dest
    | E -> ""
  in
  aux x

let deps = function
  | Input _ -> []
  | Rule r -> r.deps
  | Select (dir,_) -> [ dir ]

let depth_first_traversal w ~init ~f =
  let rec aux marked accu w =
    let id = digest w in
    if String.Set.mem marked id then (marked, accu)
    else (
      let marked, accu = List.fold_left (deps w) ~init:(marked, accu) ~f:(fun (marked, accu) -> aux marked accu) in
      String.Set.add marked id,
      f w accu
    )
  in
  snd (aux String.Set.empty init w)

let input x = Input x

let deps_of_cmd x =
  let rec aux = function
    | S _ | F _ | I _ | D | E -> []
    | L s -> (
      List.map s ~f:aux
      |> List.fold_left ~init:[] ~f:( @ )
    )
    | Q q -> aux q
    | W w -> [ w ]
  in
  List.dedup (aux x)

let make ?(np = 1) ?(mem = 100) ?(timeout = `day) cmds = Rule {
  np ; mem ; timeout ;
  cmds = cmds ;
  deps = (
    List.map cmds ~f:deps_of_cmd
    |> List.fold_left ~init:[] ~f:( @ )
    |> List.dedup
  )
}

let select dir path = Select (dir, path)

let depends wflw ~on:dep = match wflw with
  | Rule r ->
    if List.mem r.deps dep
    then wflw
    else Rule {
      r with deps = dep :: r.deps
    }
  | Input _ | Select _ -> wflw
