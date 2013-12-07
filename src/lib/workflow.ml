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
}
and cmd =
| A : string -> cmd
| W : u -> cmd
| S : cmd list -> cmd
| Q : cmd -> cmd
| D : cmd
| N : cmd

let digest x =
  Digest.to_hex (Digest.string (Marshal.to_string x []))

let rec path ~cache_dir = function
  | Input p -> p
  | Select (dir, p) ->
    Filename.concat (path ~cache_dir dir) p
  | Rule r as w ->
    Filename.concat cache_dir (digest w)


let quote = sprintf "'%s'"

let exec_cmd dest path x =
  let rec aux = function
    | A a -> [ a ]
    | W w -> [ path w ]
    | S s ->
      List.fold_right (List.map s aux) ~f:( @ ) ~init:[]
    | Q q -> [ quote (aux_quotation q) ]
    | D -> [ dest ]
    | N -> []
  and aux_quotation = function
    | A a -> a
    | W w -> path w
    | S s ->
      String.concat ~sep:"" (List.map s aux_quotation)
    | Q q -> aux_quotation q
    | D -> dest
    | N -> ""
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
    | A _ | D | N -> []
    | S s -> (
      List.map s ~f:aux
      |> List.fold_left ~init:[] ~f:( @ )
    )
    | Q q -> aux q
    | W w -> [ w ]
  in
  List.dedup (aux x)

let make cmds = Rule {
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
