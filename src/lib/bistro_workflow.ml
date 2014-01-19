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
| Q : cmd * char -> cmd (* inside a quotation, nothing is quoted *)
| D : cmd (* destination *)
| TMP : cmd
| E : cmd (* empty word *)

let digest x =
  Digest.to_hex (Digest.string (Marshal.to_string x []))

let quote c s = sprintf "%c%s%c" c s c

let export_PATH_cmd l =
  let bindir h = L [ W h ; S "/bin" ] in
  let rec aux = function
    | [] -> []
    | h :: [] -> bindir h :: []
    | h :: t -> bindir h :: (S ":") :: (aux t)
  in
  L [S "export PATH=" ; L (aux l) ; S ":$PATH"]

let exec_cmd ~dest ~tmp path x =
  let rec aux = function
    | S s -> s
    | I i -> string_of_int i
    | F f -> Float.to_string f
    | W w -> path w
    | L l ->
      List.fold_right (List.map l aux) ~f:( ^ ) ~init:""
    | Q (q, c) -> quote c (aux q)
    | D -> dest
    | E -> ""
    | TMP -> tmp
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
    | S _ | F _ | I _ | D | TMP | E -> []
    | L s -> (
      List.map s ~f:aux
      |> List.fold_left ~init:[] ~f:( @ )
    )
    | Q (q,_) -> aux q
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
