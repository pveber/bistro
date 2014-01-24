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
and cmd = Cmd of token list
and token =
| S : string -> token
| I : int -> token
| F : float -> token
| W : u -> token
| L : token list -> token
| D : token
| TMP : token
| Q : token * char -> token

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
  Cmd [S "export PATH=" ; L (aux l) ; S ":$PATH"]


module Cmd = struct
  type 'a t = (cmd -> 'a) -> 'a

  let make f = f (function Cmd l -> Cmd (List.rev l))

  let script l =
    List.map l make

  let cmd prog k = k (Cmd [S prog])

  let string (Cmd tokens) s k = k (Cmd (S s :: tokens))

  let int (Cmd tokens) i k = k (Cmd (I i :: tokens))

  let dest (Cmd tokens) k = k (Cmd (D :: tokens))

  let opt cmd f o k = match o with
    | None -> k cmd
    | Some x -> f cmd x k

  let opt2 cmd f y o k = match o with
    | None -> k cmd
    | Some x -> f cmd y x k

  let arg (Cmd tokens) f v k =
    f (Cmd (S " " :: tokens)) v k

  let argp (Cmd tokens) f o v k =
    f (Cmd (S " " :: S o :: S " " :: tokens)) v k

  let flag (Cmd tokens as cmd) f b k =
    k (
      if b then Cmd (S f :: S " " :: tokens)
      else cmd
    )

  let stdout_to (Cmd tokens) k =
    k (Cmd (S" > " :: tokens))
end


let exec_cmd ~dest ~tmp path (Cmd tokens) =
  let rec token = function
    | S s -> s
    | I i -> string_of_int i
    | F f -> Float.to_string f
    | W w -> path w
    | L l -> token_list l
    | Q (q, c) -> quote c (token q)
    | D -> dest
    | TMP -> tmp
  and token_list l =
    List.fold_right (List.map l token) ~f:( ^ ) ~init:""
  in
  token_list tokens

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

let deps_of_cmd (Cmd tokens) =
  let rec token = function
    | S _ | F _ | I _ | D | TMP -> []
    | L s -> token_list s
    | Q (q,c) -> token q
    | W w -> [ w ]
  and token_list l =
    List.map l ~f:token
    |> List.fold_left ~init:[] ~f:( @ )
  in
  List.dedup (token_list tokens)

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
