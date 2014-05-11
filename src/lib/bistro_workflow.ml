open Core.Std

type path = string

type 'a t = u
and u =
| Input of path
| Rule of rule
| Select of u * path
and rule = {
  script : script ;
  interpreter : interpreter ;
  deps : u list ;
  np : int ;
  mem : int ;
  timeout : duration ;
}
and duration = [`minute | `hour | `day | `week | `month]
and interpreter = [ `bash | `ocaml | `perl | `python | `R | `sh ]
and script = token list
and token =
| S : string -> token
| I : int -> token
| F : float -> token
| W : u -> token
| L : token list -> token
| D : token
| TMP : token

module Types = struct
  type 'a workflow = 'a t

  type 'a directory = [`directory of 'a]
  type 'a file = [`file of 'a]
  type package = [`package] directory

  type 'a zip = [`zip of 'a] file
  type 'a gz = [`gz of 'a file] file
  type 'a tgz = [`tgz of 'a] file
  type pdf = [`pdf] file
end

let digest x =
  Digest.to_hex (Digest.string (Marshal.to_string x []))

let quote c s = sprintf "%c%s%c" c s c

(*
let export_PATH_cmd l =
  let bindir h = L [ W h ; S "/bin" ] in
  let rec aux = function
    | [] -> []
    | h :: [] -> bindir h :: []
    | h :: t -> bindir h :: (S ":") :: (aux t)
  in
  Cmd [S "export PATH=" ; L (aux l) ; S ":$PATH"]
*)

module Shell_script = struct
  type 'a workflow = 'a t
  type t = script
  type 'a cons = (t -> 'a) -> 'a

  let end_ = List.rev

  let begin_ k = k []

  let newline tokens =
    if tokens = [] then tokens else S "\n" :: tokens
  let cmd tokens prog k =
    k (S prog :: (newline tokens))

  let export_path tokens ws k = match ws with
    | [] -> k tokens
    | h :: t ->
      k (
        (List.rev_map t ~f:(fun w -> L [ W w ; S ":" ]))
        @ (W h :: S "export PATH=" :: (newline tokens))
      )

  let string tokens s k = k (S s :: tokens)

  let int tokens i k = k (I i :: tokens)

  let dest tokens k = k (D :: tokens)

  let tmp tokens k = k (TMP :: tokens)

  let workflow tokens w k = k (W w :: tokens)

  let arg' tokens f o k = match o with
    | None -> k tokens
    | Some x -> f (S " " :: tokens) x k

  let arg tokens f v k = arg' tokens f (Some v) k

  let param' tokens f switch o k = match o with
    | None -> k tokens
    | Some x -> f (S " " :: S switch :: S " " :: tokens) x k

  let param tokens f switch x k = param' tokens f switch (Some x) k

  let flag' tokens f b k =
    k (
      if b then S f :: S " " :: tokens
      else tokens
    )

  let flag tokens f k = flag' tokens f true k

  let stdout_to tokens k =
    k (S" > " :: tokens)

end



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

let deps_of_script tokens =
  let rec token = function
    | S _ | F _ | I _ | D | TMP -> []
    | L s -> token_list s
    | W w -> [ w ]
  and token_list l =
    List.map l ~f:token
    |> List.fold_left ~init:[] ~f:( @ )
  in
  List.dedup (token_list tokens)

let make ?(np = 1) ?(mem = 100) ?(timeout = `day) ?(interpreter = `sh) script = Rule {
  np ; mem ; timeout ; script ;
  interpreter ;
  deps = deps_of_script script
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

let script_to_string ~dest ~tmp path tokens =
  let rec token = function
    | S s -> s
    | I i -> string_of_int i
    | F f -> Float.to_string f
    | W w -> path w
    | L l -> token_list l
    | D -> dest
    | TMP -> tmp
  and token_list l =
    List.fold_right (List.map l token) ~f:( ^ ) ~init:""
  in
  token_list tokens

